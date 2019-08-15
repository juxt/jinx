;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.resolve
  (:require
   [juxt.jinx.schema :as schema]
   [clojure.string :as str]
   [juxt.jinx.jsonpointer :as jsonpointer]
   #?@(:clj [[cheshire.core :as cheshire]
             [clojure.java.io :as io]]
       :cljs [[cljs-node-io.file :refer [File]]
              [cljs-node-io.core :as io :refer [slurp]]]))
  #?(:cljs (:import goog.Uri))
  #?(:cljs (:require-macros [juxt.jinx.resolve :refer [slurp-resource]])))

#?(:clj
   (defmacro slurp-resource [resource]
     (clojure.core/slurp (io/resource resource))))

(defn read-json-string [json-str]
  #?(:clj (cheshire/parse-string json-str)
     :cljs (js->clj (js/JSON.parse json-str))))

(defn read-json-stream [json-str]
  #?(:clj (cheshire/parse-stream (io/reader json-str))
     :cljs (js->clj (js/JSON.parse (slurp json-str)))))

(defmulti resolve-uri
  (fn [k uri]
    (cond
      (keyword? k) k
      (coll? k) (first k))))

;; Built-in

(def built-in-schemas
  {"http://json-schema.org/draft-07/schema" (slurp-resource "schemas/json-schema.org/draft-07/schema")})

(defmethod resolve-uri ::built-in [_ uri]
  (when-let [res (built-in-schemas uri)]
    (read-json-string res)))

(defprotocol DefaultResolverDereferencer
  (deref-val [_ k] "Dereference"))

(extend-protocol DefaultResolverDereferencer
  #?(:clj java.net.URL :cljs goog.Uri)
  (deref-val [res k] (read-json-stream res))

  #?(:clj Boolean :cljs boolean)
  (deref-val [res k] res)

  #?(:clj  clojure.lang.IPersistentMap :cljs cljs.core/PersistentArrayMap)
  (deref-val [res k] res)

  #?(:clj clojure.lang.Fn :cljs function)
  (deref-val [f k] (deref-val (f k) k))

  #?(:clj java.io.File :cljs cljs-node-io.file/File)
  (deref-val [file k]
    #?(:clj (read-json-stream file)
       :cljs (js->clj (read-json-stream file)))))

(defmethod resolve-uri ::default-resolver [[xx m] ^String uri]
  (when-let
   [[k val]
    (or
     ;; First strategy: lookup the url directly
     (find m uri)

     ;; Second, find a matching regex
     (some (fn [[pattern v]]
             (when #?(:clj (instance? java.util.regex.Pattern pattern)
                      :cljs (regexp?  pattern))
               (when-let [match (re-matches pattern uri)]
                 [match v])))
           m))]

    (deref-val val k)))

(defmethod resolve-uri ::function [[_ f] ^String uri]
  (f uri))

(defn resolv [uri doc resolvers]
  "Return a vector of [schema new-doc & [new-base-uri]]."
  ;; TODO: Return a map rather than vector
  (let [[docref fragment] (str/split uri #"#")]

    (if (empty? docref)
      [(jsonpointer/json-pointer doc fragment) doc]

      (if-let [embedded-schema (-> doc meta :uri->schema (get docref))]
        [(jsonpointer/json-pointer embedded-schema fragment)
         doc
         docref]

        (if-let [doc (some
                      (fn [resolver] (resolve-uri resolver docref))
                      resolvers)]
          [(jsonpointer/json-pointer doc fragment)
           doc
           docref]

          (throw (ex-info (str "Failed to resolve uri: " docref) {:uri docref})))))))
