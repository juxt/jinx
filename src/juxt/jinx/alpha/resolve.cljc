;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.alpha.resolve
  #?@
   (:clj
    [(:require
      [cheshire.core :as cheshire]
      [clojure.java.io :as io]
      [clojure.string :as str]
      [clojure.walk :refer [postwalk]]
      [juxt.jinx.alpha.jsonpointer :as jsonpointer]
      [lambdaisland.uri :as uri])]
    :cljs
    [(:require
      [cljs-node-io.core :as io :refer [slurp]]
      [cljs-node-io.file :refer [File]]
      [clojure.string :as str]
      [clojure.walk :refer [postwalk]]
      [juxt.jinx.alpha.jsonpointer :as jsonpointer]
      [lambdaisland.uri :as uri])
     (:require-macros [juxt.jinx.alpha.resolve :refer [slurp-resource]])
     (:import goog.Uri)]))

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

(defn- resolv [uri doc resolvers]
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


(defn resolve-ref [ref-object doc ctx]
  (assert ref-object)

  (let [;; "The value of the "$ref" property MUST be a URI Reference."
        ;; -- [CORE Section 8.3]
        base-uri (get (meta ref-object) :base-uri)
        ref  #?(:clj (some-> (get ref-object "$ref") java.net.URLDecoder/decode)
                :cljs (some-> (get ref-object "$ref") js/decodeURIComponent))
        uri (str (uri/join (or base-uri (:base-uri ctx)) ref))]

    (let [options
          (if false #_(contains? (:visited-memory ctx) uri)
              (throw (ex-info "Infinite cycle detected" {:uri uri}))
              (update ctx :visited-memory (fnil conj #{}) uri))]

      (let [[new-schema doc base-uri] (resolv uri doc (get-in ctx [:options :resolvers]))]
        [new-schema (cond-> ctx
                      base-uri (assoc :base-uri base-uri)
                      doc (assoc :doc doc))]))))


(defn expand-document
  ([doc ctx]
   (expand-document doc doc ctx))
  ([doc parent ctx]
   (postwalk
    (fn [m]
      (if (and (map? m) (contains? m "$ref"))
        (let [[new-doc new-ctx] (resolve-ref m parent ctx)]
          (expand-document new-doc parent new-ctx))
        m))
    doc)))
