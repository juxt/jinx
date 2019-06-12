;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.resolve
  (:require
   [juxt.jsonschema.schema :as schema]
   [clojure.string :as str]
   #?@(:clj [[cheshire.core :as cheshire]
             [clojure.java.io :as io]])
   [juxt.jsonschema.jsonpointer :as jsonpointer]
   #?(:cljs (:require-macros [juxt.jsonschema.resolve :refer [slurp-resource ]]))))

(defmulti resolve-uri (fn [k uri] (cond (keyword? k) k (coll? k) (first k))))

(defn read-json-string [json-str]
  #?(:clj
     (cheshire/parse-string json-str)
     :cljs (js/JSON.parse json-str)))

(defn read-json-string-rdr [json-str]
  #?(:clj
     (cheshire/parse-stream (io/reader json-str))
     :cljs (js/JSON.parse json-str)))

(defmacro slurp-resource [resource]
  (slurp (str "resources/" resource)))

;; Built-in

(def built-in-schemas
  {"http://json-schema.org/draft-07/schema" (slurp-resource "schemas/json-schema.org/draft-07/schema")})

(defmethod resolve-uri ::built-in [_ uri]
  (when-let [res (built-in-schemas uri)]
    (read-json-string res)))

(defprotocol DefaultResolverDereferencer
  (deref-val [_ k] "Dereference"))

(extend-protocol DefaultResolverDereferencer
  java.net.URL
  (deref-val [res k]
    (read-json-string-rdr res))

  #?(:clj Boolean :cljs boolean (deref-val [res k] res))

  clojure.lang.IPersistentMap
  (deref-val [res k] res)

  clojure.lang.Fn
  (deref-val [f k] (deref-val (f k) k))

  java.io.File
  (deref-val [file k] (read-json-string-rdr file))
  )

(defmethod resolve-uri ::default-resolver [[_ m] ^String uri]
  (when-let
      [[k val]
       (or
        ;; First strategy: lookup the url directly
        (find m uri)

        ;; Second, find a matching regex
        (some (fn [[pattern v]]
                 (when (instance? java.util.regex.Pattern pattern)
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

          (throw (ex-info (format "Failed to resolve uri: %s" docref) {:uri docref})))))))
