;; Copyright © 2019, JUXT LTD.


(ns juxt.jsonschema.resolve
  (:require
   [juxt.jsonschema.schema :as schema]
   [juxt.jsonschema.jsonpointer :as jsonpointer]
   [clojure.string :as str]
   [juxt.jsonschema.jmacro :as m :refer-macros [parse-stream-cljc]]))
  ; (:require-macros [juxt.jsonschema.jmacro :as m :refer [parse-stream-cljc]]))

(defmulti resolve-uri (fn [k uri] (cond (keyword? k) k (coll? k) (first k))))

;; Built-in

(def built-in-schemas
  {"http://json-schema.org/draft-07/schema" "schemas/json-schema.org/draft-07/schema"})

(defmethod resolve-uri ::built-in [_ uri]
  (when-let [res (built-in-schemas uri)]
    (m/parse-stream-cljc  "./resources/schemas/json-schema.org/draft-07/schema")))


(defprotocol DefaultResolverDereferencer
  (deref-val [_ k] "Dereference"))

(extend-protocol DefaultResolverDereferencer
  java.net.URL
  (deref-val [res k]
    (m/parse-stream-cljc res))

  #?(:clj Boolean :cljs boolean (deref-val [res k] res))

  clojure.lang.IPersistentMap
  (deref-val [res k] res)

  clojure.lang.Fn
  (deref-val [f k] (deref-val (f k) k))

  java.io.File
  (deref-val [file k] (m/parse-stream-cljc file))
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
