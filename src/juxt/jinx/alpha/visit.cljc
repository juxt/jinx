;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.alpha.visit
  (:refer-clojure :exclude [number? integer? array? object?])
  #?@
   (:clj
    [(:require
      [clojure.set :as set]
      [juxt.jinx.alpha :as jinx]
      [juxt.jinx.alpha.validate :refer [process-keyword]]
      )]
    :cljs
    [(:require
      [clojure.set :as set]
      [juxt.jinx.alpha :as jinx]
      [juxt.jinx.alpha.validate :refer [process-keyword]]
      )]))

(defn errors
  "Extract all errors from a report or subschema."
  [subschema]
  (concat
   (::jinx/errors subschema)
   (mapcat errors (::jinx/subschemas subschema))))

(defn visit-report [report inner outer]
  (outer
   (let [subschemas (::jinx/subschemas report)]
     (cond-> report
       (seq subschemas)
       (update
        ::jinx/subschemas
        (fn [subschemas] (mapv #(visit-report % inner outer) subschemas)))
       inner (inner)))))

(defmethod process-keyword "juxt/coerce" [keyword value instance ctx]
  {::jinx/annotations [{::coerce-to value}]})

(defmulti coerce-value (fn [coerce-to instance] coerce-to))

(defmethod coerce-value :default [coerce-to instance]
  (throw
   (ex-info
    "No implementation for coerce-value"
    {::jinx/coerce-to coerce-to
     ::jinx/instance instance})))

(defmethod coerce-value nil [coerce-to instance]
  ;; Not all properties are coerced. Return nil indicates no coercion required.
  nil)

(defmethod coerce-value "uri" [_ instance]
  (java.net.URI. instance))

(defn apply-coercions [report]
  (let [coercion (first ;; We don't support composition of coercions yet!
                  (filter
                   #(= (::jinx/keyword %) "juxt/coerce")
                   (::jinx/annotations report)))
        coerced-value (coerce-value (::coerce-to coercion) (::jinx/instance report))]
    (cond-> report
      (::coerce-to coercion)
      (assoc ::coerced-value coerced-value))))

(defn aggregate-coercions [report]
  (if (seq (::jinx/subschemas report))
    (let [report
          (let [coerced-properties
                (reduce
                 (fn [coerced-properties subschema]

                   (cond-> coerced-properties

                     (::coerced-value subschema)
                     (assoc (::jinx/property subschema) (::coerced-value subschema))
                     ;; Collect
                     (::coerced-properties subschema)
                     (merge coerced-properties
                            (if-let [prop (::jinx/property subschema)]
                              {prop (::coerced-properties subschema)}
                              (::coerced-properties subschema)))))

                 {}
                 (::jinx/subschemas report))]

            (assoc report ::coerced-properties coerced-properties))]
      (-> report
          (update ::jinx/instance merge (::coerced-properties report))))
    report))

(defmethod process-keyword "juxt/keyword-mappings" [keyword value instance ctx]
  {::jinx/annotations [{::map-to-keywords value}]})

(defn keywordize-keyword-mapping-map [m]
  (reduce-kv
   (fn [acc k v]
     (assoc acc k (keyword v)))
   {} m))

(defn apply-keyword-mappings [report]
  (let [kw-mappings
        (keywordize-keyword-mapping-map
         (apply
          merge                         ; compose multiple possible annotations
          (map ::map-to-keywords
               (filter
                #(= (::jinx/keyword %) "juxt/keyword-mappings")
                (::jinx/annotations report)))))]
    (cond-> report
      (seq kw-mappings)
      (assoc ::remapped-properties kw-mappings))))

(defn aggregate-keyword-mappings [report]
  (if (seq (::jinx/subschemas report))
    (let [remapped-properties
          (reduce
           (fn [acc subschema]
             (merge acc (::remapped-properties subschema)))
           {}
           (::jinx/subschemas report))]
      (update report ::jinx/instance set/rename-keys remapped-properties))
    report))
