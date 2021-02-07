;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.alpha.vocabularies.transformation
  (:refer-clojure :exclude [number? integer? array? object?])
  #?@
   (:clj
    [(:require
      [clojure.set :as set]
      [juxt.jinx.alpha.visit :as jinx.visit]
      [juxt.jinx.alpha :as jinx]
      [juxt.jinx.alpha.validate :refer [process-keyword]]
      )]
    :cljs
    [(:require
      [clojure.set :as set]
      [juxt.jinx.alpha.visit :as jinx.visit]
      [juxt.jinx.alpha :as jinx]
      [juxt.jinx.alpha.validate :refer [process-keyword]]
      )]))

(defmethod process-keyword "juxt.jinx.alpha/as" [keyword value instance ctx]
  {::jinx/annotations [{::as value}]})

(defmulti transform-value (fn [as instance] as))

(defmethod transform-value :default [as instance]
  (throw
   (ex-info
    "No implementation for transform-value"
    {::as as
     ::jinx/instance instance})))

(defmethod transform-value nil [as instance]
  ;; Not all properties are transformed. Return nil indicates no transformation required.
  nil)

(defmethod transform-value "uri" [_ instance]
  (java.net.URI. instance))

(defn apply-transformations [report]
  (let [as (first ;; We don't support composition of transformations yet!
            (filter
             #(= (::jinx/keyword %) "juxt.jinx.alpha/as")
             (::jinx/annotations report)))
        transformed-value (transform-value (::as as) (::jinx/instance report))]
    (cond-> report
      (::as as)
      (assoc ::transformed-value transformed-value))))

(defn aggregate-transformations [report]
  (if (seq (::jinx/subschemas report))
    (let [report
          (let [transformed-properties
                (reduce
                 (fn [transformed-properties subschema]

                   (cond-> transformed-properties

                     (::transformed-value subschema)
                     (assoc (or
                             (::jinx/property subschema)
                             (::jinx/index subschema))
                            (::transformed-value subschema))
                     ;; Collect
                     (::transformed-properties subschema)
                     (merge transformed-properties
                            (cond
                              (::jinx/property subschema)
                              {(::jinx/property subschema) (::transformed-properties subschema)}
                              (::jinx/index subschema)
                              {(::jinx/index subschema) (::transformed-properties subschema)}
                              :else
                              (::transformed-properties subschema)))))
                 {}
                 (::jinx/subschemas report))]

            (assoc report ::transformed-properties transformed-properties))]
      (update
       report
       ::jinx/instance
       (fn [instance]
         (reduce
          (fn [acc [k v]]
            (assoc acc k v))
          instance
          (::transformed-properties report)))))
    report))

(defn process-transformations [report]
  (jinx.visit/visit-report report apply-transformations aggregate-transformations))
