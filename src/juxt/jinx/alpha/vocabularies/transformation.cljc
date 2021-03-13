;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.alpha.vocabularies.transformation
  (:refer-clojure :exclude [number? integer? array? object?])
  #?@
   (:clj
    [(:require
      [clojure.edn :as edn]
      [clojure.set :as set]
      [juxt.jinx.alpha.visit :as jinx.visit]
      [juxt.jinx.alpha :as jinx]
      [juxt.jinx.alpha.validate :refer [process-keyword]]
      )]
    :cljs
    [(:require
      [clojure.edn :as edn]
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

(defmethod transform-value "keyword" [_ instance]
  (keyword instance))

(defmethod transform-value "edn" [_ instance]
  (edn/read-string instance))

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
    (let [transformed-values
          (cond
            (= (get-in report [::jinx/schema "type"]) "array")
            (reduce
             (fn [acc subschema]

               (cond

                 (::transformed-value subschema)
                 (conj acc (::transformed-value subschema))

                 ;; Collect
                 (::transformed-values subschema)
                 (conj acc (::transformed-values subschema))

                 :else
                 (conj acc (::jinx/instance subschema))
                 ))
             []
             (::jinx/subschemas report))

            :else
            (reduce
             (fn [acc subschema]

               (cond-> acc

                 :always
                 (merge acc
                        (cond
                          (::jinx/property subschema)
                          {(::jinx/property subschema) (::jinx/instance subschema)}

                          :else
                          (::jinx/instance subschema)))

                 (::transformed-value subschema)
                 (assoc (::jinx/property subschema)
                        (::transformed-value subschema))

                 ;; Collect
                 (::transformed-values subschema)
                 (merge acc
                        (cond
                          (::jinx/property subschema)
                          {(::jinx/property subschema) (::transformed-values subschema)}

                          :else
                          (::transformed-values subschema)))))
             {}
             (::jinx/subschemas report)))]

      (cond
        (= (get-in report [::jinx/schema "type"]) "array")
        (-> report
            (assoc
             ::type :array
             ::transformed-values transformed-values
             ::jinx/instance transformed-values))

        :else
        (-> report
            (assoc ::type :object
                   ::transformed-values transformed-values)
            (update ::jinx/instance
                    merge transformed-values
                    ))))
    report))

(defn process-transformations [report]
  (jinx.visit/visit-report report apply-transformations aggregate-transformations))
