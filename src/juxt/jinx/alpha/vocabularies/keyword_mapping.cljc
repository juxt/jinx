;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.alpha.vocabularies.keyword-mapping
  (:refer-clojure :exclude [number? integer? array? object?])
  #?@
   (:clj
    [(:require
      [clojure.set :as set]
      [clojure.pprint :refer [pprint]]
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

(defmethod process-keyword "juxt.jinx.alpha/keyword-mappings" [keyword value instance ctx]
  {::jinx/annotations [{::map-to-keywords value}]})

(defn keywordize-keyword-mapping-map-local [m]
  (reduce-kv
   (fn [acc k v]
     (assoc acc k (keyword v)))
   {} m))

(defn apply-keyword-mappings [report]
  (let [mappings (apply
                  merge                 ; compose multiple possible annotations
                  (map ::map-to-keywords
                       (filter
                        #(= (::jinx/keyword %) "juxt.jinx.alpha/keyword-mappings")
                        (::jinx/annotations report))))
        ]
    (cond-> report
      (seq mappings)
      (-> (assoc ::remapped-properties (keywordize-keyword-mapping-map-local mappings))))))

(defn aggregate-keyword-mappings [report]
  (if (seq (::jinx/subschemas report))
    (let [remapped-properties
          (reduce
           (fn [acc subschema]
             (merge acc (::remapped-properties subschema)))
           (::remapped-properties report)
           (::jinx/subschemas report))]
      (-> report
          (assoc
           ::remapped-properties
           (if-let [prop (::jinx/property report)]
             {[prop] remapped-properties}
             remapped-properties))))
    report))

(defn update-instance-with-mappings [instance mappings]
  (cond
    (and (map? instance) (map? mappings))
    (reduce-kv
     (fn [acc k v]
       (assoc
        acc
        (get mappings k k)
        (update-instance-with-mappings v (get mappings [k]))))
     {}
     instance)

    (and (vector? instance) (map? mappings))
    (map #(update-instance-with-mappings % mappings) instance)

    :else instance))

(defn process-keyword-mappings [report]
  (let [{::keys [remapped-properties] :as report}
        (jinx.visit/visit-report report apply-keyword-mappings aggregate-keyword-mappings)]
    (update report ::jinx/instance update-instance-with-mappings remapped-properties)))
