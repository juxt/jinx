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
