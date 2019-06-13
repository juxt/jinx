(ns juxt.jsonschema.core
  (:refer-clojure :exclude [number? integer?]))

(defn number? [x]
  (clojure.core/number? x))

(defn integer? [x]
  (or
   (clojure.core/integer? x)
   (when (number? x)
     (zero? (mod x 1)))))

(defn array? [x]
  (sequential? x))

(defn object? [x]
  (map? x))

(defn schema? [x]
  (or (object? x) (boolean? x)))
