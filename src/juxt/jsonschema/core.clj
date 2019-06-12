(ns juxt.jsonschema.core)

(defn array? [x]
  (sequential? x))

(defn object? [x]
  (map? x))

(defn schema? [x]
  (or (object? x) (boolean? x)))
