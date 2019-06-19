(ns juxt.jsonschema.core
  (:refer-clojure :exclude [number? integer? array? object?]))

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

(defn regex? [x]
  ;; TODO: Check is x is a valid regular expression "according to the
  ;; ECMA 262 regular expression dialect"
  (string? x))