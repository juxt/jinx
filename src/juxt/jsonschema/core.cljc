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
  (let [valid? (atom true)]
    (try
      #?(:clj (java.util.regex.Pattern/compile x) :cljs (new js/RegExp. x))
      (catch #?(:clj Exception :cljs js/Error) e
        (reset! valid? false)))
    @valid?))
