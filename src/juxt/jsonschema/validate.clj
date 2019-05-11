(ns juxt.jsonschema.validate
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is are]]))

(declare validate)

;; TODO: These must check against JavaScript primitive types,
;; not Clojure/Java ones
(def type-preds
  {"null" nil?
   "boolean" boolean?
   "object" map?
   "array" sequential?
   "number" number?
   "string" string?
   "integer" integer?})

(defn check-type [schema data]
  (when-let [type (get schema "type")]
    (cond
      (string? type)
      (when (not ((type-preds type) data))
        [{:message (format "Value must be of type %s" type)}])
      (vector? type)
      (when-not ((apply some-fn (vals (select-keys type-preds type))) data)
        [{:message (format "Value must be of type %s" (str/join " or " type))}]))))

;; NOTE: loop/recur may be a better design to be able to stop
;; (optionally) on the first errors.

(defn check-enum [schema data]
  (when-let [enum (get schema "enum")]
    (when-not (contains? (set enum) data)
      [{:message (format "Value %s must be in enum %s" data enum)}])))

(defn check-const [schema data]
  (when-let [const (get schema "const")]
    (when-not (= const data)
      [{:message (format "Value %s must be equal to const %s" data const)}])))

(defn check-object [schema data]
  (when (= (get schema "type") "object")
    (let [properties (get schema "properties")]
      (if (not (map? data))
        [{:message "Must be an object"}]
        (concat
         (when-let [required (get schema "required")]
           (when-not (set/subset? (set required) (set (keys data)))
             [{:message "Missing required property"}]))
         (when properties
           (apply concat
                  (for [[k v] properties
                        :let [data (get data k)]
                        :when data]
                    (validate {:path ["properties" k]} v data)))))))))

(defn validate
  ([schema data]
   (validate {} schema data))
  ([jsonpointer schema data]
   ;; We keep trying to find errors, returning them in a list
   (->>
    (concat
     (check-type schema data)
     (check-enum schema data)
     (check-const schema data)
     (check-object schema data))
    (map #(merge {:data data :schema schema} %)))))
