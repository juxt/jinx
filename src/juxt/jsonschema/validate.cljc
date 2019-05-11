;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.validate
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is are]]))

(declare validate)

;; All references here relate to
;; draft-handrews-json-schema-validation-01.txt unless otherwise
;; stated.

(defmulti check-assertion
  "Allow for additional vocabularies by making this extensible.

  'Validation keywords typically operate independently, without
   affecting each other's outcomes.' -- 3.1.1

  However, given there are some exceptions, the full schema object is
  also provided as a map.
  "
  (fn [keyword value schema data] keyword))

(defmethod check-assertion :default [keyword type schema data]
  ;; This is related to:
  ;;
  ;; "Assertion keywords that are absent never restrict validation."
  ;; -- 3.2
  ;;
  ;; Do not error if a keyword isn't supported.
  nil)

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

(defmethod check-assertion "type" [_ type schema data]
  (cond
    (string? type)
    (when (not ((type-preds type) data))
      [{:message (format "Value must be of type %s" type)}])
    (sequential? type)
    (when-not ((apply some-fn (vals (select-keys type-preds type))) data)
      [{:message (format "Value must be of type %s" (str/join " or " type))}])))

(defmethod check-assertion "enum" [_ enum schema data]
  (when-not (contains? (set enum) data)
    [{:message (format "Value %s must be in enum %s" data enum)}]))

(defmethod check-assertion "const" [_ const schema data]
  (when-not (= const data)
    [{:message (format "Value %s must be equal to const %s" data const)}]))

(defmethod check-assertion "maxLength" [_ max-length schema data]
  (when (string? data)
    ;; See https://github.com/networknt/json-schema-validator/issues/4
    (when (> (.codePointCount data 0 (.length data)) max-length)
      [{:message "String is too long"}])))

(defmethod check-assertion "minLength" [_ min-length schema data]
  (when (string? data)
    (when (< (.codePointCount data 0 (.length data)) min-length)
      [{:message "String is too short"}])))

(defmethod check-assertion "pattern" [_ pattern schema data]
  (when (string? data)
    (when-not (re-seq (re-pattern pattern) data)
      [{:message (format "String does not match pattern %s" pattern)}])))

(defmethod check-assertion "properties" [_ properties schema data]
  (when (map? data)
    (if (not (map? data))
      [{:message "Must be an object"}]
      (apply concat
             (for [[k v] properties
                   :let [data (get data k)]
                   :when data]
               (validate {:path ["properties" k]} v data))))))

(defmethod check-assertion "required" [_ required schema data]
  (when (map? data)
    (when-not (set/subset? (set required) (set (keys data)))
      [{:message "Missing required property"}])))

(defn validate
  ([schema data]
   (validate {} schema data))
  ([jsonpointer schema data]
   ;; We keep trying to find errors, returning them in a list.

   ;; Start with an ordered list of known of validation keywords,
   ;; before moving onto the validation keywords that have not yet
   ;; been processed. This allows for the errors to be fairly
   ;; determinstic and in the order expected, while allowing for
   ;; extension.
   (loop [keywords ["type" "enum" "const" "properties" "required"]
          schema schema
          data data
          errors []]
     (if-let [k (or (first keywords) (first (keys schema)))]
       (recur
        (next keywords)
        (dissoc schema k)
        data
        (cond-> errors
          (contains? schema k)
          (concat (check-assertion k (get schema k) schema data))))
       ;; Finally, return the errors (even if empty).
       errors))))
