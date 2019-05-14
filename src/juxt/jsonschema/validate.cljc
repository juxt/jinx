;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.validate
  (:refer-clojure :exclude [number?])
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is are]]
   [juxt.jsonschema.jsonpointer :as jsonpointer]
   [lambdaisland.uri :as uri]))

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
  ;; TODO: Document reason for 'schema' (to pull out additionalProperties, etc.)
  (fn [keyword ctx value schema instance] keyword))

(defmethod check-assertion :default [_ _ _ _ _]
  ;; A JSON Schema MAY contain properties which are not schema
  ;; keywords. Unknown keywords SHOULD be ignored. -- JSON Schema Core, 4.3.1
  ;;
  ;; Do not error if a method for a given keyword isn't defined, so we
  ;; return nil.
  nil)

;; TODO: These must check against JavaScript primitive types,
;; not Clojure/Java ones

(defn number? [x]
  (clojure.core/number? x))

(def type-preds
  {"null" nil?
   "boolean" boolean?
   "object" map?
   "array" sequential?
   "number" number?
   "string" string?
   "integer" integer?})

(defmethod check-assertion "type" [_ ctx type schema instance]
  (cond
    (string? type)
    (when-not ((type-preds type) instance)
      [{:message (format "Value must be of type %s" type)}])
    (sequential? type)
    (when-not ((apply some-fn (vals (select-keys type-preds type))) instance)
      [{:message (format "Value must be of type %s" (str/join " or " type))}])))

(defmethod check-assertion "enum" [_ ctx enum schema instance]
  (when-not (contains? (set enum) instance)
    [{:message (format "Value %s must be in enum %s" instance enum)}]))

(defmethod check-assertion "const" [_ ctx const schema instance]
  (when-not (= const instance)
    [{:message (format "Value %s must be equal to const %s" instance const)}]))

(defmethod check-assertion "multipleOf" [_ ctx multiple-of schema instance]
  (when (number? instance)
    (when-not (= 0 (.compareTo (.remainder (bigdec instance) (bigdec multiple-of)) BigDecimal/ZERO))
      [{:message "Failed multipleOf check"}])))

(defmethod check-assertion "maximum" [_ ctx maximum schema instance]
  (when (number? instance)
    (when-not (<= instance maximum)
      [{:message "Failed maximum check"}])))

(defmethod check-assertion "exclusiveMaximum" [_ ctx exclusive-maximum schema instance]
  (when (number? instance)
    (when-not (< instance exclusive-maximum)
      [{:message "Failed exclusiveMaximum check"}])))

(defmethod check-assertion "maxLength" [_ ctx max-length schema instance]
  (when (string? instance)
    ;; See https://github.com/networknt/json-schema-validator/issues/4
    (when (> (.codePointCount instance 0 (.length instance)) max-length)
      [{:message "String is too long"}])))

(defmethod check-assertion "minLength" [_ ctx min-length schema instance]
  (when (string? instance)
    (when (<
           #?(:clj (.codePointCount instance 0 (.length instance))
              :cljs (count instance))
           min-length)
      [{:message "String is too short"}])))

(defmethod check-assertion "pattern" [_ ctx pattern schema instance]
  (when (string? instance)
    (when-not (re-seq (re-pattern pattern) instance)
      [{:message (format "String does not match pattern %s" pattern)}])))

;; TODO: Rename schema to subschema
;; TODO: Push ctx through
;; TODO: Replace 'apply concat' with 'mapcat seq'
;; TODO: Show paths in error messages
;; TODO: Improve error messages, possibly copying Ajv or org.everit json-schema

(defmethod check-assertion "items" [_ ctx items schema instance]
  (when (sequential? instance)
    (cond
      (map? items)
      (apply concat
             (for [[idx instance] (map-indexed vector instance)]
               (validate (update ctx :path (fnil conj []) idx) items instance)))

      (sequential? items)
      ;; TODO: Consider short-circuiting
      (apply concat
             (for [[idx schema instance] (map vector (range) (concat items (repeat (get schema "additionalItems"))) instance)]
               (validate (update ctx :path (fnil conj []) idx) schema instance)))

      (boolean? items)
      (when (and (false? items) (not-empty instance))
        [{:message "Items must be empty to satisfy a false schema"}]))))

(defmethod check-assertion "maxItems" [_ ctx max-items schema instance]
  (when (sequential? instance)
    (when (> (count instance) max-items)
      [{:message "maxItems exceeded"}])))

(defmethod check-assertion "minItems" [_ ctx min-items schema instance]
  (when (sequential? instance)
    (when (< (count instance) min-items)
      [{:message "minItems not reached"}])))

(defmethod check-assertion "uniqueItems" [_ ctx unique-items? schema instance]
  (when (and (sequential? instance) unique-items?)
    (when-not (apply distinct? instance)
      [{:message "Instance elements are not all unique"}])))

(defmethod check-assertion "contains" [_ ctx contains schema instance]
  (when (sequential? instance)
    (when-not (some #(empty? (validate ctx contains %)) instance)
      [{:message "Instance is not valid against schema"}])))

(defmethod check-assertion "properties" [_ ctx properties schema instance]
  (when (map? instance)
    (if (not (map? instance))
      [{:message "Must be an object"}]
      (apply concat
             (for [[k v] properties
                   :let [instance (get instance k)]
                   :when instance]
               (validate (update ctx :path (fnil conj []) "properties" k) v instance))))))

(defmethod check-assertion "required" [_ ctx required schema instance]
  (when (map? instance)
    (when-not (set/subset? (set required) (set (keys instance)))
      [{:message "Missing required property"}])))

(defn resolve-ref [ctx ref]
  (let [[uri fragment] (str/split ref #"#")]
    (if (empty? uri)
      [ctx (jsonpointer/json-pointer (:doc ctx) fragment)]

      (let [uri (uri/join (:base-uri ctx) uri)]
        ;; TODO: Resolve uri, then load schema doc, assoc ctx :doc
        (throw (ex-info "TODO: Resolve"
                        {:uri uri
                         :fragment fragment}))))))

(defn validate
  ([schema instance]
   (validate {:doc schema} schema instance))

  ([ctx schema instance]
   ;; We keep trying to find errors, returning them in a list.
   (cond
     (and (map? schema) (contains? schema "$id"))
     (validate
      (update ctx :base-uri uri/join (get schema "$id"))
      (dissoc schema "$id")             ; avoid stack-overflow!
      instance)

     (and (map? schema) (contains? schema "$ref"))
     (let [[new-ctx new-schema] (resolve-ref ctx (get schema "$ref"))]
       (validate new-ctx new-schema instance))

     (boolean? schema)
     (if schema [] [{:message "Schema is boolean false"}])

     :else
     ;; Start with an ordered list of known of validation keywords,
     ;; before moving onto the validation keywords that have not yet
     ;; been processed. This allows for the errors to be fairly
     ;; determinstic and in the order expected, while allowing for
     ;; extension.
     (loop [keywords ["type" "enum" "const" "properties" "required"]
            schema schema
            other-keywords (set (keys schema))
            instance instance
            errors []]
       ;; TODO: What if schema is a boolean schema?
       (if-let [k (or (first keywords) (first other-keywords))]
         (recur
          (next keywords)
          schema
          (disj other-keywords k)
          instance
          (cond-> errors
            (contains? schema k)
            (concat (check-assertion k ctx (get schema k) schema instance))))
         ;; Finally, return the errors (even if empty).
         errors)))))



#_(let [test
      {:filename "uniqueItems.json",
       :test-group-description "uniqueItems validation",
       :test-description "non-unique array of integers is invalid",
       :schema {"uniqueItems" true},
       :data [1 1],
       :valid false,
       :failures [{:message "Incorrectly judged valid"}]}]

  (validate
   (:schema test)
   (:data test)))
