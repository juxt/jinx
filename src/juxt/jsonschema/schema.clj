;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.schema
  (:refer-clojure :exclude [number? integer?])
  (:require
   [juxt.jsonschema.core :refer [number? integer? array? object? schema? regex?]]
   [lambdaisland.uri :refer [join]])
  (:import
   (clojure.lang ExceptionInfo)))

(defn- with-base-uri-meta
  "For each $id in the schema, add metadata to indicate the base-uri."
  ([schema]
   (with-base-uri-meta nil schema))
  ([base-uri schema]
   (cond
     (map? schema)
     (if-let [id (get schema "$id")]
       (let [new-base-uri (str (join base-uri id))]
         (with-meta
           (assoc (with-base-uri-meta new-base-uri (dissoc schema "$id")) "$id" id)
           {:base-uri new-base-uri
            :id id}))
       (with-meta
         (zipmap (keys schema) (map (partial with-base-uri-meta base-uri) (vals schema)))
         {:base-uri base-uri}))
     (vector? schema) (mapv (partial with-base-uri-meta base-uri) schema)
     :else schema)))

(defn- index-by-uri [schema]
  (cond
    (map? schema)
    (let [mt (meta schema)]
      (if (:id mt)
        (cons [(:base-uri mt) schema]
              (index-by-uri (with-meta schema (dissoc mt :id))))
        (mapcat index-by-uri (vals schema))))

    (vector? schema)
    (mapcat index-by-uri schema)))

(declare validate)

(defmulti validate-keyword (fn [kw v options] kw))

(defmethod validate-keyword :default [kw v options] nil)

(defmethod validate-keyword "type" [kw v options]
  (when-not (or (string? v) (array? v))
    (throw (ex-info "The value of 'type' MUST be either a string or an array" {:value v})))

  (when (array? v)
    (when-not (every? string? v)
      (throw (ex-info "The value of 'type', if it is an array, elements of the array MUST be strings" {})))
    (when-not (apply distinct? v)
      (throw (ex-info "The value of 'type', if it is an array, elements of the array MUST be unique" {}))))

  (let [legal #{"null" "boolean" "object" "array" "number" "string" "integer"}]
    (when-not
        (or
         (and (string? v) (contains? legal v))
         (and (array? v) (every? #(contains? legal %) v)))
      (throw (ex-info "String values of 'type' MUST be one of the six primitive types or 'integer'" {:value v})))))

(defmethod validate-keyword "enum" [kw v options]
  (when-not (array? v)
    (throw (ex-info "The value of an enum MUST be an array" {:value v})))
  (when (:strict? options)
    (when (empty? v)
      (throw (ex-info "The value of an enum SHOULD have at least one element" {:value v})))
    (when-not (apply distinct? v)
      (throw (ex-info "Elements in the enum value array SHOULD be unique" {:value v})))))

(defmethod validate-keyword "multipleOf" [kw v options]
  (when-not (and (number? v) (pos? v))
    (throw (ex-info "The value of multipleOf MUST be a number, strictly greater than 0" {:value v}))))

(defmethod validate-keyword "maximum" [kw v options]
  (when-not (number? v)
    (throw (ex-info "The value of maximum MUST be a number" {:value v}))))

(defmethod validate-keyword "exclusiveMaximum" [kw v options]
  (when-not (number? v)
    (throw (ex-info "The value of exclusiveMaximum MUST be a number" {:value v}))))

(defmethod validate-keyword "minimum" [kw v options]
  (when-not (number? v)
    (throw (ex-info "The value of minimum MUST be a number" {:value v}))))

(defmethod validate-keyword "exclusiveMinimum" [kw v options]
  (when-not (number? v)
    (throw (ex-info "The value of exclusiveMinimum MUST be a number" {:value v}))))

(defmethod validate-keyword "maxLength" [kw v options]
  (when-not (and (integer? v) (not (neg? v)))
    (throw (ex-info "The value of maxLength MUST be a non-negative integer" {:value v}))))

(defmethod validate-keyword "minLength" [kw v options]
  (when-not (and (integer? v) (not (neg? v)))
    (throw (ex-info "The value of minLength MUST be a non-negative integer" {:value v}))))

(defmethod validate-keyword "pattern" [kw v options]
  (when-not (string? v)
    (throw (ex-info "The value of pattern MUST be a string" {:value v}))))

(defmethod validate-keyword "items" [kw v options]
  (cond
    (schema? v)
    (try
      (validate v options)
      (catch ExceptionInfo cause
        (throw (ex-info
                "The value of 'items' MUST be a valid JSON Schema"
                {:value v}
                cause))))

    (array? v)
    (try
      (doseq [el v]
        (try
          (validate el options)
          (catch ExceptionInfo cause
            (throw (ex-info
                    "The value of 'items' MUST be an array of valid JSON Schemas, but at least one element isn't valid"
                    {:element el}
                    cause))))))

    :else
    (throw (ex-info "The value of 'items' MUST be either a valid JSON Schema or an array of valid JSON Schemas" {}))))

(defmethod validate-keyword "additionalItems" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'additionalItems' MUST be a valid JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'additionalItems' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "maxItems" [kw v options]
  (when-not (and (integer? v) (not (neg? v)))
    (throw (ex-info "The value of 'maxItems' MUST be a non-negative integer" {:value v}))))

(defmethod validate-keyword "minItems" [kw v options]
  (when-not (and (integer? v) (not (neg? v)))
    (throw (ex-info "The value of 'minItems' MUST be a non-negative integer" {:value v}))))

(defmethod validate-keyword "uniqueItems" [kw v options]
  (when-not (boolean? v)
    (throw (ex-info "The value of 'uniqueItems' MUST be a boolean" {:value v}))))

(defmethod validate-keyword "contains" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'contains' MUST be a valid JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'contains' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "maxProperties" [kw v options]
  (when-not (and (integer? v) (not (neg? v)))
    (throw (ex-info "The value of 'maxProperties' MUST be a non-negative integer" {:value v}))))

(defmethod validate-keyword "minProperties" [kw v options]
  (when-not (and (integer? v) (not (neg? v)))
    (throw (ex-info "The value of 'minProperties' MUST be a non-negative integer" {:value v}))))

(defmethod validate-keyword "required" [kw v options]
  (when-not (array? v)
    (throw (ex-info "The value of 'required' MUST be an array" {:value v})))
  (when (and (array? v) (not-empty v))
    (when-not (every? string? v)
      (throw (ex-info "The value of 'required' MUST be an array. Elements of this array, if any, MUST be strings" {:value v})))
    (when-not (apply distinct? v)
      (throw (ex-info "The value of 'required' MUST be an array. Elements of this array, if any, MUST be unique" {:value v})))))

(defmethod validate-keyword "properties" [kw v options]
  (when-not (object? v)
    (throw (ex-info "The value of 'properties' MUST be an object" {:value v})))
  (doseq [[subkw subschema] v]
    (try
      (validate subschema options)
      (catch ExceptionInfo cause
        (throw (ex-info "Each value of 'properties' MUST be a valid JSON Schema" {:keyword subkw} cause))))))

(defmethod validate-keyword "patternProperties" [kw v options]
  (when-not (object? v)
    (throw (ex-info "The value of 'patternProperties' MUST be an object" {:value v})))
  (doseq [[subkw subschema] v]
    (when-not (regex? subkw)
      (throw (ex-info "Each property name of a 'patternProperties' object SHOULD be a valid regular expression")))
    (try
      (validate subschema options)
      (catch ExceptionInfo cause
        (throw (ex-info "Each value of a 'patternProperties' object MUST be a valid JSON Schema" {:keyword subkw} cause))))))

(defmethod validate-keyword "additionalProperties" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'additionalProperties' MUST be a valid JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'additionalProperties' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "dependencies" [kw v options]
  (when-not (object? v)
    (throw (ex-info "The value of 'dependencies' MUST be an object" {}))
    )
  (doseq [v (vals v)]
    (when-not (or (array? v) (schema? v))
      (throw (ex-info "Dependency values MUST be an array or a JSON Schema" {:value v})))
    (when (and (array? v) (not-empty v))
      (when-not (every? string? v)
        (throw (ex-info "Each element in a dependencies array MUST be a string" {})))
      (when-not (apply distinct? v)
        (throw (ex-info "Each element in a dependencies array MUST be unique" {}))))
    (when (schema? v)
      (try
        (validate v options)
        (catch ExceptionInfo cause
          (throw (ex-info "Dependency values MUST be an array or a valid JSON Schema" {:value v} cause)))))))

(defmethod validate-keyword "propertyNames" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'propertyNames' MUST be a JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'propertyNames' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "if" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'if' MUST be a JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'if' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "then" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'then' MUST be a JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'then' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "else" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'else' MUST be a JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'else' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "allOf" [kw v options]
  (when-not (array? v)
    (throw (ex-info "The value of 'allOf' MUST be a non-empty array" {:value v})))
  (when (empty? v)
    (throw (ex-info "The value of 'allOf' MUST be a non-empty array" {:value v})))
  (doseq [subschema v]
    (try
      (validate subschema options)
      (catch ExceptionInfo cause
        (throw (ex-info "Each item of an 'allOf' array MUST be a valid schema" {:value v} cause))))))

(defmethod validate-keyword "anyOf" [kw v options]
  (when-not (array? v)
    (throw (ex-info "The value of 'anyOf' MUST be a non-empty array" {:value v})))
  (when (empty? v)
    (throw (ex-info "The value of 'anyOf' MUST be a non-empty array" {:value v})))
  (doseq [subschema v]
    (try
      (validate subschema options)
      (catch ExceptionInfo cause
        (throw (ex-info "Each item of an 'anyOf' array MUST be a valid schema" {:value v} cause))))))

(defmethod validate-keyword "oneOf" [kw v options]
  (when-not (array? v)
    (throw (ex-info "The value of 'oneOf' MUST be a non-empty array" {:value v})))
  (when (empty? v)
    (throw (ex-info "The value of 'oneOf' MUST be a non-empty array" {:value v})))
  (doseq [subschema v]
    (try
      (validate subschema options)
      (catch ExceptionInfo cause
        (throw (ex-info "Each item of an 'oneOf' array MUST be a valid schema" {:value v} cause))))))

(defmethod validate-keyword "not" [kw v options]
  (when-not (schema? v)
    (throw (ex-info "The value of 'not' MUST be a JSON Schema" {:value v})))
  (try
    (validate v options)
    (catch ExceptionInfo cause
      (throw (ex-info "The value of 'not' MUST be a valid JSON Schema" {:value v} cause)))))

(defmethod validate-keyword "format" [kw v options]
  (when-not (string? v)
    (throw (ex-info "The value of a 'format' attribute MUST be a string" {:value v}))))

(defn validate
  "Validate a schema, checking it obeys conformance rules. When
  the :strict? option is truthy, rules that contain SHOULD are
  considered errors."
  ([schema]
   (validate schema {:strict? true}))
  ([schema options]
   (or
    (boolean? schema)
    (nil? schema)
    (doseq [[k v] (seq schema)]
      (validate-keyword k v options))
    schema)))

(defn schema
  ([s]
   (schema s {:strict? true}))
  ([schema options]
   ;; TODO: Ensure schema is returned as-is if it's existing schema
   ;; TODO: Add ^:juxt/schema true - which is the right keyword here?
   (validate schema options)
   (let [schema (with-base-uri-meta schema)
         index (into {} (index-by-uri schema))]
     (cond->
         schema
         (and (instance? clojure.lang.IMeta schema) index)
         (with-meta (-> schema meta (assoc :uri->schema index)))))))

;; TODO: Try against all schemas in test-suite

(comment
  (let [schema
        {"$id" "http://example.com/root.json"
         "definitions"
         {"A" {"$id" "#foo"}
          "B" {"$id" "other.json"
               "definitions"
               {"X" {"$id" "#bar"}
                "Y" {"$id" "t/inner.json"}}}
          "C" {"$id" "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"}}}

        schema (with-base-uri-meta schema)
        schema (into {} (index-by-uri schema))]

    (-> (get schema "http://example.com/root.json")
        (get-in ["definitions" "B" "definitions"])
        meta)))

(comment
  (let [schema
        {"$id" "http://localhost:1234/tree",
         "description" "tree of nodes",
         "type" "object",
         "properties"
         {"meta" {"type" "string"},
          "nodes" {"type" "array", "items" {"$ref" "node"}}},
         "required" ["meta" "nodes"],
         "definitions"
         {"node"
          {"$id" "http://localhost:1234/node",
           "description" "node",
           "type" "object",
           "properties"
           {"value" {"type" "number"}, "subtree" {"$ref" "tree"}},
           "required" ["value"]}}}
        schema (with-base-uri-meta schema)
        index (into {} (index-by-uri schema))]
    (-> index
        (get "http://localhost:1234/node")
        (get-in ["properties" "subtree"])
        meta
        :base-uri
        (join "tree")
        str)))


;; NOTE: A relative $ref will now be easy to resolve to a URI, using
;; the :base-uri for the object's metadata.
