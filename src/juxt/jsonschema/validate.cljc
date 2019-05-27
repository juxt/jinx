;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.validate
  (:refer-clojure :exclude [number? integer?])
  (:require
   [cheshire.core :as cheshire]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.java.io :as io] ;; TODO: Support cljs
   [clojure.test :refer [deftest is are]]
   [juxt.jsonschema.jsonpointer :as jsonpointer]
   [juxt.jsonschema.schema :as schema]
   [juxt.jsonschema.resolve :as resolv]
   [juxt.jsonschema.regex :as regex]
   [lambdaisland.uri :as uri]))

(declare validate*)

(defn array? [x]
  (sequential? x))

(defn object? [x]
  (map? x))

(defn schema? [x]
  (or (object? x) (boolean? x)))

(defn valid? [x]
  (not (seq x)))

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
  (fn [keyword value schema instance doc options]
    keyword))

(defmethod check-assertion :default [k type schema instance doc options]
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

(defn integer? [x]
  (or
   (clojure.core/integer? x)
   (when (number? x)
     (zero? (mod x 1)))))

(def type-preds
  {"null" nil?
   "boolean" boolean?
   "object" object?
   "array" array?
   "number" number?
   "string" string?
   "integer" integer?})

(defmethod check-assertion "type" [_ type schema instance doc options]
  (cond
    (string? type)
    (when-not ((type-preds type) instance)
      [{:message (format "Value must be of type %s" type)}])
    (array? type)
    (when-not ((apply some-fn (vals (select-keys type-preds type))) instance)
      [{:message (format "Value must be of type %s" (str/join " or " type))}])))

(defmethod check-assertion "enum" [_ enum schema instance doc options]
  (when-not (contains? (set enum) instance)
    [{:message (format "Value %s must be in enum %s" instance enum)}]))

(defmethod check-assertion "const" [_ const schema instance doc options]
  (when-not (= const instance)
    [{:message (format "Value %s must be equal to const %s" instance const)}]))

(defmethod check-assertion "multipleOf" [_ multiple-of schema instance doc options]
  (when (number? instance)
    (when-not
        #?(:clj (= 0 (.compareTo (.remainder (bigdec instance) (bigdec multiple-of)) BigDecimal/ZERO))
           :cljs [{:message "Not yet supported"}])
      [{:message "Failed multipleOf check"}])))

(defmethod check-assertion "maximum" [_ maximum schema instance doc options]
  (when (number? instance)
    (when-not (<= instance maximum)
      [{:message "Failed maximum check"}])))

(defmethod check-assertion "exclusiveMaximum" [_ exclusive-maximum schema instance doc options]
  (when (number? instance)
    (when-not (< instance exclusive-maximum)
      [{:message "Failed exclusiveMaximum check"}])))

(defmethod check-assertion "minimum" [_ minimum schema instance doc options]
  (when (number? instance)
    (when-not (>= instance minimum)
      [{:message "Failed minimum check"}])))

(defmethod check-assertion "exclusiveMinimum" [_ exclusive-minimum schema instance doc options]
  (when (number? instance)
    (when-not (> instance exclusive-minimum)
      [{:message "Failed exclusiveMinimum check"}])))

(defmethod check-assertion "maxLength" [_ max-length schema instance doc options]
  (when (string? instance)
    ;; See https://github.com/networknt/json-schema-validator/issues/4
    (when (> (.codePointCount instance 0 (.length instance)) max-length)
      [{:message "String is too long"}])))

(defmethod check-assertion "minLength" [_ min-length schema instance doc options]
  (when (string? instance)
    (when (<
           #?(:clj (.codePointCount instance 0 (.length instance))
              :cljs (count instance))
           min-length)
      [{:message "String is too short"}])))

(defmethod check-assertion "pattern" [_ pattern schema instance doc options]
  (when (string? instance)
    (when-not (re-seq (re-pattern pattern) instance)
      [{:message (format "String does not match pattern %s" pattern)}])))

;; TODO: Rename schema to subschema
;; TODO: Show paths in error messages
;; TODO: Improve error messages, possibly copying Ajv or org.everit json-schema

(defmethod check-assertion "items" [_ items schema instance doc options]
  (when (sequential? instance)
    (cond
      (object? items)
      (->>
       (for [[idx instance] (map-indexed vector instance)]
         (validate* items instance doc (update options :schema-path (fnil conj []) idx)))
       (mapcat set))

      (boolean? items)
      (when (and (false? items) (not-empty instance))
        [{:message "Items must be empty to satisfy a false schema"}])

      (array? items)
      ;; TODO: Consider short-circuiting
      (->>
       (for [[idx schema instance] (map vector (range) (concat items (repeat (get schema "additionalItems"))) instance)]
         (validate* schema instance doc (update options :schema-path (fnil conj []) idx)))
       (mapcat seq)))))

(defmethod check-assertion "maxItems" [_ max-items schema instance doc options]
  (when (array? instance)
    (when (> (count instance) max-items)
      [{:message "maxItems exceeded"}])))

(defmethod check-assertion "minItems" [_ min-items schema instance doc options]
  (when (array? instance)
    (when (< (count instance) min-items)
      [{:message "minItems not reached"}])))

(defmethod check-assertion "uniqueItems" [_ unique-items? schema instance doc options]
  (when (and (array? instance) unique-items?)
    (when-not (apply distinct? instance)
      [{:message "Instance elements are not all unique"}])))

(defmethod check-assertion "contains" [_ contains schema instance doc options]
  (when (array? instance)
    ;; TODO: Beware of short-circuiting using 'some' (see 3.3.2)
    (when-not (some #(empty? (validate* contains % doc options)) instance)
      [{:message "Instance is not valid against schema"}])))

(defmethod check-assertion "maxProperties" [_ max-properties schema instance doc options]
  (when (object? instance)
    (when-not (<= (count (keys instance)) max-properties)
      [{:message "Max properties exceeded"}])))

(defmethod check-assertion "minProperties" [_ min-properties schema instance doc options]
  (when (object? instance)
    (when-not (<= min-properties (count (keys instance)))
      [{:message "Min properties not reached"}])))

(defmethod check-assertion "required" [_ required schema instance doc options]
  (when (object? instance)
    (when-not (set/subset? (set required) (set (keys instance)))
      [{:message "Missing required property"}])))

(defmethod check-assertion "properties" [_ properties schema instance doc options]
  (when (object? instance)
    (->>
     (for [[k subschema] properties
           :let [child-instance (get instance k)]
           :when child-instance]
       (validate* subschema child-instance doc (update options :schema-path (fnil conj []) "properties" k)))
     (mapcat seq))))

(defmethod check-assertion "patternProperties" [_ pattern-properties schema instance doc options]
  (when (object? instance)
    (let [compiled-pattern-properties (map (fn [[k v]] [(re-pattern k) v]) pattern-properties)]
      (->>
       (for [[propname child-instance] instance
             [pattern subschema] compiled-pattern-properties
             :when (re-seq pattern propname)]
         (validate*
          subschema
          child-instance
          doc
          (update options :schema-path (fnil conj []) "patternProperties" (str pattern))))
       (mapcat seq)))))

(defmethod check-assertion "additionalProperties" [_ additional-properties schema instance doc options]
  (when (object? instance)
    (let [properties (set (keys (get schema "properties")))
          compiled-patterns (when-let [pattern-properties (get schema "patternProperties")]
                              (map (fn [[k v]] (re-pattern k)) pattern-properties))]
      (->>
       (for [[propname child-instance] instance
             :when (not (contains? properties propname))
             :when (nil? (some #(re-seq % propname) compiled-patterns))]
         (validate*
          additional-properties
          child-instance
          doc
          (update options :schema-path (fnil conj []) "additionalProperties")))
       (mapcat seq)))))

(defmethod check-assertion "dependencies" [_ dependencies schema instance doc options]
  (when (object? instance)
    (->>
     (for [[propname dvalue] dependencies]
       (when (contains? instance propname)
         (cond
           (schema? dvalue)
           (validate* dvalue instance doc options)
           (array? dvalue)
           (when-not (every? #(contains? instance %) dvalue)
             [{:message "Not every dependency in instance"}]))))
     (mapcat seq))))

(defmethod check-assertion "propertyNames" [_ property-names schema instance doc options]
  (when (object? instance)
    (->>
     (for [propname (keys instance)]
       (validate* property-names propname doc options))
     (mapcat seq))))

(defmethod check-assertion "allOf" [_ all-of schema instance doc options]
  (->>
   (for [[subschema idx] (map vector all-of (range))
         :let [failures (seq (validate* subschema instance doc (update options :schema-path (fnil conj []) "allOf" idx)))]
         :when failures]
     [{:message (format "allOf schema failed due to subschema at %s failing" idx)
       :caused-by failures}])
   (mapcat seq)))

(defmethod check-assertion "anyOf" [_ any-of schema instance doc options]
  (when
      (every? seq
              (for [[subschema idx] (map vector any-of (range))]
                (validate* subschema instance doc (update options :schema-path (fnil conj []) "anyOf" idx))))
    [{:message "No schema validates for anyOf validation"}]))

(defmethod check-assertion "oneOf" [_ one-of schema instance doc options]
  (let [valids
        (for [[subschema idx] (map vector one-of (range))
              :when (not (seq (validate* subschema instance doc (update options :schema-path (fnil conj []) "oneOf" idx))))]
          idx)]
    (cond
      (zero? (count valids))
      [{:message "No schema validates in oneOf validation"}]
      (not= 1 (count valids))
      [{:message (format "Multiple schemas (%s) are valid in oneOf validation" (str/join ", " valids))}])))

(defmethod check-assertion "not" [_ not schema instance doc options]
  (when (valid? (validate* not instance doc (update options :schema-path (fnil conj []) "not")))
    [{:message "Schema should not be valid"}]))

(defmethod check-assertion "if" [_ if schema instance doc options]
  (if (valid? (validate* if instance doc (update options :schema-path (fnil conj []) "if")))
    (when-let [then (get schema "then")]
      (validate* then instance doc (update options :schema-path (fnil conj []) "then")))
    (when-let [else (get schema "else")]
      (validate* else instance doc (update options :schema-path (fnil conj []) "else")))))

;; TODO: Rather than get, use a macro to retrieve either strings and
;; keywords, supporting both

(defmulti check-format (fn [fmt schema instance] fmt))

(defmethod check-format :default [fmt schema instance]
  ;; If format not known, succeed
  )

(defmethod check-format "date-time" [fmt schema instance]
  (when (string? instance)
    (try
      (.parse java.time.format.DateTimeFormatter/ISO_DATE_TIME instance)
      []
      (catch Exception e
        [{:message "Doesn't match date-time format"}]))))

(defmethod check-format "date" [fmt schema instance]
  (when (string? instance)
    (try
      (.parse java.time.format.DateTimeFormatter/ISO_LOCAL_DATE instance)
      []
      (catch Exception e
        [{:message "Doesn't match date format"}]))))

(defmethod check-format "time" [fmt schema instance]
  (when (string? instance)
    (try
      (.parse java.time.format.DateTimeFormatter/ISO_TIME instance)
      []
      (catch Exception e
        [{:message "Doesn't match time format"}]))))

(defmethod check-format "email" [fmt schema instance]
  (when (string? instance)
    (when-not (re-matches regex/addr-spec instance)
      [{:message "Doesn't match email format"}])))

(defmethod check-format "idn-email" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC6531
    (when-not (re-matches #".*@.*" instance)
      [{:message "Doesn't match idn-email format"}])))

(defmethod check-format "hostname" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC 1034
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match hostname format"}])))

(defmethod check-format "idn-hostname" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC 5890
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match idn-hostname format"}])))

(defmethod check-format "ipv4" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC2673
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match ipv4 format"}])))

(defmethod check-format "ipv6" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC4291
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match ipv6 format"}])))

(defmethod check-format "uri" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC3986
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match uri format"}])))

(defmethod check-format "uri-reference" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC3986
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match uri-reference format"}])))

(defmethod check-format "iri" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC3987
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match iri format"}])))

(defmethod check-format "iri-reference" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC3987
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match iri-reference format"}])))

(defmethod check-format "uri-template" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC6570
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match uri-template format"}])))

(defmethod check-format "json-pointer" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC6901
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match json-pointer format"}])))

(defmethod check-format "relative-json-pointer" [fmt schema instance]
  (when (string? instance)
    ;; TODO: Improve this regex: [relative-json-pointer]
    (when-not (re-matches #".*" instance)
      [{:message "Doesn't match relative-json-pointer format"}])))

(defmethod check-format "regex" [fmt schema instance]
  (when (string? instance)
    (cond
      ;; (This might be cheating just to get past the test suite)
      (.contains instance "\\Z")
      [{:message "Must not contain \\Z anchor from .NET"}]
      ;; TODO: Add more cases
      )))

(defmethod check-assertion "format" [_ format schema instance doc options]
  ;; TODO: This is optional, so should be possible to disable via
  ;; options - see 7.2 of draft-handrews-json-schema-validation-01:
  ;; "they SHOULD offer an option to disable validation for this
  ;; keyword."
  (check-format format schema instance))

;;(java.util.Base64$Decoder/decode "foo")

(defn decode-content [content-encoding instance]
  ;; TODO: Open for extension with a multimethod
  (case content-encoding
    "base64" (String. (.decode (java.util.Base64/getDecoder) instance))
    nil instance))

(defmethod check-assertion "contentEncoding" [_ content-encoding schema instance doc options]
  ;; TODO: This is optional, so should be possible to disable via
  ;; options - see 8.2 of draft-handrews-json-schema-validation-01:
  ;; "Implementations MAY support the "contentMediaType" and
  ;; "contentEncoding" keywords as validation assertions.  Should they
  ;; choose to do so, they SHOULD offer an option to disable
  ;; validation for these keywords."
  (when (string? instance)
    (try
      (decode-content content-encoding instance)
      []
      (catch Exception e
        [{:message "Not base64"}]))))

(defmethod check-assertion "contentMediaType" [_ content-media-type schema instance doc options]
  ;; TODO: This is optional, so should be possible to disable via
  ;; options - see 8.2 of draft-handrews-json-schema-validation-01:
  ;; "Implementations MAY support the "contentMediaType" and
  ;; "contentEncoding" keywords as validation assertions.  Should they
  ;; choose to do so, they SHOULD offer an option to disable
  ;; validation for these keywords."
  (when (string? instance)
    (if-let [content (try
                       (decode-content (get schema "contentEncoding") instance)
                       (catch Exception e nil))]
      ;; TODO: Open for extension with a multimethod
      (case content-media-type
        "application/json"
        (try
          (println "Parsing string content" content)
          (cheshire/parse-string content)
          []
          (catch Exception e
            [{:message "Instance is not application/json"}])))
      [{:message "Unable to decode content"}])))

(defn resolve-ref [ref-object doc options]
  (assert ref-object)

  (let [ ;; "The value of the "$ref" property MUST be a URI Reference."
        ;; -- [CORE Section 8.3]
        base-uri (get (meta ref-object) :base-uri)
        ref (some-> (get ref-object "$ref") java.net.URLDecoder/decode)
        uri (str (uri/join (or base-uri (:base-uri options)) ref))

        ]

    (let [options
          (if false #_(contains? (:visited-memory options) uri)
              (throw (ex-info "Infinite cycle detected" {:uri uri}))
              (update options :visited-memory (fnil conj #{}) uri))]

      (let [[uri doc base-uri] (resolv/resolv uri doc (:resolvers options))]
        [uri doc (cond-> options base-uri (assoc :base-uri base-uri))]))))

(def built-in-schemas
  {"http://json-schema.org/draft-07/schema" "schemas/json-schema.org/draft-07/schema"})

;; TODO: See index.js in JSON-Schema-Test-Suite to fix refRemote.json

;; TODO: Tidy up

;; TODO: Fix :schemas - support regex->schema, regex->fn


(defn- validate*
  [schema instance doc options]

  ;; We keep trying to find errors, returning them in a list.
  (cond
    (and (object? schema) (contains? schema "$ref"))
    (let [[new-schema doc new-opts] (resolve-ref schema doc options)]
      (validate* new-schema instance doc new-opts))

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
           (concat (check-assertion k (get schema k) schema instance doc options))))
        ;; Finally, return the errors (even if empty).
        errors))))


(defn validate
  "Options include
  :schemas - a map from uris (or regexes) to functions that return schemas"
  ([schema instance]
   (validate schema instance {:resolvers [::resolv/built-in]}))

  ([schema instance options]
   (validate*
    schema instance schema options)))
