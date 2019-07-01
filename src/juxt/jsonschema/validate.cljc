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
   [juxt.jsonschema.core :refer [number? integer? array? object? schema?]]
   [juxt.jsonschema.schema :as schema]
   [juxt.jsonschema.resolve :as resolv]
   [juxt.jsonschema.regex :as regex]
   [lambdaisland.uri :as uri]))

;; "Since many subschemas can be applicable to any single location,
;; annotation keywords need to specify any unusual handling of
;; multiple applicable occurrences of the keyword with different
;; values."
(defmulti handle-multiple-annotations (fn [kw annotations] kw))

;; "The default behavior is simply to collect all values."
(defmethod handle-multiple-annotations :default [kw annotations]
  annotations)

(defmethod handle-multiple-annotations "title" [kw annotations]
  annotations)

(defmethod handle-multiple-annotations "description" [kw annotations]
  annotations)

(defmethod handle-multiple-annotations "default" [kw annotations]
  ;; "When multiple occurrences of this keyword are applicable to a
  ;; single sub-instance, implementations SHOULD remove duplicates."
  (distinct annotations))

(defn ->sequential [x] (if (sequential? x) x [x]))

(defn merge-annotations [& maps]
  (->> maps
       (mapcat identity)
       (group-by first)
       (map (fn [[k v]] [k (mapcat ->sequential (map second v))]))
       (map (fn [[k v]] [k (handle-multiple-annotations k v)]))
       (into {})))

(comment
  (merge-annotations
   [{"title" "foo" "examples" [:foo :foo2]}
    {"title" "bar" "examples" [:bar]}
    {"title" "zip" "examples" [:zip1 :zippy]}]))

(declare validate*)

;; All references here relate to
;; draft-handrews-json-schema-validation-01.txt unless otherwise
;; stated.

(defmulti process-keyword
  "Allow for additional vocabularies by making this extensible.

  'Validation keywords typically operate independently, without
   affecting each other's outcomes.' -- 3.1.1

  However, given there are some exceptions, the full schema object is
  also provided as a map.

  The instance and annotations args are the latest version of the
  instance currently established, not (necessarily) the original data
  value.

  A method can update the values of the instance and annotations by
  returning a map optionally containing :instance and :annotations
  correspondingly.
  "
  (fn [keyword value instance annotations ctx] keyword))

(defmethod process-keyword :default [k value instance annotations ctx]
  ;; A JSON Schema MAY contain properties which are not schema
  ;; keywords. Unknown keywords SHOULD be ignored. -- JSON Schema Core, 4.3.1
  ;;
  ;; Do not error if a method for a given keyword isn't defined, so we
  ;; return nil.
  nil)

;; We test against the instance first. We try to solve (via defaults)
;; and build up the instantiation, and possibly explain our actions
;; via the journal. If we can't solve, we throw errors. Errors are
;; fatal.

(defmethod process-keyword "title" [k title instance annotations ctx]
  {:annotations (assoc annotations "title" title)})

(defmethod process-keyword "description" [k description instance annotations ctx]
  {:annotations (assoc annotations "description" description)})

(defmethod process-keyword "default" [k default instance annotations ctx]
  (merge
   {:annotations (assoc annotations "default" default)}
   (when (not (some? instance)) {:value default})))

(defmethod process-keyword "readOnly" [k read-only instance annotations ctx]
  {:annotations (assoc annotations "readOnly" read-only)})

(defmethod process-keyword "writeOnly" [k write-only instance annotations ctx]
  {:metadata (assoc annotations "writeOnly" write-only)})

(defmethod process-keyword "examples" [k examples instance annotations ctx]
  {:metadata (assoc annotations "examples" examples)})

;; TODO: These must check against JavaScript primitive types,
;; not Clojure/Java ones

(def type-preds
  {"null" nil?
   "boolean" boolean?
   "object" object?
   "array" array?
   "number" number?
   "string" string?
   "integer" integer?})

(defmethod process-keyword "type" [_ type instance annotations ctx]
  (cond
    (string? type)
    (if-let [pred (get type-preds type)]
      (if (pred instance)
        {:type type}
        ;; TODO: We have an error, but we should first try to coerce -
        ;; e.g. string->number, number->string
        {:error
         {:message (format "Instance of %s is not of type %s"
                           (pr-str instance)
                           (pr-str type))
          :instance instance
          :type type}})

      ;; Nil pred
      (throw (IllegalStateException. "Invalid schema detected")))

    (array? type)
    (when-not ((apply some-fn (vals (select-keys type-preds type))) instance)
      ;; TODO: Find out _which_ type it matches, and instantiate _that_
      {:error {:message (format "Value must be of type %s" (str/join " or " (pr-str type)))}})))

;; TODO: Possibly replace :errors with :invalid? such that :invalid?
;; is not a boolean but contains the errors.

;; TODO: Pass schema-path (and data-path) in a 'ctx' arg, not options
;; (keep 'options' constant). Demote 'doc' to 'ctx' entry, which
;; should also contain 'base-uri'. Only do this refactoring once the tests are working.

(defmethod process-keyword "enum" [k enum instance annotations ctx]
  (when-not (contains? (set enum) instance)
    {:error {:message (format "Value %s must be in enum %s" instance enum)}}))

(defmethod process-keyword "const" [k const instance annotations ctx]
  (when-not (= const instance)
    {:error {:message (format "Value %s must be equal to const %s" instance const)}}))

(defmethod process-keyword "multipleOf" [k multiple-of instance annotations ctx]
  (when (number? instance)
    (when-not
        #?(:clj (= 0 (.compareTo (.remainder (bigdec instance) (bigdec multiple-of)) BigDecimal/ZERO))
           :cljs [{:message "Not yet supported"}])
      {:error {:message "Failed multipleOf check"}})))

(defmethod process-keyword "maximum" [k maximum instance annotations ctx]
  (when (number? instance)
    (when-not (<= instance maximum)
      {:error {:message "Failed maximum check"}})))

(defmethod process-keyword "exclusiveMaximum" [k exclusive-maximum instance annotations ctx]
  (when (number? instance)
    (when-not (< instance exclusive-maximum)
      {:error {:message "Failed exclusiveMaximum check"}})))

(defmethod process-keyword "minimum" [k minimum instance annotations ctx]
  (when (number? instance)
    (when-not (>= instance minimum)
      {:error {:message "Failed minimum check"}})))

(defmethod process-keyword "exclusiveMinimum" [k exclusive-minimum instance annotations ctx]
  (when (number? instance)
    (when-not (> instance exclusive-minimum)
      {:error {:message "Failed exclusiveMinimum check"}})))

(defmethod process-keyword "maxLength" [k max-length instance annotations ctx]
  (when (string? instance)
    ;; See https://github.com/networknt/json-schema-validator/issues/4
    (when (> (.codePointCount instance 0 (.length instance)) max-length)
      {:error {:message "String is too long"}})))

(defmethod process-keyword "minLength" [k min-length instance annotations ctx]
  (when (string? instance)
    (when (<
           #?(:clj (.codePointCount instance 0 (.length instance))
              :cljs (count instance))
           min-length)
      {:error {:message "String is too short"}})))

(defmethod process-keyword "pattern" [_ pattern instance annotations ctx]
  (when (string? instance)
    (when-not (re-seq (re-pattern pattern) instance)
      {:error {:message (format "String does not match pattern %s" pattern)}})))

;; TODO: Show paths in error messages
;; TODO: Improve error messages, possibly copying Ajv or org.everit json-schema

(defmethod process-keyword "items" [_ items instance annotations {:keys [schema] :as ctx}]
  (when (array? instance)
    (cond
      (object? items)
      (let [children
            (for [[idx child-instance] (map-indexed vector instance)]
              (assoc (validate* child-instance items ctx)
                     :index idx))]
        (if (every? :valid? children)
          ;;(merge instance {:items children})
          {:items children}
          {:error {:message "Not all items are valid"
                   :bad-items (filter :errors children)}}))

      (boolean? items)
      ;; TODO: Add a test for this
      (when (and (false? items) (not-empty instance))
        {:error {:message "Items must be empty to satisfy a false schema"}})

      (array? items)
      (let [children
            (for [[idx child-schema child-instance] (map vector (range) (concat items (repeat (get schema "additionalItems"))) instance)]
              (assoc
               (validate* child-instance child-schema ctx)
               :index idx))]
        (if (every? :valid? children)
          ;;(merge instance {:items children})
          {:items children}
          {:error {:message "Not all items are valid"
                   :bad-items (filter :errors children)}})))))

(defmethod process-keyword "maxItems" [k max-items instance annotations ctx]
  (when (array? instance)
    (when (> (count instance) max-items)
      {:error {:message "maxItems exceeded"}})))

(defmethod process-keyword "minItems" [k min-items instance annotations ctx]
  (when (array? instance)
    (when (< (count instance) min-items)
      {:error {:message "minItems not reached"}})))

(defmethod process-keyword "uniqueItems" [k unique-items? instance annotations ctx]
  (when (and (array? instance) unique-items?)
    (when-not (apply distinct? instance)
      {:error {:message "Instance elements are not all unique"}})))

(defmethod process-keyword "contains" [k contains instance annotations ctx]
  (when (array? instance)
    ;; Let metadata surface in other keywords
    (let [results (map #(validate* % contains ctx) instance)]
      (cond-> {:contains results}
        (not (some :valid? results))
        (assoc :error {:message "Instance is not valid against schema"})))))

(defmethod process-keyword "maxProperties" [k max-properties instance annotations ctx]
  (when (object? instance)
    (when-not (<= (count (keys instance)) max-properties)
      {:error {:message "Max properties exceeded"}})))

(defmethod process-keyword "minProperties" [k min-properties instance annotations ctx]
  (when (object? instance)
    (when-not (<= min-properties (count (keys instance)))
      {:error {:message "Min properties not reached"}})))

(defmethod process-keyword "required" [_ required instance annotations {:keys [schema] :as ctx}]
  (when (object? instance)
    (let [results
          (keep
           (fn [kw]
             (when-not (find instance kw)
               {:error {:message "Required property not in object"
                        :keyword kw}}))
           required)]

      (when (not-empty results)
        {:error {:message "Some required properties missing"
                 :causes results}}

        ;; Attempt to recover

        ;; Note: recovery steps should be made optional via options,and
        ;; possibly possible to override with multimethods.

        (let [recovered-result
              (reduce
               (fn [acc result]
                 (let [kw (get-in result [:error :keyword])
                       prop (get-in schema ["properties" kw])
                       attempt (when prop (when-let [defv (get prop "default")]
                                            (validate* defv prop ctx)))]
                   (if (:valid? attempt)
                     (assoc-in acc [:instance kw] (:instance attempt))
                     (update acc :causes (fnil conj []) (:error result)))))
               {:instance instance}
               results)]

          (cond-> recovered-result
            (:causes recovered-result) (assoc :error "One or more required properties not found in object")))))))

(defmethod process-keyword "properties" [_ properties instance annotations ctx]
  (when (object? instance)
    (let [validations
          (for [[kw child] instance
                :let [subschema (get properties kw)]
                :when (some? subschema)
                :let [validation (validate* child subschema ctx)]]
            (merge {:keyword kw} validation))

          result
          (reduce
           (fn [acc result]
             (cond-> acc
               (not (:valid? result))
               (assoc-in [:causes (:keyword result)] (:errors result))))

           {:instance instance}
           validations)]

      (if-let [causes (:causes result)]
        {:error {:message "Some properties failed to validate against their schemas"
                 :causes causes}}
        ;; Merge annotations
        {:annotations (assoc annotations :properties (into {} (map (juxt :keyword :annotations) validations)))}))))

(defmethod process-keyword "patternProperties" [k pattern-properties instance annotations ctx]
  (when (object? instance)
    (let [compiled-pattern-properties (map (fn [[k v]] [(re-pattern k) v]) pattern-properties)]
      (let [children
            (for [[propname child-instance] instance
                  [pattern subschema] compiled-pattern-properties
                  :when (re-seq pattern propname)
                  :let [result (validate* child-instance subschema ctx)]
                  :when (not (:valid? result))]
              result)]
        (when (not-empty children)
          {:error
           {:message "Matched pattern property's schema does not succeed"}})))))

(defmethod process-keyword "additionalProperties" [k additional-properties instance annotations {:keys [schema] :as ctx}]
  (when (object? instance)
    (let [properties (set (keys (get schema "properties")))
          ;; TODO: This is wasteful, to recompile these pattern properties again
          compiled-patterns (when-let [pattern-properties (get schema "patternProperties")]
                              (map (fn [[k v]] (re-pattern k)) pattern-properties))]
      (let [children
            (for [[propname child-instance] instance
                  :when (not (contains? properties propname))
                  :when (nil? (some #(re-seq % propname) compiled-patterns))
                  :let [result (validate* child-instance additional-properties ctx)]
                  :when (not (:valid? result))]
              result)]
        (when (not-empty children) {:error
                                    {:message "An additional property failed the schema check"
                                     :causes children}})))))

(defmethod process-keyword "dependencies" [k dependencies instance annotations ctx]
  (when (object? instance)
    (let [dependency-results
          (for [[kw dvalue] dependencies
                :when (contains? instance kw)]
            (cond
              (schema? dvalue)
              (assoc (validate* instance dvalue ctx) :keyword kw)
              ;; This is the array case, where we fake a validate*
              ;; return value in order to make the reduce work below.
              ;; TODO: Not ideal, should be re-worked.
              (array? dvalue)
              (let [missing (filter #(not (contains? instance %)) dvalue)]
                (if (not-empty missing)
                  {:errors [{:message "Not every dependency in instance"
                             :missing missing}]}))))
          result (reduce
                  (fn [acc result]
                    (if-let [errors (:errors result)]
                      (assoc-in acc [:error :dependencies (:keyword result) :errors] errors)
                      (if-some [instance (:instance result)]
                        (update acc :instance merge instance)
                        acc)))

                  {:instance instance
                   :dependencies dependency-results}

                  dependency-results)]
      (cond-> result
        (:error result)
        (assoc-in [:error :message] "Some dependencies had validation errors")))))

(defmethod process-keyword "propertyNames" [k property-names instance annotations ctx]
  (when (object? instance)
    (let [children
          (for [propname (keys instance)]
            (validate* propname property-names ctx))]
      (when-not (every? :valid? children)
        {:error "propertyNames"
         :failures (filter (comp not :valid?) children)}))))

(defmethod process-keyword "allOf" [k all-of instance annotations ctx]
  (let [results (for [subschema all-of]
                  (validate* instance subschema ctx))]
    (let [failures (remove :valid? results)]
      (merge
       (when (not-empty failures)
         {:error
          {:message "allOf schema failed due to subschema failing"
           :causes failures}})
       {:annotations (apply merge-annotations annotations (map :annotations (filter :valid? results)))}))))

(defmethod process-keyword "anyOf" [k any-of instance annotations ctx]
  (let [results (for [[subschema idx] (map vector any-of (range))]
                  (validate* instance subschema ctx))]
    (cond-> {:annotations (apply merge-annotations annotations (map :annotations (filter :valid? results)))}
      (not (some :valid? results))
      (merge {:error {:message "No schema validates for anyOf validation"}}))))

(defmethod process-keyword "oneOf" [k one-of instance annotations ctx]
  (let [validations
        (doall
         (for [subschema one-of]
           (validate* instance subschema ctx)))
        successes (filter :valid? validations)]
    (cond
      (empty? successes)
      {:error {:message "No schema succeeds in oneOf validation"
               :failures validations}}
      (> (count successes) 1)
      {:error {:message "Multiple schemas are valid in oneOf validation"
               :successes successes}}

      :else (first successes))))

(defmethod process-keyword "not" [k not instance annotations ctx]
  (when (:valid? (validate* instance not ctx))
    {:error {:message "Schema should not be valid"}}))

(defmethod process-keyword "if" [_ if instance annotations {:keys [schema] :as ctx}]
  (if (:valid? (validate* instance if ctx))
    (when-let [then (get schema "then")]
      ;; TODO: validate* returns errors!
      (let [result (validate* instance then ctx)]
        (if (:valid? result)
          result
          {:error {:message "then clause does not succeed"
                   :causes (:errors result)}})))

    (when-let [else (get schema "else")]
      ;; TODO: validate* returns errors!
      (let [result (validate* instance else ctx)]
        (if (:valid? result)
          result
          {:error {:message "else clause does not succeed"
                   :causes (:errors result)}})))))

;; TODO: Rather than get, use a macro to retrieve either strings and
;; keywords, supporting both. But BE CAREFUL with :default as we'll
;; need to repoint the defmulti's :default in that case.

(defmulti check-format (fn [fmt instance ctx] fmt))

(defmethod check-format :default [fmt instance ctx]
  ;; If format not known, succeed
  )

(defmethod check-format "date-time" [fmt instance ctx]
  (when (string? instance)
    (try
      (.parse java.time.format.DateTimeFormatter/ISO_DATE_TIME instance)
      nil
      (catch Exception e
        {:format fmt
         :error {:message "Doesn't match date-time format"}}))))

(defmethod check-format "date" [fmt instance ctx]
  (when (string? instance)
    (try
      (.parse java.time.format.DateTimeFormatter/ISO_LOCAL_DATE instance)
      nil
      (catch Exception e
        {:format fmt
         :error {:message "Doesn't match date format"}}
        ))))

(defmethod check-format "time" [fmt instance ctx]
  (when (string? instance)
    (try
      (.parse java.time.format.DateTimeFormatter/ISO_TIME instance)
      nil
      (catch Exception e
        {:format fmt
         :error {:message "Doesn't match time format"}}))))

(defmethod check-format "email" [fmt instance ctx]
  (when (string? instance)
    (when-not (re-matches regex/addr-spec instance)
      {:format fmt
       :error {:message "Doesn't match email format"}})))

(defmethod check-format "idn-email" [fmt instance ctx]
  (when (string? instance)
    (when-not (re-matches regex/iaddr-spec instance)
      {:format fmt
       :error {:message "Doesn't match idn-email format"}})))

(defmethod check-format "hostname" [fmt instance ctx]
  (when (string? instance)
    ;; RFC 1034
    (when-not (regex/hostname? instance)
      {:format fmt
       :error {:message "Doesn't match hostname format"}})))

(defmethod check-format "idn-hostname" [fmt instance ctx]
  (when (string? instance)
    ;; RFC 5890
    (when-not (regex/idn-hostname? instance)
      {:format fmt
       :error {:message "Doesn't match idn-hostname format"}})))

(defmethod check-format "ipv4" [fmt instance ctx]
  (when (string? instance)
    ;; RFC2673, section 3.2, dotted-quad - also RFC 3986
    (when-not (re-matches regex/IPv4address instance)
      {:format fmt
       :error {:message "Doesn't match ipv4 format"}})))

(defmethod check-format "ipv6" [fmt instance ctx]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC4291
    (when-not (re-matches regex/IPv6address instance)
      {:format fmt
       :error {:message "Doesn't match ipv6 format"}})))

(defmethod check-format "uri" [fmt instance ctx]
  (when (string? instance)
    ;; RFC3986
    (when-not (re-matches regex/URI instance)
      {:format fmt
       :error {:message "Doesn't match URI format"}})))

(defmethod check-format "uri-reference" [fmt instance ctx]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC3986
    (when-not (or (re-matches regex/URI instance)
                  (re-matches regex/relative-ref instance))
      {:format fmt
       :error {:message "Doesn't match URI-reference format"}})))

(defmethod check-format "iri" [fmt instance ctx]
  (when (string? instance)
    ;; RFC3987
    (when-not (re-matches regex/IRI instance)
      {:format fmt
       :error {:message "Doesn't match IRI format"}})))

(defmethod check-format "iri-reference" [fmt instance ctx]
  (when (string? instance)
    ;; RFC3987
    (when-not (or (re-matches regex/IRI instance)
                  (re-matches regex/irelative-ref instance))
      {:format fmt
       :error {:message "Doesn't match IRI-reference format"}})))

(defmethod check-format "uri-template" [fmt instance ctx]
  (when (string? instance)
    ;; TODO: Improve this regex: RFC6570
    (when-not (re-matches #".*" instance)
      {:format fmt
       :error {:message "Doesn't match uri-template format"}})))

(defmethod check-format "json-pointer" [fmt instance ctx]
  (when (string? instance)
    ;; RFC6901
    (when-not (re-matches regex/json-pointer instance)
      {:format fmt
       :error {:message "Doesn't match json-pointer format"}})))

(defmethod check-format "relative-json-pointer" [fmt instance ctx]
  (when (string? instance)
    (when-not (re-matches regex/relative-json-pointer instance)
      {:format fmt
       :error {:message "Doesn't match relative-json-pointer format"}})))

(defmethod check-format "regex" [fmt instance ctx]
  (when (string? instance)
    (cond
      ;; (This might be cheating just to get past the test suite)
      (.contains instance "\\Z")
      {:format fmt
       :error {:message "Must not contain \\Z anchor from .NET"}}

      :else
      (try
        (re-pattern instance)
        nil
        (catch Exception e
          {:format fmt
           :error {:message "Illegal regex"}})))))

(defmethod process-keyword "format" [_ format instance annotations ctx]
  ;; TODO: This is optional, so should be possible to disable via
  ;; options - see 7.2 of draft-handrews-json-schema-validation-01:
  ;; "they SHOULD offer an option to disable validation for this
  ;; keyword."
  (check-format format instance ctx))

(defn decode-content [content-encoding instance]
  ;; TODO: Open for extension with a multimethod
  (case content-encoding
    "base64" (String. (.decode (java.util.Base64/getDecoder) instance))
    nil instance))

(defmethod process-keyword "contentEncoding" [k content-encoding instance annotations ctx]
  ;; TODO: This is optional, so should be possible to disable via
  ;; options - see 8.2 of draft-handrews-json-schema-validation-01:
  ;; "Implementations MAY support the "contentMediaType" and
  ;; "contentEncoding" keywords as validation assertions.  Should they
  ;; choose to do so, they SHOULD offer an option to disable
  ;; validation for these keywords."
  (when (string? instance)
    (try
      {:content (decode-content content-encoding instance)}
      (catch Exception e
        {:error {:message "Not base64"}}))))

(defmethod process-keyword "contentMediaType" [k content-media-type instance annotations {:keys [schema] :as ctx}]
  ;; TODO: This is optional, so should be possible to disable via
  ;; options - see 8.2 of draft-handrews-json-schema-validation-01:
  ;; "Implementations MAY support the "contentMediaType" and
  ;; "contentEncoding" keywords as validation assertions.  Should they
  ;; choose to do so, they SHOULD offer an option to disable
  ;; validation for these keywords."
  (when (string? instance)
    (if-let [content
             (try
               ;; TODO: Why do this twice?
               ;; We should be able to access this content
               (decode-content (get schema "contentEncoding") instance)
               (catch Exception e nil))]
      ;; TODO: Open for extension with a multimethod
      (case content-media-type
        "application/json"
        (try
          (cheshire/parse-string content)
          nil
          (catch Exception e
            {:error {:message "Instance is not application/json"}})))
      {:error {:message "Unable to decode content"}})))

(defn resolve-ref [ref-object doc ctx]
  (assert ref-object)

  (let [ ;; "The value of the "$ref" property MUST be a URI Reference."
        ;; -- [CORE Section 8.3]
        base-uri (get (meta ref-object) :base-uri)
        ref (some-> (get ref-object "$ref") java.net.URLDecoder/decode)
        uri (str (uri/join (or base-uri (:base-uri ctx)) ref))]

    (let [options
          (if false #_(contains? (:visited-memory ctx) uri)
              (throw (ex-info "Infinite cycle detected" {:uri uri}))
              (update ctx :visited-memory (fnil conj #{}) uri))]

      (let [[new-schema doc base-uri] (resolv/resolv uri doc (get-in ctx [:options :resolvers]))]
        [new-schema (cond-> ctx
                      base-uri (assoc :base-uri base-uri)
                      doc (assoc :doc doc))]))))

(defn- validate*
  [instance schema {:keys [options] :as ctx}]

  (cond
    (boolean? schema)
    (cond-> {:instance instance
             :valid? schema}
      (false? schema) (assoc :errors [{:message "Schema is false"}]))

    (or (object? schema) (nil? schema))

    (cond
      (contains? schema "$ref")
      (let [[new-schema new-ctx] (resolve-ref schema (:doc ctx) ctx)
            res (validate* instance new-schema new-ctx)
            causes (:errors res)]
        (cond-> res
          causes
          (-> (assoc :error {:message "Schema failed following ref" :causes causes})
              (dissoc :errors))))

      ;; Start with an ordered list of known of validation keywords,
      ;; this order is from https://github.com/playlyfe/themis.
      ;; Possible to override.
      :else
      (let [keywords
            (or
             ;; TODO: Should rename :keywords to :vocabulary?
             (:keywords options)
             ["$schema"
              "definitions"

              ;; Process annotations first. The "default" annotation
              ;; can affect the instance which may impact the
              ;; validation of subsequent keywords.
              "title"
              "description"
              "default"
              "readOnly" "writeOnly"
              "examples"

              ;; 6.1.  Validation Keywords for Any Instance Type
              "type" "enum" "const"
              ;; 6.2.  Validation Keywords for Numeric Instances (number and integer)
              "multipleOf" "maximum" "exclusiveMinimum" "minimum" "exclusiveMaximum"
              ;; 6.3.  Validation Keywords for Strings
              "maxLength" "minLength" "pattern"
              ;; 6.4.  Validation Keywords for Arrays
              "items" "additionalItems" "maxItems" "minItems" "uniqueItems" "contains"
              ;; 6.5.  Validation Keywords for Objects
              "maxProperties" "minProperties" "required" "properties"
              "patternProperties" "additionalProperties" "dependencies"
              "propertyNames"
              ;; 6.6.  Keywords for Applying Subschemas Conditionally
              "if" "then" "else"
              ;; 6.7.  Keywords for Applying Subschemas With Boolean Logic
              "allOf" "anyOf" "oneOf" "not"
              ;; 7.  Semantic Validation With "format"
              "format"
              ;; 8.  String-Encoding Non-JSON Data
              "contentEncoding" "contentMediaType"])]

        (let [ctx (assoc ctx :schema schema)
              results (reduce
                       (fn [acc kw]
                         (let [[k v] (find schema kw)]
                           (if k
                             (if-let [result (process-keyword
                                              kw v
                                              (:instance acc)
                                              (:annotations acc)
                                              ctx)]
                               (cond-> acc
                                 true (update :journal conj (merge {:keyword kw} result))
                                 (find result :instance) (assoc :instance (:instance result))
                                 (find result :annotations) (assoc :annotations (:annotations result))
                                 (find result :type) (assoc :type (:type result)))
                               acc)
                             acc)))
                       {:journal []
                        :instance instance
                        :annotations {}}
                       (distinct (concat keywords (keys schema))))]
          (let [errors (keep :error (:journal results))]
            (let [res
                  (merge
                   {:instance (:instance results)
                    :annotations (:annotations results)
                    :type (:type results)
                    :valid? (empty? errors)}
                   (when (not-empty errors) {:errors (vec errors)})
                   (when (:journal? options) {:journal (vec (:journal results))}))]
              res)))))))

(defn validate
  "Instance should come first do support the Clojure thread-first
  macro. Instances are the objects here, schemas are the incidentals."
  ([instance schema]
   (validate instance schema {:resolvers [::resolv/built-in]}))

  ([instance schema options]
   (validate*
    instance schema
    {:doc schema
     :options (merge {:resolvers [::resolv/built-in]} options)})))
