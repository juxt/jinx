;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.official-test-suite
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [juxt.jsonschema.validate :refer [validate]]
   [juxt.jsonschema.schema :refer [schema]]
   [juxt.jsonschema.resolve :as resolv]
   [clojure.set :as set]
   [juxt.jsonschema.schema :as schema]))

(def TESTS-ROOT (io/file (System/getenv "JUXT_REPOS") "JSON-Schema-Test-Suite"))

(defn test-jsonschema [{:keys [schema data valid] :as test}]
  (try
    (let [schema (schema/schema schema)
          result (validate
                  schema data
                  {:resolvers
                   [::resolv/built-in
                    [::resolv/default-resolver
                     {#"http://localhost:1234/(.*)"
                      (fn [match]
                        (io/file (io/file TESTS-ROOT "remotes") (second match))
                        )}]]})
          success? (if valid (empty? result)
                       (not (empty? result)))]
      (cond-> test
        success? (assoc :result :success)
        (and (not success?) valid) (assoc :failures (vec result))
        (and (empty? result) (not valid)) (assoc :failures [{:message "Incorrectly judged valid"}])))
    (catch Throwable e (merge test {:result :error
                                    :error e}))))

(defn success? [x] (= (:result x) :success))

;; Test suite

(defn tests
  ([tests-dir]
   (tests tests-dir nil))
  ([tests-dir filename-pred]
   (for [filename (-> tests-dir .list sort)
         ;; Test filenames implemented so far or being worked on currently
         :when ((or filename-pred some?) filename)
         :let [testfile (io/file tests-dir filename)]
         :when (.isFile testfile)
         :let [objects (json/parse-stream (io/reader testfile))]
         {:strs [schema tests description]} objects
         ;; Any required parsing of the schema, do it now for performance
         :let [test-group-description description]
         test tests
         :let [{:strs [description data valid]} test]]
     {:filename filename
      :test-group-description test-group-description
      :test-description description
      :schema schema
      :data data
      :valid valid})))

(def TESTS-DIR (io/file TESTS-ROOT "tests/draft7"))

(def IMPLEMENTED
  #{"boolean_schema.json"
    "type.json"
    "enum.json"
    "const.json"
    "maxLength.json"
    "minLength.json"
    "pattern.json"
    "items.json"
    "additionalItems.json"
    "maxItems.json"
    "minItems.json"
    "uniqueItems.json"
    "multipleOf.json"
    "maximum.json"
    "exclusiveMaximum.json"
    "minimum.json"
    "exclusiveMinimum.json"
    "contains.json"
    "maxProperties.json"
    "minProperties.json"
    "required.json"
    "properties.json"
    "patternProperties.json"
    "additionalProperties.json"
    "default.json"
    "dependencies.json"
    "propertyNames.json"
    "allOf.json"
    "anyOf.json"
    "oneOf.json"
    "not.json"
    "if-then-else.json"
    "ref.json"
    "definitions.json"
    "refRemote.json"
    })

(comment
  "Get a list of the tests yet to implement"
  (set/difference
   (set (filter seq (map (comp str #(.relativize (.toPath TESTS-DIR) %) (memfn toPath)) (filter (memfn isFile) (file-seq TESTS-DIR)))))
   IMPLEMENTED))

(comment
  "Run tests, show failures"
  (let [results
        (->> IMPLEMENTED
             (tests TESTS-DIR)
             (map test-jsonschema))
        failing (remove success? results)]

    {:total-run (count results)
     :passing (count (filter success? results))
     :percent (format "%f%%" (float (* 100 (/ (count (filter success? results)) (count results)))))
     :failing (count failing)
     :failure-detail failing}))

#_(let [test
      {:filename "refRemote.json",
       :test-group-description "remote ref",
       :test-description "remote ref valid",
       :schema {"$ref" "http://localhost:1234/integer.json"},
       :data 1,
       :valid true,
       :result :error,
       }]

  (validate
   (schema/schema (:schema test))
   (:data test)
   {:resolvers [::resolv/built-in
                [::resolv/default-resolver
                 {#"http://localhost:1234/(.*)"
                  (fn [match]
                    (io/file (io/file TESTS-ROOT "remotes") (second match))
                    )}]]}))
