;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.official-test-suite
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [juxt.jsonschema.validate :refer [validate]]
   [clojure.set :as set]))

(defn test-jsonschema [{:keys [schema data valid] :as test}]
  (try
    (let [result (validate schema data)
          success? (if valid (empty? result)
                       (not (empty? result)))]
      (cond-> test
        success? (assoc :result :success)
        (and (not success?) valid) (assoc :failures (vec result))
        (and (empty? result) (not valid)) (assoc :failures [{:message "Incorrectly judged valid"}])))
    (catch Exception e (merge test {:result :error
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

(def TESTS-DIR (-> (System/getenv "JUXT_REPOS")
                   (io/file "JSON-Schema-Test-Suite/tests/draft7")))

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
    "propertyNames.json"})

(comment
  "Get a list of the tests yet to implement"
  (set/difference
   (set (filter seq (map (comp str #(.relativize (.toPath TESTS-DIR) %) (memfn toPath)) (file-seq TESTS-DIR))))
   IMPLEMENTED))

(comment
  "Run tests, show failures"
  (let [results
        (->> IMPLEMENTED
             (tests TESTS-DIR)
             (map test-jsonschema))
        failing (remove success? results)]

    {:total-run (count results)
     :passing (count (keep success? results))
     :failing (count failing)
     :failure-detail failing}))
