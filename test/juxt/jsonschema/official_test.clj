;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.official-test
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [juxt.jsonschema.validate :refer [validate]]
   [juxt.jsonschema.schema :refer [schema]]
   [juxt.jsonschema.resolve :as resolv]
   [clojure.set :as set]
   [clojure.test :as test]
   [juxt.jsonschema.schema :as schema]
   [cheshire.core :as cheshire]))

(def TESTS-ROOT (io/file (System/getenv "JUXT_REPOS") "JSON-Schema-Test-Suite"))

(def TESTS-DIR (io/file TESTS-ROOT "tests/draft7"))

(defn test-jsonschema [{:keys [schema data valid] :as test}]
  (try
    (let [schema (schema/schema schema)
          result (validate
                  data schema
                  {:resolvers
                   [::resolv/built-in
                    [::resolv/default-resolver
                     {#"http://localhost:1234/(.*)"
                      (fn [match]
                        (io/file (io/file TESTS-ROOT "remotes") (second match))
                        )}]]})
          success? (if valid (:valid? result)
                       (not (:valid? result)))]
      (cond-> test
        success? (assoc :result :success)
        (and (not success?) valid) (assoc :failures (:error result))
        (and (empty? result) (not valid)) (assoc :failures [{:message "Incorrectly judged valid"}])))
    (catch Throwable e (merge test {:result :error
                                    :error e}))))

(defn success? [x] (= (:result x) :success))

;; Test suite

(defn tests
  [tests-dir]
  (for [testfile (file-seq TESTS-DIR)

        ;;         filename (-> tests-dir .list sort)
        ;; Test filenames implemented so far or being worked on currently
        ;;         :when ((or filename-pred some?) filename)
        ;;         :let [testfile (io/file tests-dir filename)]

        :when (.isFile testfile)
        :let [objects (json/parse-stream (io/reader testfile))]
        {:strs [schema tests description]} objects
        ;; Any required parsing of the schema, do it now for performance
        :let [test-group-description description]
        test tests
        :let [{:strs [description data valid]} test]]
    {:filename (str testfile)
     :test-group-description test-group-description
     :test-description description
     :schema schema
     :data data
     :valid valid}))

;; TODO: Pull out defaults and refs from validation keywords - this is
;; premature abstraction

(defn exclude-test? [test]
  (contains?
   #{"format: uri-template"
     "validation of an internationalized e-mail addresses"}
   (:test-group-description test)))

(defn make-tests []
  (doseq [test (remove exclude-test? (tests TESTS-DIR))]
    (let [testname (symbol (str (gensym "test") "-test"))]
      (eval `(test/deftest ~(vary-meta testname assoc :official true) ~testname
               (test/testing ~(:test-description test)
                 (test/is (success? (test-jsonschema ~test)))))))))

(make-tests)
