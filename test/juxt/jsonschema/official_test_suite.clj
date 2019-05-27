;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.official-test-suite
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [juxt.jsonschema.validate :refer [validate]]
   [juxt.jsonschema.schema :refer [schema]]
   [juxt.jsonschema.resolve :as resolv]
   [clojure.set :as set]
   [juxt.jsonschema.schema :as schema]
   [cheshire.core :as cheshire]))

(def TESTS-ROOT (io/file (System/getenv "JUXT_REPOS") "JSON-Schema-Test-Suite"))

(def TESTS-DIR (io/file TESTS-ROOT "tests/draft7"))

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

(comment
  "Run tests, show failures"
  (let [results
        (->> (tests TESTS-DIR)
             (map test-jsonschema))
        failing (remove success? results)]

    {:total-run (count results)
     :passing (count (filter success? results))
     :pass-rate (format "%f%%" (float (* 100 (/ (count (filter success? results)) (count results)))))
     :failing (count failing)
     :failure-detail failing}))

;; failing 58
;; failing 56
;; failing 52
;; failing 50
;; failing 49
;; failing 48
;; failing 47
;; failing 46
;; failing 45
;; failing 44
;; failing 43
;; failing 42
;; failing 40
;; failing 38
;; failing 35
;; failing 33
;; failing 27
;; failing 25
;; failing 21

(let [test
      {:filename
       "/home/malcolm/src/JSON-Schema-Test-Suite/tests/draft7/optional/format/iri.json",
       :test-group-description "validation of IRIs",
       :test-description "a valid IRI based on IPv6",
       :schema {"format" "iri"},
       :data "http://[2001:0db8:85a3:0000:0000:8a2e:0370:7334]",
       :valid true,
       :failures [{:message "Doesn't match IRI format"}]}]

  (validate
   (schema/schema (:schema test))
   (:data test)
   {:resolvers
    [::resolv/built-in
     [::resolv/default-resolver
      {#"http://localhost:1234/(.*)"
       (fn [match]
         (io/file (io/file TESTS-ROOT "remotes") (second match))
         )}]]}))
