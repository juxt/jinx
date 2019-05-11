(ns juxt.jsonschema.test
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [juxt.jsonschema.validate :refer [validate]]))

(defn failures [{:keys [schema data valid] :as all}]
  (let [result (validate schema data)
        success? (if valid (empty? result)
                     (not (empty? result)))]
    (when-not success?
      all)))

;; --- Test suite

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

(comment (count (tests TESTS-DIR #{"type.json"})))


(def TESTS-DIR (-> (System/getenv "JUXT_REPOS")
                  (io/file "JSON-Schema-Test-Suite/tests/draft7")))

(keep failures (tests TESTS-DIR #{"type.json"
                                  "enum.json"
                                  "const.json"
                                  }))
