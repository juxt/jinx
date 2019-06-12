(ns juxt.jsonschema.schema-test
  (:require
   [juxt.jsonschema.schema :refer [schema]]
   [clojure.test :refer [deftest is are testing]]
   [juxt.jsonschema.schema :as schema])
  (:import
   (clojure.lang ExceptionInfo)))

(deftest schema-test
  (testing "bad types"
    (is
     (thrown?
      ExceptionInfo
      (schema {"type" 10})))))
