;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.validate-test
  #?@(:clj [(:require
             [juxt.jinx.alpha.validate :as validate]
             [clojure.test :refer [deftest is are testing]])]
      :cljs [(:require
              [juxt.jinx.alpha.validate :as validate]
              [cljs.test :refer-macros [deftest is are testing run-tests]])]))

(defn run-validate [schema instance]
  (let [result (validate/validate schema instance)]
    [(:valid? result) (:instance result)]))

(deftest boolean-schema-test []
  (testing "true schema always true"
    (is (= [true {"foo" "bar"}]
           (run-validate true {"foo" "bar"}))))

  (testing "false schema always false"
    (is (= [false {"foo" "bar"}]
           (run-validate false {"foo" "bar"})))))

(deftest boolean-test []
  (is (= [true false]
         (run-validate
          {"type" "boolean"}
          false)))

  (is (= [true true]
         (run-validate
          {"type" "boolean"}
          true)))

  (testing "object is not boolean"
    (is (= [false {}]
           (run-validate
            {"type" "boolean"}
            {})))))

(deftest number-test []
  (is (= [true 10]
         (run-validate
          {"type" "number"}
          10)))

  (is (= [true 21]
         (run-validate
          {"type" "number"
           "multipleOf" 7}
          21)))

  (is (= [false 20]
         (run-validate
          {"type" "number"
           "multipleOf" 7}
          20)))

  (is (= [true 100]
         (run-validate
          {"type" "number"
           "maximum" 100}
          100)))

  (is (= [false 100]
         (run-validate
          {"type" "number"
           "exclusiveMaximum" 100}
          100)))

  (is (= [true 10]
         (run-validate
          {"type" "number"
           "minimum" 10}
          10)))

  (is (= [false 10]
         (run-validate
          {"type" "number"
           "exclusiveMinimum" 10}
          10))))

(deftest string-test []
  ;;  A string is a valid string.
  (is (= [true "a string"]
         (run-validate
          {"type" "string"}
          "a string")))

  ;;  A number is not a valid string.
  (is (= [false 123]
         (run-validate
          {"type" "string"}
          123)))

  ;; Nil is not a valid string.
  (is (= [false nil]
         (run-validate
          {"type" "string"}
          nil)))

  ;; Even if there is a default string, nil isn't valid.
  (is (= [false nil]
         (run-validate
          {"type" "string"
           "default" "default-string"}
          nil)))

  ;; Prefer the existing instance to the default.
  (is (= [true "a string"]
         (run-validate
          {"type" "string"
           "default" "default-string"}
          "a string")))

  ;; Prefer the existing instance to the default, even if invalid.
  ;; (reinstate)
  (is (= [false 123]
         (run-validate
          {"type" "string"
           "default" "default-string"}
          123)))

  ;; A nil value is replaced with the default value, even if the
  ;; result isn't itself valid.
  ;; (reinstate)
  #_(is (= [false 123]
           (run-validate
            {"type" "string"
             "default" 123}
            nil
            )))

  ;; If string is within the max length, validation succeeds.
  (is (= [true "fo"]
         (run-validate
          {"type" "string"
           "maxLength" 3}
          "fo")))

  ;; If string is the same as the max length, validation succeeds.
  (is (= [true "foo"]
         (run-validate
          {"type" "string"
           "maxLength" 3}
          "foo")))

  ;; If string is over the max length, validation fails.
  (is (= [false "foo"]
         (run-validate
          {"type" "string"
           "maxLength" 2}
          "foo")))

  ;; If string is over the min length, validation succeeds.
  (is (= [true "food"]
         (run-validate
          {"type" "string"
           "minLength" 3}
          "food")))

  ;; If string is the same as the min length, validation succeeds.
  (is (= [true "foo"]
         (run-validate
          {"type" "string"
           "minLength" 3}
          "foo")))

  ;; If string is under the min length, validation fails.
  (is (= [false "fo"]
         (run-validate
          {"type" "string"
           "minLength" 3}
          "fo"))))

(deftest enum-test
  (is (= [true "b"]
         (run-validate
          {"enum" ["a" "b" "c"]}
          "b")))
  ;; (reinstate)
  #_(is (= [true "b"]
           (run-validate nil {"enum" ["a" "b" "c"]
                              "default" "b"}))))

(deftest arrays-test
  (is (= [true []]
         (run-validate
          {"type" "array"}
          [])))

  (is (= [true [true true false true]]
         (run-validate
          {"type" "array"
           "items" {"type" "boolean"}}
          [true true false true] )))

  (is (= [true [1 2 3]]
         (run-validate
          {"type" "array"
           "items" {"type" "number"}}
          [1 2 3] )))


  (is (= [false [1 2 "foo"]]
         (run-validate
          {"type" "array"
           "items" {"type" "number"}}
          [1 2 "foo"] )))

  (is (= [true [1 2 "foo"]]
         (run-validate
          {"type" "array"
           "items" [{"type" "number"}
                    {"type" "number"}
                    {"type" "string"}]}
          [1 2 "foo"] )))

  (is (= [true [1 2 "foo" 10]]
         (run-validate
          {"type" "array"
           "items" [{"type" "number"}
                    {"type" "number"}
                    {"type" "string"}]}
          [1 2 "foo" 10]))))

(deftest additional-items-test
  (is (= [true [1 2 "foo" 10]]
         (run-validate
          {"type" "array"
           "items" [{"type" "number"}
                    {"type" "number"}
                    {"type" "string"}]
           "additionalItems" true}
          [1 2 "foo" 10] )))

  (is (= [false [1 2 "foo" 10]]
         (run-validate
          {"type" "array"
           "items" [{"type" "number"}
                    {"type" "number"}
                    {"type" "string"}]
           "additionalItems" false}
          [1 2 "foo" 10]))))

(deftest min-items-test
  (is (= [true [true 10 20 20]]
         (run-validate
          {"type" "array"
           "items" [{"type" "boolean"}
                    {"type" "number"}
                    {"type" "number"}
                    {"type" "number"}]
           "additionalItems" {"type" "string"}
           "uniqueItems" false}
          [true 10 20 20])))

  (is (= [false [true 10 20 20]]
         (run-validate
          {"type" "array"
           "items" [{"type" "boolean"}
                    {"type" "number"}
                    {"type" "number"}
                    {"type" "number"}]
           "additionalItems" {"type" "string"}
           "uniqueItems" true}
          [true 10 20 20]))))

(deftest empty-array-unique-items-test
  (testing "empty arrays validated with uniqueItems work (#24)"
    (is (= [true []]
          (run-validate
            {"type" "array"
             "uniqueItems" true}
            [])))

    (is (= [false []]
          (run-validate
            {"type" "array"
             "uniqueItems" true
             "minItems" 1}
            [])))))

(deftest object-test
  (is (= [true {}]
         (run-validate
          {"type" "object"}
          {})))

  (is (= [true {"foo" "bar"}]
         (run-validate {"type" "object"} {"foo" "bar"}))))

(deftest properties-test
  (is (= [false {"foo" "bar"}]
         (run-validate
          {"type" "object"
           "properties" {"foo" {"type" "number"}}}
          {"foo" "bar"})))

  (is (= [true {"foo" {"bar" 10}}]
         (run-validate
          {"type" "object"
           "properties" {"foo" {"type" "object"
                                "properties" {"bar" {"type" "number"}}}}}
          {"foo" {"bar" 10}})))

  (is (= [false {"foo" {"bar" 10}}]
         (run-validate
          {"type" "object"
           "properties" {"foo" {"type" "object"
                                "properties" {"bar" {"type" "string"}}}}}
          {"foo" {"bar" 10}}))))

(deftest properties-test1
  (is (= [true {"foo" "bar"}]
         (run-validate
          {"type" "object"
           "required" ["foo"]
           "properties" {"foo" {"type" "string"
                                "default" "bar"}}}
          {}))))

;; Do not imply default values for objects and arrays
;; (Possibly re-instate)
#_(deftest recover-from-type-failure-test
    (is (= [false nil]
           (run-validate {"type" "object"} nil)))
    (is (= [false nil]
           (run-validate {"type" "array"} nil))))


#_(validate
   {"type" "object"
  "required" ["foo"]
  "properties" {"foo" {"type" "object"
                       "required" ["bar"]
                       "properties" {"bar" {"default" "zip"}}
                       "default" {"abc" 123}}}}
    {}
)

(deftest recover-from-required-failure-test
  ;; Possibly re-instate
  #_(testing "Recover with child default"
      (is
       (= [true {"foo" {"abc" 123, "bar" "zip"}}]
          (run-validate
           {"type" "object"
            "required" ["foo"]
            "properties" {"foo" {"type" "object"
                                 "required" ["bar"]
                                 "properties" {"bar" {"default" "zip"}}
                                 "default" {"abc" 123}}}}
           {}))))

  (testing "Do not recover from nil parent, as default values are not implied"
    (is
     (= [false nil]
        (run-validate
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}
                               "default" {"abc" 123}}}}
         nil))))

  (testing "Don't recover, no implied default for an object"
    ;; I didn't think it would be a good idea to imply default values
    ;; for objects/arrays, etc. However, that causes oneOf and anyOf
    ;; branches to start passing when they should definitely not be
    ;; (principle of least surprise). So instead, let's test that we
    ;; don't start implying defaults.
    (is
     (= [false {}]
        (run-validate
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}}}}
         {}))))

  ;; Might be able to leave with this failing
  (testing "No recovery, as no implied default child with nil parent"
    (is
     (= [false nil]
        (run-validate
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}}}}
         nil)))))

(deftest dependencies_test
  (testing "Recover with child default"
    (is
     (= [true {"foo" 1 "bar" 2}]
        (run-validate
         {"dependencies"
          {"bar"
           {"properties" {"foo" {"type" "integer"}
                          "bar" {"type" "integer"}}}}}
         {"foo" 1 "bar" 2})))

    ;; No recovery, possibly re-instate
    #_(is
       (=
        [true {"bar" 1 "foo" 42}]
        (run-validate
         {"dependencies"
          {"bar"
           {"required" ["foo"]
            "properties" {"foo" {"type" "integer"
                                 "default" 42}
                          "bar" {"type" "integer"}}}}}
         {"bar" 1})))

    ;; No recovery, possibly re-instate
    #_(is
       (=
        [true {"bar" 2 "foo" 24}]
        (run-validate
         {"dependencies"
          {"bar"
           {"oneOf" [{"required" ["foo"]
                      "properties" {"foo" {"type" "integer"
                                           "default" 42}
                                    "bar" {"type" "integer"
                                           "const" 1}}}
                     {"required" ["foo"]
                      "properties" {"foo" {"type" "integer"
                                           "default" 24}
                                    "bar" {"type" "integer"
                                           "const" 2}}}]}}}
         {"bar" 2})))))
