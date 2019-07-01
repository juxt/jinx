(ns juxt.jsonschema.validate-test
  (:require
   [juxt.jsonschema.validate :refer [validate]]
   [clojure.test :refer [deftest is are testing]]))

(defn run-validate [instance schema]
  (let [result (validate instance schema)]
    [(:valid? result) (:instance result)]))

(deftest boolean-schema-test []
  (testing "true schema always true")
  (is (= [true {"foo" "bar"}]
         (run-validate {"foo" "bar"} true)))

  (testing "false schema always false"
    (is (= [false {"foo" "bar"}]
           (run-validate {"foo" "bar"} false)))))

(deftest boolean-test []
  (is (= [true false]
         (run-validate false {"type" "boolean"})))

  (is (= [true true]
         (run-validate true {"type" "boolean"})))

  (testing "object is not boolean"
    (is (= [false {}]
           (run-validate  {} {"type" "boolean"})))))

(deftest number-test []
  (is (= [true 10]
         (run-validate 10 {"type" "number"})))

  (is (= [true 21]
         (run-validate 21 {"type" "number"
                           "multipleOf" 7})))

  (is (= [false 20]
         (run-validate 20 {"type" "number"
                           "multipleOf" 7})))

  (is (= [true 100]
         (run-validate 100 {"type" "number"
                            "maximum" 100})))

  (is (= [false 100]
         (run-validate 100 {"type" "number"
                            "exclusiveMaximum" 100})))

  (is (= [true 10]
         (run-validate 10 {"type" "number"
                           "minimum" 10})))

  (is (= [false 10]
         (run-validate 10 {"type" "number"
                           "exclusiveMinimum" 10}))))

(deftest string-test []
  ;;  A string is a valid string.
  (is (= [true "a string"]
         (run-validate
          "a string"
          {"type" "string"})))

  ;;  A number is not a valid string.
  (is (= [false 123]
         (run-validate
          123
          {"type" "string"})))

  ;; Nil is not a valid string.
  (is (= [false nil]
         (run-validate
          nil
          {"type" "string"})))

  ;; Even if there is a default string, nil isn't valid.
  (is (= [false nil]
         (run-validate
          nil
          {"type" "string"
           "default" "default-string"})))

  ;; Prefer the existing instance to the default.
  (is (= [true "a string"]
         (run-validate
          "a string"
          {"type" "string"
           "default" "default-string"})))

  ;; Prefer the existing instance to the default, even if invalid.
  ;; (reinstate)
  (is (= [false 123]
         (run-validate
          123
          {"type" "string"
           "default" "default-string"})))

  ;; A nil value is replaced with the default value, even if the
  ;; result isn't itself valid.
  ;; (reinstate)
  #_(is (= [false 123]
         (run-validate
          nil
          {"type" "string"
           "default" 123})))

  ;; If string is within the max length, validation succeeds.
  (is (= [true "fo"]
         (run-validate
          "fo"
          {"type" "string"
           "maxLength" 3})))

  ;; If string is the same as the max length, validation succeeds.
  (is (= [true "foo"]
         (run-validate
          "foo"
          {"type" "string"
           "maxLength" 3})))

  ;; If string is over the max length, validation fails.
  (is (= [false "foo"]
         (run-validate "foo"
                       {"type" "string"
                        "maxLength" 2})))

  ;; If string is over the min length, validation succeeds.
  (is (= [true "food"]
         (run-validate
          "food"
          {"type" "string"
           "minLength" 3})))

  ;; If string is the same as the min length, validation succeeds.
  (is (= [true "foo"]
         (run-validate "foo"
                       {"type" "string"
                        "minLength" 3})))

  ;; If string is under the min length, validation fails.
  (is (= [false "fo"]
         (run-validate
          "fo"
          {"type" "string"
           "minLength" 3}))))

(deftest enum-test
  (is (= [true "b"]
         (run-validate "b" {"enum" ["a" "b" "c"]})))
  ;; (reinstate)
  #_(is (= [true "b"]
         (run-validate nil {"enum" ["a" "b" "c"]
                            "default" "b"}))))

(deftest arrays-test
  (is (= [true []]
         (run-validate [] {"type" "array"})))

  (is (= [true [true true false true]]
         (run-validate [true true false true] {"type" "array"
                                               "items" {"type" "boolean"}})))

  (is (= [true [1 2 3]]
         (run-validate [1 2 3] {"type" "array"
                                "items" {"type" "number"}})))


  (is (= [false [1 2 "foo"]]
         (run-validate [1 2 "foo"] {"type" "array"
                                    "items" {"type" "number"}})))

  (is (= [true [1 2 "foo"]]
         (run-validate [1 2 "foo"] {"type" "array"
                                    "items" [{"type" "number"}
                                             {"type" "number"}
                                             {"type" "string"}]})))

  (is (= [true [1 2 "foo" 10]]
         (run-validate [1 2 "foo" 10] {"type" "array"
                                       "items" [{"type" "number"}
                                                {"type" "number"}
                                                {"type" "string"}]}))))

(deftest additional-items-test
  (is (= [true [1 2 "foo" 10]]
         (run-validate [1 2 "foo" 10] {"type" "array"
                                       "items" [{"type" "number"}
                                                {"type" "number"}
                                                {"type" "string"}]
                                       "additionalItems" true})))

  (is (= [false [1 2 "foo" 10]]
         (run-validate [1 2 "foo" 10] {"type" "array"
                                       "items" [{"type" "number"}
                                                {"type" "number"}
                                                {"type" "string"}]
                                       "additionalItems" false}))))

(deftest min-items-test
  (is (= [true [true 10 20 20]]
         (run-validate
          [true 10 20 20]
          {"type" "array"
           "items" [{"type" "boolean"}
                    {"type" "number"}
                    {"type" "number"}
                    {"type" "number"}]
           "additionalItems" {"type" "string"}
           "uniqueItems" false})))

  (is (= [false [true 10 20 20]]
         (run-validate
          [true 10 20 20]
          {"type" "array"
           "items" [{"type" "boolean"}
                    {"type" "number"}
                    {"type" "number"}
                    {"type" "number"}]
           "additionalItems" {"type" "string"}
           "uniqueItems" true}))))

(deftest object-test
  (is (= [true {}]
         (run-validate {} {"type" "object"})))

  (is (= [true {"foo" "bar"}]
         (run-validate {"foo" "bar"} {"type" "object"}))))

(deftest properties-test
  (is (= [false {"foo" "bar"}]
         (run-validate
          {"foo" "bar"}
          {"type" "object"
           "properties" {"foo" {"type" "number"}}})))

  (is (= [true {"foo" {"bar" 10}}]
         (run-validate
          {"foo" {"bar" 10}}
          {"type" "object"
           "properties" {"foo" {"type" "object"
                                "properties" {"bar" {"type" "number"}}}}})))

  (is (= [false {"foo" {"bar" 10}}]
         (run-validate
          {"foo" {"bar" 10}}
          {"type" "object"
           "properties" {"foo" {"type" "object"
                                "properties" {"bar" {"type" "string"}}}}}))))

;; Possibly re-instate
#_(deftest properties-test
  (is (= [true {"foo" "bar"}]
         (run-validate
          {}
          {"type" "object"
           "required" ["foo"]
           "properties" {"foo" {"type" "string"
                                "default" "bar"}}}))))

;; Do not imply default values for objects and arrays
;; (Possibly re-instate)
#_(deftest recover-from-type-failure-test
  (is (= [false nil]
         (run-validate nil {"type" "object"})))
  (is (= [false nil]
         (run-validate nil {"type" "array"}))))


#_(validate
 {}
 {"type" "object"
  "required" ["foo"]
  "properties" {"foo" {"type" "object"
                       "required" ["bar"]
                       "properties" {"bar" {"default" "zip"}}
                       "default" {"abc" 123}}}})


(deftest recover-from-required-failure-test
  ;; Possibly re-instate
  #_(testing "Recover with child default"
    (is
     (= [true {"foo" {"abc" 123, "bar" "zip"}}]
        (run-validate
         {}
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}
                               "default" {"abc" 123}}}}))))

  (testing "Do not recover from nil parent, as default values are not implied"
    (is
     (= [false nil]
        (run-validate
         nil
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}
                               "default" {"abc" 123}}}}))))

  (testing "Don't recover, no implied default for an object"
    ;; I didn't think it would be a good idea to imply default values
    ;; for objects/arrays, etc. However, that causes oneOf and anyOf
    ;; branches to start passing when they should definitely not be
    ;; (principle of least surprise). So instead, let's test that we
    ;; don't start implying defaults.
    (is
     (= [false {}]
        (run-validate
         {}
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}}}}))))

  ;; Might be able to leave with this failing
  (testing "No recovery, as no implied default child with nil parent"
    (is
     (= [false nil]
        (run-validate
         nil
         {"type" "object"
          "required" ["foo"]
          "properties" {"foo" {"type" "object"
                               "required" ["bar"]
                               "properties" {"bar" {"default" "zip"}}}}})))))


(deftest dependencies_test
  (testing "Recover with child default"
    (is
     (= [true {"foo" 1 "bar" 2}]
        (run-validate
         {"foo" 1 "bar" 2}
         {"dependencies"
          {"bar"
           {"properties" {"foo" {"type" "integer"}
                          "bar" {"type" "integer"}}}}})))

    ;; No recovery, possibly re-instate
    #_(is
     (=
      [true {"bar" 1 "foo" 42}]
      (run-validate
       {"bar" 1}
       {"dependencies"
        {"bar"
         {"required" ["foo"]
          "properties" {"foo" {"type" "integer"
                               "default" 42}
                        "bar" {"type" "integer"}}}}})))

    ;; No recovery, possibly re-instate
    #_(is
     (=
      [true {"bar" 2 "foo" 24}]
      (run-validate
       {"bar" 2}
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
                                         "const" 2}}}]}}})))))
