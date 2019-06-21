(ns juxt.jsonschema.schema-test
  (:require
   [juxt.jsonschema.schema :refer [schema]]
   [clojure.test :refer [deftest is are testing]]
   [juxt.jsonschema.schema :as schema])
  (:import
   (clojure.lang ExceptionInfo)))


(deftest type-test
  (testing "bad type"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type' MUST be either a string or an array"
      (schema {"type" 10}))))

  (testing "bad string type"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"String values of 'type' MUST be one of the six primitive types or 'integer'"
      (schema {"type" "float"}))))

  (testing "array elements must be strings"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type', if it is an array, elements of the array MUST be strings"
      (schema {"type" ["string" 10]}))))

  (testing "array elements must be strings"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type', if it is an array, elements of the array MUST be strings"
      (schema {"type" [nil]}))))

  (testing "unique elements"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type', if it is an array, elements of the array MUST be unique"
      (schema {"type" ["string" "string"]}))))

  (testing "integer"
    (is
     (schema {"type" ["integer"]})))

  (testing "illegal"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"String values of 'type' MUST be one of the six primitive types or 'integer'"
      (schema {"type" ["float" "number"]})))))

(deftest enum-test
  (testing "must be an array"
    (is
     (schema {"enum" ["foo" "bar"]}))
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of an enum MUST be an array"
      (schema {"enum" "foo"})))
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of an enum SHOULD have at least one element"
      (schema {"enum" []})))
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"Elements in the enum value array SHOULD be unique"
      (schema {"enum" ["foo" "foo"]})))))

(deftest const-test
  (testing "may be any type"
    (is
     (schema {"const" "foo"}))
    (is
     (schema {"const" []}))
    (is
     (schema {"const" ["foo"]}))
    (is
     (schema {"const" nil}))))

(deftest multiple-of-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of multipleOf MUST be a number, strictly greater than 0"
    (schema {"multipleOf" "foo"})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of multipleOf MUST be a number, strictly greater than 0"
    (schema {"multipleOf" 0})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of multipleOf MUST be a number, strictly greater than 0"
    (schema {"multipleOf" -1})))
  (is
   (schema {"multipleOf" 0.1})))

(deftest maximum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of maximum MUST be a number"
    (schema {"maximum" "foo"})))
  (is
   (schema {"maximum" 10}))
  (is
   (schema {"maximum" 10.5})))

(deftest exclustive-maximum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of exclusiveMaximum MUST be a number"
    (schema {"exclusiveMaximum" "foo"}))))

(deftest minimum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of minimum MUST be a number"
    (schema {"minimum" "foo"}))))

(deftest exclusive-minimum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of exclusiveMinimum MUST be a number"
    (schema {"exclusiveMinimum" "foo"}))))

(deftest max-length-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of maxLength MUST be a non-negative integer"
    (schema {"maxLength" "foo"})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of maxLength MUST be a non-negative integer"
    (schema {"maxLength" -1})))
  (is (schema {"maxLength" 0}))
  (is (schema {"maxLength" 5})))

(deftest min-length-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of minLength MUST be a non-negative integer"
    (schema {"minLength" "foo"})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of minLength MUST be a non-negative integer"
    (schema {"minLength" -1})))
  (is (schema {"minLength" 0}))
  (is (schema {"minLength" 5})))

(deftest pattern-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of pattern MUST be a string"
    (schema {"pattern" 10})))
  (is (schema {"pattern" "foobar.?"})))

(deftest items-test
  (testing "Nil value"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'items' MUST be either a valid JSON Schema or an array of valid JSON Schemas"
      (schema {"items" nil}))))

  (testing "Bad subschema"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'items' MUST be a valid JSON Schema"
      (schema {"items" {"type" "foo"}}))))

  (testing "Bad subschemas"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'items' MUST be an array of valid JSON Schemas, but at least one element isn't valid"
      (schema {"items" [{"type" "string"}
                        {"type" "number"}
                        {"type" "float"}]})))))

(deftest additional-items-test
  (is (schema {"additionalItems" false}))
  (is (schema {"additionalItems" true}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'additionalItems' MUST be a valid JSON Schema"
    (schema {"additionalItems" nil})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'additionalItems' MUST be a valid JSON Schema"
    (schema {"additionalItems" {"type" "foo"}}))))

(deftest max-items-test
  (is (schema {"maxItems" 10}))
  (is (schema {"maxItems" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxItems' MUST be a non-negative integer"
    (schema {"maxItems" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxItems' MUST be a non-negative integer"
    (schema {"maxItems" 0.5}))))

(deftest min-items-test
  (is (schema {"minItems" 10}))
  (is (schema {"minItems" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minItems' MUST be a non-negative integer"
    (schema {"minItems" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minItems' MUST be a non-negative integer"
    (schema {"minItems" 0.5}))))

(deftest unique-items-test
  (is (schema {"uniqueItems" true}))
  (is (schema {"uniqueItems" false}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'uniqueItems' MUST be a boolean"
    (schema {"uniqueItems" 1}))))

(deftest contains-test
  (is (schema {"contains" true}))
  (is (schema {"contains" false}))
  (is (schema {"contains" {"type" "string"}}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'contains' MUST be a valid JSON Schema"
    (schema {"contains" {"type" "foo"}}))))

(deftest max-properties-test
  (is (schema {"maxProperties" 10}))
  (is (schema {"maxProperties" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxProperties' MUST be a non-negative integer"
    (schema {"maxProperties" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxProperties' MUST be a non-negative integer"
    (schema {"maxProperties" 0.5}))))

(deftest min-properties-test
  (is (schema {"minProperties" 10}))
  (is (schema {"minProperties" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minProperties' MUST be a non-negative integer"
    (schema {"minProperties" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minProperties' MUST be a non-negative integer"
    (schema {"minProperties" 0.5}))))

(deftest required-test
  (is (schema {"required" []}))
  (is (schema {"required" ["foo" "bar"]}))

  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'required' MUST be an array"
    (schema {"required" "foo"})))

  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'required' MUST be an array. Elements of this array, if any, MUST be strings"
    (schema {"required" ["foo" 0]})))

  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'required' MUST be an array. Elements of this array, if any, MUST be unique"
    (schema {"required" ["foo" "foo"]}))))


(deftest properties-test
  (testing "empty object"
    (is (schema {"properties" {}})))

  (testing "Bad object"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'properties' MUST be an object"
      (schema {"properties" 10}))))

  (testing "Bad subschema"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"Each value of 'properties' MUST be a valid JSON Schema"
      (schema {"properties" {"foo" {"type" "bar"}}})))))

(deftest pattern-properties-test
  (testing "empty object"
    (is (schema {"patternProperties" {}})))

  (testing "Bad object"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'patternProperties' MUST be an object"
      (schema {"patternProperties" 10}))))

  (testing "Bad subschema"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"Each value of a 'patternProperties' object MUST be a valid JSON Schema"
      (schema {"patternProperties" {"foo" {"type" "bar"}}})))))

(deftest additional-properties-test
  (is (schema {"additionalProperties" false}))
  (is (schema {"additionalProperties" true}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'additionalProperties' MUST be a valid JSON Schema"
       (schema {"additionalProperties" nil})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'additionalProperties' MUST be a valid JSON Schema"
       (schema {"additionalProperties" {"type" "foo"}}))))

(deftest dependencies-test
  (is (schema {"dependencies" {}}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'dependencies' MUST be an object"
       (schema {"dependencies" true})))

  (is (schema {"dependencies" {"a" []}}))
  (is (schema {"dependencies" {"a" ["foo" "bar"]}}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each element in a dependencies array MUST be a string"
       (schema {"dependencies" {"a" ["foo" 10]}})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each element in a dependencies array MUST be unique"
       (schema {"dependencies" {"a" ["foo" "foo"]}})))

  (is (thrown-with-msg?
       ExceptionInfo
       #"Dependency values MUST be an array or a valid JSON Schema"
       (schema {"dependencies" {"a" {"type" "foo"}}})))

  (is (thrown-with-msg?
       ExceptionInfo
       #"Dependency values MUST be an array or a JSON Schema"
       (schema {"dependencies" {"a" nil}}))))

(deftest property-names-test
  (is (schema {"propertyNames" false}))
  (is (schema {"propertyNames" true}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'propertyNames' MUST be a JSON Schema"
       (schema {"propertyNames" nil})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'propertyNames' MUST be a valid JSON Schema"
       (schema {"propertyNames" {"type" "foo"}}))))

(deftest if-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'if' MUST be a valid JSON Schema"
       (schema {"if" {"type" "foo"}}))))

(deftest then-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'then' MUST be a valid JSON Schema"
       (schema {"then" {"type" "foo"}}))))

(deftest else-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'else' MUST be a valid JSON Schema"
       (schema {"else" {"type" "foo"}}))))
