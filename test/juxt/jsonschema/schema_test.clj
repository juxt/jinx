(ns juxt.jsonschema.schema-test
  (:require
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
      (schema/validate {"type" 10}))))

  (testing "bad string type"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"String values of 'type' MUST be one of the six primitive types or 'integer'"
      (schema/validate {"type" "float"}))))

  (testing "array elements must be strings"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type', if it is an array, elements of the array MUST be strings"
      (schema/validate {"type" ["string" 10]}))))

  (testing "array elements must be strings"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type', if it is an array, elements of the array MUST be strings"
      (schema/validate {"type" [nil]}))))

  (testing "unique elements"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'type', if it is an array, elements of the array MUST be unique"
      (schema/validate {"type" ["string" "string"]}))))

  (testing "integer"
    (is
     (schema/validate {"type" ["integer"]})))

  (testing "illegal"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"String values of 'type' MUST be one of the six primitive types or 'integer'"
      (schema/validate {"type" ["float" "number"]})))))

(deftest enum-test
  (testing "must be an array"
    (is
     (schema/validate {"enum" ["foo" "bar"]}))
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of an enum MUST be an array"
      (schema/validate {"enum" "foo"})))
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of an enum SHOULD have at least one element"
      (schema/validate {"enum" []})))
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"Elements in the enum value array SHOULD be unique"
      (schema/validate {"enum" ["foo" "foo"]})))))

(deftest const-test
  (testing "may be any type"
    (is
     (schema/validate {"const" "foo"}))
    (is
     (schema/validate {"const" []}))
    (is
     (schema/validate {"const" ["foo"]}))
    (is
     (schema/validate {"const" nil}))))

(deftest multiple-of-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of multipleOf MUST be a number, strictly greater than 0"
    (schema/validate {"multipleOf" "foo"})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of multipleOf MUST be a number, strictly greater than 0"
    (schema/validate {"multipleOf" 0})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of multipleOf MUST be a number, strictly greater than 0"
    (schema/validate {"multipleOf" -1})))
  (is
   (schema/validate {"multipleOf" 0.1})))

(deftest maximum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of maximum MUST be a number"
    (schema/validate {"maximum" "foo"})))
  (is
   (schema/validate {"maximum" 10}))
  (is
   (schema/validate {"maximum" 10.5})))

(deftest exclustive-maximum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of exclusiveMaximum MUST be a number"
    (schema/validate {"exclusiveMaximum" "foo"}))))

(deftest minimum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of minimum MUST be a number"
    (schema/validate {"minimum" "foo"}))))

(deftest exclusive-minimum-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of exclusiveMinimum MUST be a number"
    (schema/validate {"exclusiveMinimum" "foo"}))))

(deftest max-length-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of maxLength MUST be a non-negative integer"
    (schema/validate {"maxLength" "foo"})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of maxLength MUST be a non-negative integer"
    (schema/validate {"maxLength" -1})))
  (is (schema/validate {"maxLength" 0}))
  (is (schema/validate {"maxLength" 5})))

(deftest min-length-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of minLength MUST be a non-negative integer"
    (schema/validate {"minLength" "foo"})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of minLength MUST be a non-negative integer"
    (schema/validate {"minLength" -1})))
  (is (schema/validate {"minLength" 0}))
  (is (schema/validate {"minLength" 5})))

(deftest pattern-test
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of pattern MUST be a string"
    (schema/validate {"pattern" 10})))
  (is (schema/validate {"pattern" "foobar.?"})))

(deftest items-test
  (testing "Nil value"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'items' MUST be either a valid JSON Schema or an array of valid JSON Schemas"
      (schema/validate {"items" nil}))))

  (testing "Bad subschema"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'items' MUST be a valid JSON Schema"
      (schema/validate {"items" {"type" "foo"}}))))

  (testing "Bad subschemas"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'items' MUST be an array of valid JSON Schemas, but at least one element isn't valid"
      (schema/validate {"items" [{"type" "string"}
                        {"type" "number"}
                        {"type" "float"}]})))))

(deftest additional-items-test
  (is (schema/validate {"additionalItems" false}))
  (is (schema/validate {"additionalItems" true}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'additionalItems' MUST be a valid JSON Schema"
    (schema/validate {"additionalItems" nil})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'additionalItems' MUST be a valid JSON Schema"
    (schema/validate {"additionalItems" {"type" "foo"}}))))

(deftest max-items-test
  (is (schema/validate {"maxItems" 10}))
  (is (schema/validate {"maxItems" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxItems' MUST be a non-negative integer"
    (schema/validate {"maxItems" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxItems' MUST be a non-negative integer"
    (schema/validate {"maxItems" 0.5}))))

(deftest min-items-test
  (is (schema/validate {"minItems" 10}))
  (is (schema/validate {"minItems" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minItems' MUST be a non-negative integer"
    (schema/validate {"minItems" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minItems' MUST be a non-negative integer"
    (schema/validate {"minItems" 0.5}))))

(deftest unique-items-test
  (is (schema/validate {"uniqueItems" true}))
  (is (schema/validate {"uniqueItems" false}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'uniqueItems' MUST be a boolean"
    (schema/validate {"uniqueItems" 1}))))

(deftest contains-test
  (is (schema/validate {"contains" true}))
  (is (schema/validate {"contains" false}))
  (is (schema/validate {"contains" {"type" "string"}}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'contains' MUST be a valid JSON Schema"
    (schema/validate {"contains" {"type" "foo"}}))))

(deftest max-properties-test
  (is (schema/validate {"maxProperties" 10}))
  (is (schema/validate {"maxProperties" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxProperties' MUST be a non-negative integer"
    (schema/validate {"maxProperties" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'maxProperties' MUST be a non-negative integer"
    (schema/validate {"maxProperties" 0.5}))))

(deftest min-properties-test
  (is (schema/validate {"minProperties" 10}))
  (is (schema/validate {"minProperties" 0}))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minProperties' MUST be a non-negative integer"
    (schema/validate {"minProperties" -1})))
  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'minProperties' MUST be a non-negative integer"
    (schema/validate {"minProperties" 0.5}))))

(deftest required-test
  (is (schema/validate {"required" []}))
  (is (schema/validate {"required" ["foo" "bar"]}))

  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'required' MUST be an array"
    (schema/validate {"required" "foo"})))

  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'required' MUST be an array. Elements of this array, if any, MUST be strings"
    (schema/validate {"required" ["foo" 0]})))

  (is
   (thrown-with-msg?
    ExceptionInfo
    #"The value of 'required' MUST be an array. Elements of this array, if any, MUST be unique"
    (schema/validate {"required" ["foo" "foo"]}))))


(deftest properties-test
  (testing "empty object"
    (is (schema/validate {"properties" {}})))

  (testing "Bad object"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'properties' MUST be an object"
      (schema/validate {"properties" 10}))))

  (testing "Bad subschema"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"Each value of 'properties' MUST be a valid JSON Schema"
      (schema/validate {"properties" {"foo" {"type" "bar"}}})))))

(deftest pattern-properties-test
  (testing "empty object"
    (is (schema/validate {"patternProperties" {}})))

  (testing "Bad object"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"The value of 'patternProperties' MUST be an object"
      (schema/validate {"patternProperties" 10}))))

  (testing "Bad subschema"
    (is
     (thrown-with-msg?
      ExceptionInfo
      #"Each value of a 'patternProperties' object MUST be a valid JSON Schema"
      (schema/validate {"patternProperties" {"foo" {"type" "bar"}}})))))

(deftest additional-properties-test
  (is (schema/validate {"additionalProperties" false}))
  (is (schema/validate {"additionalProperties" true}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'additionalProperties' MUST be a valid JSON Schema"
       (schema/validate {"additionalProperties" nil})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'additionalProperties' MUST be a valid JSON Schema"
       (schema/validate {"additionalProperties" {"type" "foo"}}))))

(deftest dependencies-test
  (is (schema/validate {"dependencies" {}}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'dependencies' MUST be an object"
       (schema/validate {"dependencies" true})))

  (is (schema/validate {"dependencies" {"a" []}}))
  (is (schema/validate {"dependencies" {"a" ["foo" "bar"]}}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each element in a dependencies array MUST be a string"
       (schema/validate {"dependencies" {"a" ["foo" 10]}})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each element in a dependencies array MUST be unique"
       (schema/validate {"dependencies" {"a" ["foo" "foo"]}})))

  (is (thrown-with-msg?
       ExceptionInfo
       #"Dependency values MUST be an array or a valid JSON Schema"
       (schema/validate {"dependencies" {"a" {"type" "foo"}}})))

  (is (thrown-with-msg?
       ExceptionInfo
       #"Dependency values MUST be an array or a JSON Schema"
       (schema/validate {"dependencies" {"a" nil}}))))

(deftest property-names-test
  (is (schema/validate {"propertyNames" false}))
  (is (schema/validate {"propertyNames" true}))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'propertyNames' MUST be a JSON Schema"
       (schema/validate {"propertyNames" nil})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'propertyNames' MUST be a valid JSON Schema"
       (schema/validate {"propertyNames" {"type" "foo"}}))))

(deftest if-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'if' MUST be a valid JSON Schema"
       (schema/validate {"if" {"type" "foo"}}))))

(deftest then-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'then' MUST be a valid JSON Schema"
       (schema/validate {"then" {"type" "foo"}}))))

(deftest else-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'else' MUST be a valid JSON Schema"
       (schema/validate {"else" {"type" "foo"}}))))

(deftest all-of-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'allOf' MUST be a non-empty array"
       (schema/validate {"allOf" {"type" "string"}})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'allOf' MUST be a non-empty array"
       (schema/validate {"allOf" []})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each item of an 'allOf' array MUST be a valid schema"
       (schema/validate {"allOf" [{"type" "foo"}]})))
  (is (schema/validate {"allOf" [{"type" "string"}]})))

(deftest any-of-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'anyOf' MUST be a non-empty array"
       (schema/validate {"anyOf" {"type" "string"}})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'anyOf' MUST be a non-empty array"
       (schema/validate {"anyOf" []})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each item of an 'anyOf' array MUST be a valid schema"
       (schema/validate {"anyOf" [{"type" "foo"}]})))
  (is (schema/validate {"anyOf" [{"type" "string"}]})))

(deftest one-of-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'oneOf' MUST be a non-empty array"
       (schema/validate {"oneOf" {"type" "string"}})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'oneOf' MUST be a non-empty array"
       (schema/validate {"oneOf" []})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"Each item of an 'oneOf' array MUST be a valid schema"
       (schema/validate {"oneOf" [{"type" "foo"}]})))
  (is (schema/validate {"oneOf" [{"type" "string"}]})))

(deftest not-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of 'not' MUST be a valid JSON Schema"
       (schema/validate {"not" {"type" "foo"}})))
  (is (schema/validate {"not" {"type" "string"}})))

(deftest format-test
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of a 'format' attribute MUST be a string"
       (schema/validate {"format" nil})))
  (is (thrown-with-msg?
       ExceptionInfo
       #"The value of a 'format' attribute MUST be a string"
       (schema/validate {"format" []})))
  (is (schema/validate {"format" "regex"})))
