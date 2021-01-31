;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.coercion-test
  #?@(:clj [(:require
             [juxt.jinx.alpha.validate :as validate]
             [clojure.test :refer [deftest is are testing]])]
      :cljs [(:require
              [juxt.jinx.alpha.validate :as validate]
              [cljs.test :refer-macros [deftest is are testing run-tests]])]))

(deftest coercion-test
  (testing "coerce string to integer"
    (is (=
         123
         (:instance
          (validate/validate
           {"type" "integer"}
           "123"
           {:coercions {#?(:clj String :cljs "string")
                        {"integer" (fn [x] (#?(:clj Integer/parseInt :cljs js/parseInt) x))}}}))))


    (is (= {"foo" 123}
           (:instance
            (validate/validate
             {"properties" {"foo" {"type" "integer"}}}
             {"foo" "123"}
             {:coercions {#?(:clj String :cljs "string")
                          {"integer" (fn [x]
                                       (#?(:clj Integer/parseInt :cljs js/parseInt) x))}}})))))

  (testing "coerce single string to integer array"
    (is
     (= {"foo" [123]}
        (:instance
         (validate/validate
          {"properties" {"foo" {"type" "array"
                                "items" {"type" "integer"}}}}
          {"foo" "123"}
          {:coercions {#?(:clj String :cljs "string")
                       {"array" vector
                        "integer" (fn [x]
                                    (#?(:clj Integer/parseInt :cljs js/parseInt) x))}}})))))

  (testing "coerce single array to integer array"
    (is
     (= {"foo" [123 456]}
        (:instance
         (validate/validate
          {"properties" {"foo" {"type" "array"
                                "items" {"type" "integer"}}}}
          {"foo" ["123" "456"]}
          {:coercions {#?(:clj String :cljs "string")
                       {"integer" (fn [x]
                                    (#?(:clj Integer/parseInt :cljs js/parseInt) x))}}}))))))
