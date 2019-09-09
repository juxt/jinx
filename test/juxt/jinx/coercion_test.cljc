(ns juxt.jinx.coercion-test
  #?@(:clj [(:require
             [juxt.jinx-alpha.validate :as validate]
             [clojure.test :refer [deftest is are testing]])]
      :cljs [(:require
              [juxt.jinx-alpha.validate :as validate]
              [cljs.test :refer-macros [deftest is are testing run-tests]])]))

(deftest coercion-test
  (is (=
       123
       (:instance
        (validate/validate
         "123"
         {"type" "integer"}
         {:coercions {#?(:clj String :cljs "string")
                      {"integer" (fn [x] (#?(:clj Integer/parseInt :cljs js/parseInt) x))}}}))))


  (is (= {"foo" 123}
         (:instance
          (validate/validate
           {"foo" "123"}
           {"properties" {"foo" {"type" "integer"}}}
           {:coercions {#?(:clj String :cljs "string")
                        {"integer" (fn [x]
                                     (#?(:clj Integer/parseInt :cljs js/parseInt) x))}}})))))
