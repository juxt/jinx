(ns juxt.jinx.vocabularies.transformation-test
  (:require [juxt.jinx.alpha.vocabularies.transformation :as transformation]
            [juxt.jinx.alpha.validate :as validate]
            #?(:clj
               [clojure.test :refer [deftest is testing]]
               :cljs
               [cljs.test :refer-macros [deftest is testing run-tests]])
            [juxt.jinx.alpha :as jinx]))

(alias 'jinx 'juxt.jinx.alpha)

(deftest process-transformations-test
  (testing "we preserve nested object schemas"
    (is
     (= {"a" "foo"
         "b" {"c" "bar"}}
        (::jinx/instance
         (transformation/process-transformations
          (validate/validate
           {"type" "object"
            "properties"
            {"a" {"type" "string"}
             "b" {"type" "object"
                  "properties" {"c" {"type" "string"}}}}}
           {"a" "foo"
            "b" {"c" "bar"}}))))))

  (testing "we preserve nested array schemas"
    (is
     (= {"a" "foo"
         "b" ["bar"]}
        (::jinx/instance
         (transformation/process-transformations
          (validate/validate
           {"type" "object"
            "properties"
            {"a" {"type" "string"}
             "b" {"type" "array"
                  "items" {"type" "string"}}}}
           {"a" "foo"
            "b" ["bar"]}))))))

  (testing "we preserve transformed values"
    (is
     (= {"a" :foo}
        (::jinx/instance
         (transformation/process-transformations
          (validate/validate
           {"type" "object"
            "properties"
            {"a" {"type" "string"
                  "juxt.jinx.alpha/as" "keyword"}}}
           {"a" "foo"})))))))
