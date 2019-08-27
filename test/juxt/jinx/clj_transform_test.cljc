;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.clj-transform-test
  (:require
   [juxt.jinx-alpha.clj-transform :refer [clj->jsch]]
   [clojure.test :refer [deftest is]]
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing run-tests]])))

(deftest clj->jsch-test
  (is (= {"type" "string"} (clj->jsch 'string)))
  (is (= {"type" "integer"} (clj->jsch 'integer)))
  (is (= {"type" "object"} (clj->jsch 'object)))
  (is (= {"type" "array" "items" {"type" "string"}} (clj->jsch '[string])))
  (is (= {"type" "array" "items" [{"type" "string"}{"type" "integer"}]} (clj->jsch '(string integer))))
  (is (= {"type" "null"} (clj->jsch nil)))
  (is (= {"allOf" [{"type" "string"}{"type" "integer"}]} (clj->jsch '(all-of string integer))))
  (is (= {"oneOf" [{"type" "string"}{"type" "integer"}]} (clj->jsch '(one-of string integer))))
  (is (= {"anyOf" [{"type" "string"}{"type" "integer"}]} (clj->jsch '(any-of string integer))))
  (is (= {"properties"
          {"a" {"type" "array", "items" {"type" "string"}},
           "b" {"type" "string", "constant" "20"}},
          "required" ["a"]}
         (clj->jsch {:properties {"a" ['string]
                                  "b" "20"}
                     :required ["a"]})))
  #?(:clj
     (is (= {"pattern" "\\w+"} (clj->jsch #"\w+")))))
