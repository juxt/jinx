;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.new.annotations-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.jinx.alpha :as jinx]
   [juxt.jinx.alpha.api :as jinx.api]
   [juxt.jinx.alpha.visit :as visit]
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing run-tests]]
      [cljs.core :refer [ExceptionInfo]])))

(deftest coerce-properties-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"type" "object"
               "required" ["userGroup"]
               "juxt/keyword-mappings" {"userGroup" "juxt/userGroup"
                                        "email" "juxt/email"}
               "properties" {"userGroup" {"type" "string"
                                          "juxt/coerce" "uri"}
                             "email" {"type" "string"
                                      "format" "email"}
                             "role" {"type" "string"
                                     "juxt/coerce" "uri"} }})
             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"})

            (visit/visit-report
             visit/apply-coercions
             visit/aggregate-coercions)

            (visit/visit-report
             visit/apply-keyword-mappings
             visit/aggregate-keyword-mappings))]
    (is (=
         {"userGroup" (java.net.URI. "owners")
          "email" "mal@juxt.pro"
          "role" (java.net.URI. "/admins")
          "foo" "bar"}
         (::jinx/instance report)))))

(deftest coerce-all-of-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"allOf"
               [
                {"type" "object"
                 "required" ["userGroup"]

                 "juxt/keyword-mappings"
                 {"userGroup" "juxt/userGroup"
                  "email" "juxt/email"}

                 "properties"
                 {"userGroup"
                  {"type" "string"
                   "title" "The user group"
                   "description" "Every user belongs to a user-group"
                   "format" "uri-reference"
                   "juxt/coerce" "uri"}

                  "email"
                  {"type" "string"
                   "format" "email"}}}

                {"type" "object"
                 "properties"
                 {"role"
                  {"type" "string"
                   "format" "uri-reference"
                   "juxt/coerce" "uri"}}}]})

             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"})

            (visit/visit-report
             visit/apply-coercions
             visit/aggregate-coercions)

            (visit/visit-report
             visit/apply-keyword-mappings
             visit/aggregate-keyword-mappings))]

    (is
     (=
      {"role" (java.net.URI. "/admins"),
       "foo" "bar",
       :juxt/userGroup (java.net.URI. "owners"),
       :juxt/email "mal@juxt.pro"}
      (::jinx/instance report)))))

(deftest password-coercion-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"properties"
               {"password"
                {"type" "string"
                 "juxt/coerce" "password"}}})

             {"password" "RigidSmell"})

            (visit/visit-report
             visit/apply-coercions
             visit/aggregate-coercions)

            (visit/visit-report
             visit/apply-keyword-mappings
             visit/aggregate-keyword-mappings))]

    (is
     (=
      {"password" "XXXXXRigidSmellXXXXX"}
      (::jinx/instance report)))))

(deftest nested-coercions-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"allOf"
               [
                {"type" "object"
                 "required" ["userGroup"]

                 "properties"
                 {"userGroup"
                  {"type" "string"
                   "title" "The user group"
                   "description" "Every user belongs to a user-group"
                   "format" "uri-reference"
                   "juxt/coerce" "uri"}

                  "email"
                  {"type" "string"
                   "format" "email"}}}

                {"type" "object"
                 "properties"
                 {"role"
                  {"type" "string"
                   "format" "uri-reference"
                   "juxt/coerce" "uri"}}}


                ;; Nest object for testing - this doesn't look to be working
                {"type" "object"
                 "properties"
                 {"details" {"type" "object"
                             "properties" {"name" {"type" "string"}
                                           "address" {"type" "string"}
                                           "webpage" {"type" "string"
                                                      "juxt/coerce" "uri"}}}}}]})

             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"
              "details" {"webpage" "https://juxt.pro"}})

            (visit/visit-report
             visit/apply-coercions
             visit/aggregate-coercions))]
    (is
     (=
      {"userGroup" (java.net.URI. "owners"),
       "email" "mal@juxt.pro",
       "role" (java.net.URI. "/admins"),
       "foo" "bar",
       "details" {"webpage" (java.net.URI. "https://juxt.pro")}}
      (::jinx/instance report)))))
