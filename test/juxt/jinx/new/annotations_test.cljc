;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.new.annotations-test
  (:require
   [juxt.jinx.alpha :as jinx]
   [juxt.jinx.alpha.api :as jinx.api]
   [juxt.jinx.alpha.vocabularies.transformation :refer [transform-value process-transformations]]
   [juxt.jinx.alpha.vocabularies.keyword-mapping :refer [process-keyword-mappings update-instance-with-mappings]]
   #?(:clj
      [clojure.test :refer [deftest is]]
      :cljs
      [cljs.test :refer-macros [deftest is]]
      [cljs.core :refer [ExceptionInfo]])))

;; Here, the term 'coercion' means the explicit post-processing of a value,
;; without affecting the validity of the instance. For example, a string maybe
;; coerced into a java.net.URI.

;; TODO: Coercion is arguably the wrong word here, since it implies implicit
;; transformation of invalid values (or values that might make the instance
;; invalid with respect to a given schema) into valid values. A better word
;; might be 'transform', since it turns a value encoded as JSON (often a string of
;; unicode characters) into a value that can be used in a language's memory
;; model.

(deftest transformation-with-properties-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"type" "object"
               "required" ["userGroup"]
               "properties" {"userGroup" {"type" "string"
                                          "juxt.jinx.alpha/as" "uri"}
                             "email" {"type" "string"
                                      "format" "email"}
                             "role" {"type" "string"
                                     "juxt.jinx.alpha/as" "uri"}}})

             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"})

            process-transformations)]

    (is (=
         {"userGroup" (java.net.URI. "owners")
          "email" "mal@juxt.pro"
          "role" (java.net.URI. "/admins")
          "foo" "bar"}
         (::jinx/instance report)))))

(deftest transformation-with-all-of-test
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
                   "juxt.jinx.alpha/as" "uri"}

                  "email"
                  {"type" "string"
                   "format" "email"}}}

                {"type" "object"
                 "properties"
                 {"role"
                  {"type" "string"
                   "format" "uri-reference"
                   "juxt.jinx.alpha/as" "uri"}}}]})

             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"})

            process-transformations)]
    (is
     (=
      {"role" (java.net.URI. "/admins"),
       "foo" "bar",
       "userGroup" (java.net.URI. "owners"),
       "email" "mal@juxt.pro"}
      (::jinx/instance report)))))

(deftest transformation-with-nested-objects-test
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
                   "juxt.jinx.alpha/as" "uri"}

                  "email"
                  {"type" "string"
                   "format" "email"}}}

                {"type" "object"
                 "properties"
                 {"role"
                  {"type" "string"
                   "format" "uri-reference"
                   "juxt.jinx.alpha/as" "uri"}}}


                ;; Nest object for testing - this doesn't look to be working
                {"type" "object"
                 "properties"
                 {"details" {"type" "object"
                             "properties" {"name" {"type" "string"}
                                           "address" {"type" "string"}
                                           "webpage" {"type" "string"
                                                      "juxt.jinx.alpha/as" "uri"}}}}}]})

             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"
              "details" {"webpage" "https://juxt.pro"}})

            process-transformations)]
    (is
     (=
      {"userGroup" (java.net.URI. "owners"),
       "email" "mal@juxt.pro",
       "role" (java.net.URI. "/admins"),
       "foo" "bar",
       "details" {"webpage" (java.net.URI. "https://juxt.pro")}}
      (::jinx/instance report)))))

(deftest transformation-with-array-test
  (is
   (=
    [(java.net.URI. "/foo")
     (java.net.URI. "/bar")]
    (->
     (jinx.api/validate
      (jinx.api/schema
       {"type" "array"
        "items" {"type" "string"
                 "juxt.jinx.alpha/as" "uri"}})

      ["/foo" "/bar"])
     process-transformations
     ::jinx/instance))))

;; TODO: Also with oneOf and array schemas

(defmethod transform-value "wrapped-password" [_ instance]
  (str "XXXXX" instance "XXXXX"))

(deftest password-instantiation-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"properties"
               {"password"
                {"type" "string"
                 "juxt.jinx.alpha/as" "wrapped-password"}}})

             {"password" "RigidSmell"})

            process-transformations
            process-keyword-mappings)]
    (is
     (= {"password" "XXXXXRigidSmellXXXXX"}
        (::jinx/instance report)))))

;; Keyword mappings allow JSON object names to be replaced by Clojure keywords.

(deftest update-instance-with-mappings-test
  (let [instance {"userGroup" "owners"
                  "email" "mal@juxt.pro"
                  "role" "/admins"
                  "foo" "bar"
                  "details" {"fullname" "Malcolm Sparks"
                             "address" {"postcode" "MK9 1LH"
                                        "country" "UK"}}}
        mappings
        {"userGroup" :juxt/userGroup,
         "email" :juxt/email,
         "details" :juxt/details,
         ["details"]
         {"fullname" :juxt/fullname,
          "address" :vcard/address,
          ["address"] {"postcode" :vcard/postcode, "country" :vcard/country}}}]

    (is
     (=
      {:juxt/userGroup "owners",
       :juxt/email "mal@juxt.pro",
       "role" "/admins",
       "foo" "bar",
       :juxt/details
       {:juxt/fullname "Malcolm Sparks",
        :vcard/address #:vcard{:postcode "MK9 1LH", :country "UK"}}}
      (update-instance-with-mappings instance mappings)))))

(deftest keyword-mappings-properties-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema
              {"type" "object"
               "required" ["userGroup"]
               "juxt.jinx.alpha/keyword-mappings" {"userGroup" "juxt/userGroup"
                                                   "email" "juxt/email"}})
             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"})

            process-keyword-mappings)]

    (is (=
         {:juxt/userGroup "owners"
          :juxt/email "mal@juxt.pro"
          "role" "/admins"
          "foo" "bar"}
         (::jinx/instance report)))))

(deftest keyword-mappings-nested-properties-test
  (let [report
        (-> (jinx.api/validate
             (jinx.api/schema

              {"type" "object"

               "required" ["userGroup"]
               "properties"
               {"details"
                {"type" "object"
                 "juxt.jinx.alpha/keyword-mappings" {"fullname" "juxt/fullname"
                                                     "address" "vcard/address"}
                 "properties" {"fullname" {"type" "string"}
                               "address" {"juxt.jinx.alpha/keyword-mappings"
                                          {"postcode" "vcard/postcode"
                                           "country" "vcard/country"}
                                          "properties"
                                          {"postcode" {"type" "string"}
                                           "country" {"type" "string"}}
                                          }}}}

               "juxt.jinx.alpha/keyword-mappings" {"userGroup" "juxt/userGroup"
                                                   "email" "juxt/email"
                                                   "details" "juxt/details"}})

             {"userGroup" "owners"
              "email" "mal@juxt.pro"
              "role" "/admins"
              "foo" "bar"
              "details" {"fullname" "Malcolm Sparks"
                         "address" {"postcode" "MK9 1LH"
                                    "country" "UK"}}})

            process-keyword-mappings)]

    (is
     (=
      {:juxt/userGroup "owners",
       :juxt/email "mal@juxt.pro",
       "role" "/admins",
       "foo" "bar",
       :juxt/details
       {:juxt/fullname "Malcolm Sparks",
        :vcard/address #:vcard{:postcode "MK9 1LH", :country "UK"}}}
      (::jinx/instance report)))))

(deftest keyword-mappings-all-of-nested-properties-test
  (is(=
      {:juxt/userGroup "owners",
       :juxt/email "mal@juxt.pro",
       "role" "/admins",
       "foo" "bar",
       :juxt/details
       {:juxt/fullname "Malcolm Sparks",
        :vcard/address #:vcard{:postcode "MK9 1LH", :country "UK"}}}

      (-> (jinx.api/validate
           (jinx.api/schema

            {"allOf"
             [{"type" "object"
               "required" ["userGroup"]
               "juxt.jinx.alpha/keyword-mappings" {"userGroup" "juxt/userGroup"
                                                   "email" "juxt/email"}}

              {"type" "object"
               "required" ["details"]
               "properties"
               {"details"
                {"type" "object"
                 "juxt.jinx.alpha/keyword-mappings" {"fullname" "juxt/fullname"
                                                     "address" "vcard/address"}
                 "properties" {"fullname" {"type" "string"}
                               "address" {"juxt.jinx.alpha/keyword-mappings"
                                          {"postcode" "vcard/postcode"
                                           "country" "vcard/country"}
                                          "properties"
                                          {"postcode" {"type" "string"}
                                           "country" {"type" "string"}}}}}}

               "juxt.jinx.alpha/keyword-mappings" {"details" "juxt/details"}}]})

           {"userGroup" "owners"
            "email" "mal@juxt.pro"
            "role" "/admins"
            "foo" "bar"
            "details" {"fullname" "Malcolm Sparks"
                       "address" {"postcode" "MK9 1LH"
                                  "country" "UK"}}})

          process-keyword-mappings
          ::jinx/instance))))

(deftest keyword-mappings-array-test
  (is
   (=
    {:zips [{:zip 1, "bar" 1} {:zip 2, "bar" 2}]}
    (->
     (jinx.api/validate
      (jinx.api/schema
       {"juxt.jinx.alpha/keyword-mappings" {"foos" "zips"}
        "properties"
        {"foos"
         {"type" "array"
          "items" {"type" "object"
                   "juxt.jinx.alpha/keyword-mappings" {"foo" "zip"}}}}})

      {"foos" [{"foo" 1 "bar" 1} {"foo" 2 "bar" 2}]})
     process-keyword-mappings
     ::jinx/instance
     ))))

;; TODO: Check keyword mappings with oneOf schemas
