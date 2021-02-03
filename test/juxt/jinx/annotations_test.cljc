;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.annotations-test
  (:require
   [juxt.jinx.alpha :as jinx]
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing run-tests]]
      [cljs.core :refer [ExceptionInfo]])
   ))

(let [doc {"components"
           {"schemas"
            {"IdentifiedUser"
             {"title" "A user with an id"
              "allOf"
              [{"$ref" "#/components/schemas/Identified"}
               {"$ref" "#/components/schemas/User"}]}


             "Identified"
             {"type" "object"
              "description"
              "The unique identifier of the document in the database. This is maps
    directly to the crux.db/id attribute."
              "required" ["id"]
              "properties"
              {"id"
               {"type" "string"
                "format" "uri-reference"}}}

             "User"
             {"allOf"
              [{"type" "object"
                "title" "User"
                "description" "A user of our system"
                "required" ["email" "userGroup"]
                "properties"
                {"email"
                 {"type" "string"
                  "format" "email"}
                 "password"
                 {"type" "string"
                  ;;:crux/classification :crux/restricted
                  "description" "The user's password, stored as a hash at rest, and only
        transmitted in the clear when being set."
                  }
                 "userGroup"
                 {"type" "string"
                  "format" "uri-reference"
                  ;;:crux.json-schema/reference? true
                  ;;:crux.json-schema/schema-of-target "/schemas/IdentifiedUserGroup"
                  }}}]}

             }}}]

  (jinx/validate
   (jinx/schema (get-in doc ["components" "schemas" "IdentifiedUser"]))

   {"id" "/_crux/pass/users/juxtmal"
    "email" "mal@juxt.pro"
    "userGroup" "/_crux/pass/user-groups/owners"}

   {:base-document doc}

   ))


(jinx/validate
 (jinx/schema
  {"title" "A identified user"
   "allOf"
   [{"type" "object"
     "required" ["id"]
     "properties"
     {"id"
      {"type" "string"
       "format" "uri-reference"}}}
    {"type" "object"
     "required" ["email" "userGroup"]
     "properties"
     {"email"
      {"type" "string"
       "format" "email"}
      "userGroup"
      {"type" "string"
       "title" "The user group"
       "description" "Every user belongs to a user-group"
       "format" "uri-reference"}}}]})
 {"userGroup" "owners"
  "email" "mal@juxt.pro"}
 {:jinx/results-by-keyword? false})


{:coerced-value "10"
 :original-value 10
 :valid? true
 :annotations {"title" "I am a string"}}


;; Let's try adding coercion annotation
