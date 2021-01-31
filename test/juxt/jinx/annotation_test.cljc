;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.annotation-test
  (:require
   [juxt.jinx.alpha.validate :as v]
   [juxt.jinx.alpha.schema :refer [schema]]
   #?(:clj
      [clojure.test :refer [deftest is testing]]
      :cljs
      [cljs.test :refer-macros [deftest is testing run-tests]]
      [cljs.core :refer [ExceptionInfo]])))

(deftest simple-annotation-test
  (is
   (=
    {:instance "Malcolm"
     :annotations {"default" "Bob"}
     :type "string"
     :valid? true}
    (v/validate
     {"type" "string"
      "default" "Bob"}
     "Malcolm" )))
  (is
   (=
    {:instance {"surname" "Sparks"
                "firstname" "Bob"}
     :annotations
     {"title" "person"
      "description" "A person, user or employee"
      :properties
      {"surname" {"title" "Surname"
                  "description" "Family name"}
       "firstname" {"default" "Bob"}}}
     :type "object"
     :valid? true}

    (v/validate
     (schema
      {"type" "object"
       "title" "person"
       "description" "A person, user or employee"
       "properties"
       {"firstname"
        {"type" "string"
         "default" "Bob"}
        "surname"
        {"type" "string"
         "title" "Surname"
         "description" "Family name"
         "examples" ["Smith" "Johnson" "Jones" "Williams"]
         }}
       "required" ["firstname" "surname"]})
     {"surname" "Sparks"}
     {:journal? false}))))


#_(v/validate
   (schema
  {"type" "object"
   "required" ["firstname"]
   "properties" {"firstname" {"type" "string" "default" "Dominic"}
                 "surname"
                 {"anyOf"
                  [{"type" "string"
                    "default" "foo"
                    "title" "Surname"
                    }
                   {"type" "number"
                    "default" "foo"
                    "title" "Family name"
                    }]}}})
    {"surname" "Sparks"}

 {:journal? false})


#_(v/validate
 (schema
  {"type" "object"
   "required" ["firstname"]
   "properties" {"firstname" {"type" "string" "default" "Dominic"}
                 "surname"
                 {"allOf"
                  [{"type" "string"
                    "default" "foo"
                    "title" "Surname"
                    }
                   {"type" "string"
                    "default" "food"
                    "title" "Family name"
                    }]}}})
 {"surname" "Sparks"}
 {:journal? false})
