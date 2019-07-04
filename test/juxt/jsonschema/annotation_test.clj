(ns juxt.jsonschema.annotation-test
  (:require
   [juxt.jsonschema.validate :as v]
   [juxt.jsonschema.schema :refer [schema]]
   [clojure.test :refer [deftest is are testing]]))

(deftest simple-annotation-test
  (is
   (=
    {:instance "Malcolm"
     :annotations {"default" "Bob"}
     :type "string"
     :valid? true}
    (v/validate "Malcolm" {"type" "string"
                           "default" "Bob"})))
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
     {"surname" "Sparks"}
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
     {:journal? false}))))


#_(v/validate
 {"surname" "Sparks"}
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
 {:journal? false})


#_(v/validate
 {"surname" "Sparks"}
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
 {:journal? false})
