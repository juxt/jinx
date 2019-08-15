;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx.demo
  (:require
   [juxt.jinx-alpha.schema :refer [schema]]
   [juxt.jinx-alpha.validate :refer [validate]]))

(comment
  (validate
   {"firstName" "John"
    "lastName" "Doe"
    "age" 21}
   (schema
    {"$id" "https://example.com/person.schema.json"
     "$schema" "http://json-schema.org/draft-07/schema#"
     "title" "Person"
     "type" "object"
     "properties"
     {"firstName"
      {"type" "string"
       "description" "The person's first name."}
      "lastName"
      {"type" "string"
       "description" "The person's last name."}
      "age" {"description" "Age in years which must be equal to or greater than zero."
             "type" "integer"
             "minimum" 0}}})))
