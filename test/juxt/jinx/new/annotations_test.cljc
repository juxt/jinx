;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.new.annotations-test
  (:require
   [clojure.walk :refer [walk]]
   [juxt.jinx.alpha :as jinx]
   [juxt.jinx.alpha.validate :refer [process-keyword]]
   [juxt.jinx.alpha.api :as jinx.api]
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

  (jinx.api/validate
   (jinx.api/schema (get-in doc ["components" "schemas" "IdentifiedUser"]))

   {"id" "/_crux/pass/users/juxtmal"
    "email" "mal@juxt.pro"
    "userGroup" "/_crux/pass/user-groups/owners"}

   {:base-document doc}

   ))


(defn errors [subschema]
  (concat
   (::jinx/errors subschema)
   (mapcat errors (::jinx/subschemas subschema))))

(jinx.api/validate
 (jinx.api/schema
  {"type" "string"
   "format" "email"})
 "mal@juxt.pro"
 {:jinx/results-by-keyword? false})

(defmethod process-keyword "juxt/coerce" [keyword value instance ctx]
  {::jinx/annotations [{::coerce-to value}]})

(defmethod process-keyword "crux/keyword-mappings" [keyword value instance ctx]
  {::jinx/annotations [{::map-to-keywords value}]})


(jinx.api/validate
 (jinx.api/schema
  {"type" "object"
   "required" ["email" "userGroup" "oo"]
   "crux/keyword-mappings" {"email" "juxt/email"
                            "userGroup" "juxt/userGroup"}
   "properties"
   {"email"
    {"type" "string"
     "format" "email"}
    "userGroup"
    {"type" "string"
     "title" "The user group"
     "description" "Every user belongs to a user-group"
     "format" "uri-reference"
     "juxt/coerce" "uri"}}})
 {"userGroup" "owners"
  "email" "mal@juxt.pro"}
 {::jinx/results-by-keyword? false})

(defn apply-coercions [report]
  (let [coercion (first ;; We don't support composition of coercions yet!
                       (filter
                        #(= (::jinx/keyword %) "juxt/coerce")
                        (::jinx/annotations report)))]
    (cond-> report
      (= (::coerce-to coercion) "uri")
      (assoc ::jinx/coerced-instance (java.net.URI. (::jinx/instance report))))))

(defn visit-report [inner outer report]
  (outer
   (let [subschemas (::jinx/subschemas report)]
     (cond-> report
       (seq subschemas) (update ::jinx/subschemas (fn [subschemas] (mapv #(visit-report inner outer %) subschemas)))
       inner (inner)))))

((comp apply-coercions)
 (jinx.api/validate
  (jinx.api/schema
   {"type" "string"
    "title" "The user group"
    "description" "Every user belongs to a user-group"
    "format" "uri-reference"
    "juxt/coerce" "uri"
    "juxt/attribute-key" "pass/user-group"})
  "owners"))


;; This works!
(let [report
      (visit-report
       apply-coercions

       (fn [report]
         (if (seq (::jinx/subschemas report))
           (let [coerced-instance
                 (into {}
                       (for [subschema (::jinx/subschemas report)
                             :let [coerced-instance (::jinx/coerced-instance subschema)]
                             :when coerced-instance]
                         [(::jinx/property subschema) coerced-instance]))]
             (assoc report ::jinx/coerced-instance coerced-instance))
           report))

       (jinx.api/validate
        (jinx.api/schema
         {"type" "object"
          "properties"
          {"userGroup"
           {"type" "string"
            "title" "The user group"
            "description" "Every user belongs to a user-group"
            "format" "uri-reference"
            "juxt/coerce" "uri"
            "juxt/attribute-key" "pass/user-group"}
           "email"
           {"type" "string"
            "format" "email"}}})
        {"userGroup" "owners"
         "email" "mal@juxt.pro"
         "foo" "bar"}))]

  (merge (::jinx/instance report) (::jinx/coerced-instance report)))


;; TODO: Get working with allOf

(defn aggregate-coercions [report]
  (if (seq (::jinx/subschemas report))
    (let [coerced-instance
          (reduce
           (fn [acc subschema]
             (if-let [coerced-instance (::jinx/coerced-instance subschema)]
               (case (::jinx/keyword subschema)

                 "properties"
                 (assoc acc (::jinx/property subschema) coerced-instance)

                 "allOf"
                 (merge acc coerced-instance))
               acc))
           {}
           (::jinx/subschemas report))]

      (assoc report ::jinx/coerced-instance coerced-instance))
    report))

(let [report
      (visit-report
       apply-coercions
       aggregate-coercions
       (jinx.api/validate
        (jinx.api/schema
         {"allOf"
          [{"type" "object"
            "properties"
            {"userGroup"
             {"type" "string"
              "title" "The user group"
              "description" "Every user belongs to a user-group"
              "format" "uri-reference"
              "juxt/coerce" "uri"
              "juxt/attribute-key" "pass/user-group"}
             "email"
             {"type" "string"
              "format" "email"}}}
           {"type" "object"
            "properties"
            {"role"
             {"type" "string"
              "format" "uri-reference"
              "juxt/coerce" "uri"
              }}}]})
        {"userGroup" "owners"
         "email" "mal@juxt.pro"
         "role" "/admins"
         "foo" "bar"}))]

  report
  ;;(merge (::jinx/instance report) (::jinx/coerced-instance report))
  )


;; TODO: Key mapping
;; TODO: Password coercion

(let [schema (jinx.api/schema
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
                   "format" "uri-reference"}}}]})]

  ;; Badly formatted email address
  (let [instance {"id" "foo"
                  "userGroup" "owners"
                  "email" "mal"}]

    (assert (not (::jinx/valid? (jinx.api/validate
                                 schema
                                 instance))))
    (assert (= 1 (-> schema
                     (jinx.api/validate
                      instance)
                     errors
                     count))))

  ;; Correctly formatted email address
  (let [instance {"id" "foo"
                  "userGroup" "owners"
                  "email" "mal@juxt.pro"}]

    (assert (::jinx/valid? (jinx.api/validate schema instance)))

    (assert (zero? (-> schema
                       (jinx.api/validate instance)
                       errors
                       count))))

  ;; Try to turn userGroup into a java.net.URI.
  (let [schema (jinx.api/schema
                {"type" "string"
                 "title" "The user group"
                 "description" "Every user belongs to a user-group"
                 "format" "uri-reference"})]

    (jinx.api/validate schema "/owners"))
  )

;; Let's try adding coercion annotation
