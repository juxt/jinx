;; Copyright © 2021, JUXT LTD.

(ns juxt.jinx.new.annotations-test
  (:require
   [clojure.walk :refer [walk]]
   [clojure.set :as set]
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

(defmethod process-keyword "juxt/coerce" [keyword value instance ctx]
  {::jinx/annotations [{::coerce-to value}]})

(defmethod process-keyword "juxt/keyword-mappings" [keyword value instance ctx]
  {::jinx/annotations [{::map-to-keywords value}]})

(defn keywordize-keyword-mapping-map [m]
  (reduce-kv
   (fn [acc k v]
     (assoc acc k (keyword v)))
   {} m))

(defn apply-annotations [report]
  (let [coercion (first ;; We don't support composition of coercions yet!
                  (filter
                   #(= (::jinx/keyword %) "juxt/coerce")
                   (::jinx/annotations report)))
        kw-mappings (keywordize-keyword-mapping-map
                     (apply
                      merge             ; compose multiple possible annotations
                      (map ::map-to-keywords
                           (filter
                            #(= (::jinx/keyword %) "juxt/keyword-mappings")
                            (::jinx/annotations report)))))]
    (cond-> report
      (= (::coerce-to coercion) "uri")
      (assoc ::jinx/instance (java.net.URI. (::jinx/instance report)))

      (seq kw-mappings)
      (update ::jinx/instance set/rename-keys kw-mappings))))

(defn aggregate-subschemas [report]
  (if (seq (::jinx/subschemas report))
    (let [instance
          (reduce
           (fn [acc subschema]
             (if-let [instance (::jinx/instance subschema)]
               (case (::jinx/keyword subschema)

                 "properties"
                 (assoc acc (::jinx/property subschema) instance)

                 "allOf"
                 (merge acc instance))
               acc))
           {}
           (::jinx/subschemas report))]

      (assoc report ::jinx/instance instance))
    report))

(defn visit-report [inner outer report]
  (outer
   (let [subschemas (::jinx/subschemas report)]
     (cond-> report
       (seq subschemas) (update ::jinx/subschemas (fn [subschemas] (mapv #(visit-report inner outer %) subschemas)))
       inner (inner)))))

(let [report
      (visit-report
       apply-annotations
       aggregate-subschemas
       (jinx.api/validate
        (jinx.api/schema
         {"type" "object"
          "required" ["userGroup"]
          "juxt/keyword-mappings" {"userGroup" "juxt/userGroup"}
          "properties"
          {"userGroup"
           {"type" "string"
            "juxt/coerce" "uri"}}})
        {"userGroup" "owners"}))]
  report)


(let [report
      (visit-report
       apply-annotations
       identity
       ;;aggregate-subschemas
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

  report)

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