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

   {:base-document doc}))

(defn errors [subschema]
  (concat
   (::jinx/errors subschema)
   (mapcat errors (::jinx/subschemas subschema))))

(defmethod process-keyword "juxt/coerce" [keyword value instance ctx]
  {::jinx/annotations [{::coerce-to value}]})

(defmethod process-keyword "juxt/keyword-mappings" [keyword value instance ctx]
  {::jinx/annotations [{::map-to-keywords value}]})

(defmethod process-keyword "juxt/type" [keyword value instance ctx]
  {::jinx/annotations [{:juxt/type value}]})

(defn keywordize-keyword-mapping-map [m]
  (reduce-kv
   (fn [acc k v]
     (assoc acc k (keyword v)))
   {} m))

(defn apply-coercions [report]
  (let [coercion (first ;; We don't support composition of coercions yet!
                  (filter
                   #(= (::jinx/keyword %) "juxt/coerce")
                   (::jinx/annotations report)))]
    (cond-> report
      (= (::coerce-to coercion) "uri")
      (assoc ::coerced-value (java.net.URI. (::jinx/instance report)))
      (= (::coerce-to coercion) "password")
      (assoc ::coerced-value (str "XXXXX" (::jinx/instance report) "XXXXX")))))

(defn aggregate-coercions [report]
  (if (seq (::jinx/subschemas report))
    (let [report
          (let [coerced-properties
                (reduce
                 (fn [coerced-properties subschema]

                   (cond-> coerced-properties

                     (::coerced-value subschema)
                     (assoc (::jinx/property subschema) (::coerced-value subschema))
                     ;; Collect
                     (::coerced-properties subschema)
                     (merge coerced-properties
                            (if-let [prop (::jinx/property subschema)]
                              {prop (::coerced-properties subschema)}
                              (::coerced-properties subschema)))))

                 {}
                 (::jinx/subschemas report))]

            (assoc report ::coerced-properties coerced-properties))]
      (-> report
          (update ::jinx/instance merge (::coerced-properties report))))
    report))

(defn apply-keyword-mappings [report]
  (let [kw-mappings
        (keywordize-keyword-mapping-map
         (apply
          merge                         ; compose multiple possible annotations
          (map ::map-to-keywords
               (filter
                #(= (::jinx/keyword %) "juxt/keyword-mappings")
                (::jinx/annotations report)))))]
    (cond-> report
      (seq kw-mappings)
      (assoc ::remapped-properties kw-mappings))))

(defn aggregate-keyword-mappings [report]
  (if (seq (::jinx/subschemas report))
    (let [remapped-properties
          (reduce
           (fn [acc subschema]
             (merge acc (::remapped-properties subschema)))
           {}
           (::jinx/subschemas report))]
      ;; TODO: Remove 'old' properties
      (-> report
          ;;(update ::remapped-properties merge remapped-properties)
          (update ::jinx/instance set/rename-keys remapped-properties)))
    report))

(defn visit-report [report inner outer]
  (outer
   (let [subschemas (::jinx/subschemas report)]
     (cond-> report
       (seq subschemas) (update ::jinx/subschemas (fn [subschemas] (mapv #(visit-report % inner outer) subschemas)))
       inner (inner)))))

;; properties
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

    (visit-report apply-coercions aggregate-coercions)
    (visit-report apply-keyword-mappings aggregate-keyword-mappings))

;; TODO: Have some examples of nested instances!!!
;; TODO: Password coercion

;; allOf
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

    (visit-report apply-coercions aggregate-coercions)
    (visit-report apply-keyword-mappings aggregate-keyword-mappings))

;; Password coercion
(-> (jinx.api/validate
     (jinx.api/schema
      {"properties"
       {"password"
        {"type" "string"
         "juxt/coerce" "password"}}})

     {"password" "RigidSmell"})

    (visit-report apply-coercions aggregate-coercions)
    (visit-report apply-keyword-mappings aggregate-keyword-mappings))

;; nested
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

    (visit-report apply-coercions aggregate-coercions)
    )
