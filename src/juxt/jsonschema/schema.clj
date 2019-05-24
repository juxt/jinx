;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.schema
  (:require
   [lambdaisland.uri :refer [join]]))

(defn with-base-uri-meta
  "For each $id in the schema, add metadata to indicate the base-uri."
  ([schema]
   (with-base-uri-meta nil schema))
  ([base-uri schema]
   (cond
     (map? schema)
     (if-let [id (get schema "$id")]
       (let [new-base-uri (str (join base-uri id))]
         (with-meta
           (assoc (with-base-uri-meta new-base-uri (dissoc schema "$id")) "$id" id)
           {:base-uri new-base-uri
            :id id}))
       (with-meta
         (zipmap (keys schema) (map (partial with-base-uri-meta base-uri) (vals schema)))
         {:base-uri base-uri}))
     (vector? schema) (mapv (partial with-base-uri-meta base-uri) schema)
     :else schema)))

(defn index-by-uri [schema]
  (cond
    (map? schema)
    (let [mt (meta schema)]
      (if (:id mt)
        (cons [(:base-uri mt) schema]
              (index-by-uri (with-meta schema (dissoc mt :id))))
        (mapcat index-by-uri (vals schema))))

    (vector? schema)
    (mapcat index-by-uri schema)))

(defn schema [schema]
  (let [schema (with-base-uri-meta schema)
        index (into {} (index-by-uri schema))]
    (cond->
        schema
        (and (instance? clojure.lang.IMeta schema) index)
        (with-meta (-> schema meta (assoc :uri->schema index))))))

;; TODO: Try against all schemas in test-suite

(comment
  (let [schema
        {"$id" "http://example.com/root.json"
         "definitions"
         {"A" {"$id" "#foo"}
          "B" {"$id" "other.json"
               "definitions"
               {"X" {"$id" "#bar"}
                "Y" {"$id" "t/inner.json"}}}
          "C" {"$id" "urn:uuid:ee564b8a-7a87-4125-8c96-e9f123d6766f"}}}

        schema (with-base-uri-meta schema)
        schema (into {} (index-by-uri schema))]

    (-> (get schema "http://example.com/root.json")
        (get-in ["definitions" "B" "definitions"])
        meta)))

(comment
  (let [schema
        {"$id" "http://localhost:1234/tree",
         "description" "tree of nodes",
         "type" "object",
         "properties"
         {"meta" {"type" "string"},
          "nodes" {"type" "array", "items" {"$ref" "node"}}},
         "required" ["meta" "nodes"],
         "definitions"
         {"node"
          {"$id" "http://localhost:1234/node",
           "description" "node",
           "type" "object",
           "properties"
           {"value" {"type" "number"}, "subtree" {"$ref" "tree"}},
           "required" ["value"]}}}
        schema (with-base-uri-meta schema)
        index (into {} (index-by-uri schema))]
    (-> index
        (get "http://localhost:1234/node")
        (get-in ["properties" "subtree"])
        meta
        :base-uri
        (join "tree")
        str)))


;; NOTE: A relative $ref will now be easy to resolve to a URI, using
;; the :base-uri for the object's metadata.


;; TODO: Basic schema validation according to the rules in
;; draft-handrews-json-schema-validation-01.txt etc.
