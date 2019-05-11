(ns juxt.jsonschema.test
  (:require
   [clojure.string :as str]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is are]]
   [clojure.java.io :as io]))

(def TESTS-DIR (-> (System/getenv "JUXT_REPOS")
                  (io/file "JSON-Schema-Test-Suite/tests/draft7")))

(defn tests
  ([tests-dir]
   (tests tests-dir nil))
  ([tests-dir filename-pred]
   (for [filename (-> tests-dir .list sort)
         ;; Test filenames implemented so far or being worked on currently
         :when ((or filename-pred some?) filename)
         :let [testfile (io/file tests-dir filename)]
         :when (.isFile testfile)
         :let [objects (json/parse-stream (io/reader testfile))]
         {:strs [schema tests description]} objects
         ;; Any required parsing of the schema, do it now for performance
         :let [test-group-description description]
         test tests
         :let [{:strs [description data valid]} test]]
     {:filename filename
      :test-group-description test-group-description
      :test-description description
      :schema schema
      :data data
      :valid valid})))


(comment (count (tests TESTS-DIR #{"type.json"})))

;; TODO: These must check against JavaScript primitive types,
;; not Clojure/Java ones
(def type-preds
  {"null" nil?
   "boolean" boolean?
   "object" map?
   "array" sequential?
   "number" number?
   "string" string?
   "integer" integer?})

(defn check-string-type [type data]
  (when (string? type)
    (when (not ((type-preds type) data))
      [{:message (format "Value must be of type %s" type)}])))

;; NOTE: loop/recur may be a better design to be able to stop
;; (optionally) on the first errors.

(declare validate)

(defn check-array-type [type data]
  (when (vector? type)
    (when-not ((apply some-fn (vals (select-keys type-preds type))) data)
      [{:message (format "Value must be of type %s" (str/join " or " type))}])))

(defn check-enum [enum data]
  (when enum
    (when-not (contains? (set enum) data)
      [{:message (format "Value %s must be in enum %s" data enum)}])))

(defn check-object [schema data]
  (when (= (get schema "type") "object")
    (concat
     (when (not (map? data)) [{:message "Must be an object"}])
     (when-let [properties (get schema "properties")]
       (apply concat
              (for [[k v] properties
                    :let [data (get data k)]
                    :when data]
                (validate {:path ["properties" k]} v data)))))))

(defn validate
  ([schema data]
   (validate {} schema data))
  ([jsonpointer {:strs [type enum properties] :as schema} data]
   ;; We keep trying to find errors, returning them in a list
   (->>
    (concat
     (check-string-type type data)
     (check-array-type type data)
     (check-enum enum data)
     (check-object schema data))
    (map #(merge {:data data :schema schema} %)))))

(defn failures [{:keys [schema data valid] :as all}]
  (let [result (validate schema data)
        success? (if valid (empty? result)
                     (not (empty? result)))]
    (when-not success?
      all)))

(keep failures (tests TESTS-DIR #{"type.json" "enum.json"}))
