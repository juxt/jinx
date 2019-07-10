;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.official-test
  #?@(:clj [(:require [clojure.java.io :as io]
                      [cheshire.core :as json]
                      [clojure.test :refer :all]
                      [clojure.test :as test]
                      [juxt.jsonschema.validate :refer [validate]]
                      [juxt.jsonschema.schema :refer [schema]]
                      [juxt.jsonschema.resolve :as resolv]
                      [juxt.jsonschema.schema :as schema])]
      :cljs [(:require [cljs-node-io.core :as io :refer [slurp]]
                       [cljs-node-io.fs :as fs]
                       [cljs-node-io.file :refer [File]]
                       [cljs.test :refer-macros [deftest is testing run-tests]]
                       [juxt.jsonschema.validate :refer [validate]]
                       [juxt.jsonschema.schema :refer [schema]]
                       [juxt.jsonschema.resolve :as resolv]
                       [cljs.nodejs :as nodejs]
                       [juxt.jsonschema.schema :as schema])]))

(defn- env [s]
  #?(:clj (System/getenv (str s)))
  #?(:cljs (aget js/process.env s)))

#?(:cljs
   (def Throwable js/Error))

(def TESTS-ROOT 
  #?(:clj (io/file (System/getenv "JUXT_REPOS") "JSON-Schema-Test-Suite")
     :cljs (str (aget js/process.env "JUXT_REPOS") "JSON-Schema-Test-Suite")))

(def TESTS-DIR 
  #?(:clj (io/file TESTS-ROOT "tests/draft7")
     :cljs (str TESTS-ROOT "/tests/draft7"))) 


#?(:cljs
   (do
     (def fs (cljs.nodejs/require "fs"))
     (def path (cljs.nodejs/require "path"))
     (defn file-exists? [f]
       (fs.existsSync f))
     (defn dir? [f]
       (and
        (file-exists? f)
        (.. fs (lstatSync f) (isDirectory))))
     (defn file? [f]
       (.. fs (lstatSync f) (isFile)))
     (defn file-seq [dir]
       (if (fs.existsSync dir)
         (tree-seq
          dir?
          (fn [d] (map (partial str d "/") (seq (fs.readdirSync d))))
          dir)
         []))
     (defn read-file-cljs [fname]
       (js->clj  (js/JSON.parse (.readFileSync fs fname))))))

(defn test-jsonschema [{:keys [schema data valid] :as test}]
  (try
    (let [schema (schema/schema schema)
          result (validate
                  data schema
                  {:resolvers
                   [::resolv/built-in
                    [::resolv/default-resolver
                     {#"http://localhost:1234/(.*)"
                      (fn [match]
                        #?(:clj (do 
                                  (io/file (io/file TESTS-ROOT "remotes") (second match)))
                           :cljs (do 
                                   (File. (str TESTS-ROOT "/remotes/" (second match))))))}]]})
          success? (if valid (:valid? result)
                       (not (:valid? result)))]
      (cond-> test
        success? (assoc :result :success)
        (and (not success?) valid) (assoc :failures (:error result))
        (and (empty? result) (not valid)) (assoc :failures [{:message "Incorrectly judged valid"}])))
    (catch Throwable e (merge test {:result :error
                                    :error e}))))

(defn success? [x] (= (:result x) :success))

;; Test suite

(defn tests
  [tests-dir]
  #?(:clj     
     (for [testfile (file-seq TESTS-DIR)

        ;;         filename (-> tests-dir .list sort)
        ;; Test filenames implemented so far or being worked on currently
        ;;         :when ((or filename-pred some?) filename)
        ;;         :let [testfile (io/file tests-dir filename)]
           
           :when (.isFile testfile)
           :let [objects (json/parse-stream (io/reader testfile))]
           {:strs [schema tests description]} objects
        ;; Any required parsing of the schema, do it now for performance
           :let [test-group-description description]
           test tests
           :let [{:strs [description data valid]} test]]
       {:filename (str testfile)
        :test-group-description test-group-description
        :test-description description
        :schema schema
        :data data
        :valid valid})
     :cljs
     (for [testfile (filter file? (file-seq TESTS-DIR))
           :when (file? testfile)
           :let [objects (read-file-cljs testfile)]
           {:strs [schema tests description]} objects
        ;; Any required parsing of the schema, do it now for performance
           :let [test-group-description description]
           test tests
           :let [{:strs [description data valid]} test]]
       {:filename (str testfile)
        :test-group-description test-group-description
        :test-description description
        :schema schema
        :data data
        :valid valid})))

;; TODO: Pull out defaults and refs from validation keywords - this is
;; premature abstraction

(defn exclude-test? [test]
  (contains?
   #{"format: uri-template"
     "validation of an internationalized e-mail addresses"}
   (:test-group-description test)))

(defn cljs-exclude-test? [test]
  (or (contains?
        #{"format: uri-template"
         }
       (:test-group-description test))
      (contains?
       #{"two supplementary Unicode code points is long enough"
         "one supplementary Unicode code point is not long enough"
         "an invalid IRI based on IPv6"
         "0.0075 is multiple of 0.0001"
         "0.00751 is not multiple of 0.0001"}
       (:test-description test))))

#?(:clj
   (do
     (defn make-tests []
       (doseq [test (remove exclude-test? (tests TESTS-DIR))]
         (let [testname (symbol (str (gensym "test") "-test"))]
           (eval `(test/deftest ~(vary-meta testname assoc :official true) ~testname
                    (test/testing ~(:test-description test)
                      (test/is (success? (test-jsonschema ~test)))))))))
     (make-tests)))

#?(:cljs
   (deftest cljs-tests
     (testing "Testing JSON-Schema-Test-Suite - cljs"
       (doseq [test (remove cljs-exclude-test? (tests TESTS-DIR))]
         (let [testname (symbol (str (gensym "test") "-test"))]
           (do
             (is (success? (test-jsonschema test)))))))))