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
                       [cljs.test :refer-macros [deftest is testing run-tests]]
                       [juxt.jsonschema.validate-cljc :refer [validate]]
                       [juxt.jsonschema.schema :refer [schema]]
                       [juxt.jsonschema.resolve :as resolv]
                       [cljs.nodejs :as nodejs]
                       [cljs.tools.reader :refer [read-string]]
                       [cljs.js :refer [empty-state eval js-eval]]
                       [cljs.env :refer [*compiler*]]
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
   (def fs (cljs.nodejs/require "fs")))
#?(:cljs
   (def path (cljs.nodejs/require "path")))
#?(:cljs
   (defn file-exists? [f]
     (fs.existsSync f)))
#?(:cljs
(defn dir? [f]
  (and
   (file-exists? f)
   (.. fs (lstatSync f) (isDirectory)))))
#?(:cljs
(defn file-seq [dir]
  (if (fs.existsSync dir)
    (tree-seq
     dir?
     (fn [d] (map (partial str d "/") (seq (fs.readdirSync d))))
     dir)
    [])))

#?(:cljs
   (defn read-file-cljs [fname]
     (when-not (dir? fname)
        (first (js->clj (js/JSON.parse (.readFileSync fs fname)))))))

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
                        (io/file (io/file TESTS-ROOT "remotes") (second match)))}]]})
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
     (for [testfile (file-seq TESTS-DIR)
           :when testfile
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

#?(:cljs
   (defn eval-str [s]
     (eval (empty-state)
           (read-string s)
           {:eval       js-eval
            :source-map true
            :context    :expr}
           (fn [result] result))))

(defn make-tests []
  (doseq [test (remove exclude-test? (tests TESTS-DIR))]
    (let [testname (symbol (str (gensym "test") "-test"))]
      #?(:clj
         (eval `(test/deftest ~testname
                  (test/testing ~(:test-description test)
                    (test/is (success? (test-jsonschema ~test))))))
         :cljs
         (eval-str `(deftest ~testname
                      (testing ~(:test-description test)
                        (is (success? (test-jsonschema ~test))))))))))


(make-tests)
