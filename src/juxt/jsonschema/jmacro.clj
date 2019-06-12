(ns juxt.jsonschema.jmacro
  (:require [cheshire.core :refer [parse-string parse-stream]]
            [clojure.java.io :as io]))


(defn parse-stream-cljc [file]
  `(quote ~(doall (parse-stream (io/reader  file)))))