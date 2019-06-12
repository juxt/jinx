(ns juxt.jsonschema.jmacro
  (:require [cheshire.core :refer [parse-string parse-stream]]
            [clojure.java.io :as io]))

(defn get-data [jsonfile]
  (parse-stream (io/reader (io/resource jsonfile))))

(defmacro parse-stream-cljc [resource-path]
  `(parse-stream (io/reader (io/resource ~resource-path))))
