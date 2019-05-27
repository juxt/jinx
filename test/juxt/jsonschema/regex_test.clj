(ns juxt.jsonschema.regex-test
  (:require
   [juxt.jsonschema.regex :as regex]
   [clojure.test :refer [deftest is are testing]]))

(deftest iri-test
  (let [m (regex/matched regex/IRI "https://jon:password@juxt.pro/site/index.html?debug=true#bar")]
    (are [group expected] (= expected (regex/re-group-by-name m group))
      "scheme" "https"
      "host" "juxt.pro"
      "path" "/site/index.html"
      "userinfo" "jon:password"
      "query" "debug=true"
      "fragment" "bar")))

(time
 (dotimes [n 1000]
   (re-matches regex/IRI "https://jon:password@juxt.pro/site/index.html?debug=true#bar")))

(time
 (dotimes [n 1000]
   (java.net.URL. "https://jon:password@juxt.pro/site/index.html?debug=true#bar")))
