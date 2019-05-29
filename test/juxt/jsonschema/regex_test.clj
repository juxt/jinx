(ns juxt.jsonschema.regex-test
  (:require
   [juxt.jsonschema.regex :as regex]
   [clojure.test :refer [deftest is are testing]]))

(deftest iri-test
  (let [m (regex/matched regex/IRI "https://jon:password@juxt.pro:8080/site/index.html?debug=true#bar")]
    (are [group expected] (= expected (regex/re-group-by-name m group))
      "scheme" "https"
      "authority" "jon:password@juxt.pro:8080"
      "userinfo" "jon:password"
      "host" "juxt.pro"
      "port" "8080"
      "path" "/site/index.html"
      "query" "debug=true"
      "fragment" "bar")))

(deftest addr-spec-test
  (let [m (regex/matched regex/addr-spec "mal@juxt.pro")]
    (are [group expected] (= expected (regex/re-group-by-name m group))
      "localpart" "mal"
      "domain" "juxt.pro")))


;; TODO: ipv6 tests from rfc2234
