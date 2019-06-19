(ns juxt.jsonschema.regex-cljc-test
  (:require
   [juxt.jsonschema.regex-cljc :as regex]
   [cljs.test :refer-macros [deftest is are testing run-tests]]))

;TODO grouped regex not supported in javascript
; (deftest iri-test
;   (let [m (regex/matched regex/IRI "https://jon:password@juxt.pro:8080/site/index.html?debug=true#bar")]
;     (are [group expected] (= expected (regex/re-group-by-name m group))
;       "scheme" "https"
;       "authority" "jon:password@juxt.pro:8080"
;       "userinfo" "jon:password"
;       "host" "juxt.pro"
;       "port" "8080"
;       "path" "/site/index.html"
;       "query" "debug=true"
;       "fragment" "bar")))

; (deftest addr-spec-test
;   (let [m (regex/matched regex/addr-spec "mal@juxt.pro")]
;     (are [group expected] (= expected (regex/re-group-by-name m group))
;       "localpart" "mal"
;       "domain" "juxt.pro")))


;; TODO: ipv6 tests from rfc2234
