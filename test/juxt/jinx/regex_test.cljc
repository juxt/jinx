(ns juxt.jinx.regex-test
  #?@(:clj [(:require
             [juxt.jinx-alpha.regex :as regex]
             [juxt.jinx-alpha.patterns :as patterns]
             [clojure.test :refer [deftest is are testing]])]
      :cljs [(:require
              [juxt.jinx-alpha.regex :as regex]
              [juxt.jinx-alpha.patterns :as patterns]
              [cljs.test :refer-macros [deftest is are testing run-tests ]])]))

#?(:clj
   (do
   (deftest iri-test
     (let [m (patterns/matched regex/IRI "https://jon:password@juxt.pro:8080/site/index.html?debug=true#bar")]
       (are [group expected] (= expected (patterns/re-group-by-name m group))
         "scheme" "https"
         "authority" "jon:password@juxt.pro:8080"
         "userinfo" "jon:password"
         "host" "juxt.pro"
         "port" "8080"
         "path" "/site/index.html"
         "query" "debug=true"
         "fragment" "bar")))

   (deftest addr-spec-test
     (let [m (patterns/matched regex/addr-spec "mal@juxt.pro")]
       (are [group expected] (= expected (patterns/re-group-by-name m group))
         "localpart" "mal"
         "domain" "juxt.pro"))))


;; TODO: ipv6 tests from rfc2234

:cljs
   (do
   (deftest addr-spec-test
     (are [x y] (= y (patterns/parse "addr-spec-test" x))
       "mal@juxt.pro"
       ["mal" "juxt.pro"]))

   (deftest iri-test
     (are [x y] (= y (patterns/parse "iri-test" x))
       "https://jon:password@juxt.pro:8080/site/index.html?debug=true#bar"
       ["https" "jon:password"  "juxt.pro" "8080" "/site/index.html" "debug=true"  "bar" ]
       "http://user:password@example.com:8080/path?query=value#fragment"
       ["http" "user:password" "example.com" "8080" "/path" "query=value" "fragment"]
       "http://example.com"
       ["http" nil "example.com" nil "" nil nil]
       ))))
