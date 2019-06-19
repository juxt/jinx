(ns juxt.jsonschema.regex-cljc
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

;; The purpose of this namespace is to allow the accurate computation
;; of Java regex patterns.

(defn matched [re  s]
  (when (re-matches re s)
    (re-matches re s)))

(defn grouper
  "Uses js/RegExp to find matching groups. Note that the JS value 
   returned by `:last-index` is the index of the first char in the 
   input string *after* the current match."
  [re input-str]
  (let [re-src re.source] ; the source string from the regexp arg
    (loop [groups []
           regexp (js/RegExp. re-src "g")] ; 'g' => global search
      (let [res     (.exec regexp input-str)
            res-clj (js->clj res)]
        (print res)
        (print res-clj)
        (if (nil? res)
          groups
          (recur
           (conj groups {:groups res-clj :match (get res-clj 0)
                         :index res.index :input res.input
                         :last-index regexp.lastIndex})
           regexp))))))

;; RFC 1034, Section 3.1: Name space specifications and terminology

(def subdomain #"(?i)^[a-z](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\.[a-z0-9](?:[-0-9a-z]{0,61}[0-9a-z])?)*$")

(defn hostname? [s]
  (and
   (re-matches subdomain s)
   ;; "Labels must be 63 characters or less." -- RFC 1034, Section 3.5
   ;; "To simplify implementations, the total number of octets that
   ;; represent a domain name (i.e., the sum of all label octets and
   ;; label lengths) is limited to 255." -- RFC 1034, Section 3.1
   (<= (count s) 255)))

(defn idn-hostname? [s]
  (and  (hostname? s)
     ;; Ensure no illegal chars
     (empty? (set/intersection (set (seq s))
                               #{\u302E ; Hangul single dot tone mark
                                 }))))

;; RFC 3986, Appendix A. Collected ABNF for URI
;(def IPv4address (compose "%s%s%s%s%s%s%s" dec-octet PERIOD dec-octet PERIOD dec-octet PERIOD dec-octet))
(def IPv4address #"^(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)$")

(def IPv6address #"(?i)^\s*(?:(?:(?:[0-9a-f]{1,4}:){7}(?:[0-9a-f]{1,4}|:))|(?:(?:[0-9a-f]{1,4}:){6}(?::[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(?:(?:[0-9a-f]{1,4}:){5}(?:(?:(?::[0-9a-f]{1,4}){1,2})|:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(?:(?:[0-9a-f]{1,4}:){4}(?:(?:(?::[0-9a-f]{1,4}){1,3})|(?:(?::[0-9a-f]{1,4})?:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){3}(?:(?:(?::[0-9a-f]{1,4}){1,4})|(?:(?::[0-9a-f]{1,4}){0,2}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){2}(?:(?:(?::[0-9a-f]{1,4}){1,5})|(?:(?::[0-9a-f]{1,4}){0,3}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){1}(?:(?:(?::[0-9a-f]{1,4}){1,6})|(?:(?::[0-9a-f]{1,4}){0,4}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?::(?:(?:(?::[0-9a-f]{1,4}){1,7})|(?:(?::[0-9a-f]{1,4}){0,5}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(?:%.+)?\s*$")

; (def gen-delims [\: \/ \? \# \[ \] \@])
; (def unreserved (concat ALPHA DIGIT [\- \. \_ \~]))
; (def sub-delims [\! \$ \& \' \( \) \* \+ \, \; \=])
; (def IPvFuture (compose "v[%s]+%s(?:%s|%s|%s)+" HEXDIG PERIOD unreserved sub-delims COLON))
(def IPvFuture  #"(?i)v[0-9a-f]+\.(?:[a-z0-9\-._~!$&'()*+,;=:])+$")

; (def IP-literal (compose "%s(?:%s|%s)%s" \[ IPv6address IPvFuture \]))
; (def IP-literal  #"(?i)^\[((\s*(?:(?:(?:[0-9a-f]{1,4}:){7}(?:[0-9a-f]{1,4}|:))|(?:(?:[0-9a-f]{1,4}:){6}(?::[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(?:(?:[0-9a-f]{1,4}:){5}(?:(?:(?::[0-9a-f]{1,4}){1,2})|:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(?:(?:[0-9a-f]{1,4}:){4}(?:(?:(?::[0-9a-f]{1,4}){1,3})|(?:(?::[0-9a-f]{1,4})?:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){3}(?:(?:(?::[0-9a-f]{1,4}){1,4})|(?:(?::[0-9a-f]{1,4}){0,2}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){2}(?:(?:(?::[0-9a-f]{1,4}){1,5})|(?:(?::[0-9a-f]{1,4}){0,3}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?:(?:[0-9a-f]{1,4}:){1}(?:(?:(?::[0-9a-f]{1,4}){1,6})|(?:(?::[0-9a-f]{1,4}){0,4}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(?::(?:(?:(?::[0-9a-f]{1,4}){1,7})|(?:(?::[0-9a-f]{1,4}){0,5}:(?:(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))(?:%.+)?\s*)|(?:v[0-9a-f]+\.(?:[a-z0-9\-._~!$&'()*+,;=:])+))\]?\s*$")

(def IP-literal  #"(?i)^\[((?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+))\]?\s*$")

;(def scheme (compose "%s[%s]*" ALPHA (set (concat ALPHA DIGIT [\+ \- \.])))) 
;(?:[a-z][a-z0-9+\-.]*:)
;(def userinfo (compose "(?<userinfo>(?:%s|%s|%s|%s)*)" unreserved pct-encoded sub-delims ":"))
;(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@) 
;(def reg-name (compose "(?:%s|%s|%s)*" unreserved pct-encoded sub-delims))
;(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})

;(def host (compose "(?<host>%s|%s|%s)" IP-literal IPv4address reg-name))

;(def port (compose "(?<port>(?:%s)*)" DIGIT))
;(?::\d*)
;(def authority (compose (str "(?<authority>" (str "(?:%s@)?" "(?:%s)" "(?:%s%s)?") ")") userinfo host COLON port))
;userinfo
;(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)
;IP-literal
;(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+)\]
;IPv4address
;|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|
;reg-name
;(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})*)
;(?::\d*)?-port

;(def segment (compose "(?:%s|%s|%s|%s|%s)*" unreserved pct-encoded sub-delims \: \@))
;(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*
;(def segment-nz (compose "(?:%s|%s|%s|%s|%s)+" unreserved pct-encoded sub-delims \: \@))
;(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+
;(def segment-nz-nc (compose "(?:%s|%s|%s|%s)+" unreserved pct-encoded sub-delims \@))
;(?:[a-z0-9\-._~!$&'()*+,;=@]|%[0-9a-f]{2})+

;(def path-abempty (compose "(?<path>(?:/%s)*)" segment))
;(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)
;(def path-absolute (compose "/%s(?:/%s)*" segment-nz segment))
;\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?
;(def path-noscheme (compose "%s(?:/%s)*" segment-nz-nc segment))
;\/(?:(?:[a-z0-9\-._~!$&'()*+,;=@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?
;(def path-rootless (compose "%s(?:/%s)*" segment-nz segment))
;(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*

(def path-empty "")

;(def hier-part (compose "(?://%s%s|%s|%s|%s)" authority path-abempty path-absolute path-rootless path-empty))

;(def pchar (compose "(?:%s|%s|%s|%s|%s)" unreserved pct-encoded sub-delims \: \@))
;(?:#(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)
;(def query (compose "(?:%s|%s|%s)*" pchar \/ \?))
;(?:\?(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)
;(def fragment (compose "(?:%s|%s|%s)*" pchar \/ \?))
;(?:\?(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)

;(def relative-part (compose "(?://%s%s|%s|%s|%s)"
                            ; authority path-abempty path-absolute
                            ; path-noscheme path-empty))

;(def URI (compose "(?<scheme>%s):(?:%s)(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" scheme hier-part QUESTION-MARK query fragment))

(def URI #"(?i)^(?:[a-z][a-z0-9+\-.]*:)(?://(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*|/(?:(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)(?:\?(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?$")

(def relative-part #"(?://(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-fA-F]+\.[A-Za-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*|/(?:(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'()*+,;=@]|%[0-9a-f]{2})+(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)")

; (def relative-ref (compose "%s(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" relative-part QUESTION-MARK query fragment))
(def relative-ref #"(?i)(?://(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-fA-F]+\.[A-Za-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*|/(?:(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'()*+,;=@]|%[0-9a-f]{2})+(?:/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)(?:\?(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?$")
;; RFC 3987

; (def ucschar
;   (set (concat (int-range 0xA0 0xD7FF)
;                (int-range 0xF900 0xFDCF)
;                (int-range 0xFDF0 0xFFEF)

               ;; These higher code-points that lie outside the BMP
               ;; are significantly impacting compile performance. We
               ;; need to be able to do partition-into-ranges in a
               ;; much more performant way. Perhap with intervals
               ;; rather than brute-force expansion of ranges into
               ;; sequences of ints.

               ;;(int-range 0x10000 0x1FFFD)
               ;;(int-range 0x20000 0x2FFFD)
               ;;(int-range 0x30000 0x3FFFD)
               ;;(int-range 0x40000 0x4FFFD)
               ;;(int-range 0x50000 0x5FFFD)
               ;;(int-range 0x60000 0x6FFFD)
               ;;(int-range 0x70000 0x7FFFD)
               ;;(int-range 0x80000 0x8FFFD)
               ;;(int-range 0x90000 0x9FFFD)
               ;;(int-range 0xA0000 0xAFFFD)
               ;;(int-range 0xB0000 0xBFFFD)
               ;;(int-range 0xC0000 0xCFFFD)
               ;;(int-range 0xD0000 0xDFFFD)
               ;;(int-range 0xE1000 0xEFFFD)
              ;  )))



; (comment ;; Example of a higher code-point - a Chinese character
;   (String. (int-array [0x2F81A]) 0 1))

; (def iunreserved (concat ALPHA DIGIT [\- \. \_ \~] ucschar))

; (def iuserinfo (compose "(?<userinfo>(?:%s|%s|%s|%s)*)" iunreserved pct-encoded sub-delims ":"))

; (def ireg-name (compose "(?:%s|%s|%s)*" iunreserved pct-encoded sub-delims))

; (def ihost (compose "(?<host>%s|%s|%s)" IP-literal IPv4address ireg-name))

; (def iauthority (compose (str "(?<authority>"
;                               (str "(?:%s@)?"
;                                    "(?:%s)"
;                                    "(?:%s%s)?") ")") iuserinfo ihost COLON port))

; (def ipchar (compose "(?:%s|%s|%s|%s|%s)" iunreserved pct-encoded sub-delims \: \@))

; (def isegment (compose "(?:%s|%s|%s|%s|%s)*" iunreserved pct-encoded sub-delims \: \@))
; (def isegment-nz (compose "(?:%s|%s|%s|%s|%s)+" iunreserved pct-encoded sub-delims \: \@))
; (def isegment-nz-nc (compose "(?:%s|%s|%s|%s)+" iunreserved pct-encoded sub-delims \@))

; (def ipath-abempty (compose "(?<path>(?:/%s)*)" isegment))
; (def ipath-absolute (compose "/%s(?:/%s)*" isegment-nz isegment))
; (def ipath-noscheme (compose "%s(?:/%s)*" isegment-nz-nc isegment))
; (def ipath-rootless (compose "%s(?:/%s)*" isegment-nz isegment))
; (def ipath-empty "")

; (def ihier-part (compose "(?://%s%s|%s|%s|%s)" iauthority ipath-abempty ipath-absolute ipath-rootless ipath-empty))


; (def iprivate (concat (int-range 0xE000 0xF8FF)
;                       ;;(int-range 0xF0000 0xFFFFD)
;                      ;;(int-range 0x100000 0x10FFFD)
;                       ))


; (def iquery (compose "(?:%s|%s|%s|%s)*" ipchar iprivate \/ \?))

; (def ifragment (compose "(?:%s|%s|%s)*" ipchar \/ \?))


; (def IRI (compose "(?<scheme>%s):(?:%s)(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" scheme ihier-part QUESTION-MARK iquery ifragment))

(def IRI URI)

; (def irelative-part (compose "(?://%s%s|%s|%s|%s)"
;                              iauthority ipath-abempty ipath-absolute
;                              ipath-noscheme ipath-empty))

; (def irelative-ref (compose "%s(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" irelative-part QUESTION-MARK iquery ifragment))

(def irelative-ref relative-ref)
;; Can't do this AND have named groups - better to ask app logic to
;; ask if a string is either an IRI or a irelative-ref

; #_(def IRI-reference (compose "(?:%s|%s)" IRI irelative-ref))

; (comment
;   (re-group-by-name (matched IRI "https://jon:pither@juxt.pro/malcolm?foo#bar") "host"))


;; RFC 5322, Section 3.2.3

;(def atext (concat ALPHA DIGIT [\! \# \$ \% \& \' \* \+ \- \/ \= \? \^ \_ \` \{ \| \} \~]))

;(def rfc5322_atom (compose "[%s]+" (as-regex-str atext)))

;(def dot-atom-text (compose "[%s]+(?:\\.[%s]+)*" (as-regex-str atext) (as-regex-str atext)))

;; RFC 5322, Section 3.4.1

;(def domain dot-atom-text)

;(def addr-spec (compose "(?<localpart>%s)@(?<domain>%s)" dot-atom-text domain))

(def addr-spec  #"(?i)((?<localpart>^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+)@(?<domain>[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\.[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?)*))")

(comment
  (re-matches addr-spec "mal@juxt.pro"))

;; RFC 6532: Internationalized Email Headers

;; We define UTF8-non-ascii, then use that to extent atext above, as per Section 3.2

; (def UTF8-tail (int-range 0x80 0xBF))

; (def UTF8-2 (compose "[%s]%s" (int-range 0xC2 0xDF) UTF8-tail))

; (def UTF8-3 (compose
;              "(?:%s[%s]%s|[%s](?:%s){2}|%s[%s]%s|[%s](?:%s){2})"
;              0xE0 (int-range 0xA0 0xBF) UTF8-tail
;              (int-range 0xE1 0xEC) UTF8-tail
;              0xED (int-range 0x80 0x9F) UTF8-tail
;              (int-range 0xEE 0xEF) UTF8-tail))

; (def UTF8-4 (compose
;              "(?:%s[%s](?:%s){2}|[%s](?:%s){3}|%s[%s](?:%s){2})"
;              0xF0 (int-range 0x90 0xBF) UTF8-tail
;              (int-range 0xF1 0xF3) UTF8-tail
;              0xF4 (int-range 0x80 0x8F) UTF8-tail))

; (def UTF8-non-ascii (compose "(?:%s|%s|%s)" UTF8-2 UTF8-3 UTF8-4))

; (def iatext (compose "(?:%s|%s)" atext UTF8-non-ascii))

; (def idot-atom-text (compose "[%s]+(?:\\.[%s]+)*" (as-regex-str iatext) (as-regex-str iatext)))

; (def idomain idot-atom-text)

; (def iaddr-spec (compose "(?<localpart>%s)@(?<domain>%s)" idot-atom-text idomain))
(def iaddr-spec addr-spec )
;; TODO: Normalize as per iaddr-spec 3.1.  UTF-8 Syntax and Normalization



;; RFC 6901: JavaScript Object Notation (JSON) Pointer
; (def unescaped (concat (int-range 0x00 0x2E)
;                        (int-range 0x30 0x7D)
;                        ;; Should be this:
;                        #_(int-range 0x7F 0x10FFFF)
;                        ;; but too slow, so do this instead for now:
;                        (int-range 0x7F 0xFFFF)))

;(def referenced-token (compose "(?:[%s]|~0|~1)*" unescaped))
;(def json-pointer (compose "(?:/%s)*" referenced-token))
(def json-pointer "(/(([^/~])|(~[01]))*)$")

;; draft-handrews-relative-json-pointer-01
;(def relative-json-pointer (compose "%s(?:#|%s)" non-negative-integer json-pointer))
(def relative-json-pointer "(?:0|[1-9][0-9]*)(/(([^/~])|(~[01]))*)$")


;; TODO: Define U-label (RFC 5890, Section 2.3.2.1)

;; RFC 6570: URI Template
;(def uritemplate  #"(?i)^(?:(?:[^\x00-\x20"'<>%\\^`{|}]|%[0-9a-f]{2})|\{[+#./;?&=,!@|]?(?:[a-z0-9_]|%[0-9a-f]{2})+(?::[1-9][0-9]{0,3}|\*)?(?:,(?:[a-z0-9_]|%[0-9a-f]{2})+(?::[1-9][0-9]{0,3}|\*)?)*\})*$")