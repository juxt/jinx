(ns juxt.jsonschema.regex
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

;; The purpose of this namespace is to allow the accurate computation
;; of Java regex patterns.

(defn matched [^java.util.regex.Pattern re ^CharSequence s]
  (let [m (re-matcher re s)]
    (when (.matches m)
      m)))

(defn re-group-by-name [^java.util.regex.Matcher matcher ^String name]
  (when matcher
    (.group matcher name)))

(defn partition-into-ranges-iter
  "Find consecutive number sequences. O(n)"
  [coll]
  (loop [[x & xs] (sort coll)
         subsequent 0
         curr []
         ranges []]
    (if-not x
      (cond-> ranges (seq curr) (conj curr))
      (if (= (inc subsequent) (int x))
        (recur xs (int x) (conj curr x) ranges)
        (recur xs (int x) [x] (cond-> ranges (seq curr) (conj curr)))))))

(defn partition-into-ranges-fj
  "Find consecutive number sequences. O(log n)"
  [coll]
  (let [consecutive?
        (fn [coll]
          (= (count coll) (inc (- (int (or (last coll) -1)) (int (first coll))))))

        fork (fn fork [coll]
               (if (consecutive? coll)
                 coll
                 (let [midpoint (quot (count coll) 2)]
                   [(fork (subvec coll 0 midpoint))
                    (fork (subvec coll midpoint))])))

        join (fn join [[coll & colls]]
               (let [consecutive? (= (inc (int (last coll))) (int (or (ffirst colls) -1)))]
                 (if consecutive?
                   (join (cons (into coll (first colls)) (rest colls)))
                   (if colls
                     (cons coll (lazy-seq (join colls)))
                     [coll]))))]

    (->> coll vec fork (tree-seq (comp sequential? first) seq)
         rest (filter (comp not sequential? first))
         join)))

(defprotocol RegExpressable
  (as-regex-str [_] "Return a string that represents the Java regex"))

(defn int-range
  "Range between n1 (inclusive) and n2 (inclusive)"
  [n1 n2]
  (range (int n1) (inc (int n2))))

(def regex-chars
  (merge
   {(int \\) "\\\\"
    (int \u0009) "\\t"
    (int \u000A) "\\n"
    (int \u000D) "\\r"
    (int \u000C) "\\f"
    (int \u0007) "\\a"
    (int \u001B) "\\e"}
   (into {} (for [n (concat
                     (int-range \A \Z)
                     (int-range \a \z)
                     (int-range \0 \9))]
              [n (str (char n))]))))

(defn int->regex [n]
  (cond (< n 256) (get regex-chars n (format "\\x%02X" n))
        (< n 65536) (format "\\u%04X" n)
        :else (format "\\x{%04X}" n)))

(defn expand-with-character-classes
  "Take a collection of characters and return a string representing the
  concatenation of the Java regex characters, including the use
  character classes wherever possible without conformance loss. This
  function is not designed for performance and should be called to
  prepare systems prior to the handling of HTTP requests."
  [s]
  (let [{:keys [classes remaining]}
        (reduce
         (fn [{:keys [remaining] :as acc} {:keys [class set]}]
           (cond-> acc
             (set/subset? set remaining) (-> (update :classes conj class)
                                             (update :remaining set/difference set))))
         {:remaining (set s) :classes []}

         [{:class "Alnum" :set (set (concat (int-range \A \Z) (int-range \a \z) (int-range \0 \9)))}
          {:class "Alpha" :set (set (concat (int-range \A \Z) (int-range \a \z)))}
          {:class "XDigit" :set (set (concat (int-range \0 \9) (int-range \A \F) (int-range \a \f)))}
          {:class "Digit" :set (set (int-range \0 \9))}
          {:class "Cntrl" :set (set (concat (int-range \u0000 \u001f) [(int \u007f)]))}
          {:class "Punct" :set (set (map int [\! \" \# \$ \% \& \' \(
                                              \) \* \+ \, \- \. \/ \:
                                              \; \< \= \> \? \@ \[ \\
                                              \] \^ \_ \` \{ \| \} \~]))}
          {:class "Blank" :set (set (map int [\space \tab]))}])]


    (let [cs (concat
              (map #(format "\\p{%s}" %) classes)
              ;; Find ranges
              (map (fn [x] (if (> (count x) 1)
                             (format "[%s-%s]"
                                     (int->regex (first x))
                                     (int->regex (last x)))
                             (int->regex (first x))))
                   (partition-into-ranges-iter remaining)))]
      (if (> (count cs) 1)
        (format "[%s]" (apply str cs))
        (apply str cs)))))

(extend-protocol RegExpressable
  clojure.lang.ISeq
  (as-regex-str [s]
    (expand-with-character-classes (map int s)))
  clojure.lang.PersistentVector
  (as-regex-str [s]
    (expand-with-character-classes (map int s)))
  String
  (as-regex-str [s] s)
  Character
  (as-regex-str [c]
    (int->regex (int c)))
  Integer
  (as-regex-str [n]
    (int->regex n))
  Long
  (as-regex-str [n]
    (assert (<= n Integer/MAX_VALUE))
    (int->regex (int n)))
  java.util.regex.Pattern
  (as-regex-str [re]
    (str re))
  clojure.lang.PersistentHashSet
  (as-regex-str [s]
    (as-regex-str (seq s))))

(defn concatenate
  [& args]
  (re-pattern (apply str (map as-regex-str args))))

(defn compose [fmt & args]
  (re-pattern (apply format fmt (map as-regex-str args))))

;; RFC 5234 B.1

(def ALPHA (concat (int-range \A \Z) (int-range \a \z)))

(def BIT [\0 \1])

(def CHAR (int-range 0x01 0x7F))

(def CR \return)

(def CRLF (concatenate \return \newline))

(def CTL (conj (int-range 0x00 0x1F) 0x7F))

(def DIGIT (int-range \0 \9))

(def DQUOTE \")

;; HEXDIG includes lower-case. RFC 5234: "ABNF strings are case
;; insensitive and the character set for these strings is US-ASCII."
(def HEXDIG (concat DIGIT (int-range \A \F) (int-range \a \f)))

(def HTAB \tab)

(def LF \newline)

(def OCTET (int-range 0x00 0xFF))

(def SP \space)

(def WSP [\space \tab])


;; Useful

(def COLON 0x3A)
(def QUESTION-MARK 0x3F)
(def PERIOD 0x2E)


;; RFC 3986, Appendix A. Collected ABNF for URI

(def dec-octet (compose "(?:%s|%s|%s|%s|%s)"
                        DIGIT
                        (concatenate (int-range 0x31 0x39) DIGIT)
                        (concatenate \1 DIGIT DIGIT)
                        (concatenate \2 (int-range 0x30 0x34) DIGIT)
                        (concatenate \2 \5 (int-range 0x30 0x35))))

(def IPv4address (compose "%s%s%s%s%s%s%s" dec-octet PERIOD dec-octet PERIOD dec-octet PERIOD dec-octet))

(def h16 (compose "%s{1,4}" HEXDIG))

(def ls32 (compose "(?:%s%s%s|%s)" h16 COLON h16 IPv4address))

;; For ease of debugging
;; 6( h16 ":" ) ls32
(def IPv6-1 (compose "(?:%s:){6}%s" h16 ls32))

;; "::" 5( h16 ":" ) ls32
(def IPv6-2 (compose "::(?:%s:){5}%s" h16 ls32))

;; [ h16 ] "::" 4( h16 ":" ) ls32
(def IPv6-3 (compose "%s?::(?:%s:){4}%s" h16 h16 ls32))

;; [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
(def IPv6-4 (compose "(?:(?:%s:){0,1}%s)?::(?:%s:){3}%s" h16 h16 h16 ls32))

;; [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
(def IPv6-5 (compose "(?:(?:%s:){0,2}%s)?::(?:%s:){2}%s" h16 h16 h16 ls32))

;; [ *3( h16 ":" ) h16 ] "::" h16 ":" ls32
(def IPv6-6 (compose "(?:(?:%s:){0,3}%s)?::%s:%s" h16 h16 h16 ls32))

;; [ *4( h16 ":" ) h16 ] "::" ls32
(def IPv6-7 (compose "(?:(?:%s:){0,4}%s)?::%s" h16 h16 ls32))

;; [ *5( h16 ":" ) h16 ] "::" h16
(def IPv6-8 (compose "(?:(?:%s:){0,5}%s)?::%s" h16 h16 h16))

;; [ *6( h16 ":" ) h16 ] "::"
(def IPv6-9 (compose "(?:(?:%s:){0,6}%s)?::" h16 h16))

(def IPv6address
  (compose
   "(?:%s|%s|%s|%s|%s|%s|%s|%s|%s)"
   IPv6-1 IPv6-2 IPv6-3 IPv6-4 IPv6-5 IPv6-6 IPv6-7 IPv6-8 IPv6-9))

(def gen-delims [\: \/ \? \# \[ \] \@])

(def unreserved (concat ALPHA DIGIT [\- \. \_ \~]))

(def sub-delims [\! \$ \& \' \( \) \* \+ \, \; \=])

(def IPvFuture (compose "v[%s]+%s(?:%s|%s|%s)+" HEXDIG PERIOD unreserved sub-delims COLON))

(def IP-literal (compose "%s(?:%s|%s)%s" \[ IPv6address IPvFuture \]))

(def scheme (compose "%s[%s]*" ALPHA (set (concat ALPHA DIGIT [\+ \- \.]))))

(def pct-encoded (concatenate \% HEXDIG HEXDIG))

(def userinfo (compose "(?<userinfo>(?:%s|%s|%s|%s)*)" unreserved pct-encoded sub-delims ":"))

(def reg-name (compose "(?:%s|%s|%s)*" unreserved pct-encoded sub-delims))

(def host (compose "(?<host>%s|%s|%s)" IP-literal IPv4address reg-name))

(def port (compose "(?<port>(?:%s)*)" DIGIT))

(def authority (compose (str "(?<authority>" (str "(?:%s@)?" "(?:%s)" "(?:%s%s)?") ")") userinfo host COLON port))

(def segment (compose "(?:%s|%s|%s|%s|%s)*" unreserved pct-encoded sub-delims \: \@))
(def segment-nz (compose "(?:%s|%s|%s|%s|%s)+" unreserved pct-encoded sub-delims \: \@))
(def segment-nz-nc (compose "(?:%s|%s|%s|%s)+" unreserved pct-encoded sub-delims \@))

(def path-abempty (compose "(?<path>(?:/%s)*)" segment))
(def path-absolute (compose "/%s(?:/%s)*" segment-nz segment))
(def path-noscheme (compose "%s(?:/%s)*" segment-nz-nc segment))
(def path-rootless (compose "%s(?:/%s)*" segment-nz segment))
(def path-empty "")

(def hier-part (compose "(?://%s%s|%s|%s|%s)" authority path-abempty path-absolute path-rootless path-empty))

(def pchar (compose "(?:%s|%s|%s|%s|%s)" unreserved pct-encoded sub-delims \: \@))

(def query (compose "(?:%s|%s|%s)*" pchar \/ \?))

(def fragment (compose "(?:%s|%s|%s)*" pchar \/ \?))

(def URI (compose "(?<scheme>%s):(?:%s)(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" scheme hier-part QUESTION-MARK query fragment))

(def relative-part (compose "(?://%s%s|%s|%s|%s)"
                             authority path-abempty path-absolute
                             path-noscheme path-empty))

(def relative-ref (compose "%s(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" relative-part QUESTION-MARK query fragment))

;; RFC 3987

(def ucschar
  (set (concat (int-range 0xA0 0xD7FF)
               (int-range 0xF900 0xFDCF)
               (int-range 0xFDF0 0xFFEF)

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
               )))



(comment ;; Example of a higher code-point - a Chinese character
  (String. (int-array [0x2F81A]) 0 1))

(def iunreserved (concat ALPHA DIGIT [\- \. \_ \~] ucschar))

(def iuserinfo (compose "(?<userinfo>(?:%s|%s|%s|%s)*)" iunreserved pct-encoded sub-delims ":"))

(def ireg-name (compose "(?:%s|%s|%s)*" iunreserved pct-encoded sub-delims))

(def ihost (compose "(?<host>%s|%s|%s)" IP-literal IPv4address ireg-name))

(def iauthority (compose (str "(?<authority>"
                              (str "(?:%s@)?"
                                   "(?:%s)"
                                   "(?:%s%s)?") ")") iuserinfo ihost COLON port))

(def ipchar (compose "(?:%s|%s|%s|%s|%s)" iunreserved pct-encoded sub-delims \: \@))

(def isegment (compose "(?:%s|%s|%s|%s|%s)*" iunreserved pct-encoded sub-delims \: \@))
(def isegment-nz (compose "(?:%s|%s|%s|%s|%s)+" iunreserved pct-encoded sub-delims \: \@))
(def isegment-nz-nc (compose "(?:%s|%s|%s|%s)+" iunreserved pct-encoded sub-delims \@))

(def ipath-abempty (compose "(?<path>(?:/%s)*)" isegment))
(def ipath-absolute (compose "/%s(?:/%s)*" isegment-nz isegment))
(def ipath-noscheme (compose "%s(?:/%s)*" isegment-nz-nc isegment))
(def ipath-rootless (compose "%s(?:/%s)*" isegment-nz isegment))
(def ipath-empty "")

(def ihier-part (compose "(?://%s%s|%s|%s|%s)" iauthority ipath-abempty ipath-absolute ipath-rootless ipath-empty))


(def iprivate (concat (int-range 0xE000 0xF8FF)
                      ;;(int-range 0xF0000 0xFFFFD)
                     ;;(int-range 0x100000 0x10FFFD)
                     ))


(def iquery (compose "(?:%s|%s|%s|%s)*" ipchar iprivate \/ \?))

(def ifragment (compose "(?:%s|%s|%s)*" ipchar \/ \?))


(def IRI (compose "(?<scheme>%s):(?:%s)(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" scheme ihier-part QUESTION-MARK iquery ifragment))

(def irelative-part (compose "(?://%s%s|%s|%s|%s)"
                             iauthority ipath-abempty ipath-absolute
                             ipath-noscheme ipath-empty))

(def irelative-ref (compose "%s(?:%s(?<query>%s))?(?:#(?<fragment>%s))?" irelative-part QUESTION-MARK iquery ifragment))

;; Can't do this AND have named groups - better to ask app logic to
;; ask if a string is either an IRI or a irelative-ref

#_(def IRI-reference (compose "(?:%s|%s)" IRI irelative-ref))

(comment
  (re-group-by-name (matched IRI "https://jon:pither@juxt.pro/malcolm?foo#bar") "host"))


;; RFC 5322, Section 3.2.3

(def atext (concat ALPHA DIGIT [\! \# \$ \% \& \' \* \+ \- \/ \= \? \^ \_ \` \{ \| \} \~]))

;(def rfc5322_atom (compose "[%s]+" (as-regex-str atext)))

(def dot-atom-text (compose "[%s]+(?:\\.[%s]+)*" (as-regex-str atext) (as-regex-str atext)))

;; RFC 5322, Section 3.4.1

(def domain dot-atom-text)

(def addr-spec (compose "(?<localpart>%s)@(?<domain>%s)" dot-atom-text domain))

(comment
  (re-matches addr-spec "mal@juxt.pro"))

;; RFC 6901: JavaScript Object Notation (JSON) Pointer

(def unescaped (concat (int-range 0x00 0x2E)
                       (int-range 0x30 0x7D)
                       ;; Should be this:
                       #_(int-range 0x7F 0x10FFFF)
                       ;; but too slow, so do this instead for now:
                       (int-range 0x7F 0xFFFF)))


(def referenced-token (compose "(?:[%s]|~0|~1)*" unescaped))

(def json-pointer (compose "(?:/%s)*" referenced-token))

;; draft-handrews-relative-json-pointer-01
(def non-negative-integer (compose "(?:%s|%s%s*)" \0 (int-range \1 \9) (int-range \0 \9)))

(def relative-json-pointer (compose "%s(?:#|%s)" non-negative-integer json-pointer))
