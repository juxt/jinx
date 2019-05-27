(ns juxt.jsonschema.regex
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

;; The purpose of this namespace is to allow the accurate computation
;; of Java regex patterns.

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
          (= (count coll) (inc (- (int (last coll)) (int (first coll))))))

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
  "Inclusive int range between n1 and n2"
  [n1 n2]
  (range (int n1) (inc (int n2))))

(def regex-chars
  {(int \\) "\\\\"
   (int \u0009) "\\t"
   (int \u000A) "\\n"
   (int \u000D) "\\r"
   (int \u000C) "\\f"
   (int \u0007) "\\a"
   (int \u001B) "\\e"})

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
          {:class "Digit" :set (set (int-range \0 \9))}
          {:class "Cntrl" :set (set (concat (int-range \u0000 \u001f) [(int \u007f)]))}
          {:class "Punct" :set (set (map int [\! \" \# \$ \% \& \' \(
                                              \) \* \+ \, \- \. \/ \:
                                              \; \< \= \> \? \@ \[ \\
                                              \] \^ \_ \` \{ \| \} \~]))}
          {:class "Blank" :set (set (map int [\space \tab]))}])]

    (println "remaining is" remaining)

    (apply str (concat
                (map #(format "\\p{%s}" %) classes)
                ;; Find ranges
                (map (fn [x] (if (> (count x) 1)
                               (format "[%s-%s]"
                                       (int->regex (first x))
                                       (int->regex (last x)))
                               (int->regex (first x))))
                     (partition-into-ranges-iter remaining))))))

(extend-protocol RegExpressable
  clojure.lang.ISeq
  (as-regex-str [s] (expand-with-character-classes (map int s)))
  clojure.lang.PersistentVector
  (as-regex-str [s] (expand-with-character-classes (map int s)))
  String
  (as-regex-str [s] s)
  Character
  (as-regex-str [c] (int->regex c))
  Integer
  (as-regex-str [n]
    (int->regex (int n)))
  Long
  (as-regex-str [n]
    (assert (<= n Integer/MAX_VALUE))
    (int->regex (int n)))
  java.util.regex.Pattern
  (as-regex-str [re] (str re))
  clojure.lang.PersistentHashSet
  (as-regex-str [s] (as-regex-str (seq s))))

(defn concatenate
  [& args]
  (re-pattern (apply str (map as-regex-str args))))

(defn compose [fmt & args]
  (re-pattern (apply format fmt (map as-regex-str args))))

;; RFC 5234

(def CR 0x0D)
(def LF 0x0A)


(def CRLF (concatenate CR LF))

(def HTAB \tab)
(def SP \space)
(def WSP (distinct (list SP HTAB)))

(def ALPHA (concat (range 0x41 (inc 0x5A)) (range 0x61 (inc 0x7A))))
(def DIGIT (range 0x30 (inc 0x39)))

(defn matched [^java.util.regex.Pattern re ^CharSequence s]
  (let [m (re-matcher re s)]
    (when (.matches m)
      m)))

(defn re-group-by-name [^java.util.regex.Matcher matcher ^String name]
  (when matcher
    (.group matcher name)))

;; RFC 3986

(def scheme (compose "%s[%s]*" ALPHA (set (concat ALPHA DIGIT [\+ \- \.]))))

(as-regex-str ALPHA)
(as-regex-str (set (concat ALPHA DIGIT [\+ \- \.])))

(identity scheme)

(comment
  (re-matches scheme "http+"))

(re-matches scheme "http+")


;; RFC 3987

;; Note don't yet declare the whole uscchar range as defined in RFC
;; 3987.

;; [\x{1D400}-\x{1D419}]

(def ucschar (set (concat (int-range \u00A0 \uD7FF)
                          (int-range \uF900 \uFDCF)
                          (int-range \uFDF0 \uFFEF))))

(Character/isLetter 0x2F81A)

;;(char 0x2F81A)

(def iunreserved (concat ALPHA DIGIT [\- \, \_ \~] ucschar))

;;(def iuserinfo (compose "(?"))

(def iauthority (compose "(?<iuserinfo>%s@)?%s(?<port>\\:%s)?" iuserinfo port))

(def ihier-part (compose "//%s%s|%s|%s|%s" iauthority ipath-abempty ipath-absolute ipath-rootless ipath-empty))

(def iri (compose "%s:%s(?:?%s)(?:#%s)" scheme ihier-part iquery ifragment))

(def iri-reference (compose "%s|%s" iri irelative-ref))

;; RFC 5322, Section 3.2.3

(def atext (concat ALPHA DIGIT [\! \# \$ \% \& \' \* \+ \- \/ \= \? \^ \_ \` \{ \| \} \~]))

(def atom (compose "[%s]+" (as-regex-str atext)))

(def dot-atom-text (compose "[%s]+(?:\\.[%s]+)*" (as-regex-str atext) (as-regex-str atext)))

;; RFC 5322, Section 3.4.1

(def domain dot-atom-text)

(def addr-spec (compose "%s@%s" dot-atom-text domain))

(comment
  (re-matches addr-spec "mal@juxt.pro"))
