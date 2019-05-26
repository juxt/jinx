(ns juxt.jsonschema.regex
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(defn partition-into-ranges-iter
  "Find consecutive number sequences. O(n)"
  [coll]
  (loop [[x & xs] (sort coll)
         subsequent 0
         curr []
         ranges []]
    (if-not x
      (cond-> ranges (seq curr) (conj curr))
      (if (= (inc subsequent) (long x))
        (recur xs (long x) (conj curr x) ranges)
        (recur xs (long x) [x] (cond-> ranges (seq curr) (conj curr)))))))

(defn partition-into-ranges-fj
  "Find consecutive number sequences. O(log n)"
  [coll]
  (let [consecutive?
        (fn [coll]
          (= (count coll) (inc (- (long (last coll)) (long (first coll))))))

        fork (fn fork [coll]
               (if (consecutive? coll)
                 coll
                 (let [midpoint (quot (count coll) 2)]
                   [(fork (subvec coll 0 midpoint))
                    (fork (subvec coll midpoint))])))

        join (fn join [[coll & colls]]
               (let [consecutive? (= (inc (long (last coll))) (long (or (ffirst colls) -1)))]
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

(defn char-range
  "Inclusive char range between c1 and c2"
  [c1 c2]
  (map char (range (int c1) (inc (int c2)))))

(def regex-chars
  {\\ "\\\\"
   \u0009 "\\t"
   \u000A "\\n"
   \u000D "\\r"
   \u000C "\\f"
   \u0007 "\\a"
   \u001B "\\e"})

(defn char->regex [c]
  (let [n (long c)]
    (if (< n 256)
      (get regex-chars c (format "\\x%02X" n))
      (format "\\u%04X" n))))

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

         [{:class "Alnum" :set (set (concat (char-range \A \Z) (char-range \a \z) (char-range \0 \9)))}
          {:class "Alpha" :set (set (concat (char-range \A \Z) (char-range \a \z)))}
          {:class "Digit" :set (set (char-range \0 \9))}
          {:class "Cntrl" :set (set (concat (char-range \u0000 \u001f) [\u007f]))}
          {:class "Punct" :set #{\! \" \# \$ \% \& \' \(
                                 \) \* \+ \, \- \. \/ \:
                                 \; \< \= \> \? \@ \[ \\
                                 \] \^ \_ \` \{ \| \} \~}}
          {:class "Blank" :set (set [\space \tab])}])]

    (apply str (concat
                (map #(format "\\p{%s}" %) classes)
                ;; Find ranges
                (map (fn [x] (if (> (count x) 1)
                               (format "[%s-%s]"
                                       (char->regex (first x))
                                       (char->regex (last x)))
                               (char->regex (first x))))
                     (partition-into-ranges-iter remaining))))))

(extend-protocol RegExpressable
  clojure.lang.ISeq
  (as-regex-str [s] (expand-with-character-classes s))
  clojure.lang.PersistentVector
  (as-regex-str [s] (expand-with-character-classes s))
  String
  (as-regex-str [s] s)
  Character
  (as-regex-str [c] (char->regex c))
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

(def CR (char 0x0D))
(def LF (char 0x0A))


(def CRLF (concatenate CR LF))

(def SP (char 0x20))

(def HTAB (char 0x09))
(def SP (char 0x20))
(def WSP (distinct (concat SP HTAB)))

(def ALPHA (map char (concat (range 0x41 (inc 0x5A)) (range 0x61 (inc 0x7A)))))
(def DIGIT (map char (range 0x30 (inc 0x39))))

(defn matched [^java.util.regex.Pattern re ^CharSequence s]
  (let [m (re-matcher re s)]
    (when (.matches m)
      m)))

(defn re-group-by-name [^java.util.regex.Matcher matcher ^String name]
  (when matcher
    (.group matcher name)))

;; RFC 5322, Section 3.2.3

(def atext (concat ALPHA DIGIT [\! \# \$ \% \& \' \* \+ \- \/ \= \? \^ \_ \` \{ \| \} \~]))

(def atom (compose "[%s]+" (as-regex-str atext)))

(def dot-atom-text (compose "[%s]+(?:\\.[%s]+)*" (as-regex-str atext) (as-regex-str atext)))

;; RFC 5322, Section 3.4.1

(def domain dot-atom-text)

(def addr-spec (compose "%s@%s" dot-atom-text domain))

(comment
  (re-matches addr-spec "mal@juxt.pro"))
