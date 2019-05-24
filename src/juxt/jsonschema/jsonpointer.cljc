;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.jsonpointer
  (:require
   [clojure.string :as str]))

(def reference-token-pattern #"/((?:[^/~]|~0|~1)*)")

(defn decode [token]
  (-> token
      (str/replace "~1" "/")
      (str/replace "~0" "~")))

(defn reference-tokens [s]
  (map decode (map second (re-seq reference-token-pattern s))))

(defn json-pointer [doc pointer]
  (loop [tokens (reference-tokens (or pointer ""))
         subdoc doc]
    (if (seq tokens)
      (recur
       (next tokens)
       (cond
         (map? subdoc)
         (let [subsubdoc (get subdoc (first tokens))]
           (if (some? subsubdoc) subsubdoc
               (throw (ex-info "Failed to locate" {:json-pointer pointer
                                                   :subsubdoc subsubdoc
                                                   :subdoc subdoc
                                                   :tokens tokens
                                                   :first-token (first tokens)
                                                   :type-subdoc (type subdoc)
                                                   :doc doc
                                                   :debug (get subdoc (first tokens))
                                                   }))))
         (sequential? subdoc)
         (if (re-matches #"[0-9]+" (first tokens))
           (let [subsubdoc
                 (get subdoc #?(:clj (Integer/parseInt (first tokens))
                                :cljs (js/Number (first tokens))))]
             (if (some? subsubdoc)
               subsubdoc
               (throw (ex-info "Failed to locate" {:json-pointer pointer
                                                   :subdoc subdoc
                                                   :doc doc}))))
           (throw (ex-info "Failed to locate, must be a number" {:json-pointer pointer
                                                                 :subdoc subdoc
                                                                 :doc doc})))))
      subdoc)))

(comment
  (json-pointer
   {"a" [{"b" "alpha"} {"b" [{"c" {"greek" "delta"}}]}]}
   "/a/1/b/0/c/greek"))

(comment
  (json-pointer
   {"a" [{"b" "alpha"} {"b" [{"c" {"greek" "delta"}}]}]}
   nil))
