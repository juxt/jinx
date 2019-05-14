;; Copyright © 2019, JUXT LTD.

(ns juxt.jsonschema.jsonpointer)

(def reference-token-pattern #"/((?:[^/~]|~0|~1)*)")

(defn reference-tokens [s]
  (map second (re-seq reference-token-pattern s)))

(defn json-pointer [doc pointer]
    (loop [tokens (reference-tokens pointer)
           doc doc]
      (if tokens
        (recur
         (next tokens)
         (cond
           (map? doc)
           (or
            (get doc (first tokens))
            (throw (ex-info "Failed to locate" {:json-pointer pointer})))
           (sequential? doc)
           (if (re-matches #"[0-9]+" (first tokens))
             (or
              (get doc #?(:clj (Integer/parseInt (first tokens))
                          :cljs (js/Number (first tokens))))
              (throw (ex-info "Failed to locate" {:json-pointer pointer})))
             (throw (ex-info "Failed to locate, must be a number" {:json-pointer pointer})))))
        doc)))

(comment
  (json-pointer
   {"a" [{"b" "alpha"} {"b" [{"c" {"greek" "delta"}}]}]}
   "/a/1/b/0/c/greek"))
