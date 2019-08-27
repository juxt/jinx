;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx-alpha.clj-transform)

(defn clj->jsch [x]
  (cond
    (vector? x)
    (if (= 1 (count x))
      {"type" "array" "items" (clj->jsch (first x))}
      (throw (ex-info "Vector can only contain one item, the type of the array items" {})))

    (boolean? x)
    {"type" "boolean" "constant" x}

    (integer? x)
    {"type" "integer" "constant" x}

    (number? x)
    {"type" "number" "constant" x}

    (string? x)
    {"type" "string" "constant" x}

    (list? x)
    (cond
      (= (first x) 'all-of) {"allOf" (mapv clj->jsch (rest x))}
      (= (first x) 'one-of) {"oneOf" (mapv clj->jsch (rest x))}
      (= (first x) 'any-of) {"anyOf" (mapv clj->jsch (rest x))}
      (= (first x) 'not) {"not" (clj->jsch (second x))}
      :else
      {"type" "array" "items" (mapv clj->jsch x)})

    (nil? x) {"type" "null"}

    (symbol? x)
    (cond
      (#{'string 'integer 'boolean 'number} x)
      {"type" (name x)}
      :else (throw (ex-info "Unexpected symbol" {:symbol x})))

    (map? x)
    (reduce-kv
     (fn [acc k v]
       (assoc acc
              (if (and (keyword? k) (nil? (namespace k)))
                (name k) k)
              (case k
                :properties (reduce-kv
                             (fn [acc k v]
                               (assoc acc
                                      k (clj->jsch v)
                                      ))
                             {} v)
                v)))
     {} x)))



#_(clj->jsch {:properties {"a" "A"}
            :required ["a"]})
