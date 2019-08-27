;; Copyright © 2019, JUXT LTD.

(ns juxt.jinx-alpha
  (:require
   [juxt.jinx-alpha.schema :as schema]
   [juxt.jinx-alpha.validate :as validate]
   [juxt.jinx-alpha.clj-transform :as transform]))

(defn schema
  "Build a JSON Schema from a map (or boolean). Must conform to
  rules. Returns a map that can be used in validation."
  ([s] (schema/schema s))
  ([s options] (schema/schema s options)))

(defn validate
  "Validate a map (or boolean) according to the given schema."
  ([instance schema] (validate/validate instance schema))
  ([instance schema options] (validate/validate instance schema options)))

(defn ^:jinx/experimental clj->jsch
  [clj]
  "Transform a Clojure syntax shorthand into JSON Schema and build it."
  (schema/schema (transform/clj->jsch clj)))
