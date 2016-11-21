(ns flanders.schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.examples :refer [Example]]
            [flanders.core :as f]
            [flanders.schema :as fs]
            [schema.core :as s]))

(deftest test-valid-schema
  (is
   (s/validate
    (fs/->schema Example)
    {:foo "foo"
     :bar {"integer" 100
           :seq [:a :b :c]
           :set #{1 3}}
     :yes? true
     :spam :eggs})))
