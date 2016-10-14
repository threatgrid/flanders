(ns flanders.schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.schema :as fs]
            [schema.core :as s]))

(f/def-entity-type Example
  {:description "Example"}
  (f/required-entries
   (f/entry :foo f/any-str)
   (f/entry :bar (f/map
                  [(f/entry (f/eq "integer") f/any-int)
                   (f/entry :seq (f/seq-of f/any-keyword))
                   (f/entry :set (f/set-of (f/enum #{1 2 3})))])))
  (f/optional-entries
   (f/entry :spam (f/eq :eggs))))

(deftest test-valid-schema
  (is
   (s/validate
    (fs/->schema-tree Example)
    {:foo "foo"
     :bar {"integer" 100
           :seq [:a :b :c]
           :set #{1 3}}
     :spam :eggs})))
