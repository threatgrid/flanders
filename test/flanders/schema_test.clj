(ns flanders.schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.examples
             :refer [Example
                     OptionalKeywordMapEntryExample]]
            [flanders.core :as f]
            [flanders.utils :refer [optionalize-all]]
            [flanders.schema :as fs]
            [schema.core :as s
             :refer [Keyword Any]]))

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

(deftest test-optional-kw-map-entry
  (let [expected-schema
        {#schema.core.OptionalKey{:k :foo} java.lang.String
         :relation_info {Keyword Any}}]
    (is (= expected-schema
           (fs/->schema OptionalKeywordMapEntryExample)))))
