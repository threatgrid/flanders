(ns flanders.schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.examples
             :refer [Example
                     OptionalKeywordMapEntryExample]]
            [flanders.core :as f]
            [flanders.utils :refer [optionalize-all]]
            [flanders.schema :as fs]
            [schema.core :as s
             :refer [Keyword Any]])
  (:import (flanders.types ParameterListType
                           SignatureType)))

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

(deftest signature-type->schema
  (is (instance? schema.core.FnSchema
                 (fs/->schema (f/sig :parameters []))))

  (is (instance? schema.core.AnythingSchema
                 (:output-schema (fs/->schema (f/sig :parameters [])))))

  (let [return (f/int)]
    (is (instance? (class (fs/->schema return))
                   (:output-schema (fs/->schema (f/sig :parameters []
                                                       :return return))))))

  (let [a (f/int)
        b (f/int)]
    (is (= 1
           (count (:input-schemas (fs/->schema (f/sig :parameters [a b])))))
        "A signature should produce only 1 input-schema"))

  (let [a (f/int)
        b (f/int)]
    (is (= [(fs/->schema a) (fs/->schema b)]
           (first (:input-schemas (fs/->schema (f/sig :parameters [a b])))))))

  (let [a (f/int)
        b (f/int)
        c (f/str)]
    (is (= [(fs/->schema a) (fs/->schema b) [(fs/->schema c)]]
           (first (:input-schemas (fs/->schema (f/sig :parameters [a b]
                                                      :rest-parameter c))))))))
