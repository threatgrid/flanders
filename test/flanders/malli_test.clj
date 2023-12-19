(ns flanders.malli-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.examples
             :refer [Example
                     OptionalKeywordMapEntryExample]]
            [flanders.core :as f]
            [flanders.utils :refer [optionalize-all]]
            [flanders.malli :as fm]
            [malli.core :as m])
  (:import (flanders.types ParameterListType
                           SignatureType)))

(deftest test-valid-schema
  (is (= [:map
          [:foo :string]
          [:bar [:map
                 ["integer" :int]
                 [:seq [:sequential :keyword]]
                 [:set [:set [:enum 1 2 3]]]]]
          [:yes? :boolean]
          [:spam {:optional true} [:enum :eggs]]]
        (-> Example
          fm/->malli
          m/form)))
  (is
   (m/validate
    (fm/->malli Example)
    {:foo "foo"
     :bar {"integer" 100
           :seq [:a :b :c]
           :set #{1 3}}
     :yes? true
     :spam :eggs}))
  (is (= [[[:foo] :malli.core/missing-key]]
         (map (juxt :path :type)
              (:errors
                (m/explain
                  (fm/->malli Example)
                  {:bar {"integer" 100
                         :seq [:a :b :c]
                         :set #{1 3}}
                   :yes? true
                   :spam :eggs}))))))

(deftest test-optional-kw-map-entry
  (let [expected-schema
        [:map
         [:foo {:optional true} :string]
         [:relation_info [:map [:malli.core/default [:map-of :keyword :any]]]]]]
    (is (= expected-schema
           (m/form (fm/->malli OptionalKeywordMapEntryExample))))))

(deftest signature-type->malli
  (is (= [:=> :cat :any]
         (m/form (fm/->malli (f/sig :parameters [])))))

  (let [return (f/int)]
    (is (= [:=> :cat :int]
           (m/form (fm/->malli (f/sig :parameters []
                                      :return return))))))

  (let [a (f/int)
        b (f/int)]
    (is (= [:=> [:cat :int :int] :any]
           (m/form (fm/->malli (f/sig :parameters [a b]))))))

  (let [a (f/int)
        b (f/int)
        c (f/str)]
    (is (= [:=> [:cat [:cat :int :int] [:* :string]] :any]
           (m/form (fm/->malli (f/sig :parameters [a b]
                                      :rest-parameter c)))))))
