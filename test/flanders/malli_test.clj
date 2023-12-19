(ns flanders.malli-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.examples
             :refer [Example
                     OptionalKeywordMapEntryExample]]
            [flanders.core :as f]
            [flanders.utils :refer [optionalize-all]]
            [flanders.malli :as fm]
            [malli.core :as m])
  (:import (flanders.types ParameterListType
                           SignatureType)))

(deftest test-basic-schemas
  (testing "str"
    (is (= :string (-> (f/str) fm/->malli m/form)))
    (is (= [:enum "a"] (-> (f/str :equals "a") fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :string :any]]] (-> (f/map [(f/entry (f/str) f/any)]) fm/->malli m/form)))
    (is (= [:map {:closed true} ["b" :any]] (-> (f/map [(f/entry (f/str :equals "b") f/any)]) fm/->malli m/form))))
  (testing "int"
    (is (= :int (-> (f/int) fm/->malli m/form)))
    (is (= [:enum 1] (-> (f/int :equals 1) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :int :any]]] (-> (f/map [(f/entry (f/int) f/any)]) fm/->malli m/form)))
    (is (= [:map {:closed true} [1 :any]] (-> (f/map [(f/entry (f/int :equals 1) f/any)]) fm/->malli m/form))))
  (testing "num"
    (is (= 'number? (-> (f/num) fm/->malli m/form)))
    (is (= [:enum 1] (-> (f/num :equals 1) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of 'number? :any]]] (-> (f/map [(f/entry (f/num) f/any)]) fm/->malli m/form)))
    (is (= [:map {:closed true} [1 :any]] (-> (f/map [(f/entry (f/num :equals 1) f/any)]) fm/->malli m/form))))
  (testing "keyword"
    (is (= :keyword (-> (f/keyword) fm/->malli m/form)))
    (is (= [:enum :a] (-> (f/keyword :equals :a) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :keyword :any]]] (-> (f/map [(f/entry (f/keyword) f/any)]) fm/->malli m/form)))
    (is (= [:map {:closed true} [:a :any]] (-> (f/map [(f/entry (f/keyword :equals :a) f/any)]) fm/->malli m/form))))
  (testing "inst"
    (is (= 'inst? (-> (f/inst) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of 'inst? :any]]] (-> (f/map [(f/entry (f/inst) f/any)]) fm/->malli m/form))))
  (testing "bool"
    (is (= :boolean (-> (f/bool) fm/->malli m/form)))
    (is (= [:enum false] (-> (f/bool :equals false) fm/->malli m/form)))
    (is (= [:enum true] (-> (f/bool :equals true) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :boolean :any]]] (-> (f/map [(f/entry (f/bool) f/any)]) fm/->malli m/form)))
    (is (= [:map {:closed true} [true :any]] (-> (f/map [(f/entry (f/bool :equals true) f/any)]) fm/->malli m/form)))
    (is (= [:map {:closed true} [false :any]] (-> (f/map [(f/entry (f/bool :equals false) f/any)]) fm/->malli m/form))))
  (testing "anything"
    (is (= :any (-> (f/anything) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :any :any]]] (-> (f/map [(f/entry (f/anything) f/any)]) fm/->malli m/form))))
  (testing "set-of"
    (is (= [:set :boolean] (-> (f/set-of (f/bool)) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:set :boolean] :any]]] (-> (f/map [(f/entry (f/set-of (f/bool)) f/any)]) fm/->malli m/form))))
  (testing "seq-of"
    (is (= [:sequential :boolean] (-> (f/seq-of (f/bool)) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:sequential :boolean] :any]]] (-> (f/map [(f/entry (f/seq-of (f/bool)) f/any)]) fm/->malli m/form))))
  (testing "either"
    (is (= [:or :boolean :string] (-> (f/either :choices [(f/bool) (f/str)]) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:or :boolean :string] :any]]]
           (-> (f/map [(f/entry (f/either :choices [(f/bool) (f/str)]) f/any)]) fm/->malli m/form))))
  (testing "sig"
    (is (= [:=> [:cat :int] :int] (-> (f/sig :parameters [(f/int)] :return (f/int)) fm/->malli m/form)))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:=> [:cat :int] :int] :any]]]
           (-> (f/map [(f/entry (f/sig :parameters [(f/int)] :return (f/int)) f/any)]) fm/->malli m/form)))))

(deftest test-valid-schema
  (is (= [:map {:closed true}
          [:foo :string]
          [:bar [:map {:closed true}
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
  (testing "closed"
    (is
      (= '[[[:extra] :malli.core/extra-key]]
         (map (juxt :path :type)
              (:errors
                (m/explain
                  (fm/->malli Example)
                  {:extra "bad"
                   :foo "foo"
                   :bar {"integer" 100
                         :seq [:a :b :c]
                         :set #{1 3}}
                   :yes? true
                   :spam :eggs}))))))
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
        [:map {:closed true}
         [:foo {:optional true} :string]
         [:relation_info [:map {:closed true} [:malli.core/default [:map-of :keyword :any]]]]]]
    (is (= expected-schema
           (m/form (fm/->malli OptionalKeywordMapEntryExample)))))
  (is (m/validate
        (fm/->malli OptionalKeywordMapEntryExample)
        {:foo "a"
         :relation_info {:asdf nil :blah "anything"}}))
  (is (= [{:path [:relation_info :malli.core/default 0]
           :in [:relation_info "not-a-kw"]
           :schema :keyword
           :value "not-a-kw"}]
         (map #(update % :schema m/form)
              (:errors
                (m/explain
                  (fm/->malli OptionalKeywordMapEntryExample)
                  {:foo "a"
                   :relation_info {"not-a-kw" nil :blah "anything"}}))))))

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
