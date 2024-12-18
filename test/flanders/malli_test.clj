(ns flanders.malli-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as w]
            [flanders.examples :as fes
             :refer [Example
                     OptionalKeywordMapEntryExample]]
            [flanders.core :as f]
            [flanders.malli :as fm]
            [malli.core :as m]
            [malli.swagger :as ms]
            [malli.generator :as mg]
            [malli.util :as mu]))

(def swagger-props #{:json-schema/description :json-schema/example})

(defn- strip-swagger-from-props [props]
  (apply dissoc props swagger-props))

(defn- strip-swagger [schema]
  ;; TODO strip properties in entry vals like [:map [:a {HERE} s]] during walk
  (m/walk schema (fn [schema _ _ _]
                   (mu/update-properties schema strip-swagger-from-props))
          {::m/walk-entry-vals true}))

(defn- form-no-swagger [schema]
  (let [options (m/options schema)
        frm (-> schema strip-swagger m/form)]
    ;; FIXME replace this dirty tree walk with strip-swagger
    (-> (w/postwalk (fn [v]
                      (cond-> v
                        (and (map? v)
                             (some v swagger-props))
                        (-> strip-swagger-from-props not-empty)))
                    frm)
        ;; convert [:string nil] to :string
        (m/form options))))

(defn- ->malli-frm [dll]
  (-> dll fm/->malli form-no-swagger))

(deftest test-basic-schemas
  (testing "str"
    (is (= :string (->malli-frm (f/str))))
    (is (= [:enum "a"] (->malli-frm (f/str :equals "a"))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :string :any]]]
           (->malli-frm (f/map [(f/entry (f/str) f/any)]))))
    (is (= [:map {:closed true} ["b" :any]]
           (->malli-frm (f/map [(f/entry (f/str :equals "b") f/any)])))))
  (testing "int"
    (is (= :int (->malli-frm (f/int))))
    (is (= [:enum 1] (->malli-frm (f/int :equals 1))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :int :any]]]
           (->malli-frm (f/map [(f/entry (f/int) f/any)]))))
    (is (= [:map {:closed true} [1 :any]]
           (->malli-frm (f/map [(f/entry (f/int :equals 1) f/any)])))))
  (testing "num"
    (is (= 'number? (->malli-frm (f/num))))
    (is (= [:enum 1] (->malli-frm (f/num :equals 1))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of 'number? :any]]]
           (->malli-frm (f/map [(f/entry (f/num) f/any)]))))
    (is (= [:map {:closed true} [1 :any]]
           (->malli-frm (f/map [(f/entry (f/num :equals 1) f/any)])))))
  (testing "keyword"
    (is (= :keyword (->malli-frm (f/keyword))))
    (is (= [:enum :a] (->malli-frm (f/keyword :equals :a))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :keyword :any]]]
           (->malli-frm (f/map [(f/entry (f/keyword) f/any)]))))
    (is (= [:map {:closed true} [:a :any]]
           (->malli-frm (f/map [(f/entry (f/keyword :equals :a) f/any)])))))
  (testing "inst"
    (is (= 'inst? (->malli-frm (f/inst))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of 'inst? :any]]]
           (->malli-frm (f/map [(f/entry (f/inst) f/any)])))))
  (testing "bool"
    (is (= :boolean (->malli-frm (f/bool))))
    (is (= [:= false] (->malli-frm (f/bool :equals false))))
    (is (= [:= true] (->malli-frm (f/bool :equals true))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :boolean :any]]]
           (->malli-frm (f/map [(f/entry (f/bool) f/any)]))))
    (is (= [:map {:closed true} [true :any]]
           (->malli-frm (f/map [(f/entry (f/bool :equals true) f/any)]))))
    (is (= [:map {:closed true} [false :any]]
           (->malli-frm (f/map [(f/entry (f/bool :equals false) f/any)])))))
  (testing "anything"
    (is (= :any (->malli-frm (f/anything))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of :any :any]]]
           (->malli-frm (f/map [(f/entry (f/anything) f/any)])))))
  (testing "set-of"
    (is (= [:set :boolean]
           (->malli-frm (f/set-of (f/bool)))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:set :boolean] :any]]]
           (->malli-frm (f/map [(f/entry (f/set-of (f/bool)) f/any)])))))
  (testing "seq-of"
    (is (= [:sequential :boolean] (->malli-frm (f/seq-of (f/bool)))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:sequential :boolean] :any]]]
           (->malli-frm (f/map [(f/entry (f/seq-of (f/bool)) f/any)])))))
  (testing "either"
    (is (= [:or :boolean :string]
           (->malli-frm (f/either :choices [(f/bool) (f/str)]))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:or :boolean :string] :any]]]
           (->malli-frm (f/map [(f/entry (f/either :choices [(f/bool) (f/str)]) f/any)])))))
  (testing "sig"
    (is (= [:=> [:cat :int] :int]
           (->malli-frm (f/sig :parameters [(f/int)] :return (f/int)))))
    (is (= [:map {:closed true} [:malli.core/default [:map-of [:=> [:cat :int] :int] :any]]]
           (->malli-frm (f/map [(f/entry (f/sig :parameters [(f/int)] :return (f/int)) f/any)]))))))

(deftest test-valid-schema
  (is (= [:map {:closed true
                :json-schema/example {:foo "string"
                                      :bar {"integer" 10, :seq [:keyword], :set #{1}}
                                      :yes? true
                                      :spam :eggs}
                :json-schema/description "Example"}
          [:foo
           {:json-schema/example "string"}
           [:string {:json-schema/example "string"}]]
          [:bar {:json-schema/example {"integer" 10, :seq [:keyword], :set #{1}}}
           [:map {:closed true
                  :json-schema/example {"integer" 10, :seq [:keyword], :set #{1}}}
            ["integer"
             {:json-schema/example 10}
             [:int {:json-schema/example 10}]]
            [:seq {:json-schema/example [:keyword]}
             [:sequential [:keyword {:json-schema/example :keyword}]]]
            [:set {:json-schema/example #{1}}
             [:set [:enum {:json-schema/example 1} 1 2 3]]]]]
          [:yes? {:json-schema/example true} [:boolean {:json-schema/example true}]]
          [:spam {:json-schema/example :eggs, :optional true}
           [:enum {:json-schema/example :eggs} :eggs]]]
         (-> Example
             fm/->malli
             m/form)))
  (is (m/validate
        (fm/->malli Example)
        {:foo "foo"
         :bar {"integer" 100
               :seq [:a :b :c]
               :set #{1 3}}
         :yes? true
         :spam :eggs}))
  (is (= [{:in [:foo], :value 1}
          {:in [:bar], :value nil}
          {:in [:spam], :value "ham"}]
         (mapv #(select-keys % [:in :value])
               (:errors (m/explain
                          (fm/->malli Example)
                          {:foo 1
                           :yes? true
                           :spam "ham"})))))
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
           (->malli-frm OptionalKeywordMapEntryExample))))
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
         (->malli-frm (f/sig :parameters []))))
  (is (= [:=> :cat :int]
         (->malli-frm (f/sig :parameters []
                             :return (f/int)))))
  (is (= [:=> [:cat :int :int] :any]
         (->malli-frm (f/sig :parameters [(f/int) (f/int)]))))
  (is (= [:=> [:cat [:cat :int :int] [:* :string]] :any]
         (->malli-frm (f/sig :parameters [(f/int) (f/int)]
                             :rest-parameter (f/str))))))

(deftest swagger-test
  (is (= {:type "object"
          :properties {:foo {:type "string", :example "string"}
                       :bar {:type "object"
                             :properties {"integer" {:type "integer", :format "int64", :example 10}
                                          :seq {:type "array", :items {:type "string", :example :keyword}, :example [:keyword]}
                                          :set {:type "array", :items {:type "integer", :enum [1 2 3], :example 1}
                                                :uniqueItems true, :example #{1}}}
                             :required ["integer" :seq :set]
                             :additionalProperties false, :example {"integer" 10, :seq [:keyword], :set #{1}}}
                       :yes? {:type "boolean", :example true}
                       :spam {:type "string", :enum [:eggs], :example :eggs}}
          :required [:foo :bar :yes?]
          :additionalProperties false
          :example {:foo "string", :bar {"integer" 10, :seq [:keyword], :set #{1}}, :yes? true, :spam :eggs}
          :description "Example"}
         (-> Example
             fm/->malli
             ms/transform))))

(deftest ref-form-test
  (is (= [:ref
          {:json-schema/example 10,
           :registry {"foo" [:int {:json-schema/example 10}]}}
          "foo"]
         (-> fes/RefExample fm/->malli m/form)))
  (is (= [:ref
          {:json-schema/example [],
           :registry
           {"foo" [:sequential [:ref {:json-schema/example []} "foo"]]}}
          "foo"]
         (-> fes/RecursiveRefExample fm/->malli m/form)))
  (is (= [:ref
          {:json-schema/example 42,
           :registry
           {"a"
            [:ref
             {:json-schema/example 42,
              :registry {"a" [:enum {:json-schema/example 42} 42]}}
             "a"]}}
          "a"]
         (-> fes/ShadowingRefExample fm/->malli m/form)))
  (is (= [:ref
          {:json-schema/example 42,
           :registry
           {"a" [:ref {:json-schema/example 42} "b"],
            "b"
            [:ref
             {:json-schema/example 42,
              :registry
              {"a" [:ref {:json-schema/example 42} "b"],
               "b" [:enum {:json-schema/example 42} 42]}}
             "a"]}}
          "a"]
         (-> fes/ShadowingMultiRefExample fm/->malli m/form)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InnerRecursionRefExample fm/->malli m/form)))
  (is (thrown-with-msg? Exception
                        #"Ref not in scope: \"a\""
                        (-> fes/UnscopedRefExample fm/->malli)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InfiniteRefExample fm/->malli)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InfiniteEitherExample fm/->malli)))
  (testing "infinite schema detected even without examples walked"
    (is (thrown-with-msg? Exception
                          #"Infinite schema detected"
                          (-> fes/InfiniteRefExample (fm/->malli {::fm/no-example true}))))
    (is (thrown-with-msg? Exception
                          #"Infinite schema detected"
                          (-> fes/InfiniteEitherExample (fm/->malli {::fm/no-example true}))))))

(deftest ref-validation-test
  (is (m/validate (fm/->malli fes/RefExample) 10))
  (is (m/explain (fm/->malli fes/RefExample) nil))
  (is (m/validate (fm/->malli fes/RecursiveRefExample) []))
  (is (m/explain (fm/->malli fes/RecursiveRefExample) 1))
  (is (m/validate (fm/->malli fes/ShadowingRefExample) 42))
  (is (m/explain (fm/->malli fes/ShadowingRefExample) 41))
  (is (m/validate (fm/->malli fes/ShadowingMultiRefExample) 42))
  (is (m/explain (fm/->malli fes/ShadowingMultiRefExample) 41)))

(deftest ref-generator-test
  (is (= '(0 -1 0 -3 0 1 16 0 7 3) (-> fes/RefExample fm/->malli (mg/sample {:seed 0}))))
  (is (= '([] [] [[] []] [[] []] [] [[]] [] [[] []] [[]] [[[] []] [[] []]])
         (-> fes/RecursiveRefExample fm/->malli (mg/sample {:seed 0}))))
  (is (= '(42 42 42 42 42 42 42 42 42 42) (-> fes/ShadowingRefExample fm/->malli (mg/sample {:seed 0}))))
  (is (= '(42 42 42 42 42 42 42 42 42 42) (-> fes/ShadowingMultiRefExample fm/->malli (mg/sample {:seed 0})))))
