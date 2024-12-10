(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.core :as f]
            [clojure.java.io :as io]
            [flanders.schema :as schema]
            [flanders.malli :as malli]
            [malli.core :as m]
            [flanders.json-schema :as sut]
            [cheshire.core :as json]))

(def union-example
  {:title "union", ;;TODO
   :type "object",
   :properties (sorted-map
                 :x {:anyOf [{:type "integer"} {:type "string"}]}
                 :y {:type "integer"}),
   :required [:x :y]})

(defn ->malli [v] (malli/->malli (sut/->flanders v nil)))
(defn ->schema [v] (schema/->schema (sut/->flanders v nil)))

;; https://github.com/metosin/malli/blob/6a2d9bd45d4973b4541cfdacdc4185240aa9a518/test/malli/json_schema_test.cljc#L9C1-L133C1
(def malli-expectations
  [;; predicates
   [pos-int? {:type "integer", :minimum 1}]
   ['pos? {:type "number" :exclusiveMinimum 0}]
   ['float? {:type "number"}]
   ;; comparators
   [[:> 6] {:type "number", :exclusiveMinimum 6}]
   [[:>= 6] {:type "number", :minimum 6}]
   [[:< 6] {:type "number", :exclusiveMaximum 6}]
   [[:<= 6] {:type "number", :maximum 6}]
   [[:= "x"] {:const "x"}]
   ;; base
   [[:not string?] {:not {:type "string"}}]
   [[:and int? pos-int?] {:allOf [{:type "integer"}
                                  {:type "integer", :minimum 1}]}]
   [[:or int? string?] {:anyOf [{:type "integer"} {:type "string"}]}]
   [[:orn [:i int?] [:s string?]] {:anyOf [{:type "integer"} {:type "string"}]}]
   [[:map
     [:a string?]
     [:b {:optional true} string?]
     [:c {:optional false} string?]]
    {:type "object"
     :properties {:a {:type "string"}
                  :b {:type "string"}
                  :c {:type "string"}}
     :required [:a :c]}]
   [[:map
     [:x :int]
     [::m/default [:map-of :int :int]]]
    {:type "object"
     :properties {:x {:type "integer"}}
     :required [:x]
     :additionalProperties {:type "integer"}}]
   #_ ;TODO
   [[:map
     [:x :int]
     [::m/default [:fn {:json-schema/default {:x 1}, :gen/gen (gen/return {})} map?]]]
    {:type "object"
     :properties {:x {:type "integer"}}
     :required [:x]
     :default {:x 1}}]
   [[:map
     [:x :int]
     [::m/default [:map
                   [:y :int]
                   [::m/default [:map
                                 [:z :int]
                                 [::m/default [:map-of :int :int]]]]]]]
    {:type "object",
     :additionalProperties {:type "integer"},
     :properties {:x {:type "integer"}
                  :y {:type "integer"}
                  :z {:type "integer"}},
     :required [:x :y :z]}]
   [[:multi {:dispatch :type
             :decode/string '(fn [x] (update x :type keyword))}
     [:sized [:map {:gen/fmap '#(assoc % :type :sized)} [:type keyword?] [:size int?]]]
     [:human [:map {:gen/fmap '#(assoc % :type :human)} [:type keyword?] [:name string?] [:address [:map [:country keyword?]]]]]
     [::m/default :string]]
    {:oneOf [{:type "object",
              :properties {:type {:type "string"}
                           :size {:type "integer"}},
              :required [:type :size]}
             {:type "object",
              :properties {:type {:type "string"},
                           :name {:type "string"},
                           :address {:type "object"
                                     :properties {:country {:type "string"}}
                                     :required [:country]}},
              :required [:type :name :address]}
             {:type "string"}]}]
   [[:map-of string? string?] {:type "object"
                               :additionalProperties {:type "string"}}]
   [[:vector string?] {:type "array", :items {:type "string"}}]
   [[:sequential string?] {:type "array", :items {:type "string"}}]
   [[:set string?] {:type "array"
                    :items {:type "string"}
                    :uniqueItems true}]
   [[:enum 1 2 "3"] {:enum [1 2 "3"]}]
   [[:enum 1 2 3] {:type "integer" :enum [1 2 3]}]
   [[:enum 1.1 2.2 3.3] {:type "number" :enum [1.1 2.2 3.3]}]
   [[:enum "kikka" "kukka"] {:type "string" :enum ["kikka" "kukka"]}]
   [[:enum "kikka" "kukka"] {:type "string" :enum [:kikka :kukka]}]
   [[:enum "kikka" "kukka"] {:type "string" :enum ['kikka 'kukka]}]
   ;;TODO
   #_[[:maybe string?] {:oneOf [{:type "string"} {:type "null"}]}]
   ;;TODO
   #_[[:tuple string? string?] {:type "array"
                                :items [{:type "string"} {:type "string"}]
                                :additionalItems false}]
   ;;TODO
   #_[[:re "^[a-z]+\\.[a-z]+$"] {:type "string", :pattern "^[a-z]+\\.[a-z]+$"}]
   ;;TODO
   #_[:nil {:type "null"}]
   ;;TODO
   #_[[:string {:min 1, :max 4}] {:type "string", :minLength 1, :maxLength 4}]
   ;;TODO
   #_[[:int {:min 1, :max 4}] {:type "integer", :minimum 1, :maximum 4}]
   [[:string {:json-schema/example "string"}] {:type "string"}]
   ;;TODO
   #_[:uuid {:type "string", :format "uuid"}]

   [[:int #:json-schema{:example 10}] {:type "integer"}]
   [['number? #:json-schema{:example 10.0}] {:type "number"}]
   ;; protocols
#_ ;TODO
   [(reify
      m/Schema
      (-properties [_])
      (-parent [_] (reify m/IntoSchema (-type [_]) (-type-properties [_])))
      (-form [_])
      (-validator [_] int?)
      (-walk [t w p o] (m/-outer w t p nil o))
      json-schema/JsonSchema
      (-accept [_ _ _] {:type "custom"})) {:type "custom"}]
   ;; type-properties
#_ ;TODO
   [malli.core-test/Over6 {:type "integer", :format "int64", :minimum 6}]
#_ ;TODO
   [[malli.core-test/Over6 {:json-schema/example 42}] {:type "integer", :format "int64", :minimum 6, :example 42}]])

(deftest ->flanders-test
  (is (= (m/form (->malli union-example))
         [:map
          {:closed true, :json-schema/example {:x 10, :y 10}}
          [:x
           #:json-schema{:example 10}
           [:or
            [:int #:json-schema{:example 10}]
            [:string #:json-schema{:example "string"}]]]
          [:y #:json-schema{:example 10} [:int #:json-schema{:example 10}]]]))
  (is (= (->schema union-example)
         [:map
          {:closed true, :json-schema/example {:x 10, :y 10}}
          [:x
           #:json-schema{:example 10}
           [:or
            [:int #:json-schema{:example 10}]
            [:string #:json-schema{:example "string"}]]]
          [:y #:json-schema{:example 10} [:int #:json-schema{:example 10}]]])))

#_
(deftest malli-expectations-test
  (doseq [[expected-malli json-schema] malli-expectations]
    (testing (pr-str json-schema)
      (is (= expected-malli (m/form (->malli json-schema)))))))

(deftest defs-test
  (is (= nil
         (sut/->flanders {:$defs {:example {:type "integer"}}
                          :type "object"
                          :properties {:foo {:$ref "#/$defs/example"}}}
                         nil))))

(defn security-finding-json [] (json/decode (slurp (io/resource "security-finding.json"))))

(deftest ocsf-test
  (is (= nil
         (sut/->flanders (security-finding-json) nil)))
  )
