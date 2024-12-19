(ns flanders.json-schema-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [flanders.core :as f]
            [flanders.example :as fe]
            [flanders.json-schema :as sut]
            [flanders.json-schema.test-helpers :as th :refer [->malli ->schema]]
            [malli.core :as m]
            [malli.generator :as mg]
            [schema.core :as s]))

;; tests

(deftest ->flanders-test
  (is (= (m/form (->malli th/union-example {:flanders.malli/no-example true}))
         [:map
          [:x
           [:or :int :string]]
          [:y :int]]))
  (is (= (s/explain (->schema th/union-example))
         '{:x (cond-pre Int Str), :y Int, Any Any})))

(def refs-json-schema-example
  {"$defs" {"aref" {"type" "array"
                    "items" {"$ref" "#/$defs/aref"}}}
   "$id" "https://schema.ocsf.io/schema/classes/security_finding"
   "$ref" "#/$defs/aref"})

(deftest refs-test
  (is (= #{"https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"}
         (set (keys (::f/registry (sut/->flanders refs-json-schema-example nil))))))
  (is (= #{"https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"}
         (set (keys (::f/registry (sut/->flanders refs-json-schema-example nil))))))
  (is (= '[:ref
           {:registry
            {"https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"
             [:sequential
              ;;TODO :sequential example
              [:ref {:json-schema/example []}
               "https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"]]}
            :json-schema/example []}
           "https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"]
         (m/form (->malli refs-json-schema-example))))
  (is (m/validate (->malli refs-json-schema-example) [])))

(deftest const-test
  (is (m/validate (->malli {"type" "integer" "const" 9007199254740992}) 9007199254740992)))

(deftest anyOf-test
  (is (= [:or
          [:map [:bar :int]]
          [:map [:foo :string]]]
         (m/form (->malli {"anyOf" [{"properties" {"bar" {"type" "integer"}}, "required" ["bar"]} {"properties" {"foo" {"type" "string"}}, "required" ["foo"]}]}
                          {:flanders.malli/no-example true}))))
  ;;FIXME should be s/either or s/conditional, cond-pre disjuncts must have distinct preconditions
  (is (= '(cond-pre {:bar Int, Any Any} {:foo Str, Any Any})
         (s/explain (->schema {"anyOf" [{"properties" {"bar" {"type" "integer"}}, "required" ["bar"]} {"properties" {"foo" {"type" "string"}}, "required" ["foo"]}]})))))

(deftest additionalProperties-test
  (testing "true => open"
    (is (= :map (m/form (->malli {"additionalProperties" true} {:flanders.malli/no-example true}))))
    (is (= '{Any Any}
           (s/explain (->schema {"additionalProperties" true})))))
  (testing "false => closed"
    (is (= [:map {:closed true}]
           (m/form (->malli {"additionalProperties" false} {:flanders.malli/no-example true}))))
    (is (= {}
           (s/explain (->schema {"additionalProperties" false}))))))

;;note these infer "type" more tightly than the spec
(deftest required-test
  (is (= [:map [:__proto__ :any] [:constructor :any] [:toString :any]]
         (m/form (->malli {"required" ["__proto__" "toString" "constructor"]} {:flanders.malli/no-example true}))))
  (is (= '{:__proto__ Any, :constructor Any, :toString Any, Any Any}
         (s/explain (->schema {"required" ["__proto__" "toString" "constructor"]})))))

(deftest enum-test
  (is (= [:enum 0 1 2 3 99] (m/form (->malli {"enum" [3, 0, 1, 2, 99], "title" "Activity ID", "type" "integer"} {:flanders.malli/no-example true})))))
