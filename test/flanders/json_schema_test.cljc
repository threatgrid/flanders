(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [flanders.core :as f]
            [flanders.json-schema :as sut]
            [flanders.json-schema.test-helpers :as th :refer [->malli ->schema]]
            [cheshire.core :as json]
            [malli.core :as m]
            [schema.core :as s]))

(defn sort-recursive [v]
  (walk/postwalk
    (fn [v]
      (cond->> v
        (map? v) (into (sorted-map))))
    v))

(defn gen-ocsf-json-schema-1-3-0 []
  (let [export-schema (json/decode (slurp (io/resource "flanders/ocsf-1.3.0-export.json")))
        export-json-schema {"objects" (into {} (map (fn [name]
                                                      (when (Thread/interrupted) (throw (InterruptedException.)))
                                                      [name (let [url (str "https://schema.ocsf.io/schema/objects/" name)]
                                                              (prn url)
                                                              (try (-> url client/get :body json/decode)
                                                                   (catch Exception e
                                                                     (prn url)
                                                                     (throw e))))]))
                                            (keys (get export-schema "objects")))
                            "base_event" (let [url "https://schema.ocsf.io/schema/classes/base_event"]
                                           (prn url)
                                           (try (-> url client/get :body json/decode)
                                                (catch Exception e
                                                  (prn url)
                                                  (throw e))))
                            "classes" (into {} (map (fn [name]
                                                      (when (Thread/interrupted) (throw (InterruptedException.)))
                                                      [name (let [url (str "https://schema.ocsf.io/schema/classes/" name)]
                                                              (prn url)
                                                              (try (-> url client/get :body json/decode)
                                                                   (catch Exception e
                                                                     (prn url)
                                                                     (throw e))))]))
                                            (keys (get export-schema "classes")))}]
    (spit "test-resources/flanders/ocsf-1.3.0-json-schema-export.json"
          (-> export-json-schema
              sort-recursive
              (json/encode {:pretty true})))))

(comment
  (gen-ocsf-json-schema-1-3-0)
  )

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
