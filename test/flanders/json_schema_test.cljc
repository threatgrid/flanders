(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [cheshire.core :as json]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [flanders.example :as fe]
            [flanders.core :as f]
            [flanders.json-schema :as sut]
            [flanders.json-schema.test-helpers :as th :refer [->malli ->schema]]
            [malli.core :as m]
            [malli.generator :as mg]
            [schema.core :as s]
            flanders.json-schema.test-helpers-schema-security-finding))

;; examples

(def security-finding-json (delay (json/decode (slurp (io/resource "security-finding.json")))))
(def FlandersSecurityFinding (delay (sut/->flanders @security-finding-json nil)))
(def example-SecurityFinding (delay (fe/->example-tree @FlandersSecurityFinding)))
(def SchemaSecurityFinding (delay (->schema @security-finding-json)))
(def MalliSecurityFinding (delay (->malli @security-finding-json)))
(def MalliSecurityFinding-no-example (delay (->malli @security-finding-json {:flanders.malli/no-example true})))

;; tests

(deftest ->flanders-test
  (is (= (m/form (->malli th/union-example))
         [:map
          {:closed true, :json-schema/example {:x 10, :y 10}}
          [:x
           #:json-schema{:example 10}
           [:or
            [:int #:json-schema{:example 10}]
            [:string #:json-schema{:example "string"}]]]
          [:y #:json-schema{:example 10} [:int #:json-schema{:example 10}]]]))
  (is (= (s/explain (->schema th/union-example))
         '{:x (cond-pre Int Str), :y Int})))

(comment
  (th/generate-expected-schema-results "test/flanders/json_schema/test_helpers_schema_security_finding.clj"
                                       'flanders.json-schema.test-helpers-schema-security-finding
                                       @SchemaSecurityFinding)
  (spit "test-resources/expected-example-SecurityFinding.edn" (with-out-str
                                                                (println ";;Generated and tested by flanders.json-schema-test")
                                                                (println ";;The expected result of flanders.example on OCSF security_finding class")
                                                                (th/pprint-reproducibly @example-SecurityFinding)))

  (spit "test-resources/expected-malli-SecurityFinding-no-example.edn"
        (with-out-str
          (println ";;Generated and tested by flanders.json-schema-test")
          (println ";;The expected result of converting OCSF security_finding class without attaching examples")
          (th/pprint-reproducibly (m/form @MalliSecurityFinding-no-example))))
  
  )

(deftest ocsf-test
  (testing "OCSF security_finding class"
    (testing "s/explain for top-level JSON Schema converted to Plumatic Schema looks correct"
      (is (= flanders.json-schema.test-helpers-schema-security-finding/expected-schema-explain
             (th/unqualify-recursive-vars-from-schema-explain @SchemaSecurityFinding))))
    (testing "transitive defschema's for top-level JSON Schema look correct"
      (is (= (set (keys flanders.json-schema.test-helpers-schema-security-finding/expected-transitive-schema-explains))
             (set (vals (th/unqualify-vars (th/collect-transitive-recursive-vars-from-schema @SchemaSecurityFinding))))))
      (is (= flanders.json-schema.test-helpers-schema-security-finding/expected-transitive-schema-explains
             (into {} (map (fn [[v uniq]]
                             {uniq (th/unqualify-recursive-vars-from-schema-explain @(find-var v))}))
                   (th/unqualify-vars (th/collect-transitive-recursive-vars-from-schema @SchemaSecurityFinding)))))))
  (testing "schema ops"
    (is (some? (s/check @SchemaSecurityFinding {})))
    (is (s/validate @SchemaSecurityFinding th/example-security-finding))
    )
  (testing "malli ops"
    (is (= (edn/read-string (slurp "test-resources/expected-example-SecurityFinding.edn"))
           @example-SecurityFinding))
    (is (= (edn/read-string (slurp "test-resources/expected-malli-SecurityFinding-no-example.edn"))
           (m/form @MalliSecurityFinding-no-example)))
    (is (m/validate @MalliSecurityFinding-no-example @example-SecurityFinding))
    (is (= [{:path [:flanders.json-schema-test/extra],
             :in [:flanders.json-schema-test/extra],
             :value true,
             :type :malli.core/extra-key}]
           (->> (m/explain @MalliSecurityFinding-no-example (assoc @example-SecurityFinding ::extra true))
                :errors
                (mapv #(dissoc % :schema)))))
    ;;FIXME OOM
    ;(is (= ::FIXME (mg/generate @MalliSecurityFinding-no-example)))
    (is (= ::FIXME (m/form @MalliSecurityFinding)))
    (is (= ::FIXME (m/explain @MalliSecurityFinding {})))
    ;;FIXME
    #_
    (is (m/validate @MalliSecurityFinding th/example-security-finding))
    ;;FIXME
    #_
    (is (mg/generate @MalliSecurityFinding {:seed 0}))
    )
  )

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
  (is (m/validate (->malli refs-json-schema-example) []))
  )

(comment
  (sut/->flanders refs-json-schema-example nil)
  (->malli refs-json-schema-example)
  (->schema refs-json-schema-example)
  )

(comment
  (generate-example-malli "security-finding.edn" @security-finding-json)
  (keys (:flanders.json-schema/defs-scope @FlandersSecurityFinding))
  )
