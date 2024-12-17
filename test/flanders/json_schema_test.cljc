(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [flanders.core :as f]
            [flanders.example :as fe]
            [flanders.json-schema :as sut]
            [flanders.malli :as malli]
            [flanders.schema :as schema]
            [malli.core :as m]
            [malli.generator :as mg]
            [schema-tools.walk :as stw]
            [schema.core :as s]
            flanders.json-schema.test-helpers-schema-security-finding))

;; helper functions

(defn ->malli [v] (malli/->malli (sut/->flanders v nil) nil))
(defn ->schema [v] (#?(:clj schema/->schema+clean :default schema/->schema) (sut/->flanders v nil) nil))

;; examples

(def union-example
  {:$id "something"
   :title "union", ;;TODO
   :type "object",
   :properties (sorted-map
                 :x {:anyOf [{:type "integer"} {:type "string"}]}
                 :y {:type "integer"}),
   :required [:x :y]})

(def security-finding-json (delay (json/decode (slurp (io/resource "security-finding.json")))))
(def FlandersSecurityFinding (delay (sut/->flanders @security-finding-json nil)))
(def SchemaSecurityFinding (delay (->schema @security-finding-json)))
(def MalliSecurityFinding (delay (->malli @security-finding-json)))

;; tests

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
  (is (= (s/explain (->schema union-example))
         '{:x (cond-pre Int Str), :y Int})))

(comment
  (generate-expected-schema-results "test/flanders/json_schema/test_helpers_schema_security_finding.clj"
                                    'flanders.json-schema.test-helpers-schema-security-finding
                                    @SchemaSecurityFinding)
  )


(def example-security-finding
  {:severity_id 0,
   :category_uid 10,
   :status_id 0,
   :data_sources ["string"],
   :api {:operation "foo"} ,
   :unmapped {} ,
   :class_name "string",
   :osint [],
   :type_uid 10,
   :start_time_dt "string",
   :impact_score 10,
   :impact_id 0,
   :enrichments [],
   :nist ["string"],
   :cloud {:provider "foo"} ,
   :time 10,
   :evidence "anything",
   :process {} ,
   :start_time 10,
   :risk_level "string",
   :observables [],
   :risk_score 10,
   :time_dt "string",
   :duration 10,
   :state "string",
   :class_uid 10,
   :kill_chain [],
   :end_time 10,
   :category_name "string",
   :analytic {:type_id 0} ,
   :activity_name "string",
   :confidence_score 10,
   :timezone_offset 10,
   :status "string",
   :count 10,
   :severity "string",
   :cis_csc [],
   :status_detail "string",
   :end_time_dt "string",
   :status_code "string",
   :attacks [],
   :finding {:title "foo" :uid "foo"} ,
   :raw_data "string",
   :confidence "string",
   :activity_id 0,
   :resources [],
   :type_name "string",
   :confidence_id 0,
   :impact "string",
   :metadata {:product {:vendor_name "asdf"} :version "foo"} ,
   :compliance {:standards []},
   :message "string",
   :vulnerabilities [],
   :malware [],
   :state_id 0,
   :risk_level_id 0})

(deftest ocsf-test
  (testing "OCSF security_finding class"
    (testing "s/explain for top-level JSON Schema converted to Plumatic Schema looks correct"
      (is (= flanders.json-schema.test-helpers-schema-security-finding/expected-schema-explain
             (unqualify-recursive-vars-from-schema-explain @SchemaSecurityFinding))))
    (testing "transitive defschema's for top-level JSON Schema look correct"
      (is (= (set (keys flanders.json-schema.test-helpers-schema-security-finding/expected-transitive-schema-explains))
             (set (vals (unqualify-vars (collect-transitive-recursive-vars-from-schema @SchemaSecurityFinding))))))
      (is (= flanders.json-schema.test-helpers-schema-security-finding/expected-transitive-schema-explains
             (into {} (map (fn [[v uniq]]
                             {uniq (unqualify-recursive-vars-from-schema-explain @(find-var v))}))
                   (unqualify-vars (collect-transitive-recursive-vars-from-schema @SchemaSecurityFinding)))))))
  (testing "schema ops"
    (is (some? (s/check @SchemaSecurityFinding {})))
    (is (s/validate @SchemaSecurityFinding example-security-finding))
    )
  (testing "malli ops"
    (is (some? (m/explain @MalliSecurityFinding {})))
    ;;FIXME
    #_
    (is (m/validate @MalliSecurityFinding example-security-finding))
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

(defn generate-example-malli [file json]
  (let [f (io/file file)]
    (io/make-parents f)
    (spit f (binding [*print-level* nil
                      *print-length* nil
                      *print-namespace-maps* false]
              (with-out-str (pp/pprint (m/form (->malli json nil))))))))

(comment
  (generate-example-malli "security-finding.edn" @security-finding-json)
  (keys (:flanders.json-schema/defs-scope @FlandersSecurityFinding))
  )
