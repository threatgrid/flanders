(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [flanders.core :as f]
            [schema-tools.walk :as stw]
            [schema.core :as s]
            [clojure.java.io :as io]
            [flanders.schema :as schema]
            [flanders.malli :as malli]
            [malli.core :as m]
            [flanders.json-schema :as sut]
            [flanders.json-schema.malli :as js->malli]
            [flanders.json-schema.schema :as js->schema]
            [cheshire.core :as json]
            flanders.json-schema.test-helpers-schema-security-finding
            ))

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

(def FlandersSecurityFinding (delay (sut/->flanders (security-finding-json) nil)))
(def SchemaSecurityFinding (delay (js->schema/->schema (security-finding-json) nil)))
(def MalliSecurityFinding (delay (js->malli/->malli (security-finding-json) nil)))

(def expected-schema-explain-SecurityFinding
  '{(optional-key :activity_name) Str,
    (optional-key :analytic) (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_analytic),
    (optional-key :api) (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_api),
    (optional-key :attacks) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_attack)],
    (optional-key :category_name) Str,
    (optional-key :cis_csc) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cis_csc)],
    (optional-key :class_name) Str,
    (optional-key :compliance) (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_compliance),
    (optional-key :confidence) Str,
    (optional-key :confidence_id) (enum 0 1 99 3 2),
    (optional-key :confidence_score) Int,
    (optional-key :count) Int,
    (optional-key :data_sources) [Str],
    (optional-key :duration) Int,
    (optional-key :end_time) Int,
    (optional-key :end_time_dt) Str,
    (optional-key :enrichments) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_enrichment)]
    (optional-key :evidence) Any,
    (optional-key :impact) Str,
    (optional-key :impact_id) (enum 0 1 4 99 3 2),
    (optional-key :impact_score) Int,
    (optional-key :kill_chain) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_kill_chain_phase)],
    (optional-key :malware) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_malware)],
    (optional-key :message) Str,
    (optional-key :nist) [Str],
    (optional-key :observables) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_observable)],
    (optional-key :process) (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_process),
    (optional-key :raw_data) Str,
    (optional-key :resources) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_resource_details)],
    (optional-key :risk_level) Str,
    (optional-key :risk_level_id) (enum 0 1 4 99 3 2),
    (optional-key :risk_score) Int,
    (optional-key :severity) Str,
    (optional-key :start_time) Int,
    (optional-key :start_time_dt) Str,
    (optional-key :state) Str,
    (optional-key :status) Str,
    (optional-key :status_code) Str,
    (optional-key :status_detail) Str,
    (optional-key :status_id) (enum 0 1 99 2),
    (optional-key :time_dt) Str,
    (optional-key :timezone_offset) Int,
    (optional-key :type_name) Str,
    (optional-key :unmapped) (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_object),
    (optional-key :vulnerabilities) [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_vulnerability)],
    :activity_id (enum 0 1 99 3 2),
    :category_uid Int,
    :class_uid Int,
    :cloud (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cloud),
    :finding (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_finding),
    :metadata (recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_metadata),
    :osint [(recursive #'https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_osint)],
    :severity_id (enum 0 1 4 99 6 3 2 5),
    :state_id (enum 0 1 4 99 3 2),
    :time Int,
    :type_uid Int})

(def expected-schema-SecurityFinding-defschemas
  '#{https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_account
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_affected_code
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_affected_package
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_agent
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_analytic
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_api
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_attack
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_autonomous_system
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cis_csc
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cloud
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_compliance
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_container
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cve
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cwe
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_data_classification
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_digital_signature
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_dns_answer
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_email
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_email_auth
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_enrichment
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_extension
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_file
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_finding
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_group
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_kb_article
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_kill_chain_phase
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_location
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_logger
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_malware
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_metadata
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_object
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_observable
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_organization
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_osint
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_package
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_process
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_product
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_related_event
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_remediation
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_reputation
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_request
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_resource_details
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_response
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_service
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_session
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_sub_technique
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_tactic
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_technique
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_user
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_vulnerability
     https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_whois})

(defn unqualify-vars [vs]
  (let [gs (group-by namespace (sort (map symbol vs)))
        pad (count (str (max 1 (dec (count gs)))))]
    (into (sorted-map)
          (map-indexed (fn [i [_ v]]
                         (into {} (map #(vector % (symbol (format (str "ns-%0" pad "d") i) (-> % symbol name))))
                               v)))
          (into (sorted-map) gs))))

(defn vars-from-distinct-namespaces [n]
  (let [vars-from-different-namespaces (set (take n (keep #(-> (ns-publics %) first second) (all-ns))))]
    (assert (= n (count vars-from-different-namespaces)))
    (assert (or (empty? vars-from-different-namespaces)
                (apply distinct? (map #(-> % symbol namespace) vars-from-different-namespaces))))
    (assert (every? var? vars-from-different-namespaces))
    vars-from-different-namespaces))

(defn syms-from-distinct-namespaces [n]
  (mapv #(symbol (str "sym" % (str (random-uuid))) (str (random-uuid))) (range n)))

(deftest unqualify-vars-test
  (is (= {} (unqualify-vars #{})))
  (is (= {`- 'ns-0/-} (unqualify-vars #{#'-})))
  (is (= {`+ 'ns-0/+
          `- 'ns-0/-
          `unqualify-vars 'ns-1/unqualify-vars}
         (unqualify-vars #{#'- #'+ #'unqualify-vars})))
  (is (= (into #{} (map #(symbol "ns-0" (-> % symbol name))) (vals (ns-publics 'clojure.core)))
         (set (vals (unqualify-vars (vals (ns-publics 'clojure.core)))))))
  (testing "pads namespaces"
    ;; we test with real vars for small numbers, since namespaces must be distinct
    (doseq [[n expected] {1 "ns-0"
                          9 "ns-0"
                          10 "ns-0"
                          11 "ns-00"
                          23 "ns-00"}]
      (testing n
        (is (= expected (first (sort (map namespace (vals (unqualify-vars (vars-from-distinct-namespaces n))))))))))
    (doseq [[n expected] {100 "ns-00"
                          101 "ns-000"
                          999 "ns-000"
                          1000 "ns-000"
                          1001 "ns-0000"}]
      (testing n
        (is (= expected (first (sort (map namespace (vals (unqualify-vars (syms-from-distinct-namespaces n))))))))))))

;; walks s/explain, schema walk might be more accurate
(defn collect-recursive-vars-from-schema [s]
  (let [vars (atom #{})
        ]
    (stw/postwalk (fn [s]
                    (when (instance? schema.core.Recursive s)
                      (swap! vars conj (:derefable s)))
                    s)
                  s)
    @vars))

(defn unqualify-recursive-vars-from-schema-explain [v]
  (let [vs (collect-recursive-vars-from-schema v)]
    (walk/postwalk (fn [v]
                     (if (and (seq? v)
                              (= 2 (count v))
                              (= 'var (first v))
                              (qualified-symbol? (second v)))
                       ;;unqualify recursive vars
                       (list 'var (-> v second name symbol))
                       v))
                   v)))

(declare BSchema)
(s/defschema ASchema [(s/recursive #'BSchema)])
(s/defschema BSchema [(s/recursive #'ASchema)])

(deftest collect-recursive-vars-from-schema-test
  (is (= #{} (collect-recursive-vars-from-schema [s/Any])))
  (is (= #{#'s/Any} (collect-recursive-vars-from-schema [(s/recursive #'s/Any)])))
  (is (= #{#'ASchema} (collect-recursive-vars-from-schema BSchema)))
  (is (= #{#'BSchema} (collect-recursive-vars-from-schema ASchema)))
  )

;; walks s/explain, schema walk might be more accurate
(defn collect-transitive-recursive-vars-from-schema [s]
  (loop [seen #{}
         remaining (collect-recursive-vars-from-schema s)]
    (if-some [v (first remaining)]
      (recur (cond-> seen
               (not (seen v)) (-> (conj v)
                                  (into (-> v deref collect-recursive-vars-from-schema))))
             (disj remaining v))
      seen)))

(defn pprint-reproducibly [v]
  (let [v (walk/postwalk (fn [v]
                           (cond
                             (s/optional-key? v) v
                             (s/required-key? v) v
                             (map? v) (into (sorted-map-by (fn [l r]
                                                             (cond
                                                               (and (seq? l) (seq? r)) (compare (vec l) (vec r))
                                                               (or (and (seq? l) (not (seqable? r)))
                                                                   (and (not (seqable? l)) (seq? r))) (compare (str (class l)) (str (class r)))
                                                               :else (compare l r))))
                                            v)
                             (set? v) (into (sorted-set) v)
                             :else v))
                         v)]
    (binding [*print-level* nil
              *print-length* nil
              *print-namespace-maps* false]
      (pp/pprint v))))

(defn generate-expected-schema-results [file nsym s]
  (let [f (io/file file)]
    (io/make-parents f)
    (spit f (with-out-str
              (println ";; generated by flanders.json-schema-test/generate-expected-schema-results")
              (pprint-reproducibly (list 'ns nsym))
              (pprint-reproducibly
                (list 'def (symbol "expected-schema-explain")
                      (list 'quote (unqualify-recursive-vars-from-schema-explain (s/explain s)))))
              (pprint-reproducibly
                (list 'def (symbol "expected-transitive-defschema-vars")
                      (list 'quote
                            (unqualify-vars (collect-transitive-recursive-vars-from-schema s)))))
              ))))

(comment
  (generate-expected-schema-results "test/flanders/json_schema/test_helpers_schema_security_finding.clj"
                                    'flanders.json-schema.test-helpers-schema-security-finding
                                    @SchemaSecurityFinding)
  )

(deftest collect-transitive-recursive-vars-from-schema-test
  (is (= #{} (collect-transitive-recursive-vars-from-schema [s/Any])))
  (is (= #{#'s/Any} (collect-transitive-recursive-vars-from-schema [(s/recursive #'s/Any)])))
  (is (= #{#'ASchema #'BSchema}
         (collect-transitive-recursive-vars-from-schema ASchema)
         (collect-transitive-recursive-vars-from-schema BSchema))))

(deftest ocsf-test
  (testing "OCSF security_finding class"
    (testing "s/explain for top-level JSON Schema converted to Plumatic Schema looks correct"
      (is (= flanders.json-schema.test-helpers-schema-security-finding/expected-schema-explain
             (unqualify-recursive-vars-from-schema-explain (s/explain @SchemaSecurityFinding)))))
    (testing "transitive defschema's for top-level JSON Schema look correct"
      (is (= flanders.json-schema.test-helpers-schema-security-finding/expected-transitive-defschema-vars
             (into #{} (map unqualify-var) (collect-transitive-recursive-vars-from-schema @SchemaSecurityFinding))))))
  (meta #'flanders.json-schema-test.security-finding/SecurityFinding)
  )
(comment
  (sort (keys (ns-publics (find-ns 'flanders.json-schema.schema.947b814e-26c7-42ce-9fce-a3cd691dfa70))))

  (deref (do #'flanders.json-schema.schema.947b814e-26c7-42ce-9fce-a3cd691dfa70/https_COLON__SLASH__SLASH_schema_DOT_ocsf_DOT_io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_cwe))
  )

(comment
  (binding [*print-level* nil
            *print-length* nil
            *print-namespace-maps* false]
    (spit "security-finding.edn" (with-out-str ((requiring-resolve 'clojure.pprint/pprint) (m/form (js->malli/->malli (security-finding-json) nil))))))
  (binding [*print-level* nil
            *print-length* nil
            *print-namespace-maps* false]
    (spit "schema-security-finding.clj" (with-out-str ((requiring-resolve 'clojure.pprint/pprint) (m/form (js->malli/->malli (security-finding-json) nil))))))
  )
