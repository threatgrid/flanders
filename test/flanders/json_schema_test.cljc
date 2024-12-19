(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
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

(set! *warn-on-reflection* true)

;; examples

(def security-finding-json (delay (json/decode (slurp (io/resource "security-finding.json")))))
(def FlandersSecurityFinding (delay (sut/->flanders @security-finding-json nil)))
(def example-SecurityFinding (delay (fe/->example-tree @FlandersSecurityFinding)))
(def SchemaSecurityFinding (delay (->schema @security-finding-json)))
(def MalliSecurityFinding (delay (->malli @security-finding-json)))
(def MalliSecurityFinding-no-example (delay (->malli @security-finding-json {:flanders.malli/no-example true})))

;; tests

(deftest ->flanders-test
  (is (= (m/form (->malli th/union-example {:flanders.malli/no-example true}))
         [:map
          [:x
           [:or :int :string]]
          [:y :int]]))
  (is (= (s/explain (->schema th/union-example))
         '{:x (cond-pre Int Str), :y Int, Any Any})))

(comment
  (th/generate-expected-schema-results "test/flanders/json_schema/test_helpers_schema_security_finding.clj"
                                       'flanders.json-schema.test-helpers-schema-security-finding
                                       @SchemaSecurityFinding)
  (spit "test-resources/expected-example-SecurityFinding.edn" (with-out-str
                                                                (println ";;Generated and tested by flanders.json-schema-test")
                                                                (println ";;The expected result of flanders.example on OCSF security_finding class")
                                                                (th/pprint-reproducibly @example-SecurityFinding)))

  (spit "test-resources/expected-malli-SecurityFinding.edn"
        (with-out-str
          (println ";;Generated and tested by flanders.json-schema-test")
          (println ";;The expected result of converting OCSF security_finding class with examples")
          (th/pprint-reproducibly (m/form @MalliSecurityFinding))))

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
    (is (nil? (s/check @SchemaSecurityFinding th/example-security-finding))))
  (testing "malli ops"
    (is (= (edn/read-string (slurp "test-resources/expected-example-SecurityFinding.edn"))
           @example-SecurityFinding))
    (is (= (edn/read-string (slurp "test-resources/expected-malli-SecurityFinding-no-example.edn"))
           (m/form @MalliSecurityFinding-no-example)))
    (is (m/validate @MalliSecurityFinding-no-example @example-SecurityFinding))
    (is (m/validate @MalliSecurityFinding-no-example th/example-security-finding))
    (is (= [{:path [:flanders.json-schema-test/extra],
             :in [:flanders.json-schema-test/extra],
             :value true,
             :type :malli.core/extra-key}]
           (->> (m/explain @MalliSecurityFinding-no-example (assoc @example-SecurityFinding ::extra true))
                :errors
                (mapv #(dissoc % :schema)))))
    (is (= (edn/read-string (slurp "test-resources/expected-malli-SecurityFinding.edn"))
           (m/form @MalliSecurityFinding)))
    (is (= [{:path [:flanders.json-schema-test/extra],
             :in [:flanders.json-schema-test/extra],
             :value true,
             :type :malli.core/extra-key}]
           (->> (m/explain @MalliSecurityFinding (assoc @example-SecurityFinding ::extra true))
                :errors
                (mapv #(dissoc % :schema)))))
    (is (m/validate @MalliSecurityFinding th/example-security-finding))
    (is (= {:severity_id 0,
            :osint
            [{:type_id 8,
              :email
              {:cc [],
               :delivered_to "",
               :from "k",
               :reply_to "",
               :size -1,
               :to [],
               :uid "V",
               :x_originating_ip []},
              :autonomous_system {:name "m", :number 0},
              :uid "3",
              :value "h",
              :type "",
              :kill_chain [],
              :signatures [],
              :attacks
              [{:tactic {:name "d", :src_url "E"}, :tactics [], :version ""}],
              :vendor_name "x",
              :location
              {:desc "",
               :geohash "2",
               :isp "V",
               :long -1,
               :postal_code "",
               :provider ""},
              :confidence_id 2,
              :email_auth
              {:dkim "", :dkim_domain "", :dmarc "", :dmarc_override ""},
              :vulnerabilities []}]}
           (select-keys (mg/generate @MalliSecurityFinding-no-example {:seed 0 :size 1})
                        [:severity_id :osint])))
    (is (m/validate @MalliSecurityFinding-no-example (mg/generate @MalliSecurityFinding-no-example {:seed 0 :size 1})))
    (is (m/validate @MalliSecurityFinding (mg/generate @MalliSecurityFinding {:seed 0 :size 1})))))

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

(def short->dialect
  {"draft7" "http://json-schema.org/draft-07/schema#"
   "draft2020-12" "https://json-schema.org/draft/2020-12/schema"})

(def dialect->dir
  {"draft7" "JSON-Schema-Test-Suite/tests/draft7"
   "draft2020-12" "JSON-Schema-Test-Suite/tests/draft2020-12"})

;; note: this test suite rarely declares "type", which results in very broad schemas that flanders can't represent.
;; https://github.com/json-schema/json-schema/issues/172
;; we filter out such test cases
(def json-schema-test-suite
  {"draft7"
   {"additionalItems.json" {}
    "additionalProperties.json" {}
    "allOf.json" {}
    "anyOf.json" {}
    "boolean_schema.json" {}
    "const.json" {:config {;; not sure
                           "float and integers are equal up to 64-bit representation limits" :skip
                           "const with -2.0 matches integer and float types" :skip
                           "const with 1 does not match true" {"float one is valid" :skip}
                           "const with 0 does not match other zero-like types" {"float zero is valid" :skip}}}
    "contains.json" {}
    "default.json" {}
    "definitions.json" {}
    "dependencies.json" {}
    "enum.json" {}
    "exclusiveMaximum.json" {}
    "exclusiveMinimum.json" {}
    "format.json" {}
    "if-then-else.json" {}
    ;;TODO default $id
    "infinite-loop-detection.json" :skip
    "items.json" {"a schema given for items"
                  {"JavaScript pseudo-array is valid" :skip}}
    "maxItems.json" {}
    "maxLength.json" {}
    "maxProperties.json" {}
    "maximum.json" {}
    "minItems.json" {}
    "minLength.json" {}
    "minProperties.json" {}
    "minimum.json" {}
    "multipleOf.json" {}
    "not.json" {}
    "oneOf.json" {}
    "pattern.json" {}
    "patternProperties.json" {}
    "properties.json" {}
    "propertyNames.json" {}
    ;:TODO
    "ref.json" :skip
    "refRemote.json" :skip
    "required.json" {"required properties whose names are Javascript object property names"
                     ;; not sure what's going on here. guessing all JS object have these properties.
                     {"ignores arrays" :skip
                      "ignores non-arrays" :skip
                      "ignores other non-objects" :skip}}
    "type.json" {}
    "uniqueItems.json" {}}
   "draft2020-12"
   {"additionalProperties.json" {}
    "allOf.json" {}
    "anchor.json" {}
    "anyOf.json" {}
    "boolean_schema.json" {}
    "const.json" {}
    "contains.json" {}
    "content.json" {}
    "default.json" {}
    "defs.json" {}
    "dependentRequired.json" {}
    "dependentSchemas.json" {}
    "dynamicRef.json" :skip
    "enum.json" {}
    "exclusiveMaximum.json" {}
    "exclusiveMinimum.json" {}
    "format.json" {}
    "if-then-else.json" {}
    "infinite-loop-detection.json" {}
    "items.json" {}
    "maxContains.json" {}
    "maxItems.json" {}
    "maxLength.json" {}
    "maxProperties.json" {}
    "maximum.json" {}
    "minContains.json" {}
    "minItems.json" {}
    "minLength.json" {}
    "minProperties.json" {}
    "minimum.json" {}
    "multipleOf.json" {}
    "not.json" {}
    "oneOf.json" {}
    "pattern.json" {}
    "patternProperties.json" {}
    "prefixItems.json" {}
    "properties.json" {}
    "propertyNames.json" {}
    "ref.json" :skip
    "refRemote.json" :skip
    "required.json" {}
    "type.json" {}
    "unevaluatedItems.json" :skip
    "unevaluatedProperties.json" :skip
    "uniqueItems.json" {}
    "vocabulary.json" {}}})

(defn ->printable [data]
  (walk/postwalk
    (fn [data]
      (if (string? data)
        ;; replace non-printable
        (str/replace data #"\p{C}" "<NOTPRINTABLE>")
        data))
    data))

(deftest json-schema-test-suite-test
  (let [ntested (atom 0)]
    (doseq [[short-dialect short-file->config] json-schema-test-suite
            [short-file suite-description->test-description->config] short-file->config
            :when (not= :skip suite-description->test-description->config)
            :let [dialect (or (short->dialect short-dialect)
                              (throw (ex-info (str "Unknown short dialect: " (pr-str short-dialect)) {})))
                  dir (or (dialect->dir short-dialect)
                          (throw (ex-info (str "Unknown dialect dir: " (pr-str short-dialect)) {})))
                  file (io/file dir short-file)
                  suite (json/decode (slurp file))
                  _ (assert (seq suite))]
            {suite-description "description" :strs [schema tests]} suite
            :let [_ (assert (seq tests))
                  test-description->config (get suite-description->test-description->config suite-description)
                  suite-description (str/trim suite-description)
                  skip? (or (= :skip test-description->config)
                            (str/includes? suite-description "$ref")
                            (str/includes? suite-description "$id")
                            (str/includes? suite-description "$defs")
                            (str/includes? suite-description "nul")
                            (str/includes? suite-description "metaschema"))]
            :when (not skip?)]
      (testing (str short-dialect "\n" short-file "\n" "Test suite: " suite-description "\n")
        (doseq [{test-description "description" :strs [data valid]} tests
                backend [:malli #_:schema]
                :let [;; we generate schemas with keyword keys so we need to coerce input
                      data (walk/keywordize-keys data)
                      test-description (str/trim test-description)
                      config (get test-description->config test-description)
                      skip? (or (= :skip config)
                                (str/includes? test-description "float")
                                (str/includes? test-description ".0")
                                (str/includes? test-description "$ref")
                                (str/includes? test-description "$id")
                                (str/includes? test-description "$defs")
                                (str/includes? test-description "definitions")
                                (str/includes? test-description "nul")
                                (str/includes? test-description "metaschema")
                                ;; https://github.com/json-schema/json-schema/issues/172
                                ;; most of these tests rely on omitting "type" for very broad schemas we don't support
                                (str/includes? test-description "ignores")
                                (str/includes? test-description "JavaScript pseudo-array")
                                (and (map? schema)
                                     (when-some [[_ const] (find schema "const")]
                                       (or (not (integer? const))
                                           (not (string? const))))))]
                :when (not skip?)]
          (testing (str test-description "\n"
                        "JSON Schema: "
                        (pr-str (->printable schema))
                        "\n"
                        "Input: "
                        (pr-str (->printable data)))
            (is (do (case backend
                      :malli (when-some [m (try (->malli schema {:flanders.malli/no-example true
                                                                 ::sut/dialect dialect})
                                                (catch Exception e
                                                  (when-not (::sut/unsupported (ex-data e))
                                                    (throw e))))]
                               (swap! ntested inc)
                               (is (= valid (try (m/coerce m data)
                                                 true
                                                 (catch Exception _ false)))
                                   (pr-str (m/form m)))))
                    ;; print testing string on error
                    true))))))
    (is (= 152 @ntested))))

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

(comment
  (clojure.test/test-vars [#'json-schema-test-suite-test])
  )
