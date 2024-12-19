(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
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
  {"draft7" "http://json-schema.org/draft-07/schema#"})

(def dialect->dir
  {"draft7" "JSON-Schema-Test-Suite/tests/draft7"})

(def json-schema-test-suite
  {"draft7"
   {"additionalItems.json" {}
    "additionalProperties.json" {}
    "allOf.json" {}
    "anyOf.json" {}
    "boolean_schema.json" {:config {;; flanders has no opposite for f/any
                                    "boolean schema 'false'" :skip}}
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
    "infinite-loop-detection.json" {}
    "items.json" {}
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
    "ref.json" {}
    "refRemote.json" {}
    "required.json" {}
    "type.json" {}
    "uniqueItems.json" {}}})

(defn ->printable [data]
  (walk/postwalk
    (fn [data]
      (if (string? data)
        ;; replace non-printable
        (str/replace data #"\p{C}" "<NOTPRINTABLE>")
        data))
    data))

(deftest json-schema-test-suite-test
  (doseq [[short-dialect short-file->config] json-schema-test-suite
          [short-file config] short-file->config
          :let [dialect (or (short->dialect short-dialect)
                            (throw (ex-info (str "Unknown short dialect: " (pr-str short-dialect)) {})))
                dir (or (dialect->dir short-dialect)
                        (throw (ex-info (str "Unknown dialect dir: " (pr-str short-dialect)) {})))
                file (io/file dir short-file)
                suite (json/decode (slurp file))
                _ (assert (seq suite))]
          {:strs [description schema tests]} suite
          :let [description (str/trim description)
                config (get config description)]
          :when (not= :skip config)]
    (testing (str short-dialect "\n" short-file "\n" "Test suite: " description "\n")
      (assert (seq tests))
      (doseq [{:strs [description data valid]} tests
              :let [description (str/trim description)
                    config (get config description)]
              :when (not= :skip config)
              backend [:malli #_:schema]]
        (let [skip? (or (str/includes? description "float with zero")
                        (str/includes? description ".0")
                        (and (map? schema)
                             (when-some [[_ const] (find schema "const")]
                               (or (not (integer? const))
                                   (not (string? const))))))]
          (when-not skip?
            (testing (str description "\n"
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
                                 (is (= valid (try (m/coerce m data)
                                                   true
                                                   (catch Exception _ false)))
                                     (pr-str (m/form m)))))
                      ;; print testing string on error
                      true)))))))))

(comment
  (clojure.test/test-vars [#'json-schema-test-suite-test])
  )

(deftest const-test
  (is (m/validate (->malli {"const" 9007199254740992}) 9007199254740992)))

(deftest additionalProperties-test
  (testing "true => open"
    ;;TODO :map
    (is (= [:map {:closed true} [:malli.core/default [:map-of :any :any]]]
           (m/form (->malli {"additionalProperties" true} {:flanders.malli/no-example true}))))
    (is (= '{Any Any}
           (s/explain (->schema {"additionalProperties" true})))))
  (testing "false => closed"
    (is (= [:map {:closed true}]
           (m/form (->malli {"additionalProperties" false} {:flanders.malli/no-example true}))))
    (is (= {}
           (s/explain (->schema {"additionalProperties" false})))))
  (testing "additionalProperties ignores applicators"
    ;;TODO [:map-of :boolean :any]
    (is (= [:map {:closed true} [:malli.core/default [:map-of :any :boolean]]]
           (m/form (->malli {"allOf" [{"properties" {"foo" {}}}], "additionalProperties" {"type" "boolean"}}
                            {:flanders.malli/no-example true}))))
    (is (= '{Bool Any}
           (s/explain (->schema {"allOf" [{"properties" {"foo" {}}}], "additionalProperties" {"type" "boolean"}}))))))

(comment
  (keys (:flanders.json-schema/defs-scope @FlandersSecurityFinding))
  )
