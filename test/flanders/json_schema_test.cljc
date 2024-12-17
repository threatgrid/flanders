(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is testing]]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]
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

(def union-example
  {:$id "something"
   :title "union", ;;TODO
   :type "object",
   :properties (sorted-map
                 :x {:anyOf [{:type "integer"} {:type "string"}]}
                 :y {:type "integer"}),
   :required [:x :y]})

(defn ->malli [v] (malli/->malli (sut/->flanders v nil) nil))
(defn ->schema [v] (#?(:clj schema/->schema :cljs schema/->schema+clean) (sut/->flanders v nil) nil))

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
         )))

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

(def security-finding-json (delay (json/decode (slurp (io/resource "security-finding.json")))))
(def FlandersSecurityFinding (delay (sut/->flanders @security-finding-json nil)))
(def SchemaSecurityFinding (delay (->schema @security-finding-json nil)))
(def MalliSecurityFinding (delay (->malli @security-finding-json nil)))

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
  (let [vars (atom #{})]
    (stw/postwalk (fn [s]
                    (when (instance? schema.core.Recursive s)
                      (swap! vars conj (:derefable s)))
                    s)
                  s)
    @vars))

(defn unqualify-recursive-vars-from-schema-explain [s]
  (let [vs (collect-recursive-vars-from-schema s)
        rename (unqualify-vars vs)]
    (walk/postwalk (fn [v]
                     (if (and (seq? v)
                              (= 2 (count v))
                              (= 'var (first v))
                              (qualified-symbol? (second v)))
                       ;;unqualify recursive vars
                       (let [vsym (second v)]
                         (list 'var (or (rename vsym) (throw (ex-info (str "Unknown var " vsym) {:rename rename})))))
                       v))
                   (s/explain s))))

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
                      (list 'quote
                            (unqualify-recursive-vars-from-schema-explain s))))
              (let [transitive-var->uniquified (unqualify-vars (collect-transitive-recursive-vars-from-schema s))]
                (pprint-reproducibly
                  (list 'def (symbol "expected-transitive-defschema-vars")
                        (list 'quote
                              (set (vals transitive-var->uniquified)))))
                (pprint-reproducibly
                  (list 'def 'transitive-schema-atom `(atom (sorted-map))))
                (doseq [[v uniq] transitive-var->uniquified]
                  (pprint-reproducibly
                    `(swap! ~'transitive-schema-atom assoc '~uniq
                            '~(unqualify-recursive-vars-from-schema-explain @(find-var v)))))
                (pprint-reproducibly
                  (list 'def 'expected-transitive-schema-explains
                        (list `deref 'transitive-schema-atom)))
                )
              ))
    (require nsym :reload)))

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
  (is (= #{"https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"}
         (-> (sut/->flanders refs-json-schema-example nil)
             )))
  (is (= '[:ref
           {:registry
            {"https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"
             [:sequential
              [:ref
               #:json-schema{:example nil}
               "https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"]]}
            :json-schema/example [[]]}
           "https://schema.ocsf.io/schema/classes/security_finding/$defs/aref"]
         (m/form (->malli refs-json-schema-example))))
  (is (= [] (fe/->example-tree (sut/->flanders refs-json-schema-example nil)))))

(comment
  (sut/->flanders refs-json-schema-example nil)
  (->malli refs-json-schema-example)
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
