(ns flanders.schema-test
  {:clj-kondo/ignore true}
  (:require
   [clojure.test :refer [deftest is testing]]
   [flanders.core :as f]
   [flanders.examples :as fes :refer [Example OptionalKeywordMapEntryExample]]
   [flanders.schema :as fs]
   [ring.swagger.json-schema :as js]
   [flanders.json-schema.test-helpers :refer [unqualify-recursive-vars-from-schema-explain explain-transitive-schema]]
   [schema.core :as s]
   [schema-tools.core :as st]))

(deftest test-valid-schema
  (is
   (s/validate
    (fs/->schema Example)
    {:foo "foo"
     :bar {"integer" 100
           :seq [:a :b :c]
           :set #{1 3}}
     :yes? true
     :spam :eggs})))

(deftest test-optional-kw-map-entry
  (let [expected-schema
        {#schema.core.OptionalKey{:k :foo} (js/field java.lang.String {})
         :relation_info {s/Keyword s/Any}}]
    (is (= expected-schema
           (fs/->schema OptionalKeywordMapEntryExample)))))

(deftest signature-type->schema
  (is (instance? schema.core.FnSchema
                 (fs/->schema (f/sig :parameters []))))

  (is (instance? schema.core.AnythingSchema
                 (:output-schema (fs/->schema (f/sig :parameters [])))))

  (let [return (f/int)]
    (is (instance? (class (fs/->schema return))
                   (:output-schema (fs/->schema (f/sig :parameters []
                                                       :return return))))))

  (let [a (f/int)
        b (f/int)]
    (is (= 1
           (count (:input-schemas (fs/->schema (f/sig :parameters [a b])))))
        "A signature should produce only 1 input-schema"))

  (let [a (f/int)
        b (f/int)]
    (is (= [(fs/->schema a) (fs/->schema b)]
           (first (:input-schemas (fs/->schema (f/sig :parameters [a b])))))))

  (let [a (f/int)
        b (f/int)
        c (f/str)]
    (is (= [(fs/->schema a) (fs/->schema b) [(fs/->schema c)]]
           (first (:input-schemas (fs/->schema (f/sig :parameters [a b]
                                                      :rest-parameter c))))))))

(deftest bool-test
  (is (= (js/field Boolean {})
         (fs/->schema (f/bool))))
  (is (= (s/enum true)
         (fs/->schema (f/bool :equals true))))
  (is (= (s/enum false)
         (fs/->schema (f/bool :equals false)))))

(defn- ->swagger [dll] (:json-schema (meta (fs/->schema dll))))
(def Time
  (f/inst :description (str "Schema definition for all date or timestamp values.  "
                            "Serialized as a string, the field should follow the "
                            "rules of the [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) "
                            "standard.")
          :name "ISO8601 Timestamp"))
(def URI (f/str :description "A URI"
                :default "https://example.com"))
(def high-med-low
  #{"Info"
    "Low"
    "Medium"
    "High"
    "None"
    "Unknown"})
(f/def-enum-type HighMedLow
  high-med-low
  :reference (str "[HighMedLowVocab](http://stixproject.github.io/"
                  "data-model/1.2/stixVocabs/HighMediumLowVocab-1.0/)"))
(f/def-map-type RelatedIdentity
  (concat
   (f/required-entries
    (f/entry :identity URI ;; Should this be a Reference or a URI?
             :description "The reference (URI) of the related Identity object."))
   (f/optional-entries
    (f/entry :confidence HighMedLow
             :description (str "Specifies the level of confidence in the assertion "
                               "of the relationship between the two objects."))
    (f/entry :information_source f/any-str
             :default "MapEntry default for information_source"
             :description (str "Specifies the source of the information about "
                               "the relationship between the two components."))
    (f/entry :relationship f/any-str)))
  :description "Describes a related Identity"
  :reference "[RelatedIdentityType](http://stixproject.github.io/data-model/1.2/stixCommon/RelatedIdentityType/)")

(deftest swagger-test
  (is (= {:example false} (->swagger (f/bool :equals false))))
  (is (= {:example true :description "Foo"} (->swagger (f/bool :equals true :description "Foo"))))
  (is (= {:example 10 :description "foo"} (->swagger (f/int :description "foo"))))
  (is (= {:example 9 :description "foo"} (->swagger (f/int :default 9 :description "foo"))))
  (is (= {:example 10 :description "outer"}
         (->swagger (f/either :description "outer"
                              :tests [(constantly true)
                                      (constantly true)]
                              :choices [(f/int :description "inner")
                                        (f/bool :equals true :description "Foo")]))))
  (is (= {:example {} :description "Description"} (->swagger (deref (f/def-entity-type Bar {:description "Description"})))))
  (is (= {:example {:start_time #inst "2016-01-01T01:01:01.000-00:00"
                    :related_identities [{:identity "https://example.com"
                                          :confidence "High"
                                          :information_source "MapEntry default for information_source"
                                          :relationship "string"}]}
          :description "Period of time when a cyber observation is valid. `start_time` must come before `end_time` (if specified)."}
         (->swagger (deref (f/def-map-type Bar
                             [(f/entry :start_time Time
                                       :description (str "Time of the observation. If the observation was "
                                                         "made over a period of time, than this field "
                                                         "indicates the start of that period."))
                              (f/entry :related_identities [RelatedIdentity]
                                       :description (str "Identifies other entity Identities related to "
                                                         "this Identity."))]
                             :description (str "Period of time when a cyber observation is valid. "
                                               "`start_time` must come before `end_time` (if specified).")
                             :reference "[ValidTimeType](http://stixproject.github.io/data-model/1.2/indicator/ValidTimeType/)")))))
  (testing "Description on map entry overrides value description"
    (is (= {:start_time {:example #inst "2525-01-01T01:01:01.000-00:00", :description "Time of the observation. If the observation was made over a period of time, than this field indicates the start of that period.", :type "string", :format "date-time"}
            :related_identities {:example [{:identity "https://example.com", :confidence "High", :information_source "MapEntry default for information_source", :relationship "string"}], :description "Identifies other entity Identities related to this Identity.", :type "array", :items {:$ref "#/definitions/RelatedIdentity"}}}
           (js/properties
             (fs/->schema (deref (f/def-map-type Bar
                                   [(f/entry :start_time Time
                                             :default #inst "2525-01-01T01:01:01.000-00:00" 
                                             :description (str "Time of the observation. If the observation was "
                                                               "made over a period of time, than this field "
                                                               "indicates the start of that period."))
                                    (f/entry :related_identities [RelatedIdentity]
                                             :description (str "Identifies other entity Identities related to "
                                                               "this Identity."))]
                                   :description (str "Period of time when a cyber observation is valid. "
                                                     "`start_time` must come before `end_time` (if specified).")
                                   :reference "[ValidTimeType](http://stixproject.github.io/data-model/1.2/indicator/ValidTimeType/)")))))))
  (is (= {:example #inst "2016-01-01T01:01:01.000-00:00"
          :description "Schema definition for all date or timestamp values.  Serialized as a string, the field should follow the rules of the [ISO8601](https://en.wikipedia.org/wiki/ISO_8601) standard."}
         (->swagger Time)))
  (is (= {:example [{:identity "https://example.com" , :confidence "High", :information_source "MapEntry default for information_source", :relationship "string"}]
          :description "Period of time when a cyber observation is valid. `start_time` must come before `end_time` (if specified)."}
         (->swagger (f/seq-of RelatedIdentity :description (str "Period of time when a cyber observation is valid. "
                                                                "`start_time` must come before `end_time` (if specified).")
                              :reference "[ValidTimeType](http://stixproject.github.io/data-model/1.2/indicator/ValidTimeType/)"))))
  (is (= {:example "anything" :description "AnYtHiNg"} (->swagger (assoc f/any :description "AnYtHiNg"))))
  (is (= {:example :keyword :description "Kw"} (->swagger (assoc f/any-keyword :description "Kw"))))
  (is (= {:example "string" :description "Str"} (->swagger (assoc f/any-str :description "Str"))))
  (is (= {:example "a" :description "Str"} (->swagger (f/enum #{"b" "c" "a"} :description "Str"))))
  (is (= {:example "default" :description "Str"} (->swagger (assoc f/any-str :description "Str" :default "default")))))

(deftest ref-form-test
  (is (= s/Int (-> fes/RefExample fs/->schema)))
  (is (= '{:schema (recursive #'ns-0/foo),
           :vars {ns-0/foo [(recursive #'ns-0/foo)]}}
         (-> fes/RecursiveRefExample
             fs/->schema
             explain-transitive-schema)))
  (is (= (s/enum 42) (-> fes/ShadowingRefExample fs/->schema)))
  (is (= (s/enum 42) (-> fes/ShadowingMultiRefExample fs/->schema)))
  ;;FIXME
  (is (= '{:schema (cond-pre (cond-pre (cond-pre (recursive #'ns-0/a) Bool) Int)),
           :vars {ns-0/a (cond-pre (recursive #'ns-0/a) Bool)}}
         (-> fes/InnerRecursionRefExample
             fs/->schema
             explain-transitive-schema)))
  (is (thrown-with-msg? Exception
                        #"Ref not in scope: \"a\""
                        (-> fes/UnscopedRefExample fs/->schema)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InfiniteRefExample fs/->schema))))

(deftest ref-validation-test
  (is (nil? (s/check (fs/->schema fes/RecursiveRefExample) [])))
  (is (nil? (s/check (fs/->schema fes/RecursiveRefExample) nil)))
  (is (= "(not (sequential? 1))" (pr-str (s/check (fs/->schema fes/RecursiveRefExample) 1))))
  (is (s/check (fs/->schema fes/InnerRecursionRefExample) 4))
  (is (thrown-with-msg? Exception
                        #"Ref not in scope: \"a\""
                        (-> fes/UnscopedRefExample fs/->schema)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InfiniteRefExample fs/->schema))))
