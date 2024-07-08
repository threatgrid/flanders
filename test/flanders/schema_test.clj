(ns flanders.schema-test
  (:require
   [clojure.test :refer [deftest is]]
   [flanders.core :as f]
   [flanders.examples :refer [Example OptionalKeywordMapEntryExample]]
   [flanders.schema :as fs]
   [schema.core :as s]))

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
        {#schema.core.OptionalKey{:k :foo} java.lang.String
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
  (is (= Boolean
         (fs/->schema (f/bool))))
  (is (= (s/enum true)
         (fs/->schema (f/bool :equals true))))
  (is (= (s/enum false)
         (fs/->schema (f/bool :equals false)))))

(defn- ->swagger [dll] (:json-schema (meta (fs/->schema dll))))

(deftest swagger-test
  (is (= {:example false} (->swagger (f/bool :equals false))))
  (is (= {:example true :description "Foo"} (->swagger (f/bool :equals true :description "Foo"))))
  (is (= {:example 10 :description "foo"} (->swagger (f/int :description "foo"))))
  (is (= {:example 10 :description "foo"} (->swagger (f/int :description "foo")))))
