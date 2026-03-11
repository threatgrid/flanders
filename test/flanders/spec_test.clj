(ns flanders.spec-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.core.match :refer [match]]
            [flanders.core :as f]
            [flanders.examples :refer [Example]]
            [flanders.spec :as fs]
            [flanders.types :as ft]))

(use-fixtures :once
  (fn [t]
    (stest/instrument 'fs/->spec)
    (t)
    (stest/unstrument 'fs/->spec)))

(deftest test-valid-spec
  (is
   (s/valid?
    (fs/->spec Example "example")
    {:foo "foo123"
     :bar {"integer" 100
           :seq [:a :b :c]
           :set #{1 3}}
     :yes? true
     :spam :eggs}))

  (testing "empty map is invalid"
    (is
     ((complement s/valid?)
      (fs/->spec Example "example")
      {}))))

(deftest test-not-valid-spec
  (testing "can provide a custom spec"
    (is
     (s/valid?
      (fs/->spec (f/map [(f/entry :strict-foo-1
                                  (f/str :spec #(re-matches #"foo.*" %)))])
                 "test-not-valid-spec")
      {:strict-foo-1 "foo123"}))

    (is
     ((complement s/valid?)
      (fs/->spec (f/map [(f/entry :strict-foo-2
                                  (f/str :spec #(re-matches #"foo.*" %)))])
                 "test-not-valid-spec")
      {:strict-foo-2 "bar123"})))

  (testing "can use an EitherType"
    (s/valid?
     (fs/->spec (ft/map->EitherType {:choices [f/any-str f/any-keyword]})
                "test-not-valid-spec")
     :foo)

    (s/valid?
     (fs/->spec (ft/map->EitherType {:choices [f/any-str f/any-keyword]})
                "test-not-valid-spec")
     "foo")

    ((complement s/valid?)
     (fs/->spec (ft/map->EitherType {:choices [f/any-str f/any-keyword]})
                "test-not-valid-spec")
     'foo)))

(deftest test-map
  (testing "map with custom spec predicate"
    (s/valid?
     (fs/->spec (f/map
                 (f/optional-entries
                  (f/entry :foo f/any-str)
                  (f/entry :bar f/any-str))
                 :spec (fn [m]
                         (= 1 (count m))))
                "test-map-1")
     {:foo "foo"})

    (s/valid?
     (fs/->spec (f/map
                 (f/optional-entries
                  (f/entry :foo f/any-str)
                  (f/entry :bar f/any-str))
                 :spec (fn [m]
                         (= 1 (count m))))
                "test-map-2")
     {:bar "bar"})

    ((complement s/valid?)
     (fs/->spec (f/map
                 (f/optional-entries
                  (f/entry :foo f/any-str)
                  (f/entry :bar f/any-str))
                 :spec (fn [m]
                         (= 1 (count m))))
                "test-map-3")
     {:foo "foo"
      :bar "bar"})

    ((complement s/valid?)
     (fs/->spec (f/map
                 (f/optional-entries
                  (f/entry :foo f/any-str)
                  (f/entry :bar f/any-str))
                 :spec (fn [m]
                         (= 1 (count m))))
                "test-map-4")
     {:spam "eggs"})))

(deftest test-seq-set
  (is (s/valid?
       (fs/->spec (f/seq-of (f/seq-of f/any)) "test-seq")
       [["foo"]]))
  (is (s/valid?
       (fs/->spec (f/set-of (f/set-of f/any-str)) "test-set")
       #{#{"foo"}})))

(defn max-len [n]
  (fn [coll] (<= (count coll) n)))

(deftest test-seq-of-with-spec
  (testing "seq-of with custom spec predicate (max length)"
    (is (s/valid?
         (fs/->spec (f/seq-of f/any-str :spec #(<= (count %) 3))
                    "test-seq-spec-1")
         ["a" "b" "c"]))
    (is ((complement s/valid?)
         (fs/->spec (f/seq-of f/any-str :spec #(<= (count %) 3))
                    "test-seq-spec-2")
         ["a" "b" "c" "d"])))
  (testing "seq-of with closure spec (e.g. from pred/max-len)"
    (is (s/valid?
         (fs/->spec (f/seq-of f/any-str :spec (max-len 2))
                    "test-seq-spec-closure-1")
         ["a" "b"]))
    (is ((complement s/valid?)
         (fs/->spec (f/seq-of f/any-str :spec (max-len 2))
                    "test-seq-spec-closure-2")
         ["a" "b" "c"])))
  (testing "seq-of without spec still works"
    (is (s/valid?
         (fs/->spec (f/seq-of f/any-str) "test-seq-spec-3")
         ["a" "b" "c" "d"]))))

(deftest test-set-of-with-spec
  (testing "set-of with custom spec predicate (max length)"
    (is (s/valid?
         (fs/->spec (f/set-of f/any-str :spec #(<= (count %) 2))
                    "test-set-spec-1")
         #{"a" "b"}))
    (is ((complement s/valid?)
         (fs/->spec (f/set-of f/any-str :spec #(<= (count %) 2))
                    "test-set-spec-2")
         #{"a" "b" "c"})))
  (testing "set-of without spec still works"
    (is (s/valid?
         (fs/->spec (f/set-of f/any-str) "test-set-spec-3")
         #{"a" "b" "c"}))))

(deftest sig-spec-test
  (let [spec-key (fs/->spec (f/sig :parameters [(f/int)]) "foo")]
    (is (match (s/describe spec-key)
          (['fspec :args (['cat :a0 _] :seq) :ret _ :fn nil] :seq)
          true

          _
          false)))

  (let [spec-key (fs/->spec (f/sig :parameters [(f/int)] :rest-parameter (f/int)) "foo")]
    (is (match (s/describe spec-key)
          (['fspec :args (['cat :a0 _ :a* (['* _] :seq)] :seq) :ret _ :fn nil] :seq)
          true

          _
          false))))
