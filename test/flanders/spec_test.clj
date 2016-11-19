(ns flanders.spec-test
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.test :refer [deftest is testing use-fixtures]]
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
