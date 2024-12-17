(ns flanders.example-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.example :as fe]
            [flanders.examples :as fes]
            [flanders.core :as f]))

(deftest ref-test
  (is (= 10 (-> fes/RefExample fe/->example-tree)))
  (is (= [] (-> fes/RecursiveRefExample fe/->example-tree)))
  (is (= [:ref {:registry {"a" [:ref {:registry {"a" [:enum 42]}}
                                "a"]}}
          "a"]
         (-> fes/ShadowingRefExample fe/->example-tree))))
