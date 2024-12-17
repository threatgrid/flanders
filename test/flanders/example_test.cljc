(ns flanders.example-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.example :as fe]
            [flanders.examples :as fes]
            [flanders.core :as f]))

(deftest ref-test
  (is (= 10 (-> fes/RefExample fe/->example-tree)))
  (is (= [] (-> fes/RecursiveRefExample fe/->example-tree)))
  (is (= 42 (-> fes/ShadowingRefExample fe/->example-tree)))
  (is (= 42 (-> fes/ShadowingMultiRefExample fe/->example-tree)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InfiniteRefExample fe/->example-tree))))
