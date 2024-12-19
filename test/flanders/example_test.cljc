(ns flanders.example-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.core :as f]
            [flanders.example :as fe]
            [flanders.examples :as fes]
            [flanders.core :as f]))

(deftest enum-test
  (is (= 0 (-> (f/enum [3, 0, 1, 2, 99]) fe/->example-tree))))

(deftest ref-test
  (is (= 10 (-> fes/RefExample fe/->example-tree)))
  (is (= [] (-> fes/RecursiveRefExample fe/->example-tree)))
  (is (= 42 (-> fes/ShadowingRefExample fe/->example-tree)))
  (is (= 42 (-> fes/ShadowingMultiRefExample fe/->example-tree)))
  (is (= true (-> fes/InnerRecursionRefExample fe/->example-tree)))
  (is (thrown-with-msg? Exception
                        #"Ref not in scope: \"a\""
                        (-> fes/UnscopedRefExample fe/->example-tree)))
  (is (thrown-with-msg? Exception
                        #"Infinite schema detected"
                        (-> fes/InfiniteRefExample fe/->example-tree))))
