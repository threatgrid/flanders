(ns flanders.json-schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.json-schema :as sut]))

(deftest ->flanders-test
  (is (= (sut/->flanders 
           {:title "union", ;;TODO
            :type "object",
            :properties (sorted-map
                          :x {:anyOf [{:type "integer"} {:type "string"}]}
                          :y {:type "integer"}),
            :required [:x :y]}
           nil)
         (f/map
           [(f/entry :x (f/either :choices [(f/int) (f/str)])
                     :required? true)
            (f/entry :y (f/int)
                     :required? true)]
           ))))
