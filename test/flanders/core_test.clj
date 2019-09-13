(ns flanders.core-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f])
  (:import (flanders.types EitherType
                           MapType)))


(deftest def-entity-type-test
  (is (thrown? clojure.lang.ExceptionInfo
               (f/def-entity-type Foo 'bad)))

  (is (thrown? clojure.lang.ExceptionInfo
               (let [bad 43]
                 (f/def-entity-type Foo bad))))

  (is (instance? clojure.lang.Var
                 (f/def-entity-type Bar "")))

  (is (instance? MapType
                 (deref (f/def-entity-type Bar ""))))

  (is (= "Description"
         (get (deref (f/def-entity-type Bar "Description"))
              :description)))

  (is (= "Description"
         (get (deref (f/def-entity-type Bar {:description "Description"}))
              :description)))

  (is (= "Description"
         (let [description "Description"]
           (get (deref (f/def-entity-type Bar description))
                :description))))

  (is (= "Description"
         (let [okay {:description "Description"}]
           (get (deref (f/def-entity-type Bar description))
                :description)))))

(deftest either-test
  (is (instance? EitherType (f/either :choices [(f/int)])))
  (is (thrown? java.lang.AssertionError (f/either)))
  (is (thrown? java.lang.AssertionError (f/either :choices []))))
