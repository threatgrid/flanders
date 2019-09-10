(ns flanders.core-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f])
  (:import (flanders.types EitherType)))

(deftest either-test
  (is (instance? EitherType (f/either :choices [(f/int)])))
  (is (thrown? java.lang.AssertionError (f/either)))
  (is (thrown? java.lang.AssertionError (f/either :choices []))))
