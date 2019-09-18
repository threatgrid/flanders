(ns flanders.markdown-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.markdown :as f.markdown]))

(deftest signuature-type->markdown
  (is (= (f.markdown/->markdown (f/sig :parameters []))
         "() => Anything\n"))

  (is (= (f.markdown/->markdown (f/sig :parameters [(f/int)]))
         "(Integer) => Anything\n\n"))

  (is (= (f.markdown/->markdown (f/sig :parameters [(f/int) (f/str)]))
         "(Integer,  String) => Anything\n\n\n"))

  (is (= (f.markdown/->markdown (f/sig :parameters [(f/int)]
                                       :rest-parameter (f/str)))
         "(Integer,  String ...) => Anything\n\n\n"))

  (is (= (f.markdown/->markdown (f/sig :parameters [(f/int)]
                                       :rest-parameter (f/str)))
         "(Integer,  String ...) => Anything\n\n\n")))
