(ns flanders.markdown-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.markdown :as f.markdown]))

(deftest signuature-type->markdown
  (is (= "### Signature\n\n() => Anything\n\n\n"
         (f.markdown/->markdown (f/sig :parameters []))))

  (is (= "### Signature\n\n(Integer) => Anything\n\n\n\n"
         (f.markdown/->markdown (f/sig :parameters [(f/int)]))))

  (is (= "### Signature\n\n(Integer,  String) => Anything\n\n\n\n\n"
         (f.markdown/->markdown (f/sig :parameters [(f/int) (f/str)]))))

  (is (= "### Signature\n\n(Integer,  String ...) => Anything\n\n\n\n\n"
         (f.markdown/->markdown (f/sig :parameters [(f/int)] :rest-parameter (f/str)))))

  (is (= "### Signature\n\n(Integer,  String ...) => Anything\n\n\n\n\n"
         (f.markdown/->markdown (f/sig :parameters [(f/int)] :rest-parameter (f/str)))))

  (is (= "# `Foo`\n\n### Signature\n\n() => Anything\n\n### Description\n\nThe Foo.\n\n\n"
         (f.markdown/->markdown (f/sig :name "Foo"
                                       :description "The Foo."
                                       :parameters [])))))
