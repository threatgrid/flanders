(ns flanders.markdown-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.core :as f]
            [flanders.markdown :as f.markdown]))

(deftest signature-type->markdown
  (is (= "### Signature\n\n() => Anything\n\n"
         (-> :parameters
             (f/sig [])
             f.markdown/->markdown
             first
             second)))

  (is (= "### Signature\n\n(Integer) => Anything\n\n"
         (-> :parameters
             (f/sig [(f/int)])
             f.markdown/->markdown
             first
             second)))

  (is (= "### Signature\n\n(Integer, String) => Anything\n\n"
         (-> :parameters
             (f/sig [(f/int) (f/str)])
             f.markdown/->markdown
             first
             second)))

  (is (= "### Signature\n\n(Integer, String, Integer, String) => Anything\n\n"
         (-> :parameters
             (f/sig [(f/int) (f/str) (f/int) (f/str)])
             f.markdown/->markdown
             first
             second)))

  (is (= "### Signature\n\n(Integer, String ...) => Anything\n\n"
         (-> :parameters
             (f/sig [(f/int)] :rest-parameter (f/str))
             f.markdown/->markdown
             first
             second)))

  (is (= "### Signature\n\n(Integer, String ...) => Anything\n\n"
         (-> :parameters
             (f/sig [(f/int)] :rest-parameter (f/str))
             f.markdown/->markdown
             first
             second)))

  (is (= "# `Foo`\n\n### Description\n\nThe Foo.\n\n### Signature\n\n() => Anything\n\n"
         (-> :name
             (f/sig "Foo"
                    :description "The Foo."
                    :parameters [])
             f.markdown/->markdown
             first
             second))))
