(ns flanders.markdown-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [flanders.core :as f]
   [flanders.markdown :as md]))

(deftest ready-for-table-test
  (is (= " \\\\|" (md/ready-for-table "\n|"))))

(f/def-entity-type Actor
  {}
  (f/optional-entries
   (f/entry :confidence f/any
            :description "separated\nby\nspaces within table")))

(deftest signature-type->markdown
  (is (= "### Signature\n\n() => Anything\n\n\n"
         (md/->markdown (f/sig :parameters []))))

  (is (= "### Signature\n\n(Integer) => Anything\n\n\n\n"
         (md/->markdown (f/sig :parameters [(f/int)]))))

  (is (= "### Signature\n\n(Integer, String) => Anything\n\n\n\n\n"
         (md/->markdown (f/sig :parameters [(f/int) (f/str)]))))

  (is (= "### Signature\n\n(Integer, String, Integer, String) => Anything\n\n\n\n\n\n\n"
         (md/->markdown (f/sig :parameters [(f/int) (f/str) (f/int) (f/str)]))))

  (is (= "### Signature\n\n(Integer, String ...) => Anything\n\n\n\n\n"
         (md/->markdown (f/sig :parameters [(f/int)] :rest-parameter (f/str)))))

  (is (= "### Signature\n\n(Integer, String ...) => Anything\n\n\n\n\n"
         (md/->markdown (f/sig :parameters [(f/int)] :rest-parameter (f/str)))))

  (is (= "# `Foo`\n\n### Description\n\nThe Foo.\n\n### Signature\n\n() => Anything\n\n\n"
         (md/->markdown (f/sig :name "Foo"
                               :description "The Foo."
                               :parameters []))))

  (is (= ["<a id=\"top\"></a>"
          "# *Actor* Object"
          ""
          "| Property | Type | Description | Required? |"
          "| -------- | ---- | ----------- | --------- |"
          "|[confidence](#propertyconfidence-anything)|Anything|separated by spaces within table||"
          ""
          ""
          "<a id=\"propertyconfidence-anything\"></a>"
          "## Property confidence ∷ Anything"
          ""
          "separated"
          "by"
          "spaces within table"
          ""
          "* This entry is optional"]
         (-> (md/->markdown Actor)
             str/split-lines))))
