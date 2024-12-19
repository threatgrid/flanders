(ns flanders.json-schema.json-schema-test-suite-test
  "Run some tests from https://github.com/json-schema-org/JSON-Schema-Test-Suite"
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [flanders.json-schema :as sut]
            [flanders.json-schema.test-helpers :as th :refer [->malli ;TODO
                                                              #_->schema]]
            [malli.core :as m]))

(def short->dialect
  {"draft7" "http://json-schema.org/draft-07/schema#"
   "draft2020-12" "https://json-schema.org/draft/2020-12/schema"})

(def dialect->dir
  {"draft7" "JSON-Schema-Test-Suite/tests/draft7"
   "draft2020-12" "JSON-Schema-Test-Suite/tests/draft2020-12"})

;; note: this test suite rarely declares "type", which results in very broad schemas that flanders can't represent.
;; https://github.com/json-schema/json-schema/issues/172
;; we filter out such test cases
(def json-schema-test-suite
  {"draft7"
   {"additionalItems.json" {}
    "additionalProperties.json" {}
    "allOf.json" {}
    "anyOf.json" {}
    "boolean_schema.json" {}
    "const.json" {:config {;; not sure
                           "float and integers are equal up to 64-bit representation limits" :skip
                           "const with -2.0 matches integer and float types" :skip
                           "const with 1 does not match true" {"float one is valid" :skip}
                           "const with 0 does not match other zero-like types" {"float zero is valid" :skip}}}
    "contains.json" {}
    "default.json" {}
    "definitions.json" {}
    "dependencies.json" {}
    "enum.json" {}
    "exclusiveMaximum.json" {}
    "exclusiveMinimum.json" {}
    "format.json" {}
    "if-then-else.json" {}
    ;;TODO default $id
    "infinite-loop-detection.json" :skip
    "items.json" {"a schema given for items"
                  {"JavaScript pseudo-array is valid" :skip}}
    "maxItems.json" {}
    "maxLength.json" {}
    "maxProperties.json" {}
    "maximum.json" {}
    "minItems.json" {}
    "minLength.json" {}
    "minProperties.json" {}
    "minimum.json" {}
    "multipleOf.json" {}
    "not.json" {}
    "oneOf.json" {}
    "pattern.json" {}
    "patternProperties.json" {}
    "properties.json" {}
    "propertyNames.json" {}
    ;:TODO
    "ref.json" :skip
    "refRemote.json" :skip
    "required.json" {"required properties whose names are Javascript object property names"
                     ;; not sure what's going on here. guessing all JS object have these properties.
                     {"ignores arrays" :skip
                      "ignores non-arrays" :skip
                      "ignores other non-objects" :skip}}
    "type.json" {}
    "uniqueItems.json" {}}
   "draft2020-12"
   {"additionalProperties.json" {}
    "allOf.json" {}
    "anchor.json" :skip
    "anyOf.json" {}
    "boolean_schema.json" {}
    "const.json" {}
    "contains.json" {}
    "content.json" {}
    "default.json" {}
    "defs.json" {}
    "dependentRequired.json" {}
    "dependentSchemas.json" {}
    "dynamicRef.json" :skip
    "enum.json" {}
    "exclusiveMaximum.json" {}
    "exclusiveMinimum.json" {}
    "format.json" {}
    "if-then-else.json" {}
    "infinite-loop-detection.json" {}
    "items.json" {}
    "maxContains.json" {}
    "maxItems.json" {}
    "maxLength.json" {}
    "maxProperties.json" {}
    "maximum.json" {}
    "minContains.json" {}
    "minItems.json" {}
    "minLength.json" {}
    "minProperties.json" {}
    "minimum.json" {}
    "multipleOf.json" {}
    "not.json" {}
    "oneOf.json" {}
    "pattern.json" {}
    "patternProperties.json" {}
    "prefixItems.json" {}
    "properties.json" {}
    "propertyNames.json" {}
    "ref.json" :skip
    "refRemote.json" :skip
    "required.json" {}
    "type.json" {}
    "unevaluatedItems.json" :skip
    "unevaluatedProperties.json" :skip
    "uniqueItems.json" {}
    "vocabulary.json" {}}})

(defn ->printable [data]
  (walk/postwalk
    (fn [data]
      (if (string? data)
        ;; replace non-printable
        (str/replace data #"\p{C}" "<NOTPRINTABLE>")
        data))
    data))

(deftest json-schema-test-suite-test
  (let [ntested (atom 0)]
    (doseq [[short-dialect short-file->config] json-schema-test-suite
            [short-file suite-description->test-description->config] short-file->config
            :when (not= :skip suite-description->test-description->config)
            :let [dialect (or (short->dialect short-dialect)
                              (throw (ex-info (str "Unknown short dialect: " (pr-str short-dialect)) {})))
                  dir (or (dialect->dir short-dialect)
                          (throw (ex-info (str "Unknown dialect dir: " (pr-str short-dialect)) {})))
                  file (io/file dir short-file)
                  suite (json/decode (slurp file))
                  _ (assert (seq suite))]
            {suite-description "description" :strs [schema tests]} suite
            :let [_ (assert (seq tests))
                  test-description->config (get suite-description->test-description->config suite-description)
                  suite-description (str/trim suite-description)
                  skip? (or (= :skip test-description->config)
                            (str/includes? suite-description "$ref")
                            (str/includes? suite-description "$id")
                            (str/includes? suite-description "$defs")
                            (str/includes? suite-description "nul")
                            (str/includes? suite-description "metaschema"))]
            :when (not skip?)]
      (testing (str short-dialect "\n" short-file "\n" "Test suite: " suite-description "\n")
        (doseq [{test-description "description" :strs [data valid]} tests
                backend [:malli #_:schema]
                :let [;; we generate schemas with keyword keys so we need to coerce input
                      data (walk/keywordize-keys data)
                      test-description (str/trim test-description)
                      config (get test-description->config test-description)
                      skip? (or (= :skip config)
                                (str/includes? test-description "float")
                                (str/includes? test-description ".0")
                                (str/includes? test-description "$ref")
                                (str/includes? test-description "$id")
                                (str/includes? test-description "$defs")
                                (str/includes? test-description "definitions")
                                (str/includes? test-description "nul")
                                (str/includes? test-description "metaschema")
                                ;; https://github.com/json-schema/json-schema/issues/172
                                ;; most of these tests rely on omitting "type" for very broad schemas we don't support
                                (str/includes? test-description "ignores")
                                (str/includes? test-description "JavaScript pseudo-array")
                                (and (map? schema)
                                     (when-some [[_ const] (find schema "const")]
                                       (or (not (integer? const))
                                           (not (string? const))))))]
                :when (not skip?)]
          (testing (str test-description "\n"
                        "JSON Schema: "
                        (pr-str (->printable schema))
                        "\n"
                        "Input: "
                        (pr-str (->printable data)))
            (is (do (case backend
                      :malli (when-some [m (try (->malli schema {:flanders.malli/no-example true
                                                                 ::sut/dialect dialect})
                                                (catch Exception e
                                                  (when-not (::sut/unsupported (ex-data e))
                                                    (throw e))))]
                               (swap! ntested inc)
                               (is (= valid (try (m/coerce m data)
                                                 true
                                                 (catch Exception _ false)))
                                   (pr-str (m/form m)))))
                    ;; print testing string on error
                    true))))))
    (is (= 160 @ntested))))

(comment
  (clojure.test/test-vars [#'json-schema-test-suite-test])
  )
