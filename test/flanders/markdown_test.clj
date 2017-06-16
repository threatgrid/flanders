(ns flanders.markdown-test
  (:require [flanders.markdown :as sut]
            [flanders.core :as f :refer [def-entity-type]]
            [clojure.test :as t]))

(def-entity-type Actor
  {:description "description text"}
  (f/required-entries
   (f/entry :type (f/eq "actor"))))

(t/deftest check-markdown-keywords
  (t/testing "keywords returns as string"
    ;; Just after Plumatic Schema:
    ;; we should have `\"type\"` instead of `\":type\"`
    (t/is (=
           "<a name=\"top\"/>
# *Actor* Map

description text

| key | type | required? |
| --- | ---- | --------- |
|[:type](#mapentrytype-string)|String|&#10003;|

<a name=\"mapentrytype-string\"/>
## MapEntry `type` ∷ `\"String\"`

* This entry is required

* Keyword Key
  * Plumatic Schema: `\"type\"`

* String Value
  * Plumatic Schema: `\"(enum ...)\"`
  * Must equal: \"actor\"
"
           
           (sut/->markdown Actor)))))
