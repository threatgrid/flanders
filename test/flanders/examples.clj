(ns flanders.examples
  (:require
   [flanders.core :as f]))

(f/def-entity-type Example
  {:description "Example"}
  (f/required-entries
   (f/entry :foo f/any-str)
   (f/entry :bar (f/map
                  [(f/entry (f/eq "integer") f/any-int)
                   (f/entry :seq (f/seq-of f/any-keyword))
                   (f/entry :set (f/set-of (f/enum #{1 2 3})))]))
   (f/entry :yes? f/any-bool))
  (f/optional-entries
   (f/entry :spam (f/eq :eggs))))

(f/def-entity-type OptionalKeywordMapEntryExample
  {:description "Foo"}
  [(f/entry :foo f/any-str :required? false)
   (f/entry
    :relation_info
    (f/map
     [(f/entry f/any-keyword f/any :required? false)])
    :required? true)])

(def RefExample
  (-> (f/ref "foo")
      (f/update-registry assoc "foo" (f/int))))

(def RecursiveRefExample
  (-> (f/ref "foo")
      (f/update-registry assoc "foo" (f/seq-of (f/ref "foo")))))

(def ShadowingRefExample
  "equivalent to (f/enum 42)"
  (-> (f/ref "a")
      (f/update-registry assoc "a"
                         (-> (f/ref "a")
                             (f/update-registry assoc "a" (f/enum #{42}))))))

(def InfiniteRefExample
  (-> (f/ref "a")
      (f/update-registry assoc "a" (f/ref "a"))))
