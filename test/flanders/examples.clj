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
  "equivalent to (f/int), but using refs"
  (-> (f/ref "foo")
      (f/update-registry assoc "foo" (f/int))))

(def RecursiveRefExample
  "equivalent to (f/seq-of (f/seq-of (f/seq-of ...)))"
  (-> (f/ref "foo")
      (f/update-registry assoc "foo" (f/seq-of (f/ref "foo")))))

;; see explanation in flanders.utils (first schema)
(def ShadowingRefExample
  "equivalent to (f/enum 42), but using multiple levels of shadowing refs"
  (-> (f/ref "a")
      (f/update-registry assoc "a"
                         (-> (f/ref "a")
                             (f/update-registry assoc "a" (f/enum #{42}))))))

;; see explanation in flanders.utils (second schema)
(def ShadowingMultiRefExample
  "equivalent to (f/enum 42), but using multiple levels of shadowing refs that
  create multiple levels of dynamic scope whose registries have the same keys
  but different values."
  (-> (f/ref "a")
      (f/update-registry assoc
                         "a" (f/ref "b")
                         "b" (-> (f/ref "a")
                                 (f/update-registry assoc
                                                    "a" (f/ref "b")
                                                    "b" (f/enum #{42}))))))

;; see explanation in flanders.utils (third schema)
(def InnerRecursionRefExample
  (-> (f/either :choices [(-> (f/either :choices [(f/ref "a") (f/ref "b")])
                              (f/update-registry assoc
                                                 "b" (f/int)))])
      (f/update-registry assoc
                         "a" (-> (f/either :choices [(f/ref "a") (f/ref "b")])
                                 (f/update-registry assoc
                                                    "b" (f/bool))))))

(def UnscopedRefExample
  (f/ref "a"))

(def InfiniteRefExample
  "uses refs to create a schema that expands infinitely with no base cases."
  (-> (f/ref "a")
      (f/update-registry assoc "a" (f/ref "a"))))

(def InfiniteEitherExample
  "uses either schemas to create a schema that makes no progress during validation."
  (-> (f/ref "a")
      (f/update-registry assoc "a" (f/either :choices [(f/ref "a") (f/bool)]))))
