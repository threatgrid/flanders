(ns flanders.predicates
  (:refer-clojure
    :exclude [key keyword keyword? map map? sequential?])
  (:require [clojure.zip :as z]
            [flanders.protocols :as prots]
            #?(:clj  [flanders.types]
               :cljs [flanders.types
                      :refer [EitherType KeywordType MapEntry MapType
                              SequenceOfType SetOfType]]))
  (:import #?(:clj [flanders.types
                    EitherType KeywordType MapEntry MapType SequenceOfType
                    SetOfType])))

;; ----------------------------------------------------------------------
;; about nodes
;; ----------------------------------------------------------------------

(def branch? prots/branch?)

(def either? (partial instance? EitherType))

(def entry? (partial instance? MapEntry))

(def map? (partial instance? MapType))

(def leaf? (complement branch?))

(def seq-of? (partial instance? SequenceOfType))

(def ^:deprecated sequence-of? seq-of?)

(def set-of? (partial instance? SetOfType))

(def col-of?
  (comp boolean (some-fn seq-of? set-of?)))

(def keyword? (partial instance? KeywordType))


;; ----------------------------------------------------------------------
;; about zipper locations
;; ----------------------------------------------------------------------

(defn entry
  "If a given loc is pointing at a MapEntry node, return the loc"
  [loc]
  (when (-> loc z/node entry?)
    loc))

(defn leaf
  "If a given loc is pointing at a leaf node, return the loc"
  [loc]
  (when (-> loc z/node leaf?)
    loc))

(defn key
  "If a given loc is pointing at a key node (1st child of an entry),
  return the loc"
  [loc]
  (when (some-> loc z/up entry z/node :key (= (z/node loc)))
    loc))

(defn keyword
  "If the loc points at a KeywordType, return the loc"
  [loc]
  (when (some-> loc z/node keyword?)
    loc))

(defn map
  "If the loc points at a MapType, return the loc"
  [loc]
  (when (some-> loc z/node map?)
    loc))

(defn sequential?
  "Given a loc that is pointing at a leaf-node, look at its parent
  node and see if it is a SequenceOfType"
  [loc]
  (boolean
   (some-> loc
           leaf
           z/up
           z/node
           sequence-of?)))

(defn seq-of
  "If the loc points at a SequenceOfType, return the loc"
  [loc]
  (when (some-> loc z/node sequence-of?)))

(defn set-of
  "If the loc points at a SetOfType, return the loc"
  [loc]
  (when (some-> loc z/node set-of?)
    loc))

(defn col-of
  "Either seq-of or set-of"
  [loc]
  (or (seq-of loc)
      (set-of loc)))
