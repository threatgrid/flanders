(ns flanders.example
  (:require [flanders.predicates :as fp]
            [flanders.schema :as fs]
            #?(:clj  [flanders.types :as ft]
               :cljs [flanders.types
                      :as ft
                      :refer [AnythingType BooleanType EitherType
                              InstType IntegerType KeywordType
                              MapEntry MapType NumberType
                              SequenceOfType SetOfType SignatureType
                              StringType]]))
  #?(:clj (:import [flanders.types
                    AnythingType BooleanType EitherType InstType
                    IntegerType KeywordType MapEntry MapType
                    NumberType SequenceOfType SetOfType SignatureType
                    StringType]
                   [java.util Date])))

(defprotocol JsonExampleNode
  (->example [node f]))

(extend-protocol JsonExampleNode

  ;; Branches

  EitherType
  (->example [{:keys [choices]} f]
    (f (first choices)))

  MapEntry
  (->example [{:keys [key type]} f]
    [(f (assoc key
               :key? true))
     (f type)])

  MapType
  (->example [{:keys [entries]} f]
    (reduce (fn [m [k v]]
              (assoc m k v))
            {}
            (map f entries)))

  SequenceOfType
  (->example [{:keys [type]} f]
    [(f type)])

  SetOfType
  (->example [{:keys [type]} f]
    #{(f type)})

  ;; Leaves

  AnythingType
  (->example [_ _]
    {:anything "anything"})

  BooleanType
  (->example [_ _]
    true)

  InstType
  (->example [_ _]
    #?(:clj  (Date. 1451610061000)
       :cljs (js/date. 1451610061000)))

  IntegerType
  (->example [_ _]
    10)

  KeywordType
  (->example [node _]
    (let [schema (fs/->schema node)]
      (if (keyword? schema)
        (name schema)
        "keyword")))

  NumberType
  (->example [_ _]
    10.0)

  StringType
  (->example [_ _]
    "string")

  SignatureType
  (->example [{:keys [parameters rest-parameter return]} f]
    (let [arguments (mapv
                     (fn [i parameter]
                       [(str "arg" i) (f parameter)])
                     (range)
                     (:parameters parameters))
          arguments (if (some? rest-parameter)
                      (conj arguments ["argN"(f rest-parameter)])
                      arguments)]
      {:arguments (into {} arguments)
       :returns (f return)})))

;; This is a fast implementation of making an example, but it could be better
;; - It could take advantage of generators (to be non-deterministic)
;; - It could use zippers, so that generators could be location aware
;; - Examples should be semantically valuable (should make sense in our domain)
(defn ->example-tree
  "Get a JSON example for a DDL node"
  [ddl]
  (->example ddl ->example-tree))
