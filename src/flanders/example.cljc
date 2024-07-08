(ns flanders.example
  (:require
   #?(:clj  [clojure.core.match :refer [match]]
      :cljs [cljs.core.match :refer-macros [match]])
   #?(:clj  [flanders.types :as ft]
      :cljs [flanders.types
             :as ft
             :refer [AnythingType BooleanType EitherType InstType IntegerType
                     KeywordType MapEntry MapType NumberType SequenceOfType
                     SetOfType SignatureType StringType]]))
  #?(:clj (:import
           [flanders.types
            AnythingType
            BooleanType
            EitherType
            InstType
            IntegerType
            KeywordType
            MapEntry
            MapType
            NumberType
            SequenceOfType
            SetOfType
            SignatureType
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
    "anything")

  BooleanType
  (->example [{:keys [default] :or {default true}} _]
    default)

  InstType
  (->example [_ _]
    #?(:clj  (Date. 1451610061000)
       :cljs (js/date. 1451610061000)))

  IntegerType
  (->example [_ _]
    10)

  KeywordType
  (->example [{:keys [default values example]} _]
    (or default
        (-> values sort first)
        :keyword))

  NumberType
  (->example [{:keys [default values example]} _]
    (or default
        (-> values sort first)
        10.0))

  StringType
  (->example [{:keys [default values example]} _]
    (or default
        (-> values sort first)
        "string"))

  SignatureType
  (->example [{:keys [parameters rest-parameter return]} f]
    (let [arguments (mapv
                     (fn [i parameter]
                       [(str "arg" i) (f parameter)])
                     (range)
                     (:parameters parameters))
          arguments (if (some? rest-parameter)
                      (conj arguments ["argN" (f rest-parameter)])
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
  (if-some [[_ example] (find ddl :example)]
    (do (when (instance? MapEntry ddl)
          (throw (ex-info "Cannot add :example to MapEntry, add to value schema instead." {:ddl ddl})))
        example)
    (->example ddl ->example-tree)))
