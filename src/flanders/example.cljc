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
  (->example [{:keys [choices] :as ddl} f]
    (let [[_ example :as example?] (find ddl :default)]
      (if example?
        example
        (f (first choices)))))

  MapEntry
  (->example [{:keys [key type] :as ddl} f]
    (let [[_ example :as example?] (find ddl :default)]
      [(f (assoc key :key? true))
       (f (cond-> type
            example? (assoc :default example)))]))

  MapType
  (->example [{:keys [entries default]} f]
    (or default
        (reduce (fn [m [k v]]
                  (assoc m k v))
                {}
                (map f entries))))

  SequenceOfType
  (->example [{:keys [type default]} f]
    (or default [(f type)]))

  SetOfType
  (->example [{:keys [type default]} f]
    (or default #{(f type)}))

  ;; Leaves

  AnythingType
  (->example [{:keys [default]} _]
    (or default "anything"))

  BooleanType
  (->example [{:keys [default] :or {default true}} _]
    default)

  InstType
  (->example [{:keys [default]} _]
    (or default
        #?(:clj  (Date. 1451610061000)
           :cljs (js/date. 1451610061000))))

  IntegerType
  (->example [{:keys [default values]} _]
    (or default
        (-> values sort first)
        10))

  KeywordType
  (->example [{:keys [default values]} _]
    (or default
        (-> values sort first)
        :keyword))

  NumberType
  (->example [{:keys [default values]} _]
    (or default
        (-> values sort first)
        10.0))

  StringType
  (->example [{:keys [default values]} _]
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
  (->example ddl ->example-tree))
