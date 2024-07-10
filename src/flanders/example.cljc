(ns flanders.example
  (:require
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
  (->example [{:keys [choices default] :as ddl} f]
    (if (some? default)
      default
      (f (first choices))))

  MapEntry
  (->example [{:keys [key type default] :as ddl} f]
    [(f (assoc key :key? true))
     (f (cond-> type
          (some? default) (assoc :default default)))])

  MapType
  (->example [{:keys [entries default]} f]
    (if (some? default)
      default
      (reduce (fn [m [k v]]
                (assoc m k v))
              {}
              (map f entries))))

  SequenceOfType
  (->example [{:keys [type default]} f]
    (if (some? default)
      default
      [(f type)]))

  SetOfType
  (->example [{:keys [type default]} f]
    (if (some? default)
      default
      #{(f type)}))

  ;; Leaves

  AnythingType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
      "anything"))

  BooleanType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
      true))

  InstType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
      #?(:clj  (Date. 1451610061000)
         :cljs (js/date. 1451610061000))))

  IntegerType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
      10))

  KeywordType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
      :keyword))

  NumberType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
      10.0))

  StringType
  (->example [{:keys [default]} _]
    (if (some? default)
      default
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
