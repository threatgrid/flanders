(ns flanders.schema
  (:refer-clojure :exclude [type key])
  (:require
   #?(:clj  [clojure.core.match :refer [match]]
      :cljs [cljs.core.match :refer-macros [match]])
   #?(:clj  [flanders.types]
      :cljs [flanders.types
             :refer [AnythingType BooleanType EitherType InstType IntegerType
                     KeywordType MapEntry MapType NumberType ParameterListType
                     SequenceOfType SetOfType SignatureType StringType]])
   #?(:clj [ring.swagger.json-schema :as rs])
   [flanders.predicates :as fp]
   [flanders.protocols :as prots]
   [schema-tools.core :as st]
   [schema.core :as s])
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
            ParameterListType
            SequenceOfType
            SetOfType
            SignatureType
            StringType])))

(defprotocol SchemaNode
  (->schema' [node f]))

(defn ->schema [node]
  (->schema' node ->schema))

(def ^:deprecated ->schema-tree ->schema)

(def get-schema
  (memoize ->schema))

(defn- describe [schema description]
  (if description
    (#?(:cljs (fn [s _] s)
        :clj  rs/describe)
     schema
     description)
    schema))

(extend-protocol SchemaNode

  ;; Branches

  EitherType
  (->schema' [{:keys [choices tests]} f]
    (let [choice-schemas (map f choices)]
      (apply s/conditional (mapcat vector tests choice-schemas))))

  MapEntry
  (->schema' [{:keys [key type required?] :as entry} f]
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    [((if (and (not required?)
               (not (:open? key))
               (seq (:values key)))
        s/optional-key
        identity)
      (f (assoc key
                :key? true
                :description (some :description [key entry]))))
     (f type)])

  MapType
  (->schema' [{:keys [description entries]} f]
    (describe
     (reduce (fn [m [k v]]
               (st/assoc m k v))
             {}
             (map f entries))
     description))

  ParameterListType
  (->schema' [{:keys [parameters]} f]
    (mapv f parameters))

  SequenceOfType
  (->schema' [{:keys [type]} f]
    [(f type)])

  SetOfType
  (->schema' [{:keys [type]} f]
    #{(f type)})

  SignatureType
  (->schema' [{:keys [parameters rest-parameter return]} f]
    (let [parameters (f parameters)]
      (s/make-fn-schema (f return)
                        (if (some? rest-parameter)
                          [(conj parameters [(f rest-parameter)])]
                          [parameters]))))

  ;; Leaves

  AnythingType
  (->schema' [{:keys [description]} _]
    (describe
     s/Any
     description))

  BooleanType
  (->schema' [{:keys [open? default description]} _]
    (describe
     (match [open? default]
            [true  _] s/Bool
            [_     d] (s/enum d))
     description))

  InstType
  (->schema' [{:keys [description]} _]
    (describe s/Inst description))

  IntegerType
  (->schema' [{:keys [description open? values]} _]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Int
            [_     nil] s/Int
            :else       (apply s/enum values))
     description))

  KeywordType
  (->schema' [{:keys [description key? open? values]} _]
    (let [kw-schema
          (match [key? open? (seq values)]
                 [_    true  _         ] s/Keyword
                 [_    _     nil       ] s/Keyword
                 [true false ([k] :seq)] k
                 :else                   (apply s/enum values))]
      (if key?
        kw-schema
        (describe kw-schema description))))

  NumberType
  (->schema' [{:keys [description open? values]} _]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Num
            [_     nil] s/Num
            :else       (apply s/enum values))
     description))

  StringType
  (->schema' [{:keys [description open? values]} _]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Str
            [_     nil] s/Str
            :else       (apply s/enum values))
     description)))

(defn ->schema-at-loc
  "Get the schema for a node, with location awareness"
  [leaf-node loc]
  (->schema
   (if (fp/key loc)
     (assoc leaf-node :key? true)
     leaf-node)))

(defn dedup-map-type
  "MapType maintains a list of MapEntryTypes.  Since this is a list
  and not a map, there is no duplicate key resolution.  This fn takes
  a MapType and returns a new MapType with duplicates removed (based
  on key schema matching)."
  [map-type]
  (let [ent-map (reduce (fn [m {key :key :as entry}]
                          (st/assoc m
                                    (->schema
                                     (assoc key :key? true))
                                    entry))
                        (array-map)
                        (:entries map-type))]
    (prots/make-node map-type (vals ent-map))))

(defn sort-map-type
  "Sort the entries in a MapType by key schema (compared as a string).
  The MapType should already be deduped (as by dedup-map-type) as this
  fn does not use the schema-tools assoc."
  [map-type]
  (let [ent-map (reduce (fn [m {key :key :as entry}]
                          (assoc m
                                 (-> (assoc key :key? true)
                                     ->schema
                                     pr-str)
                                 entry))
                        (sorted-map)
                        (:entries map-type))]
    (prots/make-node map-type (vals ent-map))))
