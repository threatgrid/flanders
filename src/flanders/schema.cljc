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
   [flanders.example :as example]
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

(defn- describe [schema {:keys [description] :as dll}]
  #?(:cljs schema
     :default (rs/field
                schema
                (cond-> {:example (if-some [[_ example] (find dll :example)]
                                    example
                                    (example/->example-tree dll))}
                  description (assoc :description description)))))

(extend-protocol SchemaNode

  ;; Branches

  EitherType
  (->schema' [{:keys [choices tests] :as dll} f]
    (-> (let [choice-schemas (map f choices)]
          (apply s/conditional (mapcat vector tests choice-schemas)))
        (describe dll)))

  MapEntry
  (->schema' [{:keys [key type required?] :as entry} f]
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    [(let [kw (assoc key :key? true)
           description (some :description [key entry])
           optionalize (if (and (not required?)
                                (not (:open? key))
                                (seq (:values key)))
                         s/optional-key
                         #?(:cljs identity
                            :default (if description
                                       (fn [k]
                                         (if (instance? clojure.lang.IObj k)
                                           k
                                           (s/->RequiredKey k)))
                                       identity)))]
       (-> (optionalize (f kw))
           #?(:cljs identity
              :default (cond-> description (rs/describe description)))))
     (f type)])

  MapType
  (->schema' [{:keys [entries] :as dll} f]
    (describe
     (with-meta
       (reduce (fn [m [k v]]
                 (st/assoc m k v))
               {}
               (map f entries))
       (when (:name dll)
         {:name (symbol (:name dll))}))
     dll))

  ParameterListType
  (->schema' [{:keys [parameters]} f]
    (mapv f parameters))

  SequenceOfType
  (->schema' [{:keys [type] :as dll} f]
    (describe [(f type)] dll))

  SetOfType
  (->schema' [{:keys [type] :as dll} f]
    (describe #{(f type)} dll))

  SignatureType
  (->schema' [{:keys [parameters rest-parameter return] :as dll} f]
    (-> (let [parameters (f parameters)]
          (s/make-fn-schema (f return)
                            (if (some? rest-parameter)
                              [(conj parameters [(f rest-parameter)])]
                              [parameters])))
        (describe dll)))

  ;; Leaves

  AnythingType
  (->schema' [dll _]
    (describe s/Any dll))

  BooleanType
  (->schema' [{:keys [open? default] :as dll} _]
    (describe
     (match [open? default]
            [true  _] s/Bool
            [_     d] (s/enum d))
     dll))

  InstType
  (->schema' [dll _]
    (describe s/Inst dll))

  IntegerType
  (->schema' [{:keys [open? values] :as dll} _]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Int
            [_     nil] s/Int
            :else       (apply s/enum values))
     dll))

  KeywordType
  (->schema' [{:keys [key? open? values] :as dll} _]
    (let [kw-schema
          (match [key? open? (seq values)]
                 [_    true  _         ] s/Keyword
                 [_    _     nil       ] s/Keyword
                 [true false ([k] :seq)] k
                 :else                   (apply s/enum values))]
      (if key?
        kw-schema
        (describe kw-schema dll))))

  NumberType
  (->schema' [{:keys [open? values] :as dll} _]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Num
            [_     nil] s/Num
            :else       (apply s/enum values))
     dll))

  StringType
  (->schema' [{:keys [open? values] :as dll} _]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Str
            [_     nil] s/Str
            :else       (apply s/enum values))
     dll)))

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
