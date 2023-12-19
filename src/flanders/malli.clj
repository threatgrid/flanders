(ns flanders.malli
  (:require
    [clojure.core.match :refer [match]]
    [flanders.predicates :as fp]
    [flanders.types]
    [flanders.protocols :as prots]
    [flanders.utils :as fu]
    [malli.core :as m]
    [malli.util :as mu])
  (:import [flanders.types
            AnythingType BooleanType EitherType InstType
            IntegerType KeywordType MapEntry MapType
            NumberType ParameterListType SequenceOfType
            SetOfType SignatureType StringType]))

(defprotocol MalliNode
  (->malli' [node opts]))

(defn- describe [schema description]
  schema)

(extend-protocol MalliNode

  ;; Branches

  EitherType
  (->malli' [{:keys [choices tests]} opts]
    ;;TODO `choices` allows dispatch like :multi
    (let [f #(->malli' % opts)
          choice-schemas (map f choices)]
      (into [:or] choice-schemas)))

  MapEntry
  (->malli' [{:keys [key type required?] :as entry} opts]
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    (let [f #(->malli' % opts)
          optional? (and (not required?)
                         (not (:open? key))
                         (seq (:values key)))
          description (some :description [key entry])
          opts (cond-> nil
                 optional? (assoc :optional true)
                 description (assoc :json-schema/description description))]
      (-> [(f (assoc key :key? true))]
          (cond-> opts (conj opts))
          (conj (f type)))))

  MapType
  (->malli' [{:keys [description entries]} opts]
    (let [f #(->malli' % opts)]
      (describe
        (into [:merge] (map (fn [e] [:map (f e)])) entries)
        description)))

  ParameterListType
  (->malli' [{:keys [parameters]} opts]
    (let [f #(->malli' % opts)]
      (into [:cat] (map f) parameters)))

  SequenceOfType
  (->malli' [{:keys [type]} opts]
    (let [f #(->malli' % opts)]
      [:sequential (f type)]))

  SetOfType
  (->malli' [{:keys [type]} opts]
    (let [f #(->malli' % opts)]
      [:set (f type)]))

  SignatureType
  (->malli' [{:keys [parameters rest-parameter return name]} opts]
    (let [f #(->malli' % opts)
          parameters (f parameters)
          parameters (if rest-parameter
                       [:cat parameters [:* (f rest-parameter)]]
                       parameters)]
      [:=> parameters (f return)]))

  ;; Leaves

  AnythingType
  (->malli' [{:keys [description]} opts]
    (describe
     :any
     description))

  BooleanType
  (->malli' [{:keys [open? default description]} opts]
    (describe
     (match [open? default]
            [true  _] :boolean
            [_     d] [:enum d])
     description))

  InstType
  (->malli' [{:keys [description]} opts]
    (describe inst? description))

  IntegerType
  (->malli' [{:keys [description open? values]} opts]
    (describe
     (match [open? (seq values)]
            [true  _  ] :int
            [_     nil] :int
            :else       (into [:enum] values))
     description))

  KeywordType
  (->malli' [{:keys [description key? open? values] :as node} opts]
    (let [kw-schema
          (match [key? open? (seq values)]
                 [_    true  _         ] :keyword
                 [_    _     nil       ] :keyword
                 [true false ([k] :seq)] k
                 :else                   (into [:enum] values))]
      (if key?
        kw-schema
        (describe kw-schema description))))

  NumberType
  (->malli' [{:keys [description open? values]} opts]
    (describe
     (match [open? (seq values)]
            [true  _  ] number?
            [_     nil] number?
            :else       (into [:enum] values))
     description))

  StringType
  (->malli' [{:keys [description open? values]} opts]
    (describe
     (match [open? (seq values)]
            [true  _  ] :string
            [_     nil] :string
            :else       (into [:enum] values))
     description)))

(def ^:private registry (merge (m/default-schemas) (mu/schemas)))

(defn ->malli
  "Convert a ctim schema to malli. Malli opts must contain a registry
  with support for :merge (usually via malli.util/schemas)."
  ([ctim-schema] (->malli ctim-schema {:registry registry}))
  ([ctim-schema opts]
   (-> ctim-schema
       (->malli' opts)
       (m/schema opts))))
