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

(defn maybe-key [{:keys [description key? open? values] :as node} opts base]
  (if key?
    (cond
      (= 1 (count values)) {:op :specific-key :schema (first values)}
      open? {:op :default-key :schema (m/schema base opts)}
      :else (throw (ex-info (format "Cannot convert schema %s to key" node)
                            {})))
    (-> (if (or open? (empty? values))
          base
          (into [:enum] (sort values)))
        (describe description)
        (m/schema opts))))

(extend-protocol MalliNode

  ;; Branches

  EitherType
  (->malli' [{:keys [choices tests key?]} opts]
    ;;TODO `choices` allows dispatch like :multi, but they're in the wrong format
    (let [f #(->malli' % opts)
          choice-schemas (map f choices)
          s (m/schema (into [:or] choice-schemas))]
      (if key?
        {:op :default-key :schema s}
        s)))

  MapEntry
  (->malli' [{:keys [key type required? key?] :as entry} opts]
    (assert (not key?))
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    (let [f #(->malli' % opts)
          optional? (and (not required?)
                         (not (:open? key))
                         (seq (:values key)))
          description (some :description [key entry])
          opts (cond-> nil
                 optional? (assoc :optional true)
                 description (assoc :json-schema/description description))
          default-or-specific-key (f (assoc key :key? true))]
      (case (:op default-or-specific-key)
        :specific-key (-> [(:schema default-or-specific-key)]
                          (cond-> opts (conj opts))
                          (conj (f type)))
        :default-key (-> [::m/default]
                         (cond-> opts (conj opts))
                         (conj
                           [:map-of
                            (:schema default-or-specific-key)
                            (f type)])))))

  MapType
  (->malli' [{:keys [description entries key?]} opts]
    (let [f #(->malli' % opts)
          s (-> (into [:merge] (map (fn [e] [:map (f e)])) entries)
                (m/schema opts)
                m/deref
                (describe description))]
      (if key?
        {:op :default-key :schema s}
        s)))

  ParameterListType
  (->malli' [{:keys [parameters key?]} opts]
    (assert (not key?))
    (let [f #(->malli' % opts)]
      (m/schema (into [:cat] (map f) parameters))))

  SequenceOfType
  (->malli' [{:keys [type key?]} opts]
    (let [f #(->malli' % opts)
          s (m/schema [:sequential (f type)])]
      (if key?
        {:op :default-key :schema s}
        s)))

  SetOfType
  (->malli' [{:keys [type key?]} opts]
    (let [f #(->malli' % opts)
          s (m/schema [:set (f type)])]
      (if key?
        {:op :default-key :schema s}
        s)))

  SignatureType
  (->malli' [{:keys [parameters rest-parameter return name key?]} opts]
    (let [f #(->malli' % opts)
          parameters (f parameters)
          parameters (if rest-parameter
                       [:cat parameters [:* (f rest-parameter)]]
                       parameters)
          s (m/schema [:=> parameters (f return)])]
      (if key?
        {:op :default-key :schema s}
        s)))

  ;; Leaves

  AnythingType
  (->malli' [{:keys [description key?]} opts]
    (if key?
      {:op :default-key :schema :any}
      (m/schema (describe :any description))))

  BooleanType
  (->malli' [{:keys [open? key? default description]} opts]
    (if key?
      (if open?
        {:op :default-key :schema :boolean}
        {:op :specific-key :schema default})
      (m/schema
        (describe
          (if open?
            :boolean
            [:enum (boolean default)])
          description)
        opts)))

  InstType
  (->malli' [{:keys [description key?]} opts]
    (let [s (m/schema (describe inst? description))]
      (if key?
        {:op :default-key :schema s}
        s)))

  IntegerType (->malli' [node opts] (maybe-key node opts :int))
  KeywordType (->malli' [node opts] (maybe-key node opts :keyword))
  NumberType  (->malli' [node opts] (maybe-key node opts number?))
  StringType  (->malli' [node opts] (maybe-key node opts :string)))

(def ^:private registry (merge (m/default-schemas) (mu/schemas)))

(defn ->malli
  "Convert a ctim schema to malli. Malli opts must contain a registry
  with support for :merge (usually via malli.util/schemas)."
  ([ctim-schema] (->malli ctim-schema {:registry registry}))
  ([ctim-schema opts]
   (-> ctim-schema
       (->malli' opts)
       (m/schema opts))))
