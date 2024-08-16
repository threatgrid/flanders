(ns flanders.malli
  "Requires malli as a dependency."
  (:refer-clojure :exclude [type])
  (:require
    [flanders.types]
    [flanders.example :as example]
    [malli.core :as m]
    [malli.util :as mu])
  (:import [flanders.types
            AnythingType BooleanType EitherType InstType
            IntegerType KeywordType MapEntry MapType
            NumberType ParameterListType SequenceOfType
            SetOfType SignatureType StringType]))

(defprotocol MalliNode
  (->malli' [node opts]))

(defn- describe [?schema {:keys [description] :as dll} opts]
  (-> ?schema
      (m/schema opts)
      (mu/update-properties
        #(-> %
             (assoc :json-schema/example (example/->example-tree dll))
             (cond-> description (assoc :json-schema/description description))))))

(defn maybe-key [{:keys [key? open? values] :as dll} opts base]
  (if key?
    (cond
      (= 1 (count values)) {:op :specific-key :schema (first values)}
      open? {:op :default-key :schema (m/schema base opts)}
      :else (throw (ex-info (format "Cannot convert schema %s to key" dll)
                            {})))
    (-> (if (or open? (empty? values))
          base
          (into [:enum] (sort values)))
        (describe dll opts))))

;; Note: we use m/schema eagerly mostly for prettier map schemas.
;; This won't work if flanders supports recursive schemas.
(extend-protocol MalliNode

  ;; Branches

  EitherType
  (->malli' [{:keys [choices key?]} opts]
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
          props (cond-> {:json-schema/example (example/->example-tree type)}
                  optional? (assoc :optional true)
                  description (assoc :json-schema/description description))
          default-or-specific-key (f (assoc key :key? true))]
      (case (:op default-or-specific-key)
        :specific-key (-> [(:schema default-or-specific-key)]
                          (cond-> props (conj props))
                          (conj (f type)))
        :default-key (-> [::m/default]
                         (cond-> props (conj props))
                         (conj
                           [:map-of
                            (:schema default-or-specific-key)
                            (f type)])))))

  MapType
  (->malli' [{:keys [entries key?] :as dll} opts]
    (let [f #(->malli' % opts)
          s (-> (into [:merge] (map (fn [e] [:map (f e)])) entries)
                (m/schema opts)
                m/deref ;; eliminate :merge
                (mu/update-properties assoc :closed true)
                (describe dll opts))]
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
  (->malli' [{:keys [parameters rest-parameter return key?]} opts]
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
  (->malli' [{:keys [key?] :as dll} opts]
    (if key?
      {:op :default-key :schema :any}
      (describe :any dll opts)))

  BooleanType
  (->malli' [{:keys [default open? key?] :as dll} opts]
    (if key?
      (if open?
        {:op :default-key :schema :boolean}
        {:op :specific-key :schema default})
      (-> (if open?
            :boolean
            [:= (boolean default)])
          (describe dll opts))))

  InstType
  (->malli' [{:keys [key?] :as dll} opts]
    (let [s (describe inst? dll opts)]
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
