(ns flanders.malli
  "Requires malli as a dependency."
  (:refer-clojure :exclude [type])
  (:require
    [flanders.core :as f]
    [flanders.types]
    [flanders.example :as example]
    [flanders.malli.utils :refer [describe]]
    [malli.core :as m]
    [malli.util :as mu])
  (:import [flanders.types
            AnythingType BooleanType EitherType InstType
            IntegerType KeywordType MapEntry MapType
            NumberType ParameterListType RefType SequenceOfType
            SetOfType SignatureType StringType]))

(def ^:private eval-protocol? (not (resolve 'MalliNode)))
(when eval-protocol?
  (defprotocol MalliNode
    (->malli' [node opts])))

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
  (->malli' [{:keys [choices key?]} {::keys [->malli] :as opts}]
    ;;TODO `choices` allows dispatch like :multi, but they're in the wrong format
    (let [choice-schemas (map ->malli choices)
          s (m/schema (into [:or] choice-schemas) opts)]
      (if key?
        {:op :default-key :schema s}
        s)))

  MapEntry
  (->malli' [{:keys [key type required? key?] :as entry} {::keys [->malli] :as opts}]
    (assert (not key?))
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    (let [optional? (and (not required?)
                         (not (:open? key))
                         (seq (:values key)))
          description (some :description [key entry])
          props (cond-> {:json-schema/example (example/->example-tree type)}
                  optional? (assoc :optional true)
                  description (assoc :json-schema/description description))
          default-or-specific-key (->malli (assoc key :key? true))]
      (case (:op default-or-specific-key)
        :specific-key (-> [(:schema default-or-specific-key)]
                          (cond-> props (conj props))
                          (conj (->malli type)))
        :default-key (-> [::m/default]
                         (cond-> props (conj props))
                         (conj
                           [:map-of
                            (:schema default-or-specific-key)
                            (->malli type)])))))

  MapType
  (->malli' [{:keys [entries key?] :as dll} {::keys [->malli] :as opts}]
    (let [s (-> (case (count entries)
                  ;; :merge has problems with 1 and 0 children https://github.com/metosin/malli/pull/1147
                  0 [:merge (m/schema :map opts)]
                  1 [:merge (m/schema [:map (->malli (first entries))] opts)]
                  (into [:merge] (map (fn [e] [:map (->malli e)])) entries))
                (m/schema opts)
                m/deref ;; eliminate :merge
                (m/-update-properties assoc :closed true)
                (describe dll opts))]
      (if key?
        {:op :default-key :schema s}
        s)))

  ParameterListType
  (->malli' [{:keys [parameters key?]} {::keys [->malli] :as opts}]
    (assert (not key?))
    (m/schema (into [:cat] (map ->malli) parameters) opts))

  SequenceOfType
  (->malli' [{:keys [type key?]} {::keys [->malli] :as opts}]
    (let [s (m/schema [:sequential (->malli type)] opts)]
      (if key?
        {:op :default-key :schema s}
        s)))

  SetOfType
  (->malli' [{:keys [type key?]} {::keys [->malli] :as opts}]
    (let [s (m/schema [:set (->malli type)] opts)]
      (if key?
        {:op :default-key :schema s}
        s)))

  SignatureType
  (->malli' [{:keys [parameters rest-parameter return key?]} {::keys [->malli] :as opts}]
    (let [parameters (->malli parameters)
          parameters (if rest-parameter
                       [:cat parameters [:* (->malli rest-parameter)]]
                       parameters)
          s (m/schema [:=> parameters (->malli return)] opts)]
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
  StringType  (->malli' [node opts] (maybe-key node opts :string))
  RefType (->malli' [{:keys [id] :as node} opts] (-> [:ref id] (describe node opts))))

(def default-opts {:registry (merge (m/default-schemas) (mu/schemas))})

(defn ->malli
  "Convert a ctim schema to malli. Malli opts must contain a registry
  with support for :merge (usually via malli.util/schemas)."
  ([node] (->malli node nil))
  ([node opts]
   (let [opts (or opts default-opts)
         ->malli (fn ->malli
                   ([node] (->malli node opts))
                   ([{::f/keys [registry] :as node} opts]
                    (let [vopts (volatile! nil)
                          opts (-> opts
                                   (update ::f/registry (fnil into {}) registry)
                                   (assoc ::->malli (fn
                                                      ([node] (->malli node @vopts))
                                                      ([node opts] (->malli node opts)))))
                          _ (vreset! vopts opts)
                          c (->malli' node opts)]
                      (cond-> c
                        (seq registry) (m/-update-properties update :registry
                                                             (fn [prev]
                                                               (assert (not prev) ":registry already exists")
                                                               (into (sorted-map) (update-vals registry #(->malli % opts)))))))))]
     (-> node
         (->malli (assoc opts ::m/allow-invalid-refs true))
         (m/schema opts)))))
