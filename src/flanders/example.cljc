(ns flanders.example
  (:require
   [flanders.core :as f]
   #?(:clj  [flanders.types :as ft]
      :cljs [flanders.types
             :as ft
             :refer [AnythingType BooleanType EitherType InstType IntegerType
                     KeywordType MapEntry MapType NumberType RefType SequenceOfType
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
            RefType
            SequenceOfType
            SetOfType
            SignatureType
            StringType]
           [java.util Date])))

(defprotocol JsonExampleNode
  (->example [node f opts]))

(defonce unreachable (Object.))
(defn unreachable? [v] (identical? unreachable v))

(extend-protocol JsonExampleNode

  ;; Branches

  EitherType
  (->example [{:keys [choices default] :as node} f _]
    (if (some? default)
      default
      (if-some [[example] (->> choices
                               (map f)
                               (remove unreachable?)
                               seq)]
        example
        (throw (ex-info (str "Cannot create example: " (pr-str node)))))))

  MapEntry
  (->example [{:keys [key type default required?]} f _]
    (let [e [(f (assoc key :key? true))
             (f (cond-> type
                  (some? default) (assoc :default default)))]]
      (if (some unreachable? e)
        (when required?
          unreachable)
        e)))

  MapType
  (->example [{:keys [entries default]} f _]
    (if (some? default)
      default
      (reduce (fn [m e]
                (if (unreachable? e)
                  (reduced unreachable)
                  (conj m e)))
              {} (keep f entries))))

  SequenceOfType
  (->example [{:keys [type default]} f _]
    (if (some? default)
      default
      (let [inner (f type)]
        (if (unreachable? inner)
          []
          [inner]))))

  SetOfType
  (->example [{:keys [type default]} f _]
    (if (some? default)
      default
      (let [inner (f type)]
        (if (unreachable? inner)
          #{}
          #{inner}))))

  ;; Leaves

  AnythingType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      "anything"))

  BooleanType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      true))

  InstType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      #?(:clj  (Date. 1451610061000)
         :cljs (js/date. 1451610061000))))

  IntegerType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      10))

  KeywordType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      :keyword))

  NumberType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      10.0))

  StringType
  (->example [{:keys [default]} _ _]
    (if (some? default)
      default
      "string"))

  SignatureType
  (->example [{:keys [parameters rest-parameter return]} f _]
    (let [arguments (mapv
                     (fn [i parameter]
                       [(str "arg" i) (f parameter)])
                     (range)
                     (:parameters parameters))
          arguments (if (some? rest-parameter)
                      (conj arguments ["argN" (f rest-parameter)])
                      arguments)]
      {:arguments (into {} arguments)
       :returns (f return)}))

  RefType
  (->example [{:keys [id default] :as node} f {::f/keys [registry] ::keys [seen] :as opts}]
    (if (some? default)
      default
      (if (contains? seen id)
        ;; heuristic to make examples finite but also informative.
        ;; detecting true cycles with dynamic scope is more involved (see notes in malli.generators)
        unreachable
        (f (or (get registry id)
               (throw (ex-info (format "Ref not in scope: %s" (pr-str id))
                               {:registry registry
                                :trace (::f/trace opts)})))
           (update opts ::seen (fnil conj #{}) id))))))

;; This is a fast implementation of making an example, but it could be better
;; - It could take advantage of generators (to be non-deterministic)
;; - It could use zippers, so that generators could be location aware
;; - Examples should be semantically valuable (should make sense in our domain)
(defn ->example-tree'
  "Get a JSON example for a DDL node"
  ([ddl] (->example-tree' ddl nil))
  ([ddl opts]
   (let [opts (-> opts
                  (update ::f/registry (fnil into {}) (::f/registry ddl))
                  (update ::f/trace (fnil conj []) (gensym "flanders.example")))]
     (->example ddl
                (fn
                  ([ddl] (->example-tree' ddl opts))
                  ([ddl opts] (->example-tree' ddl opts)))
                opts))))

(defn ->example-tree
  "Get a JSON example for a DDL node"
  ([ddl] (->example-tree ddl nil))
  ([ddl opts]
   (let [e (->example-tree' ddl opts)]
     (if (unreachable? e)
       (throw (ex-info (str "Cannot create example, please add default: " (pr-str ddl))))
       e))))
