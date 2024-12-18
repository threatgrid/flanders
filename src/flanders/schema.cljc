(ns flanders.schema
  {:clj-kondo/ignore true}
  (:refer-clojure :exclude [type key])
  (:require
   [clojure.string :as str]
   #?(:clj  [clojure.core.match :refer [match]]
      :cljs [cljs.core.match :refer-macros [match]])
   #?(:clj  [flanders.types]
      :cljs [flanders.types
             :refer [AnythingType BooleanType EitherType InstType IntegerType
                     KeywordType MapEntry MapType NumberType ParameterListType
                     RefType SequenceOfType SetOfType SignatureType StringType]])
   #?(:clj [ring.swagger.json-schema :as rs])
   [flanders.core :as f]
   [flanders.predicates :as fp]
   [flanders.schema.utils :refer [describe]]
   [flanders.utils :as fu]
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
            RefType
            SequenceOfType
            SetOfType
            SignatureType
            StringType])))

(declare ->schema)

(defprotocol SchemaNode
  (->schema' [node f opts]))

(defn def-id->var-sym [defstr]
  (symbol (namespace-munge (munge (str/replace defstr "." "_DOT_")))))

(comment
  (= ;"Logger"
     'https_COLON__SLASH__SLASH_schema.ocsf.io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_logger
     (def-id->var-sym
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/"
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/logger"))
  (= "KeyboardInfo"
     (def-id->var-sym
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/"
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/keyboard_info"))
  )

;; stable ns sorting for tests

(def max-nano-digits 18)
(def max-nano (long (Math/pow 10 max-nano-digits)))
(def nano-padder (str "%0" max-nano-digits "d"))

(defn unique-nano []
  (let [v (System/nanoTime)]
    (loop []
      (let [v' (System/nanoTime)]
        (if (= v (System/nanoTime))
          (recur)
          v')))))

;; assumes schema conversion is single threaded
;; another idea is hash the $defs and use that in the ns name
(defn stable-sortable-ns-segment []
  (let [n (unique-nano)]
    (when (>= n max-nano)
      (binding [*err* *out*]
        ;; just affects unit tests. also won't happen for a long time
        (println "WARNING: Please increment max-nano-digits for unit test stability")))
    (format nano-padder n)))

(defn ->schema
  ([node] (->schema node nil))
  ([node opts]
   (let [f (fn ->schema [node opts]
             (let [opts (-> opts
                            (update ::schema-level #(cond-> (or % [])
                                                      (fu/progress-schema? node) (conj node)))
                            (update ::f/registry (fnil into {}) (::f/registry node)))
                   temp-ns (delay (create-ns (symbol (str "flanders.json-schema.schema."
                                                          ;; helps sort schemas during unit testing.
                                                          (stable-sortable-ns-segment)
                                                          "."
                                                          (str (random-uuid))))))
                   id->var (into {} (map (fn [id]
                                           [id (delay (intern @temp-ns (def-id->var-sym id)))]))
                                 (keys (::f/registry node)))
                   ref-id->recursive (into {} (map (fn [id]
                                                     [(fu/identify-ref-type (f/ref id) opts) (delay (s/recursive @(id->var id)))]))
                                           (keys (::f/registry node)))
                   ref-id->rec-schema (into {} (map (fn [[id s]]
                                                      (let [ref-id (fu/identify-ref-type (f/ref id) opts)]
                                                        [ref-id
                                                         (delay
                                                           (let [temp-ns @temp-ns
                                                                 def-var @(id->var id)
                                                                 _generated-schema (let [frm `(s/defschema ~(-> def-var symbol name symbol)
                                                                                                ~(str "JSON Schema id: " id "\n")
                                                                                                ~(->schema s (update opts ::rec-schema ref-id->recursive)))]
                                                                                     (binding [*ns* temp-ns]
                                                                                       (eval frm)))]
                                                             @(ref-id->recursive ref-id)))])))
                                        (::f/registry node))
                   opts (update opts ::rec-schema (fnil into {}) ref-id->rec-schema)
                   s (->schema' node ->schema opts)
                   ;;force reachable defschema's
                   _ (doseq [[id d] id->var
                             :when (realized? d)]
                       @(ref-id->rec-schema (fu/identify-ref-type (f/ref id) opts)))]
               s))]
     (f node opts))))

#?(:clj (defn ->schema+clean
          "Like ->schema except makes allocated memory collectable if result is eligible for garbage collection."
          [json-schema opts]
          (let [gc (atom [])
                _ (assert (not (::gc opts)))
                s (->schema json-schema {::gc gc})]
            (.register (java.lang.ref.Cleaner/create) s (fn [] (run! #(%) @gc)))
            s)))

(def ^:deprecated ->schema-tree ->schema)

(def get-schema
  (memoize ->schema))

(defn- ref->schema [{:keys [id] :as dll} f {::f/keys [registry] ::keys [schema-level rec-ref-levels] :as opts}]
  (assert (string? id))
  (let [ref-id (fu/identify-ref-type dll opts)
        _ (when (get-in rec-ref-levels [ref-id schema-level])
            ;; made no progress when expanding recursive schema
            ;; e.g., (defschema A (s/either A s/Int))
            (throw (ex-info (str "Infinite schema detected: " (pr-str dll)) {})))
        opts (assoc-in opts [::rec-ref-levels ref-id schema-level] true)
        temp-ns (delay (create-ns (symbol (str "flanders.json-schema.schema."
                                               ;; helps sort schemas during unit testing.
                                               (stable-sortable-ns-segment)
                                               "."
                                               (str (random-uuid))))))]
    (-> (or (force (get-in opts [::rec-schema ref-id]))
            (let [s (or (get registry id)
                        (throw (ex-info (format "Ref not in scope: %s" (pr-str id)) {})))
                  d (delay
                      (let [temp-ns @temp-ns
                            def-var (intern temp-ns (def-id->var-sym id))
                            rec-schema (s/recursive def-var)
                            _generated-schema (let [frm `(s/defschema ~(-> def-var symbol name symbol)
                                                           ~(str "JSON Schema id: " id "\n")
                                                           ~(->schema s (assoc-in opts [::rec-schema ref-id] (delay rec-schema))))]
                                                (binding [*ns* temp-ns]
                                                  (eval frm)))]
                        (s/recursive def-var)))
                  res (f s (assoc-in opts [::rec-schema ref-id] d))]
              (if true #_(realized? d)
                ;; always generate defschema's to save memory when generating validators
                @d
                res)))
        (describe dll opts))))

(extend-protocol SchemaNode

  ;; Branches

  EitherType
  (->schema' [{:keys [choices tests] :as dll} f opts]
    (-> (let [choice-schemas (map #(f % opts) choices)]
          (if (empty? tests)
            ;; FIXME choice-schemas must be mutually exclusive for cond-pre to work.
            (apply s/cond-pre choice-schemas)
            (apply s/conditional (mapcat vector tests choice-schemas))))
        (describe dll opts)))

  MapEntry
  (->schema' [{:keys [key type required?] :as entry} f opts]
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    (let [description (some :description [key entry type])
          default (first (keep :default [entry type]))]
      [((if (and (not required?)
                 (not (:open? key))
                 (seq (:values key)))
          s/optional-key
          identity)
        (f (assoc key :key? true) opts))
       (f (cond-> type
            ;; TODO ideally we would attach these to the key, but this is unreliable.
            ;; for starters, st/optional-keys and any related operations clears the metadata.
            description (assoc :description description)
            (some? default) (assoc :default default))
          opts)]))

  MapType
  (->schema' [{:keys [entries] :as dll} f opts]
    (describe
     (with-meta
       (reduce (fn [m [k v]]
                 (st/assoc m k v))
               {}
               (map #(f % opts) entries))
       (when (:name dll)
         {:name (symbol (:name dll))}))
     dll
     opts))

  ParameterListType
  (->schema' [{:keys [parameters]} f opts]
    (mapv #(f % opts) parameters))

  SequenceOfType
  (->schema' [{:keys [type] :as dll} f opts]
    (describe [(f type opts)] dll opts))

  SetOfType
  (->schema' [{:keys [type] :as dll} f opts]
    (describe #{(f type opts)} dll opts))

  SignatureType
  (->schema' [{:keys [parameters rest-parameter return] :as dll} f opts]
    (-> (let [parameters (f parameters opts)]
          (s/make-fn-schema (f return opts)
                            (if (some? rest-parameter)
                              [(conj parameters [(f rest-parameter opts)])]
                              [parameters])))
        (describe dll opts)))

  ;; Leaves

  AnythingType
  (->schema' [dll _ opts]
    (describe s/Any dll opts))

  BooleanType
  (->schema' [{:keys [open? default] :as dll} _ opts]
    (describe
     (match [open? default]
            [true  _] s/Bool
            [_     d] (s/enum d))
     dll
     opts))

  InstType
  (->schema' [dll _ opts]
    (describe s/Inst dll opts))

  IntegerType
  (->schema' [{:keys [open? values] :as dll} _ opts]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Int
            [_     nil] s/Int
            :else       (apply s/enum values))
     dll
     opts))

  KeywordType
  (->schema' [{:keys [key? open? values] :as dll} _ opts]
    (let [kw-schema
          (match [key? open? (seq values)]
                 [_    true  _         ] s/Keyword
                 [_    _     nil       ] s/Keyword
                 [true false ([k] :seq)] k
                 :else                   (apply s/enum values))]
      (if key?
        kw-schema
        (describe kw-schema dll opts))))

  NumberType
  (->schema' [{:keys [open? values] :as dll} _ opts]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Num
            [_     nil] s/Num
            :else       (apply s/enum values))
     dll
     opts))

  StringType
  (->schema' [{:keys [open? values] :as dll} _ opts]
    (describe
     (match [open? (seq values)]
            [true  _  ] s/Str
            [_     nil] s/Str
            :else       (apply s/enum values))
     dll
     opts))

  RefType (->schema' [dll f opts] (ref->schema dll f opts)))

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
