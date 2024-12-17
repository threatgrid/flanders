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


(defn- create-defs [{::f/keys [registry] :as f} opts]
  (prn "create-defs" registry (::f/registry opts))
  (when (seq registry)
    (let [temp-ns (create-ns (symbol (str "flanders.json-schema.schema."
                                          ;; helps sort schemas during unit testing.
                                          (stable-sortable-ns-segment)
                                          "."
                                          (str (random-uuid)))))
          _ (alter-meta! temp-ns assoc :doc (str "Helper namespace to generate the following JSON Schema:\n"
                                                 #_ ;;TODO find original schema
                                                 (with-out-str (pp/pprint json-schema))))
          _ (run! #(ns-unmap temp-ns %) (keys (ns-map temp-ns)))
          _ (some-> (::gc opts) (swap! (fnil conj []) conj #(remove-ns temp-ns)))
          nstr (name (ns-name temp-ns))
          def-ids (sort (keys registry))
          def-vars (mapv #(intern temp-ns (def-id->var-sym %)) def-ids)
          _ (assert (or (empty? def-vars)
                        (apply distinct? def-vars)))
          def-ids->def-vars (zipmap def-ids def-vars)
          opts (update opts ::f/registry (fnil into {}) (zipmap def-ids (mapv s/recursive def-vars)))]
      (into {} (map (fn [[def-id def-var]]
                      {:pre [(var? def-var)]}
                      (let [_generated-schema (let [f (get registry def-id)
                                                    _ (assert f def-id)
                                                    frm `(s/defschema ~(-> def-var symbol name symbol)
                                                           ~(str "JSON Schema id: " def-id "\n")
                                                           ~(->schema f opts))]
                                                (binding [*ns* temp-ns]
                                                  (eval frm)))
                            f (get registry def-id)]
                        [def-id (s/recursive def-var)])))
            def-ids->def-vars))))

(defn ->schema
  ([node] (->schema node nil))
  ([node opts]
   (let [f (fn ->schema
             ([node] (->schema node opts))
             ([node opts]
              (let [opts (-> opts
                             (update ::f/registry (fnil into {}) (::f/registry node))
                             (update ::ref->var (fnil into {}) (create-defs node opts)))]
                (->schema' node
                           (fn
                             ([node] (->schema node opts))
                             ([node opts] (->schema node opts)))
                           opts))))]
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

;; ## Identifying schema recursion
;;
;; Note, this text is adapted from malli.generator: https://github.com/metosin/malli/blob/440de1b56d21a64d845314fe968a9e1511855a0f/src/malli/generator.cljc#L235
;; Flanders refs work like Malli and JSON Schema refs.
;; 
;; Refs are uniquely identified by their paired name and scope. If we see a ref with the
;; same name and scope as another ref we've dereferenced previously, we know that this is a recursion
;; point back to the previously seen ref. The rest of this section explains why.
;; 
;; Refs resolve via dynamic scope, which means its dereferenced value is the latest binding found
;; while expanding the schema until the point of finding the ref.
;; This makes the (runtime) scope at the ref's location part of a ref's identity---if the scope
;; is different, then it's (possibly) not the same ref because scope determines how schemas
;; transitively expand.
;;
;; To illustrate why a ref's name is an insufficient identifier, here is a schema that is equivalent to `[:= 42]`:
;; 
;;   [:schema {:registry {::a [:schema {:registry {::a [:= 42]}}
;;                             ;; (2)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identify refs just by name, we would have incorrectly detected (2) to be an (infinitely expanding) recursive
;; reference.
;;
;; In studying the previous example, we might think that since (1) and (2) deref to different schemas, it might sufficient to identify refs just by their derefs.
;; Unfortunately this just pushes the problem elsewhere.
;;
;; For example, here is another schema equivalent to `[:= 42]`:
;;
;;   [:schema {:registry {::a [:ref ::b] ;; (2)
;;                        ::b [:schema {:registry {::a [:ref ::b] ;; (4)
;;                                                 ::b [:= 42]}}
;;                             ;; (3)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identified ::a by its deref, it would look like (3) deref'ing to (4)
;; is a recursion point after witnessing (1) deref'ing to (2), since (2) == (4). Except this
;; is wrong since it's a different ::b at (2) and (4)! OTOH, if we identified (2) and (4) with their
;; dynamic scopes along with their form, they would be clearly different. Indeed, this
;; is another way to identify refs: pairing their derefs with their deref's scopes.
;; It is slightly more direct to use the ref's direct name and scope, which is why
;; we choose that identifier. The more general insight is that any schema is identified by its form+scope
;; (note: but only after trimming the scope of irrelevant bindings, see next pararaph).
;; That insight may be useful for detecting recursion at places other than refs.
;; 
;; Ref identifiers could be made smarter by trimming irrelevant entries in identifying scope.
;; Not all scope differences are relevant, so generators may expand more than strictly necessary
;; in the quest to find the "same" ref schema again. It could skip over refs that generate exactly the
;; same values, but their scopes are uninterestingly different (eg., unused bindings are different).
;;
;; For example, the following schema is recursive "in spirit" between (1) and (2), but since ::b
;; changes, the scope will differ, so the recursion will be detected between (2) and itself instead
;; (where the scope is constant):
;;
;;   [:schema {:registry {::a [:schema {:registry {::b :boolean}}
;;                             ;; (2)
;;                             [:or [:ref ::a] [:ref ::b]]]}}
;;    [:schema {:registry {::b :int}}
;;     ;; (1)
;;     [:or [:ref ::a] [:ref ::b]]]]

(defn- identify-ref-type [node]
  {:pre [(instance? RefType node)]}
  {:scope (::f/registry node)
   :name (:id node)})

;; we need to map json schema's dynamically scoped refs onto s/defschema.
;; we accomplish this by unfolding refs. each unique level of refs has its
;; own set of defalias's. we tie the knot when a loop is detected (when 
;; we have already seen this dynamic scope).
;; this mapping is analogous to malli.generator's mapping from dynamic refs
;; to test.check generators (in particular, its support for tying the knot for recursive-gen).
(defn- ref->schema [{:keys [id] :as dll} {::f/keys [registry] :as opts}]
  (let [ref-id (identify-ref-type dll)]
    (-> (or (force (get-in opts [::rec-schema ref-id]))
            (get registry id)
            (throw (ex-info (format "Cannot resolve ref: %s" id) {})))
        (describe dll opts))))

(extend-protocol SchemaNode

  ;; Branches

  EitherType
  (->schema' [{:keys [choices tests] :as dll} f opts]
    (-> (let [choice-schemas (map f choices)]
          (if (empty? tests)
            (apply s/cond-pre choice-schemas)
            (apply s/conditional (mapcat vector tests choice-schemas))))
        (describe dll opts)))

  MapEntry
  (->schema' [{:keys [key type required?] :as entry} f _]
    (assert (some? type) (str "Type nil for MapEntry with key " key))
    (assert (some? key) (str "Key nil for MapEntry with type " type))
    (let [description (some :description [key entry type])
          default (first (keep :default [entry type]))]
      [((if (and (not required?)
                 (not (:open? key))
                 (seq (:values key)))
          s/optional-key
          identity)
        (f (assoc key :key? true)))
       (f (cond-> type
            ;; TODO ideally we would attach these to the key, but this is unreliable.
            ;; for starters, st/optional-keys and any related operations clears the metadata.
            description (assoc :description description)
            (some? default) (assoc :default default)))]))

  MapType
  (->schema' [{:keys [entries] :as dll} f opts]
    (describe
     (with-meta
       (reduce (fn [m [k v]]
                 (st/assoc m k v))
               {}
               (map f entries))
       (when (:name dll)
         {:name (symbol (:name dll))}))
     dll
     opts))

  ParameterListType
  (->schema' [{:keys [parameters]} f _]
    (mapv f parameters))

  SequenceOfType
  (->schema' [{:keys [type] :as dll} f opts]
    (describe [(f type)] dll opts))

  SetOfType
  (->schema' [{:keys [type] :as dll} f opts]
    (describe #{(f type)} dll opts))

  SignatureType
  (->schema' [{:keys [parameters rest-parameter return] :as dll} f opts]
    (-> (let [parameters (f parameters)]
          (s/make-fn-schema (f return)
                            (if (some? rest-parameter)
                              [(conj parameters [(f rest-parameter)])]
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

  RefType (->schema' [dll _ opts] (ref->schema dll opts)))

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
