(ns flanders.json-schema
  (:refer-clojure :exclude [type])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [flanders.core :as f]
            [schema.core :as s]
            [flanders.example :as fe]
            [clojure.walk :as w]))

(defn- -normalize
  "normalize to string"
  [k opts]
  (cond
    (keyword? k) (-> k symbol str)
    (symbol? k) (str k)
    (string? k) k
    :else (throw (ex-info (str "Cannot normalize at " (pr-str (::path opts))) {:k k :opts opts}))))

(defn resolve-id [id {::keys [base-id]}]
  (assert (string? base-id))
  (cond
    (str/starts-with? id "#") (str/replace-first id "#" base-id)
    ;; TODO absolute ids
    :else (throw (ex-info (str "Unresolvable id: " id) {:id id}))))

(comment
  (resolve-id "#/$defs/fingerprint" {::base-id "https://schema.ocsf.io/schema/classes/security_finding"})
  (resolve-id "#" {::base-id "https://schema.ocsf.io/schema/classes/security_finding"})
  )

(defn absolute-id [{::keys [base-id path] :as opts}]
  (assert base-id)
  (assert (every? string? path))
  ;;FIXME don't think # should be inserted
  (str base-id (when (seq path) (str \/ (str/join "/" path)))))

(defn conj-path [opts & path-elements]
  (update opts ::path (fnil into [])
          (map #(do (assert (string? %) (str "Bad path element: " (pr-str %)))
                    %))
          path-elements))

(comment
  (absolute-id {::base-id "https://schema.ocsf.io/schema/classes/security_finding"
                ::path ["$defs" "fingerprint"]})
  )

(defn normalize-map [m opts]
  (assert (map? m))
  (let [seen (volatile! #{})]
    (update-keys m (fn [k]
                     (let [n (-normalize k (conj-path opts ""))]
                       (when (@seen n)
                         (throw (ex-info "Clash while normalizing" {:m m :k k :n n})))
                       (vswap! seen conj n)
                       n)))))

(defn unknown-schema! [v {::keys [path] :as opts}]
  (throw (ex-info (format "Unknown JSON Schema at path %s: %s" (pr-str path) (pr-str v)) {:v v :opts (select-keys opts [::path ::base-id])})))

(defn unsupported-schema! [reason v {::keys [path] :as opts}]
  (throw (ex-info (format "Unsupported JSON Schema: %s" reason)
                  {::unsupported true
                   :v v :opts (select-keys opts [::path ::base-id])})))

(declare ->flanders)

(def ^:private object-properties ["properties" "patternProperties" "additionalProperties"])
(def ^:private array-properties ["prefixItems" "contains" "items"])
(def ^:private applicator-properties ["if" "then" "else" "oneOf" "anyOf" "allOf" "not"])

(defn- check-unsupported-keys! [v opts]
  (doseq [k ["$anchor"
             "$comment"
             ;"$defs"
             "$dynamicAnchor"
             "$dynamicRef"
             ;"$id"
             ;"$ref"
             ;"$schema"
             "$vocabulary"
             ;"additionalProperties"
             ;"allOf"
             ;"anyOf"
             ;"const"
             "contains"
             "contentEncoding"
             "contentMediaType"
             "contentSchema"
             "default"
             "dependentRequired"
             "dependentSchemas"
             "deprecated"
             ;"description"
             "else"
             ;"enum"
             "examples"
             "exclusiveMaximum"
             "exclusiveMinimum"
             ;"format"
             "if"
             ;"items"
             "maxContains"
             "maximum"
             "maxItems"
             "maxLength"
             "maxProperties"
             "minContains"
             "minimum"
             "minItems"
             "minLength"
             "minProperties"
             "multipleOf"
             "not"
             "oneOf"
             "pattern"
             "patternProperties"
             "prefixItems"
             ;"properties"
             "propertyNames"
             "readOnly"
             ;"required"
             "then"
             ;"title"
             ;"type"
             "unevaluatedItems"
             "unevaluatedProperties"
             "uniqueItems"
             "writeOnly"]]
    (when (contains? v k)
      (unsupported-schema! (str "Unsupported JSON Schema keyword: " k) v opts))))

(defn- parse-map [v opts]
  (let [{:strs [description title example $schema $ref $anchor $defs $dynamicAnchor $dynamicRef $id $vocabulary] :as v} (normalize-map v opts)
        {:strs [type] :as v} (if (contains? v "type")
                               v
                               (cond
                                 (some #(contains? v %) object-properties) (assoc v "type" "object")
                                 (some #(contains? v %) array-properties) (assoc v "type" "array")
                                 :else v))
        _ (check-unsupported-keys! v opts)
        opts (update opts ::dialect #(or $schema %))
        _ (assert (nil? $anchor)) ;; TODO
        _ (assert (nil? $dynamicAnchor)) ;; TODO
        _ (assert (nil? $dynamicRef)) ;; TODO
        opts (update opts ::base-id (fn [parent-id]
                                      ;; TODO subschemas start new id https://json-schema.org/draft/2020-12/json-schema-core#section-8.2.1
                                      ;; need to reset ::path if we support this
                                      (when (and parent-id $id)
                                        (unsupported-schema! "Nested $id not yet supported" v opts))
                                      (or $id parent-id
                                          ;; TODO Establishing a Base URI https://www.rfc-editor.org/rfc/rfc3986.html#section-5.1
                                          #_(throw (ex-info "Must supply $id" {})))))
        $defs (some-> $defs (normalize-map opts) not-empty)
        local-defs (not-empty
                     (into {} (map (fn [[k v]]
                                     (let [opts (conj-path opts "$defs" k)]
                                       [(absolute-id opts) (->flanders v opts)])))
                           $defs))
        base (or (when $ref
                   (let [this-id (resolve-id $ref opts)]
                     (assoc (f/ref this-id)
                            ;;TODO rename or remove, for debugging purposes (e.g., defalias strings)
                            :v v)))
                 (when-some [disjuncts (get v "anyOf")]
                   (f/either :choices (into [] (comp (map (fn [d]
                                                            (cond-> d
                                                              (and (map? d)
                                                                   (not (contains? v "type"))
                                                                   type)
                                                              (assoc "type" type))))
                                                     (map-indexed #(->flanders %2 (conj-path opts "anyOf" (str %1)))))
                                            disjuncts)))
                 (when-some [conjuncts (get v "allOf")]
                   (when-not (= 1 (count conjuncts))
                     (unsupported-schema! "Only a single allOf schema supported" v opts))
                   (->flanders (first conjuncts) (conj-path opts "allOf" "0")))
                 (when-some [[_ const] (find v "const")]
                   (f/enum [const]))
                 (case (some-> type (-normalize (conj-path opts "type")))
                   ;; https://json-schema.org/understanding-json-schema/reference/numeric
                   ;; TODO all json-schema numbers assume 1.0 and 1 are identical.
                   "integer" (if-some [enum (seq (get v "enum"))]
                               (f/enum (mapv long enum))
                               (f/int))
                   "number" (if-some [enum (seq (get v "enum"))]
                              (f/enum (mapv num enum))
                              (f/num))
                   "boolean" (if-some [enum (not-empty (set (get v "enum")))]
                               (cond
                                 (= #{true false} enum) (f/bool)
                                 (= #{true} enum) (f/bool :equals true)
                                 (= #{false} enum) (f/bool :equals false)
                                 :else (throw (ex-info (str "Unsupported boolean enum: " (pr-str (get v "enum")))
                                                       {})))
                               (f/bool))
                   "string" (let [{fmt "format" :strs [enum]} v]
                              (assert (nil? fmt) (pr-str fmt))
                              (if (seq enum)
                                (f/enum (into [] (map-indexed #(-normalize %2 (conj-path opts (str %)))) enum))
                                (f/str)))
                   "null" (unsupported-schema! "Flanders cannot check for nil" v opts)
                   "array" (let [{:strs [items]} v]
                             (f/seq-of (->flanders items (conj-path opts "items"))))
                   "object" (let [properties (not-empty (into (sorted-map) (map (fn [[k v]] [(keyword k) v])) (get v "properties")))
                                  required (not-empty (into #{} (map keyword) (get v "required")))
                                  additionalProperties (get v "additionalProperties")]
                              (assert ((some-fn nil? boolean?) additionalProperties))
                              (when (and additionalProperties (or properties required)) ;;TODO
                                (unsupported-schema! "Cannot combine properties and additionalProperties" v opts))
                              (if properties
                                (f/map (mapv (fn [[k s]]
                                               (f/entry k (->flanders s (conj-path opts (-normalize k opts))) :required? (contains? required k)))
                                             properties))
                                (if additionalProperties
                                  (f/map-of {})
                                  (unsupported-schema! "TODO closed map" v opts))))
                   ;; https://github.com/json-schema/json-schema/issues/172
                   nil f/any
                   (unknown-schema! v opts))
                 (when-some [enum (seq (get v "enum"))]
                   (f/enum (cond->> enum
                             (some (some-fn ident? string?) enum) (into [] (map #(-normalize %2 (conj-path opts (str %))))))))
                 (unknown-schema! v opts))]
    (cond-> (assoc base ::base-id (::base-id opts))
      ;; TODO unit test
      description (assoc :description description)
      local-defs (assoc ::f/registry local-defs)
      ;;TODO recursive examples involving refs (currently :ref example is nil)
      ;example (assoc :example example)
      ;default (assoc :default default)
      ;; TODO utilize
      ;title (assoc :title title)
      )))

(defn ->flanders
  "Converts parsed JSON Schema to Flanders."
  ([v] (->flanders v nil))
  ([v opts]
   (let [opts (-> opts
                  (update ::dialect #(or % "http://json-schema.org/draft-07/schema#")))]
     (cond
       (boolean? v) (if v
                      f/any
                      (unsupported-schema! "no opposite of f/any" v opts))
       (nil? v) (unsupported-schema! "nil is not checkable in flanders" v opts)
       (map? v) (let [v (normalize-map v opts)]
                  (if (sequential? (get v "type"))
                    (f/either :choices (mapv #(parse-map (assoc v "type" %) opts) (get v "type")))
                    (parse-map v opts)))
       :else (unknown-schema! v opts)))))
