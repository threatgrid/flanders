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

(def ^:private object-properties ["properties" "patternProperties" "additionalProperties" "required"])
(def ^:private array-properties ["prefixItems" "contains" "items"])
(def ^:private applicator-properties ["if" "then" "else" "oneOf" "anyOf" "allOf" "not"])

(defn- check-unsupported-keys! [v opts]
  (doseq [k (case (::dialect opts)
              "http://json-schema.org/draft-07/schema#"
              ["$comment"
               ;"$id"
               ;"$ref"
               ;"$schema"
               "additionalItems"
               ;"additionalProperties"
               "allOf"
               ;"anyOf"
               ;"const"
               "contains"
               "contentEncoding"
               "contentMediaType"
               "default"
               ;"definitions" ;; also support $defs but not both
               "dependencies"
               ;"description"
               "else"
               ;"enum"
               "examples"
               "exclusiveMaximum"
               "exclusiveMinimum"
               ;"format"
               "if"
               ;"items"
               "maximum"
               "maxItems"
               "maxLength"
               "maxProperties"
               "minimum"
               "minItems"
               "minLength"
               "minProperties"
               "multipleOf"
               "not"
               "oneOf"
               "pattern"
               "patternProperties"
               ;"properties"
               "propertyNames"
               "readOnly"
               ;"required"
               "then"
               ;"title"
               ;"type"
               "uniqueItems"
               "writeOnly"]

              "https://json-schema.org/draft/2020-12/schema"
              ["$anchor"
               "$comment"
               ;"$defs" ;; also support $defs but not both
               "$dynamicAnchor"
               "$dynamicRef"
               ;"$id"
               ;"$ref"
               ;"$schema"
               "$vocabulary"
               ;"additionalProperties"
               "allOf"
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
               "writeOnly"])]
    (when (contains? v k)
      (unsupported-schema! (str "Unsupported JSON Schema keyword: " k) v opts))))

(defn- parse-map [v {::keys [->infer-type] :as opts}]
  (let [{:strs [description title example $schema $ref $anchor definitions $defs $dynamicAnchor $dynamicRef $id $vocabulary] :as v} (normalize-map v opts)
        {:strs [type] :as v} (if (contains? v "type")
                               v
                               ;; infer type
                               ;;Note this is against the spec https://github.com/json-schema/json-schema/issues/172
                               ;; but flanders isn't expressive enough to encode such schemas.
                               ;; for example {"required": ["a"]} is [:or [:not map?] [:map [:a :any]]] in malli.
                               ;; we filter out examples that require negation in the json schema test suite.
                               (cond
                                 $ref v
                                 (some #(contains? v %) applicator-properties) v
                                 ;; assumes we don't have any other fields like "minimum"
                                 (some #(contains? v %) object-properties) (assoc v "type" "object")
                                 (some #(contains? v %) array-properties) (assoc v "type" "array")
                                 ;; we assume a concrete type below to simplify the code. we might
                                 ;; be able to expand the schema to multiple types and distribute the relevant
                                 ;; fields e.g., {"minimum": 1, "required": ["a"]} => [:or [:int {:min 1}] [:map [:a :any]]]
                                 :else (or (when ->infer-type
                                             (->infer-type v opts))
                                           (unsupported-schema! "cannot infer type" v opts))))
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
                                          (str "unique-base-id-" (random-uuid))
                                          ;; TODO Establishing a Base URI https://www.rfc-editor.org/rfc/rfc3986.html#section-5.1
                                          #_(throw (ex-info "Must supply $id" {})))))
        _ (assert (not (and $defs definitions)))
        $defs (or $defs definitions)
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
                 (when-some [[_ const] (find v "const")]
                   (f/enum [const]))
                 (when (not type)
                   (when-some [enum (seq (get v "enum"))]
                     (let [enum (cond->> enum
                                  (every? (some-fn ident? string?) enum) (into [] (map-indexed #(-normalize %2 (conj-path opts (str %))))))]
                       (if (not-any? (fn [p] (every? p enum)) [integer? number? keyword? string?])
                         (unsupported-schema! "complex enum" v opts)
                         (f/enum enum)))))
                 (case (when type
                         (if (qualified-keyword? type)
                           type
                           (-normalize type (conj-path opts "type"))))
                   ::any f/any
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
                             (when (sequential? items)
                               (unsupported-schema! "items vector" v opts))
                             (f/seq-of (->flanders items (conj-path opts "items"))))
                   "object" (let [required (not-empty (into #{} (map keyword) (get v "required")))
                                  properties (not-empty (-> (sorted-map)
                                                            (into (map (fn [k] [k true]))
                                                                  required)
                                                            (into (map (fn [[k v]] [(keyword k) v]))
                                                                  (get v "properties"))))
                                  fixed (mapv (fn [[k s]]
                                                (f/entry k (->flanders s (conj-path opts (-normalize k opts))) :required? (contains? required k)))
                                              properties)
                                  default (if-some [[k additionalProperties] (find v "additionalProperties")]
                                            (case additionalProperties
                                              ;; ->flanders doesn't support translating false, but it corresponds to a closed map which is flanders' default.
                                              false nil
                                              (if (and (seq fixed) (not (boolean? additionalProperties)))
                                                ;; TODO because we keywordize map schema keys, string keys on maps are not handled by the fixed
                                                ;; fields. see additionalProperties-test
                                                (unsupported-schema! "additionalProperties must be true or false" v opts)
                                                [(f/entry f/any (->flanders additionalProperties (conj-path opts (-normalize k opts)))
                                                          :required false)]))
                                            ;; JSON Schema maps are open by default
                                            [(f/entry f/any f/any :required false)])]
                              (f/map (into fixed default)))
                   (unknown-schema! v opts))
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
