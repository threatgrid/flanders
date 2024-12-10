(ns flanders.json-schema
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [flanders.core :as f]))

(defn- -normalize
  "normalize to string"
  [k]
  (prn "-normalize" k)
  (cond
    (keyword? k) (-> k symbol str)
    (symbol? k) (str k)
    (string? k) k
    :else (throw (ex-info "Cannot normalize" {:k k}))))

(defrecord JSONSchemaRef [v opts])

(defn resolve-id [id {::keys [base-id]}]
  (assert (string? base-id))
  (cond
    (str/starts-with? id "#") (str/replace-first id "#" base-id)
    ;; TODO absolute ids
    :else (throw (ex-info (str "Unresolvable id: " id) {:id id})))
  )

(comment
  (resolve-id "#/$defs/fingerprint" {::base-id "https://schema.ocsf.io/schema/classes/security_finding"})
  (resolve-id "#" {::base-id "https://schema.ocsf.io/schema/classes/security_finding"})
  )

;https://datatracker.ietf.org/doc/html/rfc6901
(defn resolve-ref [id opts]
  )

(defn absolute-id [{::keys [base-id path] :as opts}]
  (assert base-id)
  (assert (every? string? path))
  (str base-id (when (seq path) (str \# (str/join "/" path)))))

(defn conj-path [opts & path-elements]
  (update opts ::path (fnil into [])
          (map #(do (assert (string? %) (str "Bad path element: " (pr-str %)))
                    %))
          path-elements))

(comment
  (absolute-id {::base-id "https://schema.ocsf.io/schema/classes/security_finding"
                ::path ["$defs" "fingerprint"]})
  )

(defn normalize-map [m]
  (assert (map? m))
  (let [seen (volatile! #{})]
    (update-keys m (fn [k]
                     (let [n (-normalize k)]
                       (when (@seen n)
                         (throw (ex-info "Clash while normalizing" {:m m :k k :n n})))
                       (vswap! seen conj n)
                       n)))))

(defn unknown-schema! [v {::keys [path] :as opts}]
  (throw (ex-info (format "Unknown JSON Schema at path %s: %s" (pr-str path) (pr-str v)) {:v v :opts (select-keys opts [::path ::base-id])})))

(defn ->flanders
  "Converts parsed JSON Schema to Flanders."
  [v opts]
  (cond
    (map? v) (let [{:strs [description title example
                           $ref $defs $dynamicAnchor $dynamicRef $id $vocabulary $schema]} (normalize-map v)
                   ;; TODO
                   _ (when $schema (assert (= "http://json-schema.org/draft-07/schema#" $schema) (pr-str $schema)))
                   _ (assert (nil? $dynamicAnchor)) ;; TODO
                   _ (assert (nil? $dynamicRef)) ;; TODO
                   opts (update opts ::base-id (fn [parent-id]
                                                 ;; TODO subschemas start new id https://json-schema.org/draft/2020-12/json-schema-core#section-8.2.1
                                                 ;; need to reset ::path if we support this
                                                 (when (and parent-id $id)
                                                   (throw (ex-info "$id only supported at top-level" {})))
                                                 (or $id parent-id
                                                     ;; TODO Establishing a Base URI https://www.rfc-editor.org/rfc/rfc3986.html#section-5.1
                                                     (throw (ex-info "Must supply $id" {})))))
                   opts (cond-> opts
                          (seq $defs)
                          (update ::defs
                                  (fn [defs]
                                    (let [defs (or defs {})
                                          $defs (normalize-map $defs)]
                                      (when-some [clashes (seq (set/intersection (-> defs keys set)
                                                                                 (-> $defs keys set)))]
                                        (throw (ex-info "Clashing $def's" {:clashes clashes})))
                                      (into defs
                                            (map (fn [[k v]]
                                                   (let [opts (conj-path opts "$defs" k)
                                                         this-id (absolute-id opts)]
                                                     [this-id
                                                      (->flanders v (update opts ::seen (fnil conj #{}) this-id))])))
                                            $defs)))))
                   base (or ;; https://datatracker.ietf.org/doc/html/draft-pbryan-zyp-json-ref-03#section-3
                            (when-some [this-id (some-> $ref (resolve-id opts))]
                              (when (contains? (::seen opts) this-id)
                                (throw (ex-info "Recursive schemas not allowed" {:id this-id})))
                              (or ;; TODO assoc other fields from v at this level
                                  ;; TODO ref object?
                                  (get-in opts [::defs this-id])
                                  (throw (ex-info "Could not resolve id" {:absolute-id this-id :relative-id $ref}))))
                            (when-some [disjuncts (get v "anyOf")]
                              (f/either :choices (into [] (map-indexed #(->flanders %2 (conj-path opts "anyOf" (str %1)))) disjuncts)))
                            (when-some [conjuncts (get v "allOf")]
                              (when-not (= 1 (count conjuncts))
                                (throw (ex-info "Only a single allOf schema supported" {})))
                              (->flanders (first conjuncts) (conj-path opts "allOf" "0")))
                            (case (-normalize (doto (get v "type") prn))
                              "integer" (if-some [enum (seq (get v "enum"))]
                                          (f/enum (mapv long enum))
                                          (f/int))
                              "number" (if-some [enum (seq (get v "enum"))]
                                         (f/enum (mapv num enum))
                                         (f/num))
                              "string" (let [{fmt "format" :strs [enum]} v]
                                         (assert (nil? fmt) (pr-str fmt))
                                         (if (seq enum)
                                           (f/enum (mapv -normalize enum))
                                           (f/str)))
                              "null" (throw (ex-info "Flanders cannot check for nil" {}))
                              "array" (let [{:strs [items uniqueItems]} v]
                                        (assert (nil? uniqueItems))
                                        (f/seq-of (->flanders items (conj-path opts "items"))))
                              "object" (let [properties (not-empty (into (sorted-map) (map (fn [[k v]] [(keyword k) v])) (get v "properties")))
                                             required (not-empty (into #{} (map keyword) (get v "required")))
                                             additionalProperties (some-> (get v "additionalProperties") (->flanders opts))]
                                         (when (and additionalProperties (or properties required)) ;;TODO
                                           (throw (ex-info "Cannot combine properties and additionalProperties" {})))
                                         (if properties
                                           (f/map (mapv (fn [[k s]]
                                                          (f/entry k (->flanders s (conj-path opts (-normalize k))) :required? (contains? required k))
                                                          properties)))
                                           (assert nil "TODO f/map-of")
                                           #_(f/map-of)))
                              nil)
                            (when-some [enum (seq (get v "enum"))]
                              (f/enum (cond-> enum
                                        (some (some-fn ident? string?) enum) (mapv -normalize))))
                            (unknown-schema! v opts))]
               (cond-> base
                 ;; TODO unit test
                 description (assoc :description description)
                 ;;TODO
                 ;example (assoc :example example)
                 ;default (assoc :default default)
                 ;title (assoc :title title)
                 ))
    :else (unknown-schema! v opts)))
