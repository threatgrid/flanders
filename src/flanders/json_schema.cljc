(ns flanders.json-schema
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [flanders.core :as f]
            [clojure.walk :as w]))

(defn- -normalize
  "normalize to string"
  [k opts]
  (prn "-normalize" k)
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
    :else (throw (ex-info (str "Unresolvable id: " id) {:id id})))
  )

(comment
  (resolve-id "#/$defs/fingerprint" {::base-id "https://schema.ocsf.io/schema/classes/security_finding"})
  (resolve-id "#" {::base-id "https://schema.ocsf.io/schema/classes/security_finding"})
  )

;https://datatracker.ietf.org/doc/html/rfc6901
;https://datatracker.ietf.org/doc/html/draft-pbryan-zyp-json-ref-03#section-3
(defn resolve-ref [{:strs [$ref] :as v} opts]
  (let [this-id (resolve-id $ref opts)]
    (when (contains? (::seen opts) this-id)
      (throw (ex-info "Recursive schemas not allowed" {:id this-id})))
    (or ;; TODO assoc other fields from v at this level
        (get-in opts [::defs this-id])
        (throw (ex-info (format "Could not resolve id %s at path %s" this-id (pr-str (::path v)))
                        {::error :unresolved-ref
                         :current-path (::path opts) :scope (-> opts ::defs keys set) :absolute-id this-id :relative-id $ref})))))

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

(defn normalize-map [m opts]
  (assert (map? m))
  (let [seen (volatile! #{})]
    (update-keys m (fn [k]
                     (let [n (-normalize k (conj-path opts k))]
                       (when (@seen n)
                         (throw (ex-info "Clash while normalizing" {:m m :k k :n n})))
                       (vswap! seen conj n)
                       n)))))

(defn unknown-schema! [v {::keys [path] :as opts}]
  (throw (ex-info (format "Unknown JSON Schema at path %s: %s" (pr-str path) (pr-str v)) {:v v :opts (select-keys opts [::path ::base-id])})))

(defrecord FlandersRef [id v])

(defn ->flanders
  "Converts parsed JSON Schema to Flanders."
  [v opts]
  (cond
    (map? v) (let [{:strs [description title example
                           $ref $defs $dynamicAnchor $dynamicRef $id $vocabulary $schema]} (normalize-map v opts)
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
                          ;; simpler if we supported refs in flanders
                          (update ::defs
                                  (fn [outer-defs]
                                    (let [parsed-defs (into {} (map (fn [[k v]]
                                                                      (let [opts (conj-path opts "$defs" k)
                                                                            id (absolute-id opts)]
                                                                        {id (->flanders v (assoc opts ::resolve-ref (fn [v opts] (->FlandersRef id v))))})))
                                                            $defs)
                                          resolve-refs (fn resolve-refs [parsed-defs {::keys [seen] :as opts}]
                                                         (w/postwalk (fn [v]
                                                                       (if (instance? FlandersRef v)
                                                                         (let [{the-ref :v :keys [id]} v]
                                                                           (if (seen id)
                                                                             (throw (ex-info "Recursive schemas not supported" {:id id :seen seen}))
                                                                             (-> the-ref
                                                                                 (resolve-ref opts)
                                                                                 (resolve-refs (update opts ::seen conj id)))))
                                                                         v))
                                                                     parsed-defs))]
                                      (into (or outer-defs {})
                                            (resolve-refs parsed-defs (update opts ::seen #(or % #{}))))))))
                   base (or (when $ref
                              ((::resolve-ref opts resolve-ref) v opts))
                            (when-some [disjuncts (get v "anyOf")]
                              (f/either :choices (into [] (map-indexed #(->flanders %2 (conj-path opts "anyOf" (str %1)))) disjuncts)))
                            (when-some [conjuncts (get v "allOf")]
                              (when-not (= 1 (count conjuncts))
                                (throw (ex-info "Only a single allOf schema supported" {})))
                              (->flanders (first conjuncts) (conj-path opts "allOf" "0")))
                            (case (some-> (get v "type")
                                          (-normalize (conj-path opts "type")))
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
                                                                  {:schema v})))
                                          (f/bool))
                              "string" (let [{fmt "format" :strs [enum]} v]
                                         (assert (nil? fmt) (pr-str fmt))
                                         (if (seq enum)
                                           (f/enum (into [] (map-indexed #(-normalize %2 (conj-path opts (str %)))) enum))
                                           (f/str)))
                              "null" (throw (ex-info "Flanders cannot check for nil" {}))
                              "array" (let [{:strs [items uniqueItems]} v]
                                        (assert (nil? uniqueItems))
                                        (f/seq-of (->flanders items (conj-path opts "items"))))
                              "object" (let [properties (not-empty (into (sorted-map) (map (fn [[k v]] [(keyword k) v])) (get v "properties")))
                                             required (not-empty (into #{} (map keyword) (get v "required")))
                                             additionalProperties (get v "additionalProperties")]
                                         (assert ((some-fn nil? boolean?) additionalProperties))
                                         (when (and additionalProperties (or properties required)) ;;TODO
                                           (throw (ex-info "Cannot combine properties and additionalProperties" {})))
                                         (if properties
                                           (f/map (mapv (fn [[k s]]
                                                          (f/entry k (->flanders s (conj-path opts (-normalize k opts))) :required? (contains? required k)))
                                                        properties))
                                           
                                           (if additionalProperties
                                             (f/map-of {})
                                             (assert nil (str "TODO closed map" (pr-str v))))))
                              ;; https://github.com/json-schema/json-schema/issues/172
                              nil f/any
                              (unknown-schema! v opts))
                            (when-some [enum (seq (get v "enum"))]
                              (f/enum (cond->> enum
                                        (some (some-fn ident? string?) enum) (into [] (map #(-normalize %2 (conj-path opts (str %))))))))
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
