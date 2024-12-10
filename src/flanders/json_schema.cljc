(ns flanders.json-schema
  (:require [clojure.string :as str]
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

(defn resolve-id [{::keys [base-id]} id]
  (assert (string? base-id))
  (cond
    (str/starts-with? id "#") (str/replace-first id "#" base-id)
    :else (throw (ex-info (str "Unresolvable id: " id) {:id id})))
  )

(comment
  (resolve-id {::base-id "https://schema.ocsf.io/schema/classes/security_finding"}
              "#/$defs/fingerprint")
  (resolve-id {::base-id "https://schema.ocsf.io/schema/classes/security_finding"}
              "#")
  )

;https://datatracker.ietf.org/doc/html/rfc6901
(defn resolve-ref [v opts]
  )

(defn absolute-id [{::keys [base-id path] :as opts}]
  (str base-id (when (seq path) (str \# (str/join "/" path)))))

(comment
  (absolute-id {::base-id "https://schema.ocsf.io/schema/classes/security_finding"
                ::path ["$defs" "fingerprint"]})
  )

(defn ->flanders
  "Converts parsed JSON Schema to Flanders."
  [v opts]
  (cond
    ;; TODO "default", "example", "description", "title"
    (map? v) (let [{:strs [$defs $dynamicAnchor $dynamicRef $id $vocabulary $schema]} (update-keys v -normalize)
                   ;; TODO
                   _ (when $schema (assert (= "http://json-schema.org/draft-07/schema#" $schema) (pr-str $schema)))
                   _ (assert (nil? $dynamicAnchor)) ;; TODO
                   _ (assert (nil? $dynamicRef)) ;; TODO
                   $defs (some-> $defs (update-keys -normalize))
                   opts (-> opts
                            (update ::base-id (fn [parent-id]
                                                ;; TODO subschemas start new id https://json-schema.org/draft/2020-12/json-schema-core#section-8.2.1
                                                (when (and parent-id $id)
                                                  (throw (ex-info "$id only supported at top-level" {})))
                                                (or $id parent-id
                                                    ;; TODO Establishing a Base URI https://www.rfc-editor.org/rfc/rfc3986.html#section-5.1
                                                    (throw (ex-info "Must supply $id" {})))))
                            (as-> opts
                              (update opts ::defs (fnil into {})
                                      (map (fn [[k v]]
                                             (let [{::keys [base-id path]} opts
                                                   k (-normalize k)]
                                               [(-normalize k)
                                                (->flanders v (update opts ::path (fnil conj []) "$defs" k))])))
                                      $defs)))]
               (or ;; https://datatracker.ietf.org/doc/html/draft-pbryan-zyp-json-ref-03#section-3
                   (when (get v "$ref")
                     (->JSONSchemaRef v opts))
                   (when-some [disjuncts (get v "anyOf")]
                     (f/either :choices (into [] (map-indexed #(->flanders %2 (update opts ::path (fnil conj []) "anyOf" %1))) disjuncts)))
                   (when-some [conjuncts (get v "allOf")]
                     (when-not (= 1 (count conjuncts))
                       (throw (ex-info "Only a single allOf schema supported" {})))
                     (->flanders (first conjuncts) (update opts ::path (fnil conj []) "allOf" 0)))
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
                               (f/seq-of (->flanders items (update opts ::path (fnil conj []) "items"))))
                     "object" (let [properties (not-empty (into (sorted-map) (map (fn [[k v]] [(keyword k) v])) (get v "properties")))
                                    required (not-empty (into #{} (map keyword) (get v "required")))
                                    additionalProperties (some-> (get v "additionalProperties") (->flanders opts))]
                                (when (and additionalProperties (or properties required)) ;;TODO
                                  (throw (ex-info "Cannot combine properties and additionalProperties" {})))
                                (if properties
                                  (f/map (mapv (fn [[k s]]
                                                 (f/entry k (->flanders s (update opts ::path (fnil conj []) (-normalize k))
                                                                        :required? (contains? required k)))
                                                 properties)))
                                  (assert nil "TODO f/map-of")
                                  #_(f/map-of)))
                     nil)
                   (when-some [enum (seq (get v "enum"))]
                     (f/enum (cond-> enum
                               (some (some-fn ident? string?) enum) (mapv -normalize))))
                   (throw (ex-info "Unknown JSON Schema" {:v v}))))
    :else (throw (ex-info "Unknown JSON Schema" {:v v}))))
