(ns flanders.json-schema
  (:require [flanders.core :as f]))

(defn- -normalize
  "normalize to string"
  [k]
  (prn "-normalize" k)
  (cond
    (keyword? k) (-> k symbol str)
    (symbol? k) (str k)
    (string? k) k
    :else (throw (ex-info "Cannot normalize" {:k k}))))

(defrecord JSONSchemaRef [s])

;https://datatracker.ietf.org/doc/html/rfc6901
(defn resolve-ref [v opts]
  )

(defn ->flanders
  "Converts parsed JSON Schema to Flanders."
  [v opts]
  (cond
    ;; TODO "default", "example", "description", "title"
    (map? v) (let [{:strs [$defs $dynamicAnchor $dynamicRef $id $vocabulary $schema]} (update-keys v -normalize)
                   _ (when $schema (assert (= "http://json-schema.org/draft-07/schema#" $schema) (pr-str $schema)))
                   _ (assert (not $dynamicAnchor))
                   _ (assert (not $dynamicRef))
                   _ (assert (not $id))
                   $defs (some-> $defs (update-keys -normalize))
                   opts (update opts ::defs (fnil into {})
                                (map (fn [[k v]]
                                       (prn "kv" k v)
                                       [(-normalize k) (->flanders v opts)]))
                                $defs)]
               (or ;; https://datatracker.ietf.org/doc/html/draft-pbryan-zyp-json-ref-03#section-3
                   (when (get v "$ref")
                     (->JSONSchemaRef v))
                   (when-some [disjuncts (get v "anyOf")]
                     (f/either :choices (mapv #(->flanders % opts) disjuncts)))
                   (when-some [conjuncts (get v "allOf")]
                     (when-not (= 1 (count conjuncts))
                       (throw (ex-info "Only a single allOf schema supported" {})))
                     (->flanders (first conjuncts) opts))
                   (case (-normalize (doto (get v "type") prn))
                     "integer" (if-some [enum (seq (get v "enum"))]
                                 (f/enum (mapv long enum))
                                 (f/int))
                     "number" (if-some [enum (seq (get v "enum"))]
                                (f/enum (mapv num enum))
                                (f/num))
                     "string" (if-some [enum (seq (get v "enum"))]
                                (f/enum (mapv -normalize enum))
                                (f/str))
                     "null" (throw (ex-info "Flanders cannot check for nil" {}))
                     "array" (let [{:strs [items]} v]
                               (f/seq-of (->flanders items opts)))
                     "object" (let [properties (not-empty (into (sorted-map) (map (fn [[k v]] [(keyword k) v])) (get v "properties")))
                                    required (not-empty (into #{} (map keyword) (get v "required")))
                                    additionalProperties (some-> (get v "additionalProperties") (->flanders opts))]
                                (when (and additionalProperties (or properties required)) ;;TODO
                                  (throw (ex-info "Cannot combine properties and additionalProperties" {})))
                                (if properties
                                  (f/map (mapv (fn [[k s]]
                                                 (f/entry k (->flanders s opts) :required? (contains? required k)))
                                               properties))
                                  (f/map-of))))))
    :else (throw (ex-info "Unknown JSON Schema" {:v v}))))
