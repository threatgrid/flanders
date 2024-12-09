(ns flanders.json-schema
  (:require [flanders.core :as f]))

(defn- -normalize [k]
  (prn "-normalize" k)
  (cond
    (keyword? k) (-> k symbol str)
    (symbol? k) (str k)
    (string? k) k
    :else (throw (ex-info "Cannot normalize" {:k k}))))

(defn ->flanders
  "Converts parsed JSON Schema to Flanders."
  [v opts]
  (cond
    ;; TODO "default", "example", "description", "title"
    (map? v) (let [v (update-keys v -normalize)]
               (or (when-some [disjuncts (get v "anyOf")]
                     (f/either :choices (mapv #(->flanders % opts) disjuncts)))
                   (when-some [conjuncts (get v "allOf")]
                     (when-not (= 1 (count conjuncts))
                       (throw (ex-info "Only a single allOf schema supported" {})))
                     (->flanders (first conjuncts) opts))
                   (prn v)
                   (case (-normalize (doto (get v "type") prn))
                     "integer" (f/int)
                     "number" (f/num)
                     "string" (if-some [enum (get v "enum")]
                                (f/enum (mapv str enum))
                                (f/str))
                     "null" (throw (ex-info "Flanders cannot check for nil" {}))
                     "array" (throw (ex-info "Flanders cannot check for tuples" {}))
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
