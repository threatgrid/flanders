(ns flanders.json-schema
  (:require [flanders.core :as f]))

(defn- -normalize [k]
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
               (or (when-some [disjuncts (get v "oneOf")]
                     (f/either :choices (mapv #(->flanders % opts) disjuncts)))
                   (when-some [conjuncts (get v "allOf")]
                     (when-not (= 1 (count conjuncts))
                       (throw (ex-info "Only a single allOf schema supported" {})))
                     (->flanders (first conjuncts) opts))
                   (case (-normalize (get v "type"))
                     "integer" (f/int)
                     "number" (f/num)
                     "object" (let [properties (into (sorted-map) (map (fn [[k v]] [(keyword k) v])) (get v "properties"))
                                    required (into #{} (map keyword) (get v "required"))]
                                (f/map (map (fn [[k s]]
                                              (f/entry k (->flanders s opts) :required? (contains? required k)))
                                            properties))))))
    :else (throw (ex-info "Unknown JSON Schema" {:v v}))))
