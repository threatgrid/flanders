(ns flanders.json-schema.malli
  (:require [malli.core :as m]
            [flanders.malli :as fm]
             [flanders.json-schema.types :as fjst]
             [flanders.json-schema :as fjs])
  (:import [flanders.json_schema.types JSONSchemaRef]))

(extend-type JSONSchemaRef
  fm/MalliNode
  (->malli' [{:keys [id]} opts]
    ;;TODO assoc properties
    [:ref id]))

(defn ->malli [json-schema opts]
  (let [opts (into fm/default-opts opts)
        {::fjs/keys [defs] :as f} (fjs/->flanders json-schema opts)
        c (fm/->malli f (assoc opts ::m/allow-invalid-refs true))]
    (m/schema [:schema {:registry (into (sorted-map) (update-vals defs #(fm/->malli % (assoc opts ::m/allow-invalid-refs true))))} c] opts)))
