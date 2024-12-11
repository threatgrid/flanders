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

(defn ->malli [json-schema {::fjs/keys [defs] :as opts}]
  (let [refs? (boolean (seq defs))
        opts (into fm/default-opts opts)
        c (-> json-schema
              (fjs/->flanders opts)
              (fm/->malli (cond-> opts refs? (assoc ::m/allow-invalid-refs true))))]
    (if refs?
      (m/schema [:schema {:registry defs} c] opts)
      c)))
