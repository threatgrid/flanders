(ns flanders.json-schema.malli
  (:require [malli.core :as m]
            [flanders.core :as f]
            [flanders.example :as fe]
            [flanders.malli :as fm]
            [flanders.malli.utils :as fmu]
            [flanders.json-schema :as fjs])
  (:import [flanders.types RefType]))

(extend-type RefType
  fm/MalliNode
  (->malli' [{:keys [id] :as ddl} opts]
    (-> [:ref id]
        (fmu/describe ddl opts))))

(defn ->malli [json-schema opts]
  (let [opts (into fm/default-opts opts)
        {::f/keys [registry] :as f} (fjs/->flanders json-schema opts)
        c (fm/->malli f (assoc opts ::m/allow-invalid-refs true))]
    (m/-update-properties c update :registry
                          (fn [prev]
                            (assert (not prev) ":registry already exists")
                            (into (sorted-map) (update-vals registry #(fm/->malli % (assoc opts ::m/allow-invalid-refs true))))))))
