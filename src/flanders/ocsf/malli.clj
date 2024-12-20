(ns flanders.ocsf.malli
  (:require [flanders.ocsf :as ocsf]
            [flanders.malli :as malli]))

(defn schemas
  "Return a map from OCSF identifier to Malli schema for the schemas in the given OCSF version.

  Example:
  (schemas \"v1.3.0\")
  => 
  {\"https://schema.ocsf.io/1.3.0/classes/patch_state\" [:map ...]
   ...}"
  ([version] (schemas version nil))
  ([version opts] (update-vals (ocsf/schemas version opts) #(malli/->malli % opts))))
