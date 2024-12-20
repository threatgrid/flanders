(ns flanders.ocsf.schema
  (:require [flanders.schema :as schema]
            [flanders.ocsf :as ocsf]))

(defn schemas
  "Return a map from OCSF identifier to Plumatic Schema schema for the schemas in the given OCSF version.

  Example:
  (schemas \"v1.3.0\")
  => 
  {\"https://schema.ocsf.io/1.3.0/classes/patch_state\" {:severity_id Int, :category_uid Int ...}
   ...}"
  ([version] (schemas version nil))
  ([version opts] (update-vals (ocsf/schemas version opts) schema/->schema)))
