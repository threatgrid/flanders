(ns flanders.ocsf
  (:require [flanders.core :as f]))

;; caption => title
;; all maps are closed
(defn parse-attribute [[k {:keys [description requirement enum type is_array]}]]
  (let [optional? (not= requirement "required")
        type (case type
               "string_t" (assert nil)
               "timestamp_t" (assert nil)
               "integer_t" (assert nil)
               "long_t" (assert nil)
               "float_t" (assert nil)
               "boolean_t" (assert nil)
               )]
    (f/entry k (->)))
  )

(defn ->flanders
  "Converts parsed (keywordized) OCSF schemas to Flanders."
  ([v] (->flanders v nil))
  ([{:keys [attributes] :as v} opts]
   (f/map (mapv ))
   ))
