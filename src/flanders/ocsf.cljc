(ns flanders.ocsf
  (:require [flanders.core :as f]))

;; caption => title
;; all maps are closed
(defn parse-attribute [[k {:keys [description requirement enum type is_array observable caption]}
                        :as e]
                       opts]
  (assert (nil? enum))
  (assert (nil? is_array))
  (assert (nil? observable) (pr-str e))
  (f/entry k (-> (case type
                   "string_t" (f/str)
                   "integer_t" (f/int)
                   "long_t" (f/int)
                   "float_t" (f/num)
                   "boolean_t" (f/bool)
                   nil f/any)
                 (assoc :description (or description caption)))
           :required? (case requirement "required" true ("recommended" "optional") false)))

(defn ->flanders
  "Converts parsed (keywordized) OCSF schemas to Flanders."
  ([v] (->flanders v nil))
  ([{:keys [attributes] :as v} opts]
   (f/map (mapv #(parse-attribute % opts) attributes))))
