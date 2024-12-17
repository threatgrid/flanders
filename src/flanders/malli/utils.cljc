(ns flanders.malli.utils
  (:require
    [flanders.example :as example]
    [malli.core :as m]))

(defn describe [?schema {:keys [description] :as dll} opts]
  (-> ?schema
      (m/schema opts)
      (m/-update-properties
        #(-> %
             (assoc :json-schema/example (example/->example-tree dll opts))
             (cond-> description (assoc :json-schema/description description))))))
