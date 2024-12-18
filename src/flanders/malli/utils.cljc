(ns flanders.malli.utils
  (:require
    [flanders.example :as example]
    [malli.core :as m]))

;;TODO move back to flanders malli
(defn describe [?schema {:keys [description] :as dll} opts]
  (-> ?schema
      (m/schema opts)
      (m/-update-properties
        #(cond-> %
           (not (:flanders.malli/no-example opts)) (assoc :json-schema/example (try (example/->example-tree dll opts)
                                                                                    (catch Exception e
                                                                                      (throw (ex-info "Error generating example"
                                                                                                      {:schema dll}
                                                                                                      e)))))
           description (assoc :json-schema/description description)))))
