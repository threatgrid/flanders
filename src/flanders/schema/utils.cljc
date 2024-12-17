(ns flanders.schema.utils
  #?(:clj (:require
            [ring.swagger.json-schema :as rs]
            [flanders.example :as example])))

(defn describe [schema dll opts]
  #?(:cljs schema
     :clj (rs/field
            schema
            (let [{:keys [description]} dll]
              (cond-> {:example (example/->example-tree dll opts)}
                description (assoc :description description))))))
