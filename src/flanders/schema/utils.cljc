(ns flanders.schema.utils
  #?(:clj (:require
            [ring.swagger.json-schema :as rs]
            [flanders.example :as example]
            [flanders.schema :as-alias fs])))

(defn describe [schema dll opts]
  #?(:cljs schema
     :clj (rs/field
            schema
            (let [{:keys [description]} dll]
              (cond-> {}
                (not (::fs/no-example opts)) (assoc :example (example/->example-tree dll opts))
                description (assoc :description description))))))
