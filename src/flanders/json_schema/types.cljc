(ns flanders.json-schema.types
  (:require [schema.core :as s]
            #?(:clj  [flanders.macros :refer [defleaf]]
               :cljs [flanders.macros :refer-macros [defleaf]])))

(defleaf JSONSchemaRef [id :- s/Str
                        v :- s/Any])
