;;TODO move to flanders.schema
(ns flanders.json-schema.schema
  (:require [schema.core :as s]
            [flanders.core :as f]
            [clojure.string :as str]
            [flanders.schema :as fs]
            [flanders.schema.utils :as fsu]
            [flanders.json-schema :as fjs]
            [clojure.pprint :as pp])
  (:import [flanders.types RefType]))

(defn ->schema [json-schema opts]
  (let [f (fjs/->flanders json-schema opts)]
    (fs/->schema f (assoc opts ::ref->var (create-defs f json-schema opts)))))

#?(:clj (defn ->schema+clean
          "Like ->schema except makes allocated memory collectable if result is garbage collected."
          [json-schema opts]
          (let [gc (atom [])
                _ (assert (not (::gc opts)))
                s (->schema json-schema {::gc gc})]
            (.register (java.lang.ref.Cleaner/create) s (fn [] (run! #(%) @gc)))
            s)))
