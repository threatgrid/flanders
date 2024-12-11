(ns flanders.json-schema.schema
  (:require [schema.core :as s]
            [clojure.string :as str]
            [flanders.schema :as fs]
            [flanders.json-schema.types :as fjst]
            [flanders.json-schema :as fjs])
  (:import [flanders.json_schema.types JSONSchemaRef]))

(extend-type JSONSchemaRef
  fs/SchemaNode
  (->schema' [{:keys [id]} _ {::keys [ref->var] :as opts}]
    (s/recursive (find-var (get ref->var id)))))

(defn def-id->var-sym [base-id defstr]
  (munge (str/join "" (map str/capitalize (str/split (str/replace-first defstr base-id "") #"[/_-]")))))

(comment
  (= "Logger"
     (def-id->var-sym
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/"
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/logger"))
  (= "KeyboardInfo"
     (def-id->var-sym
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/"
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/keyboard_info"))
  )

(def ^:dynamic ^:internal *opts* nil)

(defn ->schema [json-schema opts]
  (let [{::fjs/keys [defs base-id] :as f} (fjs/->flanders json-schema opts)
        nstr (name (ns-name *ns*))
        base-id (str base-id "/$defs/")
        def-ids (sort (keys defs))
        _ (assert (seq base-id))
        _ (assert (every? #(str/starts-with? % base-id) def-ids) "Ambiguous refs")
        def-vars (mapv #(symbol nstr (name (def-id->var-sym base-id %))) def-ids)
        _ (assert (or (empty? def-vars)
                      (apply distinct? def-vars)))
        def-ids->def-vars (zipmap def-ids def-vars)
        _ (run! #(intern (-> % namespace symbol) (-> % name symbol)) def-vars)
        _ (run! (fn [[def-id def-var]]
                  (let [f (get defs def-id)
                        _ (assert f def-id)]
                    (eval `(s/defschema ~(-> def-var name symbol)
                             ~(str "JSON Schema id: " def-id "\n")
                             ~(fm/->schema f (assoc opts ::ref->var def-ids->def-vars))))))
                def-ids->def-vars)
        s (fm/->schema f (assoc opts ::ref->var def-ids->def-vars))]
    s))
