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

(defn def-id->var-sym [base-id defstr]
  (symbol (namespace-munge (munge (str/replace defstr "." "_DOT_")))))

(comment
  (= ;"Logger"
     'https_COLON__SLASH__SLASH_schema.ocsf.io_SLASH_schema_SLASH_classes_SLASH_security_finding_SLASH_$defs_SLASH_logger
     (def-id->var-sym
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/"
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/logger"))
  (= "KeyboardInfo"
     (def-id->var-sym
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/"
       "https://schema.ocsf.io/schema/classes/security_finding/$defs/keyboard_info"))
  )

;; stable ns sorting for tests

(def max-nano-digits 18)
(def max-nano (long (Math/pow 10 max-nano-digits)))
(def nano-padder (str "%0" max-nano-digits "d"))

(defn unique-nano []
  (let [v (System/nanoTime)]
    (loop []
      (let [v' (System/nanoTime)]
        (if (= v (System/nanoTime))
          (recur)
          v')))))

;; assumes schema conversion is single threaded
;; another idea is hash the $defs and use that in the ns name
(defn stable-sortable-ns-segment []
  (let [n (unique-nano)]
    (when (>= n max-nano)
      (binding [*err* *out*]
        ;; just affects unit tests. also won't happen for a long time
        (println "WARNING: Please increment max-nano-digits for unit test stability")))
    (format nano-padder n)))

(defn- create-defs [{::f/keys [registry] ::fjs/keys [base-id] :as f} json-schema opts]
  (when (seq registry)
    (let [base-id (str base-id "/$defs/")
          temp-ns (create-ns (symbol (str "flanders.json-schema.schema."
                                          ;; helps sort schemas during unit testing.
                                          (stable-sortable-ns-segment)
                                          "."
                                          (str (random-uuid)))))
          _ (some-> (::gc opts) (swap! (fnil conj []) conj #(remove-ns temp-ns)))
          _ (alter-meta! temp-ns assoc :doc (str "Helper namespace to generate the following JSON Schema:\n"
                                                 (with-out-str (pp/pprint json-schema))))
          _ (run! #(ns-unmap temp-ns %) (keys (ns-map temp-ns)))
          _ (assert (empty? (ns-map temp-ns)))
          _ (assert (empty? (ns-interns temp-ns)))
          _ (assert (empty? (ns-imports temp-ns)))
          _ (assert (empty? (ns-refers temp-ns)))
          _ (assert (empty? (ns-aliases temp-ns)))
          nstr (name (ns-name temp-ns))
          def-ids (sort (keys registry))
          _ (assert (seq base-id))
          _ (assert (every? #(str/starts-with? % base-id) def-ids) "Ambiguous refs")
          def-vars (mapv #(symbol nstr (name (def-id->var-sym base-id %))) def-ids)
          _ (assert (or (empty? def-vars)
                        (apply distinct? def-vars)))
          def-ids->def-vars (zipmap def-ids def-vars)
          _ (run! #(intern (-> % namespace symbol) (-> % name symbol)) def-vars)
          _ (run! (fn [[def-id def-var]]
                    (let [f (get registry def-id)
                          _ (assert f def-id)
                          frm `(s/defschema ~(-> def-var name symbol)
                                 ~(str "JSON Schema id: " def-id "\n")
                                 ~(fs/->schema f (assoc opts ::ref->var def-ids->def-vars)))]
                      (binding [*ns* temp-ns]
                        (eval frm))))
                  def-ids->def-vars)]
      def-ids->def-vars)))

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
