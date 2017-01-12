(ns flanders.munge
  (:require [clojure
             [zip :as z]]
            [flanders
             [predicates :as fp]
             [utils :as fu]]))

(defn- dynaload
  "Copyright (c) Rich Hickey (EPL 1.0). Copied from gen.clj"
  [s]
  (let [ns (namespace s)]
    (assert ns)
    (require (symbol ns))
    (let [v (resolve s)]
      (if v
        @v
        (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))

(defn- first-value [{v :values}]
  (first v))

(defn- key-match [entry-loc kw]
  (when-let [test-kw
             (some-> entry-loc fp/entry z/down fp/keyword z/node first-value)]
    (when (= kw test-kw)
      kw)))

(declare munge-ddl)

(defn- apply-munge-action [ddl-loc munge-value]
  (let [actions (if (vector? munge-value) munge-value [munge-value])]
    (reduce
     (fn [loc action]
       (let [{node-type :type, :as node} (z/node loc)]
         (cond
           (and (map? action)
                (fp/map? node-type))
           (z/replace loc
                      (assoc node :type (munge-ddl node-type action)))

           (let [g? (delay (dynaload 'clojure.test.check.generators/generator?))]
             (@g? action))
           (z/replace loc
                      (assoc node :gen action))

           (fn? action)
           (action loc)

           :else
           loc)))
     ddl-loc
     actions)))

(defn munge-ddl [ddl-map munge-map]
  (loop [ddl-loc (some-> (fu/->ddl-zip ddl-map)
                         fp/map
                         z/down
                         fp/entry)
         last-loc nil]
    (if (nil? ddl-loc)
      (and last-loc
           (z/root last-loc)) ;; terminate
      (let [this-loc (if-let [key (some (partial key-match ddl-loc)
                                        (keys munge-map))]
                       (apply-munge-action ddl-loc (get munge-map key))
                       ddl-loc)]
        (recur
         (z/right this-loc)
         this-loc)))))

(defn require-node [current-loc]
  (z/replace current-loc
             (assoc (z/node current-loc) :required? true)))

(def delete-node fu/remove-in-place)
