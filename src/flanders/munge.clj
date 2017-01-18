(ns flanders.munge
  (:require [clojure
             [zip :as z]]
            [flanders
             [navigation :as fn]
             [predicates :as fp]
             [protocols :as fprot]
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

(def generator?
  (let [g? (delay (dynaload 'clojure.test.check.generators/generator?))]
    @g?))

(defmulti munge-action (fn [_ rule]
                         (let [action (last rule)]
                           (cond
                             (fn? action) :apply-fn
                             (generator? action) :use-gen
                             :else action))))

(defn- entry-under-map [loc kw]
  (some-> loc fp/map (fn/find-entry-loc kw)))

(defn- entry-under-entry [loc kw]
  (some-> loc fp/entry z/down z/rightmost (entry-under-map kw)))

(defn- entry-under-map-under-col [loc kw]
  (some-> loc fp/entry z/down z/rightmost fp/col-of z/down (entry-under-map kw)))

(defn- find-loc-in-rule [root-loc rule]
  (reduce
   (fn [loc part]
     (or
      (entry-under-map loc part)
      (entry-under-entry loc part)
      (entry-under-map-under-col loc part)
      (throw (ex-info (str "Could not navigate rule: " rule)
                      {:node (z/node root-loc)
                       :rule rule}))))
   root-loc
   (butlast rule)))

(defn munge-ddl [ddl-map munge-rules]
  (reduce
   (fn [ddl-map rule]
     (let [ddl-loc (fu/->ddl-zip ddl-map)
           munge-loc (find-loc-in-rule ddl-loc rule)
           new-node (munge-action (z/node munge-loc) rule)]
       (z/root
        (if (nil? new-node)
          (z/remove munge-loc)
          (z/replace munge-loc new-node)))))
   ddl-map
   munge-rules))

;; --- Munge Actions ---

(defmethod munge-action :default [node rule]
  (throw (ex-info (str "Unsupported flanders.munge action: " (last rule))
                  {:node node
                   :rule rule})))

(defmethod ^{:doc "Mark a MapEntry node :required? true"}
  munge-action :require [node rule]
  (if (fp/entry? node)
    (assoc node :required? true)
    (throw (ex-info (str "Cannot require " (class node) " node")
                    {:node node
                     :rule rule}))))

(defmethod ^{:doc "Mark a MapEntry node :required? false"}
  munge-action :offer [node rule]
  (if (fp/entry? node)
    (assoc node :required? false)
    (throw (ex-info (str "Cannot offer " (class node) " node")
                    {:node node
                     :rule rule}))))

(defmethod ^{:doc "Set the :gen field of a MapEntry's :type node"}
  munge-action :use-gen [{{inner-type-node :type
                           :as type-node} :type
                          :as node}
                         rule]
  (cond
    (fp/leaf? type-node)
    (assoc node :type (assoc type-node :gen (last rule)))

    (and (fp/col-of? type-node)
         (fp/leaf? inner-type-node))
    (assoc node
           :type (assoc type-node
                        :type (assoc inner-type-node
                                     :gen (last rule))))

    :else
    (throw (ex-info (str "Cannot apply generator for " (class node) "node")
                    {:node node
                     :rule rule}))))

(defmethod
  ^{:doc "Modify a node using the fn in the rule.  The node
  that matched the rule location will be replaced by the result of the
  fn.  The fn must return a DDL node or nil (to indicate that the node
  should be delete)."}
  munge-action :apply-fn [node rule]
  (let [f (last rule)
        new-node (f node rule)]
    (cond
      (nil? new-node) nil
      (satisfies? fprot/TreeNode new-node) new-node
      :else (throw (ex-info (str "Unsupported new-node type: " (class new-node))
                            {:node node
                             :rule rule})))))

(defmethod ^{:doc "Trigger a removal of the node from the DDL tree"}
  munge-action :delete [_ _]
  nil)
