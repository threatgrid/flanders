(ns flanders.utils
  (:require
   [clojure.zip :as z]
   [flanders.predicates :as fp]
   [flanders.protocols :refer [branch? make-node node-children]]
   [flanders.types :as ft]))

(defn right-loc-seq
  "Lazy seq of loc and its right siblings"
  [loc]
  (if (nil? loc)
    nil
    (lazy-seq (cons loc
                    (right-loc-seq (z/right loc))))))

(defn children-locs [loc]
  (some-> loc z/down right-loc-seq))

(defn ->ddl-zip [root]
  (z/zipper branch? node-children make-node root))

;; Adds zip support for maps.
;; (Source: http://stackoverflow.com/a/15020649/42188)
(defn ->map-zip [m]
  (z/zipper
   (fn [x] (or (map? x) (map? (nth x 1))))
   (fn [x] (seq (if (map? x) x (nth x 1))))
   (fn [x children]
     (if (map? x)
       (into {} children)
       (assoc x 1 (into {} children))))
   m))

(defn- replace-with-any [loc description]
  (z/replace loc
             (ft/map->AnythingType {:description description})))

(defn replace-either-with-any
  "Walks the DDL tree, replacing EitherType nodes with AnythingType nodes"
  [ddl]
  (loop [ddl-loc (->ddl-zip ddl)]
    (cond
      ;; Terminate
      (z/end? ddl-loc)
      (z/root ddl-loc)

      ;; Replace
      (fp/either? (z/node ddl-loc))
      (recur (z/next (replace-with-any ddl-loc
                                       "Simplified conditional branch")))

      ;; Recur
      :else
      (recur (z/next ddl-loc)))))

(defn toggle-require-all
  "Walk the DDL tree making all MapEntry nodes setting the required option"
  [ddl required?]
  (loop [ddl-loc (->ddl-zip ddl)]
    (cond
      (z/end? ddl-loc) (z/root ddl-loc)
      (fp/entry ddl-loc) (recur
                          (z/next
                           (z/replace ddl-loc
                                      (assoc (z/node ddl-loc)
                                             :required? required?))))
      :else (recur (z/next ddl-loc)))))

(defn require-all
  "Walk the DDL tree making all MapEntry nodes required"
  [ddl]
  (toggle-require-all ddl true))

(defn optionalize-all
  "Walk the DDL tree making all MapEntry nodes not required"
  [ddl]
  (toggle-require-all ddl false))
