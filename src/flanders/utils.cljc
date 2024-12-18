(ns flanders.utils
  (:require
   [clojure.zip :as z]
   [flanders.core :as-alias f]
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

;; ## Identifying schema recursion
;;
;; > Note: this text is adapted from malli.generator: https://github.com/metosin/malli/blob/440de1b56d21a64d845314fe968a9e1511855a0f/src/malli/generator.cljc#L235
;; > Flanders, Malli and JSON Schema refs all use dynamic scope.
;; 
;; Refs are uniquely identified by their paired name and scope. If we see a ref with the
;; same name and scope as another ref we've dereferenced previously, we know that this is a recursion
;; point back to the previously seen ref. The rest of this section explains why.
;; 
;; Refs resolve via dynamic scope, which means its dereferenced value is the latest binding found
;; while expanding the schema until the point of finding the ref.
;; This makes the (runtime) scope at the ref's location part of a ref's identity---if the scope
;; is different, then it's (possibly) not the same ref because scope determines how schemas
;; transitively expand.
;;
;; To illustrate why a ref's name is an insufficient identifier, here is a schema that is equivalent to `[:= 42]`:
;; 
;;   [:schema {:registry {::a [:schema {:registry {::a [:= 42]}}
;;                             ;; (2)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identify refs just by name, we would have incorrectly detected (2) to be an (infinitely expanding) recursive
;; reference.
;;
;; In studying the previous example, we might think that since (1) and (2) deref to different schemas, it might sufficient to identify refs just by their derefs.
;; Unfortunately this just pushes the problem elsewhere.
;;
;; For example, here is another schema equivalent to `[:= 42]`:
;;
;;   [:schema {:registry {::a [:ref ::b] ;; (2)
;;                        ::b [:schema {:registry {::a [:ref ::b] ;; (4)
;;                                                 ::b [:= 42]}}
;;                             ;; (3)
;;                             [:ref ::a]]}}
;;    ;; (1)
;;    [:ref ::a]]
;;
;; If we identified ::a by its deref, it would look like (3) deref'ing to (4)
;; is a recursion point after witnessing (1) deref'ing to (2), since (2) == (4). Except this
;; is wrong since it's a different ::b at (2) and (4)! OTOH, if we identified (2) and (4) with their
;; dynamic scopes along with their form, they would be clearly different. Indeed, this
;; is another way to identify refs: pairing their derefs with their deref's scopes.
;; It is slightly more direct to use the ref's direct name and scope, which is why
;; we choose that identifier. The more general insight is that any schema is identified by its form+scope
;; (note: but only after trimming the scope of irrelevant bindings, see next pararaph).
;; That insight may be useful for detecting recursion at places other than refs.
;; 
;; Ref identifiers could be made smarter by trimming irrelevant entries in identifying scope.
;; Not all scope differences are relevant, so generators may expand more than strictly necessary
;; in the quest to find the "same" ref schema again. It could skip over refs that generate exactly the
;; same values, but their scopes are uninterestingly different (eg., unused bindings are different).
;;
;; For example, the following schema is recursive "in spirit" between (1) and (2), but since ::b
;; changes, the scope will differ, so the recursion will be detected between (2) and itself instead
;; (where the scope is constant):
;;
;;   [:schema {:registry {::a [:schema {:registry {::b :boolean}}
;;                             ;; (2)
;;                             [:or [:ref ::a] [:ref ::b]]]}}
;;    [:schema {:registry {::b :int}}
;;     ;; (1)
;;     [:or [:ref ::a] [:ref ::b]]]]

(defn identify-ref-type
  "Return an opaque identifier suitable for detecting when this schema
  occurs again in the same dynamic scope."
  [{:keys [id] :as node} {::f/keys [registry]}]
  {:pre [(instance? flanders.types.RefType node)]}
  {:scope registry
   :name id})

(defn progress-schema? [node]
  (not (or (instance? flanders.types.EitherType node)
           (instance? flanders.types.RefType node))))
