(ns flanders.macros
  (:require
   [flanders.protocols :as p]
   [schema.core :as s]))

(defmacro defleaf [name argsv & sexprs]
  `(s/defrecord ~name ~argsv
     p/TreeNode
     (~'branch? [_#] false)
     (~'node-children [_#] nil)
     (~'make-node [this# _#] this#)
     ~@sexprs))
