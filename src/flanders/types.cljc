(ns flanders.types
  (:refer-clojure :exclude [defrecord type MapEntry ->MapEntry])
  (:require
   #?(:clj  [flanders.macros :refer [defleaf]]
      :cljs [flanders.macros :refer-macros [defleaf]])
   #?(:clj  [schema.core :as s :refer [defrecord]]
      :cljs [schema.core :as s :refer-macros [defrecord]])
   [flanders.protocols :refer [TreeNode]]))

(defrecord MapEntry [key :- (s/protocol TreeNode)
                     type :- (s/protocol TreeNode)
                     required? :- s/Bool
                     description :- (s/maybe s/Str)
                     reference :- (s/maybe s/Str)
                     comment :- (s/maybe s/Str)
                     usage :- (s/maybe s/Str)]
  TreeNode
  (branch? [_] true)
  (node-children [_] (list key type))
  (make-node [this [new-key new-type]]
    (merge this
           (MapEntry. new-key new-type required? description reference comment
                      usage))))

(defrecord MapType [entries :- [MapEntry]
                    name :- s/Str
                    description :- (s/maybe s/Str)
                    reference :- (s/maybe s/Str)
                    comment :- (s/maybe s/Str)
                    usage :- (s/maybe s/Str)]
  TreeNode
  (branch? [_] true)
  (node-children [_] (seq entries))
  (make-node [this new-entries]
    (merge this
           (MapType. new-entries name description reference comment usage))))

(defrecord SequenceOfType [type :- (s/protocol TreeNode)
                           description :- (s/maybe s/Str)
                           reference :- (s/maybe s/Str)
                           comment :- (s/maybe s/Str)
                           usage :- (s/maybe s/Str)]
  TreeNode
  (branch? [_] true)
  (node-children [_] (list type))
  (make-node [this [new-type]]
    (merge this
           (SequenceOfType. new-type description reference comment usage))))

(defrecord SetOfType [type :- (s/protocol TreeNode)
                      description :- (s/maybe s/Str)
                      reference :- (s/maybe s/Str)
                      comment :- (s/maybe s/Str)
                      usage :- (s/maybe s/Str)]
  TreeNode
  (branch? [_] true)
  (node-children [_] (list type))
  (make-node [this [new-type]]
    (merge this
           (SetOfType. new-type description reference comment usage))))

(defrecord EitherType [choices :- [(s/protocol TreeNode)]
                       tests :- (s/maybe [s/Any])
                       description :- (s/maybe s/Str)
                       reference :- (s/maybe s/Str)
                       comment :- (s/maybe s/Str)
                       usage :- (s/maybe s/Str)]
  TreeNode
  (branch? [_] true)
  (node-children [_] (seq choices))
  (make-node [this new-choices]
    (merge this
           (EitherType. new-choices nil description reference comment usage))))

(defleaf AnythingType [description :- (s/maybe s/Str)
                       reference :- (s/maybe s/Str)
                       comment :- (s/maybe s/Str)
                       usage :- (s/maybe s/Str)])

(defleaf BooleanType [default :- (s/maybe s/Bool)
                      open? :- s/Bool
                      description :- (s/maybe s/Str)
                      reference :- (s/maybe s/Str)
                      comment :- (s/maybe s/Str)
                      usage :- (s/maybe s/Str)])

(defleaf IntegerType [values :- (s/maybe #{s/Int})
                      default :- (s/maybe s/Int)
                      open? :- s/Bool
                      description :- (s/maybe s/Str)
                      reference :- (s/maybe s/Str)
                      comment :- (s/maybe s/Str)
                      usage :- (s/maybe s/Str)])

(defleaf NumberType [values :- (s/maybe #{s/Int})
                     default :- (s/maybe s/Int)
                     open? :- s/Bool
                     description :- (s/maybe s/Str)
                     reference :- (s/maybe s/Str)
                     comment :- (s/maybe s/Str)
                     usage :- (s/maybe s/Str)])

(defleaf StringType [values :- (s/maybe #{s/Str})
                     default :- (s/maybe s/Str)
                     open? :- s/Bool
                     description :- (s/maybe s/Str)
                     reference :- (s/maybe s/Str)])

(defleaf InstType [description :- (s/maybe s/Str)
                   reference :- (s/maybe s/Str)
                   comment :- (s/maybe s/Str)
                   usage :- (s/maybe s/Str)])

(defleaf KeywordType [values :- (s/maybe #{s/Keyword})
                      default :- (s/maybe s/Keyword)
                      open? :- s/Bool
                      description :- (s/maybe s/Str)
                      reference :- (s/maybe s/Str)
                      comment :- (s/maybe s/Str)
                      usage :- (s/maybe s/Str)])

(defrecord ParameterListType [parameters :- [(s/protocol TreeNode)]]
  TreeNode
  (branch? [_] true)
  (node-children [_] (seq parameters))
  (make-node [_ new-parameters]
    (ParameterListType. (vec new-parameters))))

(defrecord SignatureType [description :- (s/maybe s/Str)
                          parameters :- ParameterListType
                          rest-parameter :- (s/maybe (s/protocol TreeNode))
                          return :- (s/protocol TreeNode)
                          name :- (s/maybe s/Str)]
  TreeNode
  (branch? [_] true)
  (node-children [_]
    (if (some? rest-parameter)
      [parameters return rest-parameter]
      [parameters return]))
  (make-node [_ [new-parameters new-return new-rest-parameter]]
    (SignatureType. description
                    new-parameters
                    new-rest-parameter
                    new-return
                    name)))

(defleaf RefType [id :- s/Str])
