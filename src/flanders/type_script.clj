(ns flanders.type-script
  "Render flanders.types types as TypeScript types.

  See: https://github.com/microsoft/TypeScript/blob/f30e8a284ac479a96ac660c94084ce5170543cc4/doc/spec.md#A"
  (:require [clojure.string :as string]
            [flanders.types]))

(defn type-script-munge
  [s]
  (clojure.string/replace s #"[^A-Za-z0-9]+" "_"))

(defprotocol TypeScriptType
  (-type-script-type [this]))

(defn type-script-type [x]
  (if (satisfies? TypeScriptType x)
    (-type-script-type x)
    "any"))

(defn type-script-union [xs]
  (let [types (map type-script-type xs)]
    (if (some #{nil "any"} types)
      "any"
      (string/join " | " (sort (distinct types))))))

(defn union?
  "true if `x` can be represented as a TypeScript union, false
  otherwise."
  {:private true}
  [x]
  (instance? flanders.types.EitherType x))

(defn type-script-sequence-type [x]
  (let [ts-type (type-script-type x)]
    (if (string? ts-type)
      (if (union? x)
        (str "(" ts-type ")[]")
        (str ts-type "[]"))
      "any[]")))

(defprotocol TypeScriptInterfaceDeclaration
  (-type-script-interface-declaration [this]))

(defn type-script-interface-declaration
  [x]
  (if (satisfies? TypeScriptInterfaceDeclaration x)
    (-type-script-interface-declaration x)))

(defprotocol TypeScriptEnumDeclaration
  (-type-script-enum-declaration [this]))

(defn type-enum-declaration [x]
  (if (satisfies? TypeScriptEnumDeclaration x)
    (-type-script-enum-declaration x)))

(defprotocol TypeScriptPropertyNames
  (-type-script-property-names [this]))

(defn type-script-property-names
  [x]
  (if (satisfies? TypeScriptPropertyNames x)
    (-type-script-property-names x)))

(defprotocol TypeScriptPropertySignatures
  (-type-script-property-signatures [this]))

(defn type-script-property-signatures
  [x]
  (if (satisfies? TypeScriptPropertySignatures x)
    (-type-script-property-signatures x)))

(defprotocol TypeScriptTypeName
  (-type-script-type-name [this]))

(defn type-script-type-name
  "Attempt to return the TypeScript type name of `x`. Returns `nil` if
  the type name cannot be determined."
  [x]
  (let [x-name (if (satisfies? TypeScriptTypeName x)
                 (-type-script-type-name x)
                 (get x :name))]
    (if (string? x-name)
      (type-script-munge x-name))))

(defn type-alias-declaration
  [x]
  (if-some [type-name (type-script-type-name x)]
    (if-some [type (type-script-type x)]
      (format "type %s = %s;" type-name type))))

(defn type-script-declaration
  "Attempt to render `x` as a TypeScript declaration."
  [x]
  (or (type-script-interface-declaration x)
      (type-alias-declaration x)
      (type-enum-declaration x)))

;; ---------------------------------------------------------------------
;; Protocol implementation

(extend-type flanders.types.AnythingType
  TypeScriptType
  (-type-script-type [this]
    "any"))

(extend-type flanders.types.BooleanType
  TypeScriptType
  (-type-script-type [this]
    "boolean"))

(extend-type flanders.types.EitherType
  TypeScriptType
  (-type-script-type [this]
    (type-script-union (get this :choices))))

(extend-type flanders.types.KeywordType
  TypeScriptType
  (-type-script-type [this]
    "string")

  TypeScriptPropertyNames
  (-type-script-property-names [this]
    (map
     (fn [value]
       (type-script-munge (name value)))
     (get this :values))))

(extend-type flanders.types.IntegerType
  TypeScriptType
  (-type-script-type [this]
    "number"))

(extend-type flanders.types.InstType
  TypeScriptType
  (-type-script-type [this]
    "Date | string"))

(extend-type flanders.types.MapEntry
  TypeScriptPropertySignatures
  (-type-script-property-signatures [this]
    (let [? (if (get this :required?) "" "?")
          entry-type (get this :type)
          field-type-name (or (type-script-type-name entry-type)
                              (type-script-type entry-type)
                              "any")]
      (map
       (fn [field-name]
         (format "%s%s: %s" field-name ? field-type-name))
       (type-script-property-names (get this :key))))))

(extend-type flanders.types.MapType
  TypeScriptType
  (-type-script-type [this]
    (if-some [signatures (seq (type-script-property-signatures this))]
      (let [type-body (string/replace (string/join ";\n" (sort signatures))
                                      #"(?m:^)"
                                      "\n  ")]
        (format "{%s\n}" type-body))
      "{}"))
  
  TypeScriptInterfaceDeclaration
  (-type-script-interface-declaration [this]
    (if-some [type-name (type-script-type-name this)]
      (format "interface %s %s" type-name (-type-script-type this))))

  TypeScriptPropertySignatures
  (-type-script-property-signatures [this]
    (let [;; Because it is possible to construct a MapType with
          ;; duplicate keys and type script does not allow interfaces
          ;; to contain duplicate fields, we need a strategy for
          ;; electing keys whenever duplicates exist. Here we do a
          ;; simple thing which is to pick either the first
          ;; non-required duplicate key or the first key.
          entries (reduce
                   (fn [entries [k duplicate-entries]]
                     (when (< 1 (count duplicate-entries))
                       (println "WARNING:" (type-script-type-name this)
                                "contains duplicate definitions for the field(s)"
                                (string/join ", " (map pr-str (type-script-property-names k)))))
                     (conj entries
                           (or (some (fn [entry]
                                       (if-not (get entry :required?)
                                         entry))
                                     duplicate-entries)
                               (first duplicate-entries))))
                   []
                   (group-by :key (get this :entries)))]
      (mapcat type-script-property-signatures entries)))

  TypeScriptTypeName
  (-type-script-type-name [this]
    (let [this-name (get this :name)]
      (if (string? this-name)
        this-name))))

(extend-type flanders.types.NumberType
  TypeScriptType
  (-type-script-type [this]
    "number"))

(extend-type flanders.types.SetOfType
  TypeScriptType
  (-type-script-type [this]
    (type-script-sequence-type (get this :type))))

(extend-type flanders.types.SequenceOfType
  TypeScriptType
  (-type-script-type [this]
    (type-script-sequence-type (get this :type))))

(extend-type flanders.types.StringType
  TypeScriptType
  (-type-script-type [this]
    "string"))

;; ---------------------------------------------------------------------
;; Type graph

(def ^{:private true}
  empty-graph
  {:from-to {}
   :to-from {}})

(defn initialize-from-to
  {:private true}
  [graph node]
  (update-in graph [:from-to node] (fnil identity #{})))

(defn initialize-to-from
  {:private true}
  [graph node]
  (update-in graph [:to-from node] (fnil identity #{})))

(defn add-node
  {:private true}
  [graph node]
  (-> graph
      (initialize-to-from node)
      (initialize-from-to node)))

(defn add-from-to
  {:private true}
  [graph from-node to-node]
  (let [old-to-nodes (get (get graph :from-to) from-node)
        new-to-nodes (if (set? old-to-nodes)
                       (conj old-to-nodes to-node)
                       #{to-node})]
    (assoc-in graph [:from-to from-node] new-to-nodes)))

(defn add-to-from
  {:private true}
  [graph to-node from-node]
  (let [old-from-nodes (get (get graph :to-from) to-node)
        new-from-nodes (if (set? old-from-nodes)
                         (conj old-from-nodes from-node)
                         #{from-node})]
    (assoc-in graph [:to-from to-node] new-from-nodes)))

(defn add-edge
  {:private true}
  [graph from-node to-node]
  (add-to-from (add-from-to graph
                            from-node
                            to-node)
               to-node
               from-node))

(defn add-edges
  {:private true}
  [graph from-node to-nodes]
  (reduce
   (fn [new-graph to-node]
     (add-edge new-graph from-node to-node))
   (add-node graph from-node)
   to-nodes))

(defn into-graph
  {:private true}
  [graph root]
  (loop [graph graph
         queue (conj clojure.lang.PersistentQueue/EMPTY root)]
    (if-some [node (peek queue)]
      (let [node-children (flanders.protocols/node-children node)
            new-graph (add-edges graph node node-children)
            new-queue (into (pop queue) node-children)]
        (recur new-graph new-queue))
      graph)))

(defn type-script-declarations
  [xs]
  {:pre [(sequential? xs)]}
  (let [graph (reduce into-graph empty-graph xs)
        from-to (get graph :from-to)
        named-nodes (filter type-script-type-name (keys from-to))
        ranked-nodes (sort-by
                      (fn [node]
                        (count (get from-to node)))
                      named-nodes)
        type-script-lines (sequence
                           (comp (mapcat
                                  (fn [node]
                                    (if-some [ts (type-script-declaration node)]
                                      (if-some [description (get node :description)]
                                        [(string/replace description #"(?m:^)" "// ") ts]
                                        [ts]))))
                                 (distinct))
                           ranked-nodes)]
    (string/join "\n" type-script-lines)))
