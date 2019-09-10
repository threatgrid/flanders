(ns flanders.type-script
  (:require [clojure.string :as string]
            [flanders.types]))

;; ---------------------------------------------------------------------
;; Protocols

(defprotocol TypeScript
  (-type-script [this]))

(defprotocol TypeScriptEnum
  (-type-script-enum [this]))

(defprotocol TypeScriptFieldNames
  (-type-script-field-names [this]))

(defprotocol TypeScriptFields
  (-type-script-fields [this]))

(defprotocol TypeScriptTypeName
  (-type-script-type-name [this]))

(defprotocol TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]))

(defn type-script-munge
  [s]
  (clojure.string/replace s #"[^A-Za-z0-9]+" "_"))

(defn type-script-primitive-type-name
  "Return the primitive TypeScript type name of `x`. Returns `nil` if `x`
  does not have a primitive type."
  [x]
  (if (satisfies? TypeScriptPrimitiveTypeName x)
    (-type-script-primitive-type-name x)))

(defn type-script-type-name
  "Attempt to return the TypeScript type name of `x`. Returns `nil` if
  the type name cannot be determined."
  [x]
  (let [x-name (if (satisfies? TypeScriptTypeName x)
                 (-type-script-type-name x)
                 (get x :name))]
    (if (string? x-name)
      (type-script-munge x-name))))

(defn type-script-type-alias
  {:private true}
  [x]
  (if-some [type-name (type-script-type-name x)]
    (if-some [primitive-name (type-script-primitive-type-name x)]
      (format "type %s = %s;" type-name primitive-name))))

(defn type-script-field-names
  [x]
  (if (satisfies? TypeScriptFieldNames x)
    (-type-script-field-names x)))

(defn type-script-fields
  [x]
  (if (satisfies? TypeScriptFields x)
    (-type-script-fields x)))

(defn type-script
  "Attempts to convert `x` into a `string?` of TypeScript
  code. Returns `nil` if `x` cannot be converted."
  [x]
  (if (satisfies? TypeScript x)
    (-type-script x)
    (type-script-type-alias x)))

;; ---------------------------------------------------------------------
;; Protocol implementation

(extend-type flanders.types.AnythingType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    "any"))

(extend-type flanders.types.BooleanType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    "boolean"))

(extend-type flanders.types.EitherType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    (let [choice-names (map type-script-type-name (get this :choices))]
      (if (some #{nil "any"} choice-names)
        "any"
        (string/join " | " (sort (distinct choice-names)))))))

(extend-type flanders.types.KeywordType
  TypeScriptFieldNames
  (-type-script-field-names [this]
    (map
     (fn [value]
       (type-script-munge (name value)))
     (get this :values))))

(extend-type flanders.types.IntegerType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    "number"))

(extend-type flanders.types.InstType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    "Date | string"))

(extend-type flanders.types.MapEntry
  TypeScriptFields
  (-type-script-fields [this]
    (let [? (if (get this :required?) "" "?")
          entry-type (get this :type)
          field-type-name (or (type-script-type-name entry-type)
                              (type-script-primitive-type-name entry-type)
                              "any")]
      (map
       (fn [field-name]
         (format "%s%s: %s;" field-name ? field-type-name))
       (type-script-field-names (get this :key))))))

(extend-type flanders.types.MapType
  TypeScript
  (-type-script [this]
    (if-some [type-name (type-script-type-name this)]
      (let [type-fields (type-script-fields this)]
        (format "interface %s {\n%s\n}"
                type-name
                (string/replace (string/join "\n" (sort type-fields))
                                #"(?m:^)"
                                "  ")))))

  TypeScriptFields
  (-type-script-fields [this]
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
                                (string/join ", " (map pr-str (type-script-field-names k)))))
                     (conj entries
                           (or (some (fn [entry]
                                       (if-not (get entry :required?)
                                         entry))
                                     duplicate-entries)
                               (first duplicate-entries))))
                   []
                   (group-by :key (get this :entries)))]
      (mapcat type-script-fields entries)))

  TypeScriptTypeName
  (-type-script-type-name [this]
    (let [this-name (get this :name)]
      (if (string? this-name)
        (str "I" this-name)))))

(extend-type flanders.types.NumberType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    "number"))

(extend-type flanders.types.SetOfType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    (let [sequence-type (get this :type)]
      (str (or (type-script-type-name sequence-type)
               (type-script-primitive-type-name sequence-type)
               "any")
           "[]"))))

(extend-type flanders.types.SequenceOfType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
    (let [sequence-type (get this :type)]
      (str (or (type-script-type-name sequence-type)
               (type-script-primitive-type-name sequence-type)
               "any")
           "[]"))))

(extend-type flanders.types.StringType
  TypeScriptPrimitiveTypeName
  (-type-script-primitive-type-name [this]
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

(defn type-script-of-sequential
  {:private true}
  [schemas]
  (let [graph (reduce into-graph empty-graph schemas)
        from-to (get graph :from-to)
        named-nodes (filter type-script-type-name (keys from-to))
        ranked-nodes (sort-by
                      (fn [node]
                        (count (get from-to node)))
                      named-nodes)
        type-script-lines (sequence
                           (comp (mapcat
                                  (fn [node]
                                    (if-some [ts (type-script node)]
                                      (if-some [description (get node :description)]
                                        [(string/replace description #"(?m:^)" "// ") ts]
                                        [ts]))))
                                 (distinct))
                           ranked-nodes)]
    (string/join "\n" type-script-lines)))

(extend-type clojure.lang.ISeq
  TypeScript
  (-type-script [this]
    (type-script-of-sequential this)))

(extend-type clojure.lang.IPersistentVector
  TypeScript
  (-type-script [this]
    (type-script-of-sequential this)))
