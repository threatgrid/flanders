(ns flanders.spec
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clojure.zip :as z]
   [flanders.types]
   [flanders.utils :as fu])
  (:import
   [flanders.types
    AnythingType
    BooleanType
    EitherType
    InstType
    IntegerType
    KeywordType
    MapEntry
    MapType
    NumberType
    SequenceOfType
    SetOfType
    SignatureType
    StringType]))

(defprotocol SpecedNode
  (->spec' [node ns f]))

(defn speced-node? [node]
  (satisfies? SpecedNode node))

(defn ns-str? [s]
  (and (string? s)
       (re-matches #"\w+(\.?\w+)*" s)))

(defn- key? [spec]
  (and (set? spec)
       (= 1 (count spec))
       (keyword? (first spec))))

(defn- dedup-entries
  "MapType maintains a list of MapEntryTypes.  Since this is a list
  and not a map, there is no duplicate key resolution.  This fn takes
  the entries from a MapType and returns a new list of entries with
  duplicates removed (based on the value of :spec, which must be set
  before calling this fn)."
  [entries]
  (->> entries
       (reduce (fn [m {:keys [spec] :as entry}]
                 (assoc m spec entry))
               (array-map))
       vals))

(defn with-gen
  "Used like clojure.spec/with-gen, except that it takes a node (that
  may have :_gen set) and a spec.  If set, :_gen must be a function
  of zero args that returns a generator."
  [{gen :_gen} spec]
  (if gen
    (s/with-gen spec gen)
    spec))

(extend-protocol SpecedNode

  ;; Branches

  EitherType
  (->spec' [{:keys [tests choices]} ns f]
    (when (seq tests)
      (throw (ex-info (str "WARNING: "))))
    (let [choices (->> choices
                       (map-indexed
                        (fn [i c]
                          (let [label (str "choice" i)
                                spec (f c (str ns "." label))]
                            (eval `(s/def ~(keyword ns label) ~spec))
                            (assoc c
                                   :label (keyword label)
                                   :spec spec)))))
          specs (map :spec choices)
          keys (map :label choices)]
      (eval `(s/or ~@(interleave keys specs)))))

  MapEntry
  (->spec' [{:keys [key type]} ns f]
    (let [key-spec (f key ns)]
      (when (key? key-spec)
        (let [key-name (-> key-spec first name)
              type-ns (str ns "." key-name)
              type-spec (f type type-ns)
              result-kw (keyword ns key-name)]
          (eval `(s/def ~result-kw ~type-spec))
          result-kw))))

  MapType
  (->spec' [{:keys [entries spec] :as node} ns f]
    (let [entries (->> entries
                       (map #(assoc % :spec (f % ns)))
                       (filter (comp keyword? :spec))
                       dedup-entries)
          [req-ents opt-ents] ((juxt filter remove) :required? entries)
          req-specs (mapv :spec req-ents)
          opt-specs (mapv :spec opt-ents)
          map-spec (if spec
                     (eval `(s/and (s/keys :req-un ~req-specs
                                           :opt-un ~opt-specs)
                                   ~spec))
                     (eval `(s/keys :req-un ~req-specs
                                    :opt-un ~opt-specs)))
          map-kw (keyword ns "map")]
      (eval `(s/def ~map-kw (with-gen ~node ~map-spec)))
      map-kw))

  SequenceOfType
  (->spec' [{:keys [type]} ns f]
    (let [result-kw (keyword ns "seq-of")]
      (eval `(s/def ~result-kw ~(f type (str ns "." "seq-of"))))
      (eval `(s/coll-of ~result-kw))))

  SetOfType
  (->spec' [{:keys [type]} ns f]
    (let [result-kw (keyword ns "set-of")]
      (eval `(s/def ~result-kw ~(f type (str ns "." "set-of"))))
      (eval `(s/coll-of ~result-kw :kind set?))))

  SignatureType
  (->spec' [{:keys [parameters rest-parameter return]} ns f]
    (let [parameters (get parameters :parameters)
          parameter-count (count parameters)
          ;; Using gensym to produce a unique function name pending a
          ;; better approach.
          fn-name (str (gensym "fn__") "_" parameter-count
                       (when (some? rest-parameter) "*"))
          result-kw (keyword ns fn-name)]
      (eval
       `(s/fdef ~result-kw
          :args (s/cat ~@(mapcat
                          (fn [i parameter]
                            (let [parameter-name (str "a" i)]
                              [(keyword parameter-name) (f parameter (str ns "." parameter-name))]))
                          (range)
                          parameters)
                       ~@(when (some? rest-parameter)
                           [:a* `(s/* ~(f rest-parameter (str ns ".a*")))]))
          :ret ~(if (some? return)
                  (f return (str ns ".return"))
                  `any?)))))

  ;; Leaves

  AnythingType
  (->spec' [{:keys [spec] :as node} _ _]
    (with-gen node
      (or spec any?)))

  BooleanType
  (->spec' [{:keys [open? spec default] :as node} _ _]
    (with-gen node
      (match [(some? spec) open? default]
             [true _   _  ] spec
             [_   true _  ] boolean?
             [_   _    nil] boolean?
             :else          #{default})))

  InstType
  (->spec' [{:keys [spec] :as node} _ _]
    (with-gen node
      (or spec
          inst?)))

  IntegerType
  (->spec' [{:keys [open? spec values] :as node} _ _]
    (with-gen node
      (match [(some? spec) open? values]
             [true _    _  ] spec
             [_    true _  ] integer?
             [_    _    nil] integer?
             :else           (set values))))

  KeywordType
  (->spec' [{:keys [open? spec values] :as node} _ _]
    (with-gen node
      (match [(some? spec) open? values]
             [true _     _  ] spec
             [_    true  _  ] keyword?
             [_    _     nil] keyword?
             :else            (set values))))

  NumberType
  (->spec' [{:keys [open? spec values] :as node} _ _]
    (with-gen node
      (match [(some? spec) open? (seq values)]
             [true _    _  ] spec
             [_    true _  ] number?
             [_    _    nil] number?
             :else           (set values))))

  StringType
  (->spec' [{:keys [open? spec values] :as node} _ _]
    (with-gen node
      (match [(some? spec) open? (seq values)]
             [true  _   _  ] spec
             [_    true _  ] string?
             [_    _    nil] string?
             :else           (set values)))))

(defn- assoc-generator
  "Used to assoc the generator function that will be used by the spec
  for the given node. Takes a clojure.zip location of a node.  The
  resulting generator will be assoc'ed onto the node as :_gen.  The
  result will be a function of zero args that returns a generator, as
  used by clojure.spec/with-gen.  If no generator has been set, then
  node is unchanged.

  Generators may be set on the node in one of three ways:
  - Using :gen-fn, the no-arg, generator returning function is set.
  - Using :gen, a generator is set, which will be wrapped in a fn.
  - Using :loc-gen, a function that takes a location and returns a
    generator should be set.  The resulting generator will be wrapped
    in a fn."
  [node-loc]
  (let [{:keys [gen loc-gen gen-fn] :as node} (z/node node-loc)]
    (cond
      gen-fn (assoc node :_gen gen-fn)
      gen (assoc node :_gen (constantly gen))
      loc-gen (assoc node :_gen (constantly (loc-gen node-loc)))
      :else node)))

(defn- assoc-generators
  "Walk the DDL tree and try to assoc a generator to each node,
  returning the updated tree"
  [ddl-node]
  (loop [current-loc (fu/->ddl-zip ddl-node)]
    (if (z/end? current-loc)
      (z/root current-loc)
      (recur (z/next (z/replace current-loc
                                (assoc-generator current-loc)))))))

(s/fdef ->spec
  :args (s/cat :node speced-node? :ns ns-str?)
  :ret #(satisfies? s/Spec %))

(defn ->spec [node ns]
  (letfn [(recursive-spec [node ns]
            (->spec' node ns recursive-spec))]
    (recursive-spec (assoc-generators node) ns)))
