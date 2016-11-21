(ns flanders.spec
  (:require [clojure.core.match :refer [match]]
            [clojure.future :refer :all] ;; Remove after CLJ 1.9
            [clojure.spec :as s]
            [flanders.types])
  (:import [flanders.types
            AnythingType BooleanType EitherType InstType IntegerType
            KeywordType MapEntry MapType NumberType SequenceOfType
            SetOfType StringType]))

(defprotocol SpecedNode
  (->spec' [node ns f]))

(defn speced-node? [node]
  (satisfies? SpecedNode node))

(defn ns-str? [s]
  (and (string? s)
       (re-matches #"\w+(\.?\w+)*" s)))

(defn ->spec [node ns]
  (->spec' node ns ->spec))

(s/fdef ->spec
        :args (s/cat :node speced-node? :ns ns-str?)
        :ret #(satisfies? s/Spec %))

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

(extend-protocol SpecedNode

  ;; Branches

  EitherType
  (->spec' [{:keys [choices]} ns f]
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
  (->spec' [{:keys [entries]} ns f]
    (let [entries (->> entries
                       (map #(assoc % :spec (f % ns)))
                       (filter (comp keyword? :spec))
                       dedup-entries)
          [req-ents opt-ents] ((juxt filter remove) :required? entries)
          req-specs (mapv :spec req-ents)
          opt-specs (mapv :spec opt-ents)
          map-spec (eval `(s/keys :req-un ~req-specs
                                  :opt-un ~opt-specs))
          map-kw (keyword ns "map")]
      (eval `(s/def ~map-kw ~map-spec))
      map-kw))

  SequenceOfType
  (->spec' [{:keys [type]} ns f]
    (let [result-kw (keyword ns "seq-of")]
      (eval `(s/def ~result-kw ~(f type ns)))
      (eval `(s/coll-of ~result-kw))))

  SetOfType
  (->spec' [{:keys [type]} ns f]
    (let [result-kw (keyword ns "set-of")]
      (eval `(s/def ~result-kw ~(f type ns)))
      (eval `(s/coll-of ~(f type ns) :kind set?))))

  ;; Leaves

  AnythingType
  (->spec' [{:keys [spec]} _ _]
    (or spec
        any?))

  BooleanType
  (->spec' [{:keys [open? spec default]} _ _]
    (match [(some? spec) open? default]
           [true _   _  ] spec
           [_   true _  ] boolean?
           [_   _    nil] boolean?
           :else          #{default}))

  InstType
  (->spec' [{:keys [spec]} _ _]
    (or spec
        inst?))

  IntegerType
  (->spec' [{:keys [open? spec values]} _ _]
    (match [(some? spec) open? values]
           [true _    _  ] spec
           [_    true _  ] integer?
           [_    _    nil] integer?
           :else           (set values)))

  KeywordType
  (->spec' [{:keys [open? spec values]} _ _]
    (match [(some? spec) open? values]
           [true _     _  ] spec
           [_    true  _  ] keyword?
           [_    _     nil] keyword?
           :else            (set values)))

  NumberType
  (->spec' [{:keys [open? spec values]} _ _]
    (match [(some? spec) open? (seq values)]
           [true _    _  ] spec
           [_    true _  ] number?
           [_    _    nil] number?
           :else           (set values)))

  StringType
  (->spec' [{:keys [open? spec values]} _ _]
    (match [(some? spec) open? (seq values)]
           [true  _   _  ] spec
           [_    true _  ] string?
           [_    _    nil] string?
           :else           (set values))))
