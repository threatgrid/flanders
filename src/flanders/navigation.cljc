(ns flanders.navigation
  (:require
   [clojure.zip :as z]
   [flanders.predicates :as fp]
   [flanders.utils :as fu]))

(defn- keyword-key
  "Given a zip location of a KeywordType, if it is the :key of a
  MapEntry, and if it must equal a specific keyword value, then return
  that value."
  [kw-loc]
  (when-let [{:keys [open? values]} (some-> kw-loc fp/keyword fp/key z/node)]
    (when (and (not open?)
               (= 1 (count values)))
      (first values))))

(defn- type-value
  "Given a zip location of a leaf node, it if must equal a specific
  value, then return that value."
  [type-loc]
  (when-let [{:keys [open? values]} (some-> type-loc fp/leaf z/node)]
    (when (and (not open?)
               (= 1 (count values)))
      (first values))))

(defn find-entry-loc
  "Given a zip location pointing at a MapType, find the first
  entry-location who's KeywordType key matches a given keyword."
  [map-loc kw-to-match]
  (last ;; there may be duplicates and last-match is preferred
   (filter (fn [entry-loc]
             (when-let [key-loc (some-> entry-loc fp/entry z/down)]
               (when (= kw-to-match
                        (keyword-key key-loc))
                 entry-loc)))
           (some-> map-loc z/down fu/right-loc-seq))))

(defn find-entry-value
  "Given a zip location pointing at a MapType, find the first
  entry-location who's KeywordType key matches a given keyword, and
  return the entry's type value if the type value must equal a single
  specific value."
  [map-loc key-kw]
  (some-> map-loc
          (find-entry-loc key-kw)
          z/down
          z/rightmost
          type-value))
