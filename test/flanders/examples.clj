(ns flanders.examples
  (:require [flanders.core :as f]))

(f/def-entity-type Example
  {:description "Example"}
  (f/required-entries
   (f/entry :foo f/any-str)
   (f/entry :bar (f/map
                  [(f/entry (f/eq "integer") f/any-int)
                   (f/entry :seq (f/seq-of f/any-keyword))
                   (f/entry :set (f/set-of (f/enum #{1 2 3})))]))
   (f/entry :yes? f/any-bool))
  (f/optional-entries
   (f/entry :spam (f/eq :eggs))))
