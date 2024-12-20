(ns flanders.ocsf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [flanders.ocsf :as ocsf]))

(deftest flanders-test
  (ocsf/->flanders (json/decode (slurp (io/file "ocsf-schema/objects/cve.json")) keyword))
  )
