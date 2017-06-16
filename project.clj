(def schema-tools-version "0.9.0")
(def schema-version "1.1.3")

(defproject threatgrid/flanders "0.1.6"
  :description "flanders"
  :url "http://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clojure-future-spec "1.9.0-alpha14"]
                 [org.clojure/core.match "0.3.0-alpha4"
                  :exclusions [org.clojure/tools.reader]]
                 [cheshire "5.6.3"]

                 [prismatic/schema ~schema-version]
                 [metosin/ring-swagger "0.22.11"]
                 [metosin/schema-tools ~schema-tools-version]]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.9.0"]]}})
