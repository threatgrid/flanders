(def schema-tools-version "0.12.2")
(def schema-version "1.1.12")

(defproject threatgrid/flanders "0.1.23-SNAPSHOT"
  :description "flanders"
  :url "http://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "0.3.0"]
                 [cheshire "5.9.0"]

                 [prismatic/schema ~schema-version]
                 [metosin/ring-swagger "0.26.2"]
                 [metosin/schema-tools ~schema-tools-version]]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.10.0"]]}})
