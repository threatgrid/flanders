(def schema-tools-version "0.9.1")
(def schema-version "1.1.7")

(defproject threatgrid/flanders "0.1.15"
  :description "flanders"
  :url "http://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"
                  :exclusions [org.clojure/tools.reader]]
                 [cheshire "5.8.0"]

                 [prismatic/schema ~schema-version]
                 [metosin/ring-swagger "0.24.4"]
                 [metosin/schema-tools ~schema-tools-version]]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "0.9.0"]]}})
