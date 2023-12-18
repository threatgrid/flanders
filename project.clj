(defproject threatgrid/flanders "0.1.24-SNAPSHOT"
  :description "flanders"
  :url "http://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :pedantic? :abort
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.match "1.0.0"]
                 [cheshire "5.9.0"]

                 [prismatic/schema "1.1.12"]
                 [metosin/ring-swagger "0.26.2"]
                 [metosin/schema-tools "0.12.2"]]
  :global-vars {*warn-on-reflection* true}
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "1.0.0"]
                             [metosin/malli "0.13.0"]]}})
