(defproject threatgrid/flanders "1.0.0-alpha1"
  :description "flanders"
  :url "https://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.2"]
                 [org.clojure/core.match "1.0.0"]
                 [cheshire "5.9.0"]
                 [prismatic/schema "1.1.12"]
                 [metosin/ring-swagger "0.26.2"]
                 [metosin/schema-tools "0.12.2"]]
  :profiles {:test {:dependencies [[org.clojure/test.check "1.0.0"]]}
             :ci {:pedantic? :abort
                  :global-vars {*warn-on-reflection* true}}})
