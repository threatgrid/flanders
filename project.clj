(defproject threatgrid/flanders "1.0.3-SNAPSHOT"
  :description "flanders"
  :url "http://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :pedantic? :abort
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/core.match "1.0.0"]
                 [prismatic/schema "1.2.0"]
                 [cheshire "5.9.0"]
                 [metosin/ring-swagger "1.0.0"]
                 [metosin/schema-tools "0.12.3"]]
  :global-vars {*warn-on-reflection* true}
  :release-tasks [["clean"]
                  ["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "--no-sign"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :resource-paths ["resources"]
  :profiles {:dev
             {:dependencies [[org.clojure/test.check "1.1.1"]
                             [clj-http "3.13.0"]
                             [potemkin "0.4.7"]
                             [metosin/malli "0.17.0"]]
              :resource-paths ["test-resources"]}})
