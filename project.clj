(defproject threatgrid/flanders "1.0.3-SNAPSHOT"
  :description "flanders"
  :url "http://github.com/threatgrid/flanders"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :pedantic? :abort
  :dependencies [[org.clojure/clojure "1.11.4"]
                 [org.clojure/core.match "1.0.0"]
                 [cheshire "5.9.0"]

                 [prismatic/schema "1.2.0"]
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

  :profiles {:dev
             {:dependencies [[org.clojure/test.check "1.1.1"]
                             [metosin/malli "0.16.4"]
                             [prismatic/schema-generators "0.1.5" :exclusions [prismatic/schema]]]}})
