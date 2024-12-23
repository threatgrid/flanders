(ns flanders.ocsf-test
  (:refer-clojure :exclude [prn println])
  (:require [babashka.process :as proc]
            [clojure.test :refer [deftest is testing]]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [flanders.malli :as malli]
            [schema.core :as s]
            [flanders.schema :as schema]
            [flanders.ocsf :as ocsf]
            [clojure.walk :as walk]
            [malli.core :as m]))

(defn prn [& args] (locking prn (apply clojure.core/prn args)))
(defn println [& args] (locking prn (apply clojure.core/println args)))

(defn sort-recursive [v]
  (walk/postwalk
    (fn [v]
      (cond->> v
        (map? v) (into (sorted-map))))
    v))

(defn ocsf-server-down []
  (println "down")
  (proc/shell {:dir "tmp/ocsf-server"
               :out *out*
               :err *err*}
              "docker-compose" "down"))

(defn ocsf-server-up [commit]
  (ocsf-server-down)
  (println "reset")
  (proc/shell {:dir "tmp/ocsf-schema"
               :out *out*
               :err *err*}
              "git" "reset" "--hard" commit)
  (println "up")
  (try (proc/shell {:dir "tmp/ocsf-server"
                    :out *out*
                    :err *err*}
                   "docker-compose" "up" "--wait")
       (catch Exception e
         ;;?? ocsf-server-ocsf-elixir-1 exited with code 0
         (prn e)))
  (reduce 
    (fn [_ _]
      (Thread/sleep 500)
      (try (-> "http://localhost:8080/" client/get)
           (reduced true)
           (catch Exception _)))
    nil (range 10)
    )
  )

(defn assert-map! [m] (assert (map? m)) m)

(defn gen-ocsf-json-schema [{:keys [origin version base-url ocsf-schema]
                             :or {origin "https://schema.ocsf.io"
                                  version "1.3.0"}}]
  (let [base-url (or base-url
                     (str origin "/" version "/"))
        export-schema (-> (str base-url "export/schema") client/get :body json/decode assert-map!)
        export-json-schema {"objects" (into {} (map (fn [name]
                                                      (when (Thread/interrupted) (throw (InterruptedException.)))
                                                      [name (let [url (str base-url "schema/objects/" name)]
                                                              (prn url)
                                                              (try (-> url client/get :body json/decode assert-map!)
                                                                   (catch Exception e
                                                                     (prn "FAILED" url)
                                                                     (throw e))))]))
                                            (keys (get export-schema "objects")))
                            "base_event" (let [url (str base-url "schema/classes/base_event")]
                                           (prn url)
                                           (try (-> url client/get :body json/decode assert-map!)
                                                (catch Exception e
                                                  (prn "FAILED" url)
                                                  (throw e))))
                            "classes" (into {} (map (fn [name]
                                                      (when (Thread/interrupted) (throw (InterruptedException.)))
                                                      [name (let [url (str base-url "schema/classes/" name)]
                                                              (prn url)
                                                              (try (-> url client/get :body json/decode assert-map!)
                                                                   (catch Exception e
                                                                     (prn "FAILED" url)
                                                                     (throw e))))]))
                                            (keys (get export-schema "classes")))}]
    (spit (format "resources/flanders/ocsf-%s-json-schema-export.json" version)
          (-> export-json-schema
              sort-recursive
              (json/encode {:pretty true})))))

;"https://schema.ocsf.io/api/1.3.0/classes/http_activity"
(defn gen-ocsf-schema [{:keys [origin version base-url ocsf-schema nsamples]
                 :or {origin "https://schema.ocsf.io"
                      version "1.3.0"}}]
  (let [base-url (or base-url
                     (str origin "/" version "/"))
        _ (prn "base-url" base-url)
        export-schema (-> (str base-url "export/schema") client/get :body json/decode assert-map!)
        _ (assert (map? export-schema))
        sample {"objects" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall
                                                  (pmap (fn [_]
                                                          (do (when (Thread/interrupted) (throw (InterruptedException.)))
                                                              (let [url (str base-url "sample/objects/" name)]
                                                                (prn url)
                                                                (try (-> url client/get :body json/decode assert-map!)
                                                                     (catch Exception e
                                                                       (prn url)
                                                                       (throw e))))))
                                                        (range nsamples)))]))
                                (keys (get export-schema "objects")))
                "base_event" (doall
                               (pmap (fn [_]
                                       (when (Thread/interrupted) (throw (InterruptedException.)))
                                       (let [url (str base-url "sample/base_event")]
                                         (prn url)
                                         (try (-> url client/get :body json/decode assert-map!)
                                              (catch Exception e
                                                (prn url)
                                                (throw e)))))
                                     (range nsamples)))
                "classes" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall (pmap (fn [_]
                                                               (do (when (Thread/interrupted) (throw (InterruptedException.)))
                                                                   (let [url (str base-url "sample/classes/" name)]
                                                                     (prn url)
                                                                     (try (-> url client/get :body json/decode assert-map!)
                                                                          (catch Exception e
                                                                            (prn url)
                                                                            (throw e))))))
                                                             (range nsamples)))]))
                                (keys (get export-schema "classes")))}]
    (try (spit (format "resources/flanders/ocsf-%s-export.json" version)
               (-> export-schema
                   sort-recursive
                   (json/encode {:pretty true})))
         (finally
           (spit (format "test-resources/flanders/ocsf-%s-sample.json" version)
                 (-> sample
                     sort-recursive
                     (json/encode {:pretty true})))))))

(defn prep-ocsf-repos []
  (doseq [repo ["ocsf-server" "ocsf-schema"]
          :let [tmp-dir "tmp"
                dir (str tmp-dir "/" repo)]]
    (if (.exists (io/file dir))
      (proc/shell {:dir dir
                   :out *out*
                   :err *err*}
                  "git" "fetch" "--all")
      (do (proc/shell {:out *out*
                       :err *err*}
                      "mkdir" "-p" tmp-dir)
          (proc/shell {:dir tmp-dir
                       :out *out*
                       :err *err*}
                      "git" "clone" (format "https://github.com/ocsf/%s.git" repo))))))

(def all-ocsf-exports
  [{:version "1.4.0-dev"
    :nsamples 10
    :nobjects 141
    :nclasses 78
    :ocsf-schema "origin/main"}
   {:version "1.3.0"
    :nsamples 10
    :nobjects 121
    :nclasses 72
    :ocsf-schema "1.3.0"}
   {:version "1.2.0"
    :nsamples 10
    :nobjects 111
    :nclasses 65
    :ocsf-schema "v1.2.0"}
   {:version "1.1.0"
    :nsamples 10
    :nobjects 106
    :nclasses 50
    :ocsf-schema "v1.1.0"}
   {:version "1.0.0"
    :nsamples 10
    :nobjects 84
    :nclasses 36
    :ocsf-schema "v1.0.0"}])

(defn sync-ocsf-export []
  (prep-ocsf-repos)
  (try (doseq [m all-ocsf-exports]
         (some-> (:ocsf-schema m) ocsf-server-up)
         (gen-ocsf-schema (assoc m :base-url "http://localhost:8080/"))
         (gen-ocsf-json-schema (assoc m :base-url "http://localhost:8080/"))
         (ocsf-server-down))
       (finally
         (ocsf-server-down))))

(comment
  (sync-ocsf-export)
  ;;regenerate
  (gen-ocsf {:version "1.3.0"})
  (gen-ocsf {:version "1.4.0-dev"})
  (gen-ocsf {:base-url "http://localhost:8080/"
             :version "1.4.0-dev"
             :ocsf-schema "origin/main"})
  (gen-ocsf {:base-url "http://localhost:8080/"
             :version "1.3.0"
             :ocsf-schema "1.3.0"})
  (gen-ocsf {:base-url "http://localhost:8080/"
             :version "1.2.0"
             :ocsf-schema "v1.2.0"})
  (gen-ocsf {:base-url "http://localhost:8080/"
             :version "1.1.0"
             :ocsf-schema "v1.1.0"})
  (gen-ocsf {:base-url "http://localhost:8080/"
             :version "1.0.0"
             :ocsf-schema "v1.0.0"})
  )

(deftest flanders-test
  (is (= [:map
          {:closed true,
           :json-schema/example
           {:cvss "anything",
            :desc "anything",
            :cwe "anything",
            :uid "anything",
            :epss "anything",
            :created_time "anything",
            :type "anything",
            :cwe_url "anything",
            :references "anything",
            :title "anything",
            :product "anything",
            :modified_time "anything",
            :cwe_uid "anything"}}
          [:created_time
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "The Record Creation Date identifies when the CVE ID was issued to a CVE Numbering Authority (CNA) or the CVE Record was published on the CVE List. Note that the Record Creation Date does not necessarily indicate when this vulnerability was discovered, shared with the affected vendor, publicly disclosed, or updated in CVE."}
           [:any {:json-schema/example "anything"}]]
          [:cvss
           {:json-schema/example "anything", :optional true}
           [:any {:json-schema/example "anything"}]]
          [:cwe
           {:json-schema/example "anything", :optional true}
           [:any {:json-schema/example "anything"}]]
          [:cwe_uid
           {:json-schema/example "anything", :optional true}
           [:any {:json-schema/example "anything"}]]
          [:cwe_url
           {:json-schema/example "anything", :optional true}
           [:any {:json-schema/example "anything"}]]
          [:desc
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description "A brief description of the CVE Record."}
           [:any {:json-schema/example "anything"}]]
          [:epss
           {:json-schema/example "anything", :optional true}
           [:any {:json-schema/example "anything"}]]
          [:modified_time
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "The Record Modified Date identifies when the CVE record was last updated."}
           [:any {:json-schema/example "anything"}]]
          [:product
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "The product where the vulnerability was discovered."}
           [:any {:json-schema/example "anything"}]]
          [:references
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "A list of reference URLs with additional information about the CVE Record."}
           [:any {:json-schema/example "anything"}]]
          [:title
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "A title or a brief phrase summarizing the CVE record."}
           [:any {:json-schema/example "anything"}]]
          [:type
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "<p>The vulnerability type as selected from a large dropdown menu during CVE refinement.</p>Most frequently used vulnerability types are: <code>DoS</code>, <code>Code Execution</code>, <code>Overflow</code>, <code>Memory Corruption</code>, <code>Sql Injection</code>, <code>XSS</code>, <code>Directory Traversal</code>, <code>Http Response Splitting</code>, <code>Bypass something</code>, <code>Gain Information</code>, <code>Gain Privileges</code>, <code>CSRF</code>, <code>File Inclusion</code>. For more information see <a target='_blank' href='https://www.cvedetails.com/vulnerabilities-by-types.php'>Vulnerabilities By Type</a> distributions."}
           [:any {:json-schema/example "anything"}]]
          [:uid
           {:json-schema/example "anything",
            :json-schema/description
            "The Common Vulnerabilities and Exposures unique number assigned to a specific computer vulnerability. A CVE Identifier begins with 4 digits representing the year followed by a sequence of digits that acts as a unique identifier. For example: <code>CVE-2021-12345</code>."}
           [:any {:json-schema/example "anything"}]]]
         (m/form (malli/->malli (ocsf/->flanders (json/decode (slurp (io/file "ocsf-schema/objects/cve.json"))))))))
  (is (= [:map
          {:closed true,
           :json-schema/example
           {:compliance "anything",
            :remediation "anything",
            :resource "anything",
            :resources "anything"}}
          [:compliance
           {:json-schema/example "anything"}
           [:any {:json-schema/example "anything"}]]
          [:remediation
           {:json-schema/example "anything", :optional true}
           [:any {:json-schema/example "anything"}]]
          [:resource
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "Describes details about the resource that is the subject of the compliance check."}
           [:any {:json-schema/example "anything"}]]
          [:resources
           {:json-schema/example "anything",
            :optional true,
            :json-schema/description
            "Describes details about the resource/resouces that are the subject of the compliance check."}
           [:any {:json-schema/example "anything"}]]]
         (m/form (malli/->malli (ocsf/->flanders (json/decode (slurp (io/file "ocsf-schema/events/findings/compliance_finding.json")))))))))

(defn test-ocsf-version [{:keys [version nobjects nclasses nsamples]}]
  (let [export (json/decode (slurp (io/resource (format "flanders/ocsf-%s-export.json" version))))
        sample (json/decode (slurp (io/resource (format "flanders/ocsf-%s-sample.json" version))))]
    (when (is (= version (get export "version")))
      (doseq [[k nexpected] {"objects" nobjects "classes" nclasses}]
        (let [objects (get export k)
              examples (get sample k)]
          (when (is (= nexpected (count objects)))
            (doseq [[name obj] objects]
              (when (Thread/interrupted) (throw (InterruptedException.)))
              (testing name
                (let [m (is (malli/->malli (ocsf/->flanders obj)))
                      s (is (schema/->schema (ocsf/->flanders obj)))
                      good-examples (map walk/keywordize-keys (get examples name))]
                  (when (is (= nsamples (count good-examples)))
                    (doseq [good-example good-examples
                            :let [bad-example (assoc good-example ::junk "foo")]]
                      (is (nil? (m/explain m good-example)))
                      (is (nil? (s/check s good-example)))
                      (is (m/explain m bad-example))
                      (is (s/check s bad-example))))))))))
      #_
      (let [types (get export "types")]
        (is (= 22 (count types)))
        (doseq [[name obj] types]
          (when (Thread/interrupted) (throw (InterruptedException.)))
          (testing name
            (is (malli/->malli (ocsf/->flanders obj)))
            (is (schema/->schema (ocsf/->flanders obj))))))
      (testing "base_event"
        (let [base-event (get export "base_event")
              examples (get sample "base_event")
              m (is (malli/->malli (ocsf/->flanders base-event)))
              s (is (schema/->schema (ocsf/->flanders base-event)))
              good-examples (map walk/keywordize-keys examples)]
          (is (= nsamples (count good-examples)))
          #_ ;;TODO examples for base event seem to be an open map?
          (doseq [good-example good-examples
                  :let [bad-example (assoc good-example ::junk "foo")]]
            (is (nil? (m/explain m good-example)))
            (is (nil? (s/check s good-example)))
            (is (m/explain m bad-example))
            (is (s/check s bad-example))))))))

(deftest test-all-ocsf-versions
  (doseq [config all-ocsf-exports]
    (testing (:version config)
      (test-ocsf-version config))))
