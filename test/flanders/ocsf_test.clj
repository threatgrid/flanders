(ns flanders.ocsf-test
  (:refer-clojure :exclude [prn println])
  (:require [babashka.process :as proc]
            [cheshire.core :as json]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [flanders.malli :as malli]
            [flanders.ocsf :as ocsf]
            [flanders.schema :as schema]
            [malli.core :as m]
            [schema.core :as s]))

(defn prn [& args] (locking prn (apply clojure.core/prn args)))
(defn println [& args] (locking prn (apply clojure.core/println args)))

(defn sort-recursive [v]
  (walk/postwalk
    (fn [v]
      (cond->> v
        (map? v) (into (sorted-map))))
    v))

(defn ocsf-server-down []
  (println "docker down ocsf-server")
  (proc/shell {:dir "tmp/ocsf-server"
               :out *out*
               :err *err*}
              "docker" "compose" "down"))

(defn ocsf-server-up [commit]
  (ocsf-server-down)
  (println "resetting ocsf-schema repo")
  (proc/shell {:dir "tmp/ocsf-schema"
               :out *out*
               :err *err*}
              "git" "reset" "--hard" commit)
  (println "docker up ocsf-server")
  (try (proc/shell {:dir "tmp/ocsf-server"
                    :out *out*
                    :err *err*}
                   "docker" "compose" "up" "--wait")
       (catch Exception e
         ;;?? ocsf-server-ocsf-elixir-1 exited with code 0
         (prn e)))
  (reduce 
    (fn [_ _]
      (Thread/sleep 500)
      (try (-> "http://localhost:8080/" client/get)
           (reduced true)
           (catch Exception _)))
    nil (range 10)))

(defn assert-map! [m] (assert (map? m)) m)

(defn gen-ocsf-schema-samples
  [{:keys [version base-url nsamples]}]
  (let [export-schema (-> (doto (format "threatgrid/ocsf-%s-export.json" version) prn) io/resource slurp json/decode)
        sample {"objects" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall
                                                  (pmap (fn [_]
                                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                                          (let [url (str base-url "sample/objects/" name)]
                                                            ;(prn url)
                                                            (try (-> url client/get :body json/decode assert-map!)
                                                                 (catch Exception e
                                                                   (prn url)
                                                                   (throw e)))))
                                                        (range nsamples)))]))
                                (keys (get export-schema "objects")))
                "base_event" (doall
                               (pmap (fn [_]
                                       (when (Thread/interrupted) (throw (InterruptedException.)))
                                       (let [url (str base-url "sample/base_event")]
                                         ;(prn url)
                                         (try (-> url client/get :body json/decode assert-map!)
                                              (catch Exception e
                                                (prn url)
                                                (throw e)))))
                                     (range nsamples)))
                "classes" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall (pmap (fn [_]
                                                               (when (Thread/interrupted) (throw (InterruptedException.)))
                                                               (let [url (str base-url "sample/classes/" name)]
                                                                 ;(prn url)
                                                                 (try (-> url client/get :body json/decode assert-map!)
                                                                      (catch Exception e
                                                                        (prn url)
                                                                        (throw e)))))
                                                             (range nsamples)))]))
                                (keys (get export-schema "classes")))}]
    (spit (doto (format "tmp/flanders/ocsf-%s-sample.json" version)
            io/make-parents)
          (-> sample
              sort-recursive
              (json/encode {:pretty true})))))

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
  [;; not included in https://github.com/frenchy64/ocsf-schema-export
   #_{:version "1.4.0-dev"
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

(defn gen-ocsf-samples []
  (prep-ocsf-repos)
  (try (doseq [m all-ocsf-exports]
         (ocsf-server-up (:ocsf-schema m))
         (gen-ocsf-schema-samples (assoc m :base-url "http://localhost:8080/"))
         (ocsf-server-down))
       (finally
         (ocsf-server-down))))

(def ocsf-1-3-0-export (delay (json/decode (slurp (io/resource "threatgrid/ocsf-1.3.0-export.json")))))

(deftest flanders-test
  (is (= [:map
          {:closed true,
           :json-schema/example
           {:cvss [{"anything" "anything"}],
            :desc "string",
            :cwe {"anything" "anything"},
            :uid "string",
            :epss {"anything" "anything"},
            :created_time 10,
            :type "string",
            :cwe_url "string",
            :references ["string"],
            :title "string",
            :product {"anything" "anything"},
            :modified_time 10,
            :created_time_dt "string",
            :cwe_uid "string",
            :modified_time_dt "string"},
           :json-schema/description
           "The Common Vulnerabilities and Exposures (CVE) object represents publicly disclosed cybersecurity vulnerabilities defined in CVE Program catalog (<a target='_blank' href='https://cve.mitre.org/'>CVE</a>). There is one CVE Record for each vulnerability in the catalog."}
          [:created_time
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The Record Creation Date identifies when the CVE ID was issued to a CVE Numbering Authority (CNA) or the CVE Record was published on the CVE List. Note that the Record Creation Date does not necessarily indicate when this vulnerability was discovered, shared with the affected vendor, publicly disclosed, or updated in CVE."}
           [:int #:json-schema{:example 10}]]
          [:created_time_dt
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The Record Creation Date identifies when the CVE ID was issued to a CVE Numbering Authority (CNA) or the CVE Record was published on the CVE List. Note that the Record Creation Date does not necessarily indicate when this vulnerability was discovered, shared with the affected vendor, publicly disclosed, or updated in CVE."}
           [:string #:json-schema{:example "string"}]]
          [:cvss
           {:json-schema/example [{"anything" "anything"}],
            :optional true,
            :json-schema/description
            "The CVSS object details Common Vulnerability Scoring System (<a target='_blank' href='https://www.first.org/cvss/'>CVSS</a>) scores from the advisory that are related to the vulnerability."}
           [:sequential
            [:map
             {:closed true, :json-schema/example {"anything" "anything"}}
             [:malli.core/default
              #:json-schema{:example "anything"}
              [:map-of :any [:any #:json-schema{:example "anything"}]]]]]]
          [:cwe
           {:json-schema/example {"anything" "anything"},
            :optional true,
            :json-schema/description
            "The CWE object represents a weakness in a software system that can be exploited by a threat actor to perform an attack. The CWE object is based on the <a target='_blank' href='https://cwe.mitre.org/'>Common Weakness Enumeration (CWE)</a> catalog."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:cwe_uid
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The <a target='_blank' href='https://cwe.mitre.org/'>Common Weakness Enumeration (CWE)</a> unique identifier. For example: <code>CWE-787</code>."}
           [:string #:json-schema{:example "string"}]]
          [:cwe_url
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "Common Weakness Enumeration (CWE) definition URL. For example: <code>https://cwe.mitre.org/data/definitions/787.html</code>."}
           [:string #:json-schema{:example "string"}]]
          [:desc
           {:json-schema/example "string",
            :optional true,
            :json-schema/description "A brief description of the CVE Record."}
           [:string #:json-schema{:example "string"}]]
          [:epss
           {:json-schema/example {"anything" "anything"},
            :optional true,
            :json-schema/description
            "The Exploit Prediction Scoring System (EPSS) object describes the estimated probability a vulnerability will be exploited. EPSS is a community-driven effort to combine descriptive information about vulnerabilities (CVEs) with evidence of actual exploitation in-the-wild. (<a target='_blank' href='https://www.first.org/epss/'>EPSS</a>)."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:modified_time
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The Record Modified Date identifies when the CVE record was last updated."}
           [:int #:json-schema{:example 10}]]
          [:modified_time_dt
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The Record Modified Date identifies when the CVE record was last updated."}
           [:string #:json-schema{:example "string"}]]
          [:product
           {:json-schema/example {"anything" "anything"},
            :optional true,
            :json-schema/description
            "The product where the vulnerability was discovered."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:references
           {:json-schema/example ["string"],
            :optional true,
            :json-schema/description
            "A list of reference URLs with additional information about the CVE Record."}
           [:sequential [:string #:json-schema{:example "string"}]]]
          [:title
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "A title or a brief phrase summarizing the CVE record."}
           [:string #:json-schema{:example "string"}]]
          [:type
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "<p>The vulnerability type as selected from a large dropdown menu during CVE refinement.</p>Most frequently used vulnerability types are: <code>DoS</code>, <code>Code Execution</code>, <code>Overflow</code>, <code>Memory Corruption</code>, <code>Sql Injection</code>, <code>XSS</code>, <code>Directory Traversal</code>, <code>Http Response Splitting</code>, <code>Bypass something</code>, <code>Gain Information</code>, <code>Gain Privileges</code>, <code>CSRF</code>, <code>File Inclusion</code>. For more information see <a target='_blank' href='https://www.cvedetails.com/vulnerabilities-by-types.php'>Vulnerabilities By Type</a> distributions."}
           [:string #:json-schema{:example "string"}]]
          [:uid
           #:json-schema{:example "string",
                         :description
                         "The Common Vulnerabilities and Exposures unique number assigned to a specific computer vulnerability. A CVE Identifier begins with 4 digits representing the year followed by a sequence of digits that acts as a unique identifier. For example: <code>CVE-2021-12345</code>."}
           [:string #:json-schema{:example "string"}]]]
         (m/form (malli/->malli (ocsf/->flanders (get-in @ocsf-1-3-0-export ["objects" "cve"]))))))
  (is (= '{(optional-key :comment) Str,
           :severity_id Int,
           :category_uid Int,
           (optional-key :remediation) {Any Any},
           (optional-key :status_code) Str,
           (optional-key :message) Str,
           (optional-key :count) Int,
           (optional-key :start_time_dt) Str,
           (optional-key :end_time) Int,
           :osint [{Any Any}],
           :type_uid Int,
           (optional-key :resources) [{Any Any}],
           :cloud {Any Any},
           :time Int,
           (optional-key :status) Str,
           (optional-key :observables) [{Any Any}],
           (optional-key :api) {Any Any},
           (optional-key :duration) Int,
           :class_uid Int,
           (optional-key :confidence) Str,
           (optional-key :end_time_dt) Str,
           (optional-key :start_time) Int,
           :finding_info {Any Any},
           (optional-key :unmapped) {Any Any},
           (optional-key :activity_name) Str,
           (optional-key :timezone_offset) Int,
           (optional-key :time_dt) Str,
           (optional-key :severity) Str,
           (optional-key :category_name) Str,
           (optional-key :class_name) Str,
           (optional-key :actor) {Any Any},
           (optional-key :raw_data) Str,
           (optional-key :confidence_id) Int,
           (optional-key :status_id) Int,
           (optional-key :type_name) Str,
           :activity_id Int,
           (optional-key :confidence_score) Int,
           (optional-key :resource) {Any Any},
           :metadata {Any Any},
           :compliance {Any Any},
           (optional-key :status_detail) Str,
           (optional-key :device) {Any Any},
           (optional-key :enrichments) [{Any Any}]}
         (s/explain (schema/->schema (ocsf/->flanders (get-in @ocsf-1-3-0-export ["classes" "compliance_finding"])))))))

(defn test-ocsf-version [{:keys [version nobjects nclasses nsamples]}]
  (let [;; via https://github.com/frenchy64/ocsf-schema-export
        export (-> (json/decode (slurp (io/resource (format "threatgrid/ocsf-%s-export.json" version))))
                   ocsf/parse-exported-schemas)
        ;; generated by (gen-ocsf-samples)
        sample (json/decode (slurp (io/file (format "tmp/flanders/ocsf-%s-sample.json" version))))]
    (when (is (= version (get export "version")))
      (doseq [[k nexpected] {"objects" nobjects "classes" nclasses}]
        (let [objects (get export k)
              examples (get sample k)]
          (when (is (= nexpected (count objects)))
            (doseq [[name fl] objects]
              (when (Thread/interrupted) (throw (InterruptedException.)))
              (testing name
                (let [m (malli/->malli fl)
                      s (schema/->schema fl)
                      good-examples (map walk/keywordize-keys (get examples name))]
                  (when (is (= nsamples (count good-examples)))
                    (doseq [good-example good-examples
                            :let [good-example (if (and (= version "1.1.0")
                                                        (= k "classes")
                                                        (= name "incident_finding"))
                                                 ;; fix https://github.com/ocsf/ocsf-server/issues/123
                                                 (reduce (fn [good-example field]
                                                           (if (string? (get good-example field))
                                                             (update good-example field count)
                                                             good-example))
                                                         good-example [:priority :distributor])
                                                 good-example)
                                  bad-example (assoc good-example ::junk "foo")]]
                      (is (nil? (m/explain m good-example)) (pr-str good-example))
                      (is (nil? (s/check s good-example)) (pr-str good-example))
                      (is (m/explain m bad-example) (pr-str bad-example))
                      (is (s/check s bad-example) (pr-str bad-example))))))))))
      (testing "base_event"
        (let [base-event (get export "base_event")
              examples (get sample "base_event")
              _m (is (malli/->malli base-event))
              _s (is (schema/->schema base-event))
              good-examples (map walk/keywordize-keys examples)]
          (is (= nsamples (count good-examples)))
          #_ ;;TODO examples for base event seem to be an open map?
          (doseq [good-example good-examples
                  :let [bad-example (assoc good-example ::junk "foo")]]
            (is (nil? (m/explain m good-example)))
            (is (nil? (s/check s good-example)))
            (is (m/explain m bad-example))
            (is (s/check s bad-example))))))))

;; requires docker running
(deftest ^:integration test-all-ocsf-versions
  (gen-ocsf-samples)
  (doseq [config all-ocsf-exports]
    (testing (:version config)
      (test-ocsf-version config))))

(deftest cisco-ocsf-schema-test
  (is (= 33 (-> "flanders_test/cisco_network_activity.json" io/resource slurp json/decode ocsf/->flanders schema/->schema count))))
