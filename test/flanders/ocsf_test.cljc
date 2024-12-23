(ns flanders.ocsf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-http.client :as client]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [flanders.malli :as malli]
            [schema.core :as s]
            [flanders.schema :as schema]
            [flanders.ocsf :as ocsf]
            [clojure.walk :as walk]
            [malli.core :as m]))

(defn sort-recursive [v]
  (walk/postwalk
    (fn [v]
      (cond->> v
        (map? v) (into (sorted-map))))
    v))

(def nsamples 10)

;"https://schema.ocsf.io/api/1.3.0/classes/http_activity"
(defn gen-ocsf [{:keys [origin version base-url]
                 :or {origin "https://schema.ocsf.io"
                      version "1.3.0"}}]
  (let [base-url (or base-url
                     (str origin "/" version "/"))
        _ (prn "base-url" base-url)
        export-schema (-> (str base-url "export/schema") client/get :body json/decode)
        _ (assert (map? export-schema))
        sample {"objects" (into {} (map (fn [name]
                                          (when (Thread/interrupted) (throw (InterruptedException.)))
                                          [name (doall
                                                  (pmap (fn [_]
                                                          (do (when (Thread/interrupted) (throw (InterruptedException.)))
                                                              (let [url (str base-url "sample/objects/" name)]
                                                                (prn url)
                                                                (try (-> url client/get :body json/decode)
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
                                         (try (-> url client/get :body json/decode)
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
                                                                     (try (-> url client/get :body json/decode)
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
           (spit (format "test-resources/flanders/ocsf-1.3.0-sample.json" version)
                 (-> sample
                     sort-recursive
                     (json/encode {:pretty true})))))))

(comment
  ;;regenerate
  (gen-ocsf {:version "1.3.0"})
  (gen-ocsf {:version "1.4.0-dev"})
  (gen-ocsf {:base-url "http://localhost:8080/"
             :version "1.4.0-dev"})
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

(def ocsf-1-3-0-export (delay (json/decode (slurp (io/resource "ocsf-1.3.0-export.json")))))
(def ocsf-1-3-0-sample (delay (json/decode (slurp (io/resource "ocsf-1.3.0-sample.json")))))

(deftest ocsf-1-3-0-export-test
  (is (= "1.3.0" (get @ocsf-1-3-0-export "version")))
  (doseq [[k nexpected] {"objects" 121 "classes" 72}]
    (let [objects (get @ocsf-1-3-0-export k)
          examples (get @ocsf-1-3-0-sample k)]
      (is (= nexpected (count objects)))
      (doseq [[name obj] objects]
        (testing name
          (let [m (is (malli/->malli (ocsf/->flanders obj)))
                s (is (schema/->schema (ocsf/->flanders obj)))
                good-examples (map walk/keywordize-keys (get examples name))]
            (is (= nsamples (count good-examples)))
            (doseq [good-example good-examples
                    :let [bad-example (assoc good-example ::junk "foo")]]
              (is (nil? (m/explain m good-example)))
              (is (nil? (s/check s good-example)))
              (is (m/explain m bad-example))
              (is (s/check s bad-example))))))))
  (let [types (get @ocsf-1-3-0-export "types")]
    (is (= 22 (count types)))
    (doseq [[name obj] types]
      (testing name
        (is (malli/->malli (ocsf/->flanders obj)))
        (is (schema/->schema (ocsf/->flanders obj))))))
  (testing "base_event"
    (let [base-event (get @ocsf-1-3-0-export "base_event")
          examples (get @ocsf-1-3-0-sample "base_event")
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
        (is (s/check s bad-example))))))
