(ns flanders.ocsf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [flanders.malli :as malli]
            [flanders.ocsf :as ocsf]
            [malli.core :as m]))

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
         (m/form (malli/->malli (ocsf/->flanders (json/decode (slurp (io/file "ocsf-schema/events/findings/compliance_finding.json"))))))))
#_
  (is (= nil
         (m/form (malli/->malli (ocsf/->flanders (json/decode (slurp (io/file "ocsf-schema/dictionary.json"))))))))
)
