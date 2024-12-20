(ns flanders.ocsf-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [flanders.malli :as malli]
            [flanders.ocsf :as ocsf]
            [malli.core :as m]))

(deftest flanders-test
  (is (= [:map {:closed true,
                :json-schema/example
                {:cvss "anything", :desc "anything", :cwe "anything", :uid "anything", :epss "anything",
                 :created_time "anything", :type "anything", :cwe_url "anything", :references "anything",
                 :title "anything", :product "anything", :modified_time "anything", :cwe_uid "anything"}}
          [:cvss {:json-schema/example "anything", :optional true}
           [:any #:json-schema{:example "anything"}]]
          [:desc {:json-schema/example "anything", :optional true}
           [:any #:json-schema{:example "anything", :description "A brief description of the CVE Record."}]]
          [:cwe {:json-schema/example "anything", :optional true}
           [:any #:json-schema{:example "anything"}]]
          [:uid #:json-schema{:example "anything"} [:any #:json-schema{:example "anything", :description "The Common Vulnerabilities and Exposures unique number assigned to a specific computer vulnerability. A CVE Identifier begins with 4 digits representing the year followed by a sequence of digits that acts as a unique identifier. For example: <code>CVE-2021-12345</code>."}]]
          [:epss {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything"}]]
          [:created_time {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything", :description "The Record Creation Date identifies when the CVE ID was issued to a CVE Numbering Authority (CNA) or the CVE Record was published on the CVE List. Note that the Record Creation Date does not necessarily indicate when this vulnerability was discovered, shared with the affected vendor, publicly disclosed, or updated in CVE."}]]
          [:type {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything", :description "<p>The vulnerability type as selected from a large dropdown menu during CVE refinement.</p>Most frequently used vulnerability types are: <code>DoS</code>, <code>Code Execution</code>, <code>Overflow</code>, <code>Memory Corruption</code>, <code>Sql Injection</code>, <code>XSS</code>, <code>Directory Traversal</code>, <code>Http Response Splitting</code>, <code>Bypass something</code>, <code>Gain Information</code>, <code>Gain Privileges</code>, <code>CSRF</code>, <code>File Inclusion</code>. For more information see <a target='_blank' href='https://www.cvedetails.com/vulnerabilities-by-types.php'>Vulnerabilities By Type</a> distributions."}]]
          [:cwe_url {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything"}]]
          [:references {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything", :description "A list of reference URLs with additional information about the CVE Record."}]]
          [:title {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything", :description "A title or a brief phrase summarizing the CVE record."}]]
          [:product {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything", :description "The product where the vulnerability was discovered."}]]
          [:modified_time {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything", :description "The Record Modified Date identifies when the CVE record was last updated."}]]
          [:cwe_uid {:json-schema/example "anything", :optional true} [:any #:json-schema{:example "anything"}]]]
   (m/form (malli/->malli (ocsf/->flanders (json/decode (slurp (io/file "ocsf-schema/objects/cve.json")) keyword)))))))
