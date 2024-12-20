(ns flanders.ocsf.malli-test
  (:require [clojure.test :refer [deftest is testing]]
            [flanders.ocsf.malli :as ocsf-malli]
            [malli.core :as m]
            [malli.generator :as mg]))

(def schemas-v1-3-0 (delay (ocsf-malli/schemas "v1.3.0")))
(def patch-state-v1-3-0 (delay (@schemas-v1-3-0 "https://schema.ocsf.io/1.3.0/classes/patch_state")))

(deftest ocsf-schemas-v1-3-0-test
  (is (= 215 (count @schemas-v1-3-0)))
  (is (= [:map
          {:closed true,
           :json-schema/example
           {:severity_id 10,
            :category_uid 10,
            :status_id 10,
            :api {"anything" "anything"},
            :unmapped {"anything" "anything"},
            :class_name "string",
            :osint [{"anything" "anything"}],
            :type_uid 10,
            :start_time_dt "string",
            :enrichments [{"anything" "anything"}],
            :cloud {"anything" "anything"},
            :time 10,
            :start_time 10,
            :observables [{"anything" "anything"}],
            :time_dt "string",
            :duration 10,
            :class_uid 10,
            :kb_article_list [{"anything" "anything"}],
            :end_time 10,
            :category_name "string",
            :activity_name "string",
            :timezone_offset 10,
            :status "string",
            :count 10,
            :severity "string",
            :status_detail "string",
            :end_time_dt "string",
            :device {"anything" "anything"},
            :status_code "string",
            :raw_data "string",
            :activity_id 10,
            :type_name "string",
            :metadata {"anything" "anything"},
            :message "string",
            :actor {"anything" "anything"}},
           :json-schema/description
           "Operating System Patch State reports the installation of an OS patch to a device and any associated knowledgebase articles."}
          [:activity_id
           #:json-schema{:example 10,
                         :description
                         "The normalized identifier of the activity that triggered the event."}
           [:int #:json-schema{:example 10}]]
          [:activity_name
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event activity name, as defined by the activity_id."}
           [:string #:json-schema{:example "string"}]]
          [:actor
           {:json-schema/example {"anything" "anything"},
            :optional true,
            :json-schema/description
            "The actor object describes details about the user/role/process that was the source of the activity."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:api
           {:json-schema/example {"anything" "anything"},
            :optional true,
            :json-schema/description
            "Describes details about a typical API (Application Programming Interface) call."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:category_name
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event category name, as defined by category_uid value: <code>Discovery</code>."}
           [:string #:json-schema{:example "string"}]]
          [:category_uid
           #:json-schema{:example 10,
                         :description
                         "The category unique identifier of the event."}
           [:int #:json-schema{:example 10}]]
          [:class_name
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event class name, as defined by class_uid value: <code>Operating System Patch State</code>."}
           [:string #:json-schema{:example "string"}]]
          [:class_uid
           #:json-schema{:example 10,
                         :description
                         "The unique identifier of a class. A class describes the attributes available in an event."}
           [:int #:json-schema{:example 10}]]
          [:cloud
           #:json-schema{:example {"anything" "anything"},
                         :description
                         "Describes details about the Cloud environment where the event was originally created or logged."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:count
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The number of times that events in the same logical group occurred during the event <strong>Start Time</strong> to <strong>End Time</strong> period."}
           [:int #:json-schema{:example 10}]]
          [:device
           #:json-schema{:example {"anything" "anything"},
                         :description
                         "An addressable device, computer system or host."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:duration
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The event duration or aggregate time, the amount of time the event covers from <code>start_time</code> to <code>end_time</code> in milliseconds."}
           [:int #:json-schema{:example 10}]]
          [:end_time
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The end time of a time period, or the time of the most recent event included in the aggregate event."}
           [:int #:json-schema{:example 10}]]
          [:end_time_dt
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The end time of a time period, or the time of the most recent event included in the aggregate event."}
           [:string #:json-schema{:example "string"}]]
          [:enrichments
           {:json-schema/example [{"anything" "anything"}],
            :optional true,
            :json-schema/description
            "The additional information from an external data source, which is associated with the event or a finding. For example add location information for the IP address in the DNS answers:</p><code>[{\"name\": \"answers.ip\", \"value\": \"92.24.47.250\", \"type\": \"location\", \"data\": {\"city\": \"Socotra\", \"continent\": \"Asia\", \"coordinates\": [-25.4153, 17.0743], \"country\": \"YE\", \"desc\": \"Yemen\"}}]</code>"}
           [:sequential
            [:map
             {:closed true, :json-schema/example {"anything" "anything"}}
             [:malli.core/default
              #:json-schema{:example "anything"}
              [:map-of :any [:any #:json-schema{:example "anything"}]]]]]]
          [:kb_article_list
           {:json-schema/example [{"anything" "anything"}],
            :optional true,
            :json-schema/description
            "A list of KB articles or patches related to an endpoint. A KB Article contains metadata that describes the patch or an update."}
           [:sequential
            [:map
             {:closed true, :json-schema/example {"anything" "anything"}}
             [:malli.core/default
              #:json-schema{:example "anything"}
              [:map-of :any [:any #:json-schema{:example "anything"}]]]]]]
          [:message
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The description of the event/finding, as defined by the source."}
           [:string #:json-schema{:example "string"}]]
          [:metadata
           #:json-schema{:example {"anything" "anything"},
                         :description
                         "The metadata associated with the event or a finding."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]
          [:observables
           {:json-schema/example [{"anything" "anything"}],
            :optional true,
            :json-schema/description
            "The observables associated with the event or a finding."}
           [:sequential
            [:map
             {:closed true, :json-schema/example {"anything" "anything"}}
             [:malli.core/default
              #:json-schema{:example "anything"}
              [:map-of :any [:any #:json-schema{:example "anything"}]]]]]]
          [:osint
           #:json-schema{:example [{"anything" "anything"}],
                         :description
                         "The OSINT (Open Source Intelligence) object contains details related to an indicator such as the indicator itself, related indicators, geolocation, registrar information, subdomains, analyst commentary, and other contextual information. This information can be used to further enrich a detection or finding by providing decisioning support to other analysts and engineers."}
           [:sequential
            [:map
             {:closed true, :json-schema/example {"anything" "anything"}}
             [:malli.core/default
              #:json-schema{:example "anything"}
              [:map-of :any [:any #:json-schema{:example "anything"}]]]]]]
          [:raw_data
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The raw event/finding data as received from the source."}
           [:string #:json-schema{:example "string"}]]
          [:severity
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event/finding severity, normalized to the caption of the severity_id value. In the case of 'Other', it is defined by the source."}
           [:string #:json-schema{:example "string"}]]
          [:severity_id
           #:json-schema{:example 10,
                         :description
                         "<p>The normalized identifier of the event/finding severity.</p>The normalized severity is a measurement the effort and expense required to manage and resolve an event or incident. Smaller numerical values represent lower impact events, and larger numerical values represent higher impact events."}
           [:int #:json-schema{:example 10}]]
          [:start_time
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The start time of a time period, or the time of the least recent event included in the aggregate event."}
           [:int #:json-schema{:example 10}]]
          [:start_time_dt
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The start time of a time period, or the time of the least recent event included in the aggregate event."}
           [:string #:json-schema{:example "string"}]]
          [:status
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event status, normalized to the caption of the status_id value. In the case of 'Other', it is defined by the event source."}
           [:string #:json-schema{:example "string"}]]
          [:status_code
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event status code, as reported by the event source.<br /><br />For example, in a Windows Failed Authentication event, this would be the value of 'Failure Code', e.g. 0x18."}
           [:string #:json-schema{:example "string"}]]
          [:status_detail
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The status detail contains additional information about the event/finding outcome."}
           [:string #:json-schema{:example "string"}]]
          [:status_id
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The normalized identifier of the event status."}
           [:int #:json-schema{:example 10}]]
          [:time
           #:json-schema{:example 10,
                         :description
                         "The normalized event occurrence time or the finding creation time."}
           [:int #:json-schema{:example 10}]]
          [:time_dt
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The normalized event occurrence time or the finding creation time."}
           [:string #:json-schema{:example "string"}]]
          [:timezone_offset
           {:json-schema/example 10,
            :optional true,
            :json-schema/description
            "The number of minutes that the reported event <code>time</code> is ahead or behind UTC, in the range -1,080 to +1,080."}
           [:int #:json-schema{:example 10}]]
          [:type_name
           {:json-schema/example "string",
            :optional true,
            :json-schema/description
            "The event/finding type name, as defined by the type_uid."}
           [:string #:json-schema{:example "string"}]]
          [:type_uid
           #:json-schema{:example 10,
                         :description
                         "The event/finding type ID. It identifies the event's semantics and structure. The value is calculated by the logging system as: <code>class_uid * 100 + activity_id</code>."}
           [:int #:json-schema{:example 10}]]
          [:unmapped
           {:json-schema/example {"anything" "anything"},
            :optional true,
            :json-schema/description
            "The attributes that are not mapped to the event schema. The names and values of those attributes are specific to the event source."}
           [:map
            {:closed true, :json-schema/example {"anything" "anything"}}
            [:malli.core/default
             #:json-schema{:example "anything"}
             [:map-of :any [:any #:json-schema{:example "anything"}]]]]]]
         (m/form @patch-state-v1-3-0)))
    (is (= {:severity_id 0,
            :category_uid -1,
            :unmapped {#{} #uuid "947ce02a-4fe5-48d9-96e8-13a58277f170"},
            :class_name "",
            :osint [{}],
            :type_uid 0,
            :start_time_dt "",
            :enrichments [{nil :.p/-y}],
            :cloud {},
            :time -1,
            :observables [],
            :time_dt "",
            :class_uid -1,
            :kb_article_list [],
            :timezone_offset 0,
            :status "",
            :status_detail "",
            :end_time_dt "s",
            :device {},
            :status_code "0",
            :activity_id -1,
            :type_name "",
            :metadata {},
            :message "Z",
            :actor {#{} #{}}}
           (mg/generate @patch-state-v1-3-0 {:seed 0 :size 1})))
     (let [seed (rand-int 50000)]
       (testing (pr-str seed)
         (is (m/validate @patch-state-v1-3-0 (mg/generate @patch-state-v1-3-0 {:seed seed}))))))
