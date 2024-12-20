(ns flanders.ocsf.schema-test
  (:require [clojure.test :refer [deftest is]]
            [flanders.ocsf.schema :as ocsf-schema]
            [schema.core :as s]))

(def schemas-v1-3-0 (delay (ocsf-schema/schemas "v1.3.0")))

(deftest ocsf-schemas-v1-3-0-test
  (is (= 215 (count @schemas-v1-3-0)))
  (is (= '{(optional-key :activity_name) Str,
           (optional-key :actor) {Any Any},
           (optional-key :api) {Any Any},
           (optional-key :category_name) Str,
           (optional-key :class_name) Str,
           (optional-key :count) Int,
           (optional-key :duration) Int,
           (optional-key :end_time) Int,
           (optional-key :end_time_dt) Str,
           (optional-key :enrichments) [{Any Any}],
           (optional-key :kb_article_list) [{Any Any}],
           (optional-key :message) Str,
           (optional-key :observables) [{Any Any}],
           (optional-key :raw_data) Str,
           (optional-key :severity) Str,
           (optional-key :start_time) Int,
           (optional-key :start_time_dt) Str,
           (optional-key :status) Str,
           (optional-key :status_code) Str,
           (optional-key :status_detail) Str,
           (optional-key :status_id) Int,
           (optional-key :time_dt) Str,
           (optional-key :timezone_offset) Int,
           (optional-key :type_name) Str,
           (optional-key :unmapped) {Any Any},
           :activity_id Int,
           :category_uid Int,
           :class_uid Int,
           :cloud {Any Any},
           :device {Any Any},
           :metadata {Any Any},
           :osint [{Any Any}],
           :severity_id Int,
           :time Int,
           :type_uid Int}
         (s/explain (@schemas-v1-3-0 "https://schema.ocsf.io/1.3.0/classes/patch_state"))))
  (is (= {:json-schema
          {:example
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
           :description
           "Operating System Patch State reports the installation of an OS patch to a device and any associated knowledgebase articles."}}
         (meta (@schemas-v1-3-0 "https://schema.ocsf.io/1.3.0/classes/patch_state"))))
  (is (= {:duration
          {:json-schema
           {:example 10,
            :description
            "The event duration or aggregate time, the amount of time the event covers from <code>start_time</code> to <code>end_time</code> in milliseconds."}},
          :severity_id
          {:json-schema
           {:example 10,
            :description
            "<p>The normalized identifier of the event/finding severity.</p>The normalized severity is a measurement the effort and expense required to manage and resolve an event or incident. Smaller numerical values represent lower impact events, and larger numerical values represent higher impact events."}},
          :category_uid
          {:json-schema
           {:example 10,
            :description "The category unique identifier of the event."}},
          :api
          {:json-schema
           {:example {"anything" "anything"},
            :description
            "Describes details about a typical API (Application Programming Interface) call."}},
          :observables
          {:json-schema
           {:example [{"anything" "anything"}],
            :description
            "The observables associated with the event or a finding."}},
          :status
          {:json-schema
           {:example "string",
            :description
            "The event status, normalized to the caption of the status_id value. In the case of 'Other', it is defined by the event source."}},
          :end_time_dt
          {:json-schema
           {:example "string",
            :description
            "The end time of a time period, or the time of the most recent event included in the aggregate event."}},
          :activity_name
          {:json-schema
           {:example "string",
            :description
            "The event activity name, as defined by the activity_id."}},
          :osint
          {:json-schema
           {:example [{"anything" "anything"}],
            :description
            "The OSINT (Open Source Intelligence) object contains details related to an indicator such as the indicator itself, related indicators, geolocation, registrar information, subdomains, analyst commentary, and other contextual information. This information can be used to further enrich a detection or finding by providing decisioning support to other analysts and engineers."}},
          :type_uid
          {:json-schema
           {:example 10,
            :description
            "The event/finding type ID. It identifies the event's semantics and structure. The value is calculated by the logging system as: <code>class_uid * 100 + activity_id</code>."}},
          :message
          {:json-schema
           {:example "string",
            :description
            "The description of the event/finding, as defined by the source."}},
          :time_dt
          {:json-schema
           {:example "string",
            :description
            "The normalized event occurrence time or the finding creation time."}},
          :end_time
          {:json-schema
           {:example 10,
            :description
            "The end time of a time period, or the time of the most recent event included in the aggregate event."}},
          :cloud
          {:json-schema
           {:example {"anything" "anything"},
            :description
            "Describes details about the Cloud environment where the event was originally created or logged."}},
          :time
          {:json-schema
           {:example 10,
            :description
            "The normalized event occurrence time or the finding creation time."}},
          :start_time_dt
          {:json-schema
           {:example "string",
            :description
            "The start time of a time period, or the time of the least recent event included in the aggregate event."}},
          :status_code
          {:json-schema
           {:example "string",
            :description
            "The event status code, as reported by the event source.<br /><br />For example, in a Windows Failed Authentication event, this would be the value of 'Failure Code', e.g. 0x18."}},
          :status_detail
          {:json-schema
           {:example "string",
            :description
            "The status detail contains additional information about the event/finding outcome."}},
          :enrichments
          {:json-schema
           {:example [{"anything" "anything"}],
            :description
            "The additional information from an external data source, which is associated with the event or a finding. For example add location information for the IP address in the DNS answers:</p><code>[{\"name\": \"answers.ip\", \"value\": \"92.24.47.250\", \"type\": \"location\", \"data\": {\"city\": \"Socotra\", \"continent\": \"Asia\", \"coordinates\": [-25.4153, 17.0743], \"country\": \"YE\", \"desc\": \"Yemen\"}}]</code>"}},
          :status_id
          {:json-schema
           {:example 10,
            :description "The normalized identifier of the event status."}},
          :class_uid
          {:json-schema
           {:example 10,
            :description
            "The unique identifier of a class. A class describes the attributes available in an event."}},
          :raw_data
          {:json-schema
           {:example "string",
            :description
            "The raw event/finding data as received from the source."}},
          :type_name
          {:json-schema
           {:example "string",
            :description
            "The event/finding type name, as defined by the type_uid."}},
          :category_name
          {:json-schema
           {:example "string",
            :description
            "The event category name, as defined by category_uid value: <code>Discovery</code>."}},
          :severity
          {:json-schema
           {:example "string",
            :description
            "The event/finding severity, normalized to the caption of the severity_id value. In the case of 'Other', it is defined by the source."}},
          :start_time
          {:json-schema
           {:example 10,
            :description
            "The start time of a time period, or the time of the least recent event included in the aggregate event."}},
          :unmapped
          {:json-schema
           {:example {"anything" "anything"},
            :description
            "The attributes that are not mapped to the event schema. The names and values of those attributes are specific to the event source."}},
          :class_name
          {:json-schema
           {:example "string",
            :description
            "The event class name, as defined by class_uid value: <code>Operating System Patch State</code>."}},
          :device
          {:json-schema
           {:example {"anything" "anything"},
            :description "An addressable device, computer system or host."}},
          :count
          {:json-schema
           {:example 10,
            :description
            "The number of times that events in the same logical group occurred during the event <strong>Start Time</strong> to <strong>End Time</strong> period."}},
          :kb_article_list
          {:json-schema
           {:example [{"anything" "anything"}],
            :description
            "A list of KB articles or patches related to an endpoint. A KB Article contains metadata that describes the patch or an update."}},
          :activity_id
          {:json-schema
           {:example 10,
            :description
            "The normalized identifier of the activity that triggered the event."}},
          :metadata
          {:json-schema
           {:example {"anything" "anything"},
            :description
            "The metadata associated with the event or a finding."}},
          :actor
          {:json-schema
           {:example {"anything" "anything"},
            :description
            "The actor object describes details about the user/role/process that was the source of the activity."}},
          :timezone_offset
          {:json-schema
           {:example 10,
            :description
            "The number of minutes that the reported event <code>time</code> is ahead or behind UTC, in the range -1,080 to +1,080."}}}
         (-> (@schemas-v1-3-0 "https://schema.ocsf.io/1.3.0/classes/patch_state")
             (update-keys s/explicit-schema-key)
             (update-vals meta)))))
