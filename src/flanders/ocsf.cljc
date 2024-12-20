(ns flanders.ocsf
  (:require [flanders.core :as f]))

;; :caption => title
;; all maps are closed
;; :observable => seems to be a class id
(defn parse-attribute [[k {:strs [description requirement enum type is_array caption]}]
                       opts]
  (let [enum (when enum
               (assert (not is_array))
               (case type
                 "integer_t" Long/parseLong
                 (throw (ex-info (str "enum: " (pr-str type)) {}))))]
    (f/entry (keyword k)
             (cond-> (case type
                       "string_t" (f/str)
                       "integer_t" (f/int)
                       "long_t" (f/int)
                       "float_t" (f/num)
                       "boolean_t" (f/bool)
                       (
                        "datetime_t"
                        "hostname_t"
                        "ip_t"
                        "subnet_t"
                        "mac_t"
                        "email_t"
                        "port_t"
                        "file_name_t"
                        "path_t"
                        ) (assert nil (pr-str type))
                       "timestamp_t" f/str
                       "user" f/str
                       "account" f/str
                       "actor" f/str
                       "affected_code" f/str
                       ("json_t" nil) f/any)
               is_array f/seq-of)
             :description (or description caption)
             :required? (case requirement "required" true ("recommended" "optional" nil) false))))

(defn ->flanders
  "Converts parsed OCSF schemas to Flanders."
  ([v] (->flanders v nil))
  ([{:strs [attributes] :as v} opts]
   (f/map (mapv #(parse-attribute % opts)
                (sort-by key attributes)))))
