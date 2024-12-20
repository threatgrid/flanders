(ns flanders.ocsf
  (:require [flanders.core :as f]))

;; :caption => title
;; all maps are closed
;; :observable => seems to be a class id
(defn parse-attribute [[k {:keys [description requirement enum type is_array caption]}
                        :as e]
                       opts]
  (assert (nil? enum))
  (assert (nil? is_array))
  (f/entry k (case type
               "string_t" (f/str)
               "integer_t" (f/int)
               "long_t" (f/int)
               "float_t" (f/num)
               "boolean_t" (f/bool)
               ("timestamp_t"
                 "datetime_t"
                 "hostname_t"
                 "ip_t"
                 "subnet_t"
                 "mac_t"
                 "email_t"
                 "port_t"
                 "file_name_t"
                 "path_t") (assert nil (pr-str type))
               nil f/any)
           :description (or description caption)
           :required? (case requirement "required" true ("recommended" "optional") false)))

(defn ->flanders
  "Converts parsed (keywordized) OCSF schemas to Flanders."
  ([v] (->flanders v nil))
  ([{:keys [attributes] :as v} opts]
   (f/map (mapv #(parse-attribute % opts)
                (sort-by first attributes)))))
