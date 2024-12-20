(ns flanders.ocsf
  (:require [flanders.core :as f]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            flanders.types)
  (:import [flanders.types IntegerType NumberType StringType]))

;; :caption => title
;; all maps are closed
;; :observable => seems to be a class id
(defn parse-attribute [[k {:strs [description requirement enum type is_array caption]}]
                       _opts]
  (let [f (case type
            "string_t" (f/str)
            "integer_t" (f/int)
            "long_t" (f/int)
            "float_t" (f/num)
            "uuid_t" (f/str)
            "boolean_t" (f/bool)
            "port_t" (f/int)
            "file_hash_t" (f/str)
            "file_name_t" (f/str)
            "process_name_t" (f/str)
            "username_t" (f/str)
            "timestamp_t" (f/int)
            "user" (f/str)
            "account" (f/str)
            "actor" (f/str)
            "affected_code" (f/str)
            "url_t" (f/str)
            "datetime_t" (f/str)
            "object_t" (f/map [(f/entry f/any f/any :required? false)])
            "hostname_t" (f/str)
            "ip_t" (f/str)
            "mac_t" (f/str)
            "subnet_t" (f/str)
            "email_t" (f/str)
            "json_t" f/any
            (throw (ex-info (str "Unknown type: " (pr-str type)) {})))
        f (if enum
            (cond
              (instance? IntegerType f) (assoc f :values (mapv Long/parseLong (keys enum)))
              (instance? NumberType f) (assoc f :values (mapv Double/parseDouble (keys enum)))
              (instance? StringType f) (assoc f :values (mapv str (keys enum)))
              :else (throw (ex-info (str "enum on " (clojure.core/type f)) {})))
            f)
        f (cond-> f
            is_array f/seq-of)]
    (f/entry (keyword k) f
             :description (or description caption)
             :required? (case requirement "required" true ("recommended" "optional" nil) false))))

(defn ->flanders
  "Converts parsed OCSF schemas to Flanders.

  Example:
  https://schema.ocsf.io/api/1.3.0/classes/file_activity"
  ([v] (->flanders v nil))
  ([{:strs [attributes description]} opts]
   (-> (f/map (mapv #(parse-attribute % opts)
                    (sort-by key attributes)))
       (assoc :description description))))

(defn schemas
  "Returns a map from OCSF identifier to Flanders schema for the given version.

  Example:
  (schemas \"v1.3.0\")
  => 
  {\"https://schema.ocsf.io/1.3.0/classes/patch_state\" #flanders.types.MapType{...}
  ...}"
  ([version] (schemas version nil))
  ([version opts]
   (case version
     "v1.3.0" (let [base-id "https://schema.ocsf.io/1.3.0/"
                    {:strs [classes types objects base_event]} (json/decode (slurp (io/resource "flanders/impl/ocsf-1.3.0-export.json")))]
                (-> {}
                    (assoc (str base-id "classes/base_event") (->flanders base_event opts))
                    (into (map (fn [[n t]] [(str base-id "classes/" n) (->flanders t opts)])) classes)
                    (into (map (fn [[n t]] [(str base-id "types/" n) (->flanders t opts)])) types)
                    (into (map (fn [[n t]] [(str base-id "objects/" n) (->flanders t opts)])) objects)))
     (throw (ex-info (str "Unsupported OCSF version " version) {})))))
