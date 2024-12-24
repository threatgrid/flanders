(ns flanders.ocsf
  (:require [flanders.core :as f]
            #?(:clj flanders.types
               :cljs [flanders.types :refer [IntegerType NumberType StringType]]))
  #?(:clj (:import [flanders.types IntegerType NumberType StringType])))

;; :caption => title
;; all maps are closed
;; :observable => seems to be a class id
(defn parse-attribute [[k {:strs [description requirement enum type is_array caption]}] _opts]
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
            ("json_t" nil) f/any)
        f (if enum
            (cond
              (instance? IntegerType f) (assoc f :values (mapv #?(:clj Long/parseLong :cljs parse-long) (keys enum)))
              (instance? NumberType f) (assoc f :values (mapv #?(:clj Double/parseDouble :cljs parse-double) (keys enum)))
              (instance? StringType f) (assoc f :values (mapv str (keys enum)))
              :else (throw (ex-info (str "enum on " (type f)) {})))
            f)
        f (cond-> f
            is_array f/seq-of)]
    (f/entry (keyword k) f
             :description (or description caption)
             :required? (case requirement
                          "required" true
                          ("recommended" "optional" nil) false))))

(defn ->flanders
  "Converts parsed OCSF schemas to Flanders."
  ([v] (->flanders v nil))
  ([{:strs [attributes description]} opts]
   (-> (f/map (mapv #(parse-attribute % opts)
                    (sort-by key attributes)))
       (assoc :description description))))

(defn parse-exported-schemas
  "Takes the result of https://schema.ocsf.io/export/schema parsed as edn
  and updates the base_event, objects and classes schemas to flanders.

  Flanders includes a dependency on https://github.com/frenchy64/ocsf-schema-export
  which provides OCSF schemas on the classpath.
  
  Example:
  (parse-exported-schemas (-> \"threatgrid/ocsf-1.3.0-export.json\" io/resource slurp json/decode))"
  ([export] (parse-exported-schemas export nil))
  ([export opts]
   (-> export
       (update "base_event" ->flanders opts)
       (update "objects" update-vals #(->flanders % opts))
       (update "classes" update-vals #(->flanders % opts)))))
