(ns flanders.ocsf
  (:require [flanders.core :as f]
            flanders.types)
  #?(:clj (:import
           [flanders.types
            AnythingType
            BooleanType
            EitherType
            InstType
            IntegerType
            KeywordType
            MapEntry
            MapType
            NumberType
            ParameterListType
            RefType
            SequenceOfType
            SetOfType
            SignatureType
            StringType])))

(def unhandled (atom #{}))
(comment
  (do @unhandled)
  )

;; :caption => title
;; all maps are closed
;; :observable => seems to be a class id
(defn parse-attribute [[k {:strs [description requirement enum type is_array caption]} :as e]
                       opts]
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
            "timestamp_t" f/str
            "user" f/str
            "account" f/str
            "actor" f/str
            "affected_code" f/str
            "url_t" f/str
            "datetime_t" f/str
            "object_t" (f/map [(f/entry f/any f/any :required? false)])
            "hostname_t" f/str
            "ip_t" f/str
            "mac_t" f/str
            "subnet_t" f/str
            "email_t" f/str
            ("json_t" nil) f/any

            (do (swap! unhandled conj type) (assert nil)))
        f (if enum
            (cond
              (instance? IntegerType f) (assoc f :values (mapv Long/parseLong (keys enum)))
              (instance? NumberType f) (assoc f :values (mapv Double/parseDouble (keys enum)))
              (instance? StringType f) (assoc f :values (mapv str (keys enum)))
              :else (throw (ex-info (str "enum on " (class f)) {})))
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
  ([{:strs [attributes name description] :as v} opts]
   (-> (f/map (mapv #(parse-attribute % opts)
                    (sort-by key attributes)))
       (assoc :description description))))
