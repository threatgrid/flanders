# flanders

A Clojure library used by [CTIM](https://github.com/threatgrid/ctim/)
to define data types, to annotate them with domain specific
information, and to generate artifacts such as schemas, documentation,
validators, etc.

## OCSF Schemas

[flanders.ocsf](src/flanders/ocsf.cljc) creates flanders schemas from OCSF schemas.

Use `flanders.ocsf/->flanders` to translate an OCSF schema to flanders. The OCSF schemas
are in an internal format returned by urls like https://schema.ocsf.io/api/objects/cve.

You can export OCSF schemas in bulk at https://schema.ocsf.io/export/schema.
This repository depends on [ocsf-schema-export](https://github.com/threatgrid/ocsf-schema-export)
which provides bulk OCSF schemas exports for each major OCSF version.

Please add the following library to your classpath (already a dev dep in flanders):

```clojure
[io.github.threatgrid/ocsf-schema-export "1.0.0-SNAPSHOT"]
```

These files are now available on the classpath:

```
threatgrid/ocsf-1.0.0-export.json
threatgrid/ocsf-1.1.0-export.json
threatgrid/ocsf-1.2.0-export.json
threatgrid/ocsf-1.3.0-export.json
```

Once you choose your version, they can be converted in bulk to flanders using `flanders.ocsf/parse-exported-schemas`.

```clojure
(require '[flanders.ocsf :as ocsf]
         '[cheshire.core :as json]
         '[clojure.java.io :as io])

(def ocsf-1-3-0-export-json
  (-> "threatgrid/ocsf-1.3.0-export.json" io/resource slurp json/decode))

(def ocsf-1-3-0-schemas
  (ocsf/parse-exported-schemas ocsf-1-3-0-export-json))
```

The result `ocsf-1-3-0-schemas` will have the vals of the `"objects"` and `"classes"` maps to converted to flanders schemas
(and also the `"base-event"` field).

```clojure
(-> ocsf-1-3-0-schemas
    (select-keys ["base_event" "objects" "classes"])
    (update "base_event" class)
    (update "objects" update-vals class)
    (update "classes" update-vals class)
    prn)
;=> {"base_event" flanders.types.MapType,
;    "objects"
;    {"kill_chain_phase" flanders.types.MapType,
;     "sub_technique" flanders.types.MapType,
;     "table" flanders.types.MapType,
;     ...},
;    "classes"
;    {"win/registry_key_query" flanders.types.MapType,
;     "datastore_activity" flanders.types.MapType,
;     "event_log" flanders.types.MapType,
;     ...}}
```

From there, you can convert them to other formats such as malli or schema:

```clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plumatic Schema
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[schema.core :as s]
         'flanders.schema)

(s/defschema OCSFAuthorizationSchema
  (flanders.schema/->schema (get-in ocsf-1-3-0-schemas ["objects" "authorization"])))

(s/explain OCSFAuthorizationSchema)
;=> {(optional-key :decision) Str, (optional-key :policy) {Any Any}}

(meta OCSFAuthorizationSchema)
;=> {:json-schema {:example {:decision "string", :policy {"anything" "anything"}},
;    :description "The Authorization Result object provides details about the authorization outcome and associated policies related to activity."},
;    :name OCSFAuthorizationSchema, :ns user}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; malli
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[malli.core :as m]
         'flanders.malli)

(def OCSFAuthorizationMalli
  (flanders.malli/->malli (get-in ocsf-1-3-0-schemas ["objects" "authorization"])))

(m/form OCSFAuthorizationMalli)
;=> [:map
;    {:closed true,
;     :json-schema/example
;     {:decision "string", :policy {"anything" "anything"}},
;     :json-schema/description
;     "The Authorization Result object provides details about the authorization outcome and associated policies related to activity."}
;    [:decision
;     {:json-schema/example "string",
;      :optional true,
;      :json-schema/description
;      "Authorization Result/outcome, e.g. allowed, denied."}
;     [:string {:json-schema/example "string"}]]
;    [:policy
;     {:json-schema/example {"anything" "anything"},
;      :optional true,
;      :json-schema/description
;      "Details about the Identity/Access management policies that are applicable."}
;     [:map
;      {:closed true, :json-schema/example {"anything" "anything"}}
;      [:malli.core/default
;       {:json-schema/example "anything"}
;       [:map-of :any [:any {:json-schema/example "anything"}]]]]]]
```


You can also just convert the schemas you need directly from the OCSF schema instead
of via the bulk export:

```clojure
;; schema
(s/defschema OCSFAuthorization
  (flanders.schema/->schema (ocsf/->flanders (get-in ocsf-1-3-0-export ["objects" "authorization"]))))

;; malli
(def OCSFAuthorization
  (flanders.malli/->malli (ocsf/->flanders (get-in ocsf-1-3-0-export ["objects" "authorization"]))))
```

## Releasing

To release flanders `x.y.z`:

1. Create a new feature branch.
2. Update `CHANGES.md` for the upcoming version.
3. Change the project.clj version to `x.y.z-SNAPSHOT`.
4. Run `lein deploy clojars` to deploy a snapshot to test your credentials.
5. If that works, run `lein release` to release a new version.
6. Open a PR for your branch.
7. You must merge your PR using a merge commit, not squash.
8. Once merged, double check the git tag `x.y.z` is present.

## License

Copyright © 2016-2025 Cisco Systems

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
