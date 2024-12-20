# flanders

A Clojure library used by [CTIM](https://github.com/threatgrid/ctim/)
to define data types, to annotate them with domain specific
information, and to generate artifacts such as schemas, documentation,
validators, etc.

## OCSF Schemas

Flanders provides [OCSF](https://schema.ocsf.io/) schemas via `flanders.ocsf.{schema,malli}`.

Supported OCSF versions:
- [v1.3.0](https://schema.ocsf.io/1.3.0/)

See `flanders.ocsf/->flanders` for translating custom schemas.

## License

Copyright © 2016-2024 Cisco Systems

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
