[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / DbConfig

# Type Alias: DbConfig

> **DbConfig** = \{ `encryptionKey?`: `string`; `filePrefix`: `string`; `type`: `"sqlite"`; \} \| \{ `connectionString`: `string`; `schemaPrefix?`: `string`; `type`: `"postgres"`; \}

Defined in: [src/api.ts:63](../src/api.ts#L63)

Database configuration. `type` selects the libsimplex backend loaded by
`ChatApi.init`; one backend per process.

## Union Members

### Type Literal

\{ `encryptionKey?`: `string`; `filePrefix`: `string`; `type`: `"sqlite"`; \}

#### encryptionKey?

> `optional` **encryptionKey?**: `string`

Optional SQLCipher encryption key. Empty/omitted = unencrypted.

#### filePrefix

> **filePrefix**: `string`

File prefix — two schema files are named `<prefix>_chat.db` and `<prefix>_agent.db`.

#### type

> **type**: `"sqlite"`

SQLite backend (default).

***

### Type Literal

\{ `connectionString`: `string`; `schemaPrefix?`: `string`; `type`: `"postgres"`; \}

#### connectionString

> **connectionString**: `string`

PostgreSQL connection string (e.g. `postgres://user:pass@host/db`).

#### schemaPrefix?

> `optional` **schemaPrefix?**: `string`

Schema prefix used to namespace tables. Defaults to `"simplex_v1"` when omitted.

#### type

> **type**: `"postgres"`

PostgreSQL backend (Linux x86_64 only, libpq5 required).
