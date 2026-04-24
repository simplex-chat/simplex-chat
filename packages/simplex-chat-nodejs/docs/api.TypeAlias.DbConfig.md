[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / DbConfig

# Type Alias: DbConfig

> **DbConfig** = \{ `encryptionKey?`: `string`; `filePrefix`: `string`; `kind`: `"sqlite"`; \} \| \{ `connectionString`: `string`; `kind`: `"postgres"`; `schemaPrefix?`: `string`; \}

Defined in: [src/api.ts:65](../src/api.ts#L65)

Database configuration. The native library is built against exactly one
backend (see `simplex_backend` / `SIMPLEX_BACKEND` at install time); this
type makes the caller state which one they are targeting so field names
can't lie about their meaning.

## Union Members

### Type Literal

\{ `encryptionKey?`: `string`; `filePrefix`: `string`; `kind`: `"sqlite"`; \}

#### encryptionKey?

> `optional` **encryptionKey?**: `string`

Optional SQLCipher encryption key. Empty/omitted = unencrypted.

#### filePrefix

> **filePrefix**: `string`

File prefix — two schema files are named `<prefix>_chat.db` and `<prefix>_agent.db`.

#### kind

> **kind**: `"sqlite"`

SQLite backend (default).

***

### Type Literal

\{ `connectionString`: `string`; `kind`: `"postgres"`; `schemaPrefix?`: `string`; \}

#### connectionString

> **connectionString**: `string`

PostgreSQL connection string (e.g. `postgres://user:pass@host/db`).

#### kind

> **kind**: `"postgres"`

PostgreSQL backend (Linux x86_64 only, libpq5 required).

#### schemaPrefix?

> `optional` **schemaPrefix?**: `string`

Schema prefix used to namespace tables. Defaults to `"simplex_v1"` when omitted.
