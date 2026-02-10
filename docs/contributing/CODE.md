# Coding and building

This file provides guidance on coding style and approaches and on building the code.

## Code Style, Formatting and Approaches

The project uses **fourmolu** for Haskell code formatting. Configuration is in `fourmolu.yaml`.

**Key formatting rules:**
- 2-space indentation
- Trailing function arrows, commas, and import/export style
- Record brace without space: `{field = value}`
- Single newline between declarations
- Never use unicode symbols
- Inline `let` style with right-aligned `in`

**Format code before committing:**

```bash
# Format a single file
fourmolu -i src/Simplex/Messaging/Protocol.hs
```

Some files that use CPP language extension cannot be formatted as a whole, so individual code fragments need to be formatted.

**Follow existing code patterns:**
- Match the style of surrounding code
- Use qualified imports with short aliases (e.g., `import qualified Data.ByteString.Char8 as B`)
- Use record syntax for types with multiple fields
- Prefer explicit pattern matching over partial functions

**Comments policy:**
- Avoid redundant comments that restate what the code already says
- Only comment on non-obvious design decisions or tricky implementation details
- Function names and type signatures should be self-documenting
- Do not add comments like "wire format encoding" (Encoding class is always wire format) or "check if X" when the function name already says that
- Assume a competent Haskell reader

**Diff and refactoring:**
- Avoid unnecessary changes and code movements
- Never do refactoring unless it substantially reduces cost of solving the current problem, including the cost of refactoring
- Aim to minimize the code changes - do what is minimally required to solve users' problems

**Document and code structure:**
- **Never move existing code or sections around** - add new content at appropriate locations without reorganizing existing structure.
- When adding new sections to documents, continue the existing numbering scheme.
- Minimize diff size - prefer small, targeted changes over reorganization.

**Code analysis and review:**
- Trace data flows end-to-end: from origin, through storage/parameters, to consumption. Flag values that are discarded and reconstructed from partial data (e.g. extracted from a URI missing original fields) — this is usually a bug.
- Read implementations of called functions, not just signatures — if duplication involves a called function, check whether decomposing it resolves the duplication.
- Do not save time on analysis. Read every function in the data flow even when the interface seems clear — wrong assumptions about internals are the main source of missed bugs.

### Haskell Extensions
- `StrictData` enabled by default
- Use STM for safe concurrency
- Assume concurrency in PostgreSQL queries
- Comprehensive warning flags with strict pattern matching

## Build Commands

```bash
# Standard build
cabal build

# Fast build
cabal build --ghc-options -O0

# Build specific executables
cabal build exe:simplex-chat

# Build with PostgreSQL client support
cabal build -fclient_postgres

# Client-only library build (no server code)
cabal build -fclient_library

# Find binary location
cabal list-bin exe:simplex-chat
```

### Cabal Flags

- `swift`: Enable Swift JSON format
- `client_library`: Build without server code
- `client_postgres`: Use PostgreSQL instead of SQLite for agent persistence
- `server_postgres`: PostgreSQL support for server queue/notification store

## External Dependencies

Custom forks specified in `cabal.project`:
- `aeson`, `hs-socks` (SimpleX forks)
- `direct-sqlcipher`, `sqlcipher-simple` (encrypted SQLite)
- `warp`, `warp-tls` (HTTP server)
