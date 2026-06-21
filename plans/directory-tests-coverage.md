# Directory Modules: Test Coverage Report

## Final Coverage

| Module | Expressions | Coverage | Gap |
|---|---|---|---|
| **Captcha** | 84/84 | **100%** | -- |
| **Search** | 3/3 | **100%** | -- |
| **BlockedWords** | 158/158 | **100%** | -- |
| **Events** | 527/559 | **94%** | 32 expr |
| **Options** | 223/291 | **76%** | 68 expr |
| **Store** | 1137/1306 | **87%** | 169 expr |
| **Listing** | 379/650 | **58%** | 271 expr |

84 tests, 0 failures.

## What was covered

Tests added to `tests/Bots/DirectoryTests.hs`:

- **Search**: `SearchRequest` field selectors (`searchType`, `searchTime`, `lastGroup`)
- **BlockedWords**: `BlockedWordsConfig` field selectors, `removeTriples` with `'\0'` input to force initial `False` argument
- **Options**: `directoryOpts` parser via `execParserPure` (minimal args, non-default args, all `MigrateLog` variants), `mkChatOpts` remaining fields
- **Events**: command parser edge cases (`/`, `/filter 1 name=all`, `/submit`, moderate/strong presets), `Show` instances for `DirectoryCmdTag`, `DirectoryCmd`, `SDirectoryRole`, `DirectoryHelpSection`, `DirectoryEvent`, `ADirectoryCmd` (including `showList`), `DCApproveGroup` field selectors via `OverloadedRecordDot`, `CEvtChatErrors` path
- **Store**: `Show` instances for `GroupRegStatus` constructors, `ProfileCondition`, `noJoinFilter`, `GroupReg.createdAt` field
- **Listing**: `DirectoryEntryType` JSON round-trip with field selectors

Source changes:

- `Directory/Options.hs`: exported `directoryOpts`
- `Directory/Events.hs`: exported `DirectoryCmdTag (..)`

## Why not 100%

### Events (32 expr remaining)

**Field selectors (9 expr)** on `DEGroupInvitation`, `DEServiceJoinedGroup`, `DEGroupUpdated` -- need `Contact`, `GroupInfo`, `GroupMember` types which have 20+ nested required fields each with no test constructors available.

**`crDirectoryEvent_` branches (3 expr)**: `DEItemDeleteIgnored`, `DEUnsupportedMessage`, `CEvtMessageError` -- need `AChatItem` or `User`, both strict-data types with deep dependency chains impossible to construct in unit tests.

**`DCSubmitGroup` paths (2 expr)**: constructor and `directoryCmdTag` case -- need a valid `ConnReqContact` (SMP queue URI with cryptographic keys).

**Lazy `fail` strings (2 expr)**: `"bad command tag"` and `"bad help section"` -- Attoparsec discards the string argument to `fail` without evaluating it. Inherently uncoverable by HPC.

### Options (68 expr remaining)

**Parser metadata strings (~50 expr)**: `metavar` and `help` string literals in `optparse-applicative` option declarations are evaluated lazily by the library. `execParserPure` constructs the parser but doesn't force help strings unless `--help` is invoked.

**`getDirectoryOpts` (~10 expr)**: wraps `execParser` which reads process `argv` -- can't unit-test without spawning a process.

**`parseKnownGroup` internals (~8 expr)**: the `--owners-group` arg is parsed but the `KnownContacts` parser internals are instrumented separately.

### Store (169 expr remaining)

**DB operations (~150 expr)**: `withDB'` wrappers, SQL query strings, error message literals inside database functions (`setGroupStatusStore`, `setGroupRegOwnerStore`, `searchListedGroups`, `getAllGroupRegs_`, etc.) -- all require a running SQLite database with realistic data.

**Pagination branches (~15 expr)**: `searchListedGroups` and `getAllGroupRegs_` cursor pagination -- need multi-page result sets.

**Parser failure (~4 expr)**: `GroupRegStatus` `strDecode` failure path -- needs malformed stored data.

### Listing (271 expr remaining)

**Image processing (~80 expr)**: `imgFileData`, image file Base64 encoding paths -- require groups with profile images.

**Listing generation (~120 expr)**: `generateListing`, `groupDirectoryEntry` -- require `GroupInfo` (21+ fields), `GroupLink`, `CreatedLinkContact` types with deep nesting into chat protocol internals.

**Field selectors (~40 expr)**: `DirectoryEntry` fields (`displayName`, `fullName`, `image`, `memberCount`, etc.) -- need full `DirectoryEntry` construction which requires `CreatedLinkContact`.

**TH-generated JSON (~30 expr)**: Template Haskell `deriveJSON` expressions are marked as runtime-uncovered by HPC despite executing at compile time.

## Summary

All remaining gaps fall into three categories:

1. **DB integration paths** -- require a running database (Store)
2. **Complex chat protocol types** -- types with 20+ required nested fields (Events, Listing)
3. **Lazy evaluation artifacts** -- HPC can't observe values that are never forced at runtime (Options `help` strings, Attoparsec `fail` strings, TH-generated code)

None are testable with pure unit tests without either standing up a database or constructing massive type hierarchies.
