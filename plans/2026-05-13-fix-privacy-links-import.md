# "Remove link tracking" setting does not persist across database import

PR: [#6977](https://github.com/simplex-chat/simplex-chat/pull/6977) · branch `nd/fix-privacy-links-import` → `master`

## 1. Problem statement

The **Settings → Privacy & security → Remove link tracking** toggle (`privacySanitizeLinks`) is silently dropped when a user moves their chat database to another device or reinstalls. Reproduction:

1. Device A: enable "Remove link tracking", export chat database.
2. Device B (fresh install) or same device after re-install: import the database.
3. Open Settings → Privacy & security on B: the toggle is **off**.

All three platforms are affected (Android, desktop, iOS) and any combination of source/target. Every other v6.5 "Safe web links" privacy guarantee survives the import; only "Remove link tracking" reverts.

## 2. Solution summary

The preference is stored locally only (Android `SharedPreferences`, iOS `UserDefaults` group). The cross-device transport for app settings is the `AppSettings` JSON record that travels with the database via `apiGetAppSettings` / `apiSaveAppSettings`. `privacySanitizeLinks` was absent from this record in all three layers (Haskell core, Kotlin multiplatform, Swift iOS), so it had nothing to ride on.

Fix: add `privacySanitizeLinks :: Maybe Bool` to the `AppSettings` record in each of the three layers, wired identically to the reference field `privacyAskToApproveRelays`. Default in all three layers is `false`, matching today's local default. The fix is strictly additive (`+18` lines, 5 files, no deletions); no schema change, no command/API change, no UI change.

## 3. Detailed tech design

### 3.1 The round-trip the fix plugs into

```
Device A                                Device B
─────────                               ─────────
local pref store                        local pref store
  ↑                                       ↑ importIntoApp()
  │ user toggles UI                       │
  │                                     AppSettings ← apiGetAppSettings(local prepareForExport)
  │                                       ↑
  │           archive (.zip with         │
local pref → AppSettings.current        chat.db) ─────┐
            → prepareForExport          │             │
            → apiSaveAppSettings          │             │
            → app_settings DB row ─────────┘             │
                                                       │
                                       core: combineAppSettings
                                         stored <|> platformDefaults <|> defaults
```

`AppSettings.current` reads every local pref; `prepareForExport` strips fields equal to their default (space optimisation); `apiSaveAppSettings` writes the JSON into the `app_settings` table of the chat DB, which travels inside the archive. On import, the receiving client runs `apiGetAppSettings(local.prepareForExport())`; the core merges stored ⟶ local-platform ⟶ hardcoded-defaults with `Alternative` (`<|>`) and returns the result; the client's `importIntoApp` applies any non-null fields to its local store.

A field that is **absent from `AppSettings`** at any of the three layers never enters this pipeline and is therefore lost on import. `privacySanitizeLinks` was such a field.

### 3.2 Three-layer parity

The three `AppSettings` definitions must agree on every field name, default value, and the four operations:

| Operation | Haskell | Kotlin | Swift |
|---|---|---|---|
| field declaration | `data AppSettings` (`src/Simplex/Chat/AppSettings.hs:28`) | `data class AppSettings` (`SimpleXAPI.kt:8038`) | `struct AppSettings` (`AppAPITypes.swift:2118`) |
| default | `defaultAppSettings` (`AppSettings.hs:79`) | `defaults` (`SimpleXAPI.kt:8157`) | `defaults` (`AppAPITypes.swift:2188`) |
| "missing key" parse default | `defaultParseAppSettings` (`AppSettings.hs:116`) | implicit `null` | implicit `nil` |
| merge fallback | `combineAppSettings` (`AppSettings.hs:153`) | n/a (only one source) | n/a |
| JSON parser | hand-written `parseJSON` (`AppSettings.hs:207`) | `@Serializable` derived | `Codable` derived |
| read-from-local | n/a (clients send it) | `AppSettings.current` (`SimpleXAPI.kt:8193`) | `AppSettings.current` (`AppSettings.swift:71`) |
| write-to-local | n/a (clients apply it) | `importIntoApp` (`SimpleXAPI.kt:8110`) | `updateIosGroupDefaults` / `init from cfg` (`AppSettings.swift:13`) |
| serialize-only-non-default | n/a | `prepareForExport` (`SimpleXAPI.kt:8072`) | `prepareForExport` (`AppAPITypes.swift:2151`) |

The fix adds one line to every cell that exists for `privacyAskToApproveRelays`. Default value is `false` (matches `mkBoolPreference(..., false)` and the `registerGroupDefaults` entry).

### 3.3 Round-trip correctness — case analysis

The core's `combineAppSettings = stored <|> platformDefaults <|> defaultAppSettings` (with `Alternative` on `Maybe`) means: take the stored value if present, else what the client said its default is, else the hardcoded default. The client's `prepareForExport` only includes a field when it differs from the client's `defaults`. With both `defaults` set to `false`:

| Case | Archive carries | Local pref before | platformDefaults sent | Merged | Result |
|---|---|---|---|---|---|
| New archive, source had on | `Just true` | false | `Nothing` (default) | `Just true` | **on** ✓ |
| New archive, source had off (default) | `Nothing` (stripped) | false | `Nothing` | `Just false` (from defaults) | **off** ✓ |
| New archive, source had off | `Nothing` | true (local toggled) | `Just true` | `Just true` | **on** (local wins, archive silent) ✓ |
| Old archive (pre-fix) | field unknown | false | `Nothing` | `Just false` | **off** (unchanged from before fix) |
| Old archive | field unknown | true | `Just true` | `Just true` | **on** (local preserved) ✓ |
| Cross-platform | `Just true` | false | `Nothing` | `Just true` | **on** ✓ |

The only "interesting" semantic — *archive silent on the field while local has it on* — preserves local. This matches how every other field in `AppSettings` behaves and matches user intent ("I toggled it on this device, then imported some old archive — keep it on").

### 3.4 Edge cases verified

- **Downgrade then upgrade.** New code → toggle on → export. Imported on *old* code: `parseJSON` ignores unknown keys, DB row is rewritten without the field. Re-upgrade: field absent, falls through to `Just false`. This is the standard "old client drops new fields" semantics for every prior AppSettings addition; not introduced by this PR.

- **iOS `BoolDefault` before `set` is ever called.** `apps/ios/SimpleXChat/AppGroup.swift:100` already registers `GROUP_DEFAULT_PRIVACY_SANITIZE_LINKS: false` in `registerGroupDefaults`. So `privacySanitizeLinksGroupDefault.get()` returns `false` on first read — no NaN/nil sentinel risk.

- **JSON field ordering.** `deriveToJSON defaultJSON` uses record-field order; new field is inserted between `privacyLinkPreviews` and `privacyShowChatPreviews`, shifting subsequent keys. No external consumer compares the JSON byte-for-byte; the existing `testAppSettings` test compares `J.encode defaultAppSettings` on both sides of the wire and so is self-consistent under the addition.

- **`omitNothingFields = True`.** The Haskell `defaultJSON` config (`Simplex.Messaging.Parsers`) strips `Nothing` fields from JSON output, so `defaultParseAppSettings` (every field `Nothing`) does not pollute archives or wire payloads when used as a fallback.

- **iOS NSE / SE extensions.** Neither references `privacySanitizeLinks`. No additional wiring required.

### 3.5 What was deliberately not done

- **Flipping the *user-facing* default to `true`.** Other privacy fields in `defaultAppSettings` are `Just True` (encrypt local files, ask to approve relays). "Remove link tracking" remains `Just False` because the local pref default (`mkBoolPreference(..., false)`, iOS `registerGroupDefaults: false`) is `false`. Aligning the `AppSettings` default with the local default keeps the `prepareForExport` "differs-from-default" comparison consistent — otherwise off-by-default users would suddenly serialise `false` everywhere and on-by-default users would serialise nothing, inverting the wire shape. Whether the *product* default should be flipped to on is a separate question for a separate change.

- **Adding `apiSaveAppSettings` on toggle.** Toggling the pref in `PrivacySettings.kt` writes only to shared prefs; the DB's `app_settings` row stays stale until a separate trigger (theme change, export, migration) syncs. The export and migration paths already call `apiSaveAppSettings(AppSettings.current.prepareForExport())` immediately before producing the archive, so every UI-initiated export captures the current value. Plugging the sync into every toggle is a broader change affecting every AppSettings field equally — out of scope.

- **Fixing `privacyChatListOpenLinks`.** The Kotlin `AppSettings` declares it (`SimpleXAPI.kt:8046`); the Haskell record and the Swift struct do not. Same failure mode as the bug being fixed here — almost certainly does not persist across Android-to-Android imports. Out of scope; should be tracked separately.

- **Adding a targeted test.** The existing `testAppSettings` exercises a JSON round-trip with `defaultAppSettings`, so the new field rides through implicitly. A field-specific test (`defaultAppSettings { privacySanitizeLinks = Just True }`) would tighten coverage against a future client dropping the field; recommended as a small follow-up.

## 4. Detailed implementation plan

### 4.1 Files touched

| File | Δ | Purpose |
|---|---|---|
| `src/Simplex/Chat/AppSettings.hs` | +6 / 0 | record field, `defaultAppSettings`, `defaultParseAppSettings`, `combineAppSettings`, JSON parser line, record reassembly |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` | +5 / 0 | data class field, `prepareForExport`, `importIntoApp`, `defaults`, `current` |
| `apps/ios/Shared/Model/AppAPITypes.swift` | +3 / 0 | struct field, `prepareForExport`, `defaults` |
| `apps/ios/SimpleXChat/AppGroup.swift` | +2 / 0 | new `privacySanitizeLinksGroupDefault: BoolDefault` next to existing privacy defaults |
| `apps/ios/Shared/Views/UserSettings/AppSettings.swift` | +2 / 0 | import side (`set`), export side (`get`) |

Total: 5 files, +18 / 0. No deletions.

### 4.2 Step-by-step (commit `15457a903`)

1. **`AppSettings.hs`** — add `privacySanitizeLinks :: Maybe Bool` to the record (between `privacyLinkPreviews` and `privacyShowChatPreviews`); set `Just False` in `defaultAppSettings`; `Nothing` in `defaultParseAppSettings`; `p privacySanitizeLinks` in `combineAppSettings`; `privacySanitizeLinks <- p "privacySanitizeLinks"` in `parseJSON`; add to record reassembly. Field position consistent with name groupings.

2. **`SimpleXAPI.kt`** — same insertions in `data class AppSettings`, `prepareForExport`, `importIntoApp`, `defaults`, `current`. Local pref already exists (`SimpleXAPI.kt:126`).

3. **`AppAPITypes.swift`** — same insertions in `struct AppSettings`, `prepareForExport`, `defaults`.

4. **`AppGroup.swift`** — add `public let privacySanitizeLinksGroupDefault = BoolDefault(defaults: groupDefaults, forKey: GROUP_DEFAULT_PRIVACY_SANITIZE_LINKS)`. The key constant (line 31) and registered default `false` (line 100) already exist; only the typed wrapper for non-`@AppStorage` access was missing.

5. **`AppSettings.swift` (iOS view extension)** — import side: `if let val = privacySanitizeLinks { privacySanitizeLinksGroupDefault.set(val) }`. Export side: `c.privacySanitizeLinks = privacySanitizeLinksGroupDefault.get()`.

### 4.3 Verification

- Haskell `testAppSettings` (`tests/ChatTests/Direct.hs:2768`) covers the JSON round-trip through `defaultAppSettings`; the new field flows through both sides of the equality, so existing assertions hold.
- Manual test plan (in PR description):
  1. Enable on Android, export DB, import on a second Android device — toggle stays on.
  2. Enable on iOS, export, import on a second iOS device — toggle stays on.
  3. Enable on desktop, export, fresh-install + import — toggle stays on.
  4. Cross-platform: export from Android, import on iOS, and vice versa — toggle preserved.
  5. Fresh install with no archive — toggle defaults to off (unchanged).

### 4.4 Risk and rollback

- **Blast radius**: the `AppSettings` JSON payload. Every other field is untouched (positional inserts, no reordering of existing fields beyond the natural shift).
- **Backwards compatibility**: old clients (no field) parsing new JSON ignore the key. New clients (with field) parsing old JSON see `Nothing`, fall through to `defaultAppSettings` and the local pref is set to its default. Either direction is safe.
- **Rollback**: `git revert 15457a903`. Restores pre-fix behaviour (the field-loss bug returns).

## 5. Why this specific shape

- The bug has exactly one cause: a missing field in the round-trip payload. The smallest fix is to add the field. Anything larger (e.g. broadening `importIntoApp` to scan all shared prefs, or pinning the value in a side channel) would be a structural change that does not improve correctness.
- The `<|>` merge in `combineAppSettings` already gives the right behaviour for every edge case (archive-silent local-set, fresh install, downgrade) once the field exists. No new merge logic needed.
- The default `false` is forced: any other choice would either contradict the local pref default (`mkBoolPreference(..., false)`, iOS `registerGroupDefaults: false`) or invert the wire shape of `prepareForExport`.
- Final PR is 5 files, +18 / 0. Three of those files are the three `AppSettings` records; the other two are the iOS wiring the new field needs in order to read and write its group default. No other file in the codebase needed touching.
