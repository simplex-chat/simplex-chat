# Forward-compatible preferences: preserve unknown preference fields

## Problem

Adding a new preference — a new field in `Preferences` (direct contact preferences) or `GroupPreferences` (group/channel preferences) — currently requires a **two-stage release**:

1. First ship a version that can *parse and retain* the new preference field (dormant, not settable in the UI).
2. Only in a later release enable setting it in the UI.

Without this staging, when an owner on a new client sets the new preference, every client still on the previous version silently drops the unknown field on parse and, on its next re-store or re-broadcast, wipes the value for everyone. The preference is then "lost forever" — the only recovery is for the owner to toggle it again after the whole membership has upgraded, which is impossible to coordinate for a group or channel owner.

We want to eliminate the per-preference two-stage tax: a client on version *N* should transparently preserve preference fields introduced in version *N+1* that it does not yet understand.

## Root cause

`Preferences` and `GroupPreferences` are serialized with aeson `deriveJSON defaultJSON`:

- `Preferences` — `src/Simplex/Chat/Types/Preferences.hs:153`, JSON at `:1144`.
- `GroupPreferences` — `src/Simplex/Chat/Types/Preferences.hs:331`, JSON at `:1186`.
- `defaultJSON = J.defaultOptions {J.omitNothingFields = True}` — `simplexmq/src/Simplex/Messaging/Parsers.hs:153`. `rejectUnknownFields` is not set (defaults to `False`).

With these options, unknown object keys are **silently dropped on parse** (not rejected, not preserved), and `omitNothingFields` drops `Nothing` fields on encode. Because the value is always reconstructed from the parsed record, any future key is gone.

Crucially, both persistence and the wire go through the same instances:

- **Storage.** `contact_profiles.preferences` and `group_profiles.preferences` are TEXT columns holding the JSON of `Preferences` / `GroupPreferences` via `ToField = encodeJSON` and `FromField = decodeJSON` (`Types/Preferences.hs:1146-1150`, `:1188-1192`). Written at `Store/Direct.hs:744`, `Store/Groups.hs:2732`, `Store/Groups.hs:2762`; read via `toContact` (`Store/Shared.hs:497`) and `toGroupInfo` (`Store/Shared.hs:694`).
- **Wire.** `XInfo Profile`, `XGrpInfo GroupProfile`, `XGrpPrefs GroupPreferences`. The derived `Profile` / `GroupProfile` JSON delegate to the `Preferences` / `GroupPreferences` instances.

So a value that retains unknown keys is retained in the DB *and* re-sent on the wire, with no changes to storage or protocol code.

## Guarantee

Make a previous client version a transparent pass-through for preference keys it does not understand. Two sub-guarantees:

- **(a) Dormant survival.** A received-but-unknown preference is retained in local storage, so after the client upgrades it becomes active with no re-toggle.
- **(b) Loss-free re-broadcast.** A client that re-encodes or re-broadcasts profile/preferences state does not strip unknown keys.

Both reduce to a single mechanism: **preserve unknown JSON keys inside the `Preferences` / `GroupPreferences` value**, because storage and wire both flow through the value's JSON instances.

This is a **one-time** infrastructure cost that must ship before the first single-stage preference. It needs **no protocol version bump** (purely additive and transparent) and **no DB schema change** (same columns). The codebase already uses this `J.Object` pattern elsewhere: `CustomData` (`Types.hs:262`), `MCUnknown {json :: J.Object}`, `MRUnknown`, `LCUnknown`, and `CInfoInvalidJSON` (`Messages.hs:175`, per PR #6105 in `docs/CONTRIBUTING.md`).

## Data-flow facts (verified)

Receive and store-gating:

- `processContactProfileUpdate` (`Library/Subscriber.hs:2665`) stores the received profile when `contentChanged = not (sameProfileContent p p')` (`:2683`). `sameProfileContent` (`Types.hs:746`) compares profiles including `preferences`.
- `xGrpInfo` (`Library/Subscriber.hs:3681`) stores when `unless (p == p')` (`:3689`); `xGrpPrefs` / `updateGroupPrefs_` (`:3707`, `:3712`) store when `unless (groupPreferences p == Just ps')` (`:3714`).
- **Consequence:** `unknownFields` must be part of derived `Eq`. Otherwise a change whose only difference is a new unknown key reads as "no change" and is never stored, breaking guarantee (a).

No spurious UI items result from including unknown fields in `Eq`:

- "Group updated" text item is gated on `sameGroupProfileInfo` (`Library/Internal.hs:2988`), which nulls `groupPreferences` before comparing — unaffected by unknown pref keys.
- "Profile updated" direct item is gated on visible display fields only (`visibleProfileUpdated`, `Library/Subscriber.hs:2703`).
- Feature-change items iterate known features only (`createGroupFeatureChangedItems`, `createRcvFeatureItems`), so an unknown pref produces none.

Modification paths:

- **Direct.** `setPreference` / `setPreference'` funnel through `setPreference_` (`Types/Preferences.hs:141`), a record update on `Preferences` — an added field is preserved automatically.
- **Group.** `setGroupPreference` (`:362`), `setGroupPreferenceRole` (`:369`), `setGroupPreference'` (`:376`), `setGroupTimedMessagesPreference` (`:397`) all funnel through `mergeGroupPreferences` (`Maybe GroupPreferences -> FullGroupPreferences`, `:1008`) and `toGroupPreferences` (`FullGroupPreferences -> GroupPreferences`, `:1029`). `toGroupPreferences` is called only from this set family, and every entry point takes `Maybe GroupPreferences` and returns `GroupPreferences`. So unknown keys can be re-attached at the four entry points without adding a field to `FullGroupPreferences`.

Send paths:

- Group toggles via `SetGroupFeature` / `SetGroupFeatureRole` / `SetGroupTimedMessages` (`Library/Commands.hs:3434-3459`) go through `updateGroupProfileByName` (`:3994`), which reads the **stored** `GroupProfile` and applies the modifier to `groupPreferences p` — a core-side path that preserves unknowns once storage and the set family preserve them.
- General group profile edit via `APIUpdateGroupProfile groupId p'` (`Library/Commands.hs:3132`) stores and broadcasts the caller-supplied `p'` wholesale through `runUpdateGroupProfile` (`:3909`, store at `:3913`).

Lossy UI path (important):

- The mobile group-preferences screen rebuilds the entire `groupPreferences` from its Kotlin model on every save and sends the whole `GroupProfile` via `apiUpdateGroupProfile`: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupPreferences.kt:48` — `val gp = gInfo.groupProfile.copy(groupPreferences = preferences.toGroupPreferences())`. The Swift UI does the equivalent.
- So an owner on the previous version strips unknown pref keys on their *first* preference toggle — the normal save path, not a rare edge case. The core cannot see what the UI dropped, so full coverage of guarantee (b) for the mobile path requires either a core-side re-injection of stored unknown keys in `APIUpdateGroupProfile`, or UI-level preservation.

Encoding detail:

- `encodeJSON` uses `J.encode`, which uses `toEncoding`. The aeson fork keeps the default `toEncoding = E.value . toJSON` (`dist-newstyle/src/aeson-*/src/Data/Aeson/Types/ToJSON.hs:313`). So defining only a merging `toJSON` makes both DB and wire preserve unknown keys; no separate `toEncoding` needed.

## Design

### 1. Core representation — `Types/Preferences.hs`

- Add `unknownFields :: J.Object` as the last field of `Preferences` (`:153`) and `GroupPreferences` (`:331`). Keep `deriving (Eq, Show)` — `Eq` must include it (store-gating).
- Replace the two `deriveJSON` splices with hand-written instances:
  - `parseJSON = withObject ...` parses the known fields exactly as today (`.:?` per field — all fields are `Maybe`, matching the derived Maybe semantics) and sets `unknownFields = <input object> \\ knownKeys`.
  - `toJSON x = Object (knownPairs <> unknownFields x)` with known keys winning the left-biased merge, so a stale duplicate can never shadow a real field. Rely on the default `toEncoding`.
  - One `knownKeys` list per type drives the difference (single source of truth; a round-trip test asserts no known field ever lands in `unknownFields`).
  - `ToField` / `FromField` are unchanged — they now carry unknowns automatically.
- Set `unknownFields = mempty` at the literal construction sites: `emptyChatPrefs` (`:494`), `emptyGroupPrefs` (`:515`), `defaultBusinessGroupPrefs` (`:534`), `toGroupPreferences` (`:1029`), the `Preferences {..}` and `GroupPreferences {..}` record literals. `businessGroupPrefs` (`:517`) is a record update over `defaultBusinessGroupPrefs` and inherits `mempty`.
- Group modify: in `setGroupPreference`, `setGroupPreferenceRole`, `setGroupPreference'`, `setGroupTimedMessagesPreference`, re-attach `maybe mempty unknownFields prefs_` onto the result. The direct set path needs no change (record update preserves).

### 2. Core-side defence against lossy UIs — `Library/Commands.hs`

- In `APIUpdateGroupProfile groupId p'`, before `runUpdateGroupProfile`, re-inject unknown preference keys from the stored `groupProfile gInfo` into `p'`. A small helper `restoreUnknownGroupPrefs stored new` sets the result's `groupPreferences.unknownFields` to `storedUnknown <> newUnknown` (falling back to `emptyGroupPrefs` when `new`'s `groupPreferences` is `Nothing`). This makes the normal mobile group-prefs save path loss-free without touching Kotlin or Swift. The semantics — a client cannot delete a preference key it does not understand — are correct.
- Symmetric option (low value): the same for `APIUpdateProfile` / `updateProfile` (the user's own `Preferences`). This only matters on downgrade; propagation-critical contact preferences are store-only and already covered by section 1.

### 3. Tests — `tests/ProtocolTests.hs` (and `tests/JSONTests.hs` if useful)

- Round-trip: an object with an extra unknown key decodes then encodes with the key present and known fields intact.
- Guard: all known fields present ⇒ `unknownFields == mempty` (catches a forgotten `knownKeys` entry).
- Modify preserves: `setGroupPreference` on a `GroupPreferences` carrying an unknown key ⇒ the encoded result still contains the key.
- Eq: two values differing only by an unknown key are `/=` (store-gating relies on this).
- Per project rules: core JSON/round-trip tests only; no UI unit tests.

## Change surface

- `src/Simplex/Chat/Types/Preferences.hs` — sections 1 (representation, JSON, constructors, group set family). The bulk of the change.
- `src/Simplex/Chat/Library/Commands.hs` — section 2 (small helper + one call site; optional symmetric direct case).
- `tests/ProtocolTests.hs` (+ possibly `tests/JSONTests.hs`) — section 3.
- No schema, no migration, no protocol-version change.

## Open decisions

1. **Robustness against the lossy mobile-UI re-broadcast path.**
   - (A, recommended) Core JSON preservation **plus** the `APIUpdateGroupProfile` merge (section 2). Makes the normal mobile save path loss-free with zero UI changes.
   - (B) Core JSON preservation only. Guarantees dormant-survival and loss-free re-broadcast via core toggle commands, but an older-version owner editing group prefs through the mobile UI still strips unknown keys.
   - (C) A, plus add unknown-pref preservation to the Kotlin and Swift preference models (belt-and-suspenders; unnecessary for correctness once A is in place; touches two UI codebases; verified by review only).

2. **Depth.**
   - (Recommended) Top-level features only — the described problem (a whole new preference like `comments` / `sessions`). Leaf preference types keep using `omittedField` for removed fields.
   - Also leaf sub-fields — apply the same `J.Object` capture to each leaf preference type so future sub-fields within an existing preference also survive. More code across the leaf types; rarely needed.

## Out of scope

- Unknown sub-fields inside a leaf preference type (unless decision 2 selects it).
- Kotlin/Swift UI-level unknown-pref preservation (unless decision 1 selects C).
