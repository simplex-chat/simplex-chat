# Forward-compatible preferences: preserve unknown preference fields

## Problem

Adding a preference to `Preferences` (direct) or `GroupPreferences` (group) needs a two-stage release: first ship a version that parses+keeps the field (dormant), then a later version that lets the UI set it. Otherwise, when a new client sets the pref, every client on the previous version drops the unknown key on parse and wipes it for everyone on its next store/re-send. Recovery requires re-toggling after the whole membership upgrades — impossible for a group/channel owner to coordinate.

## Root cause

`Preferences` (`Types/Preferences.hs:153`) and `GroupPreferences` (`:331`) use `deriveJSON defaultJSON` (`:1144`, `:1186`), which **silently drops unknown JSON keys on parse**. Both storage (the `preferences` TEXT columns, via `ToField`/`FromField = encodeJSON`/`decodeJSON`) and the wire (`Profile`/`GroupProfile` JSON delegate to these instances) reconstruct the value from the parsed record, so any future key is lost in the DB and on every re-send.

## Fix

Give each type an `unknownFields :: J.Object` that captures unrecognized keys on parse and re-emits them on encode — the pattern already used by `CustomData`, `MCUnknown{json}`, `CInfoInvalidJSON`. Because storage and wire share these instances, this one change fixes both. **No schema change, no protocol version bump** (purely additive/transparent). It's a one-time cost; afterwards every new pref is single-stage.

## What a reviewer needs to know

- **`Eq` must include `unknownFields`.** Receive handlers store only when the value changed (`xGrpInfo` `unless (p == p')` `Subscriber.hs:3689`; `updateGroupPrefs_` `:3714`; `processContactProfileUpdate` via `sameProfileContent` `:2683`). If Eq ignored unknown keys, a pure-unknown-pref change would look like "no change" and never get stored. No spurious UI items result: the "group/profile updated" items are gated on separate functions that ignore prefs (`sameGroupProfileInfo` nulls prefs; direct item gated on visible fields), and feature-change items only iterate known features.
- **Modification paths.** Direct `setPreference_` (`:141`) is a record update — preserves the new field for free. Group `setGroupPreference*` (`:362`–`:399`) funnel through `mergeGroupPreferences`/`toGroupPreferences`, which rebuild the record; re-attach `maybe mempty unknownFields prefs_` at the four `setGroupPreference*` entry points (leaves `FullGroupPreferences` untouched).
- **Lossy mobile UI.** The group-prefs screen rebuilds the whole `groupPreferences` from its Kotlin model and re-sends the full profile via `apiUpdateGroupProfile` on every save (`GroupPreferences.kt:48`). So an owner on the previous version strips unknown keys on their *first* toggle. Core-only preservation can't see what the UI dropped — see decision 1.

## Change surface (small)

- `Types/Preferences.hs`: add the field to the 2 types; hand-write their `FromJSON`/`ToJSON` (parse known fields as today + capture the rest; encode known + merge unknowns, known wins); set `unknownFields = mempty` at the literal constructors (`emptyChatPrefs`, `emptyGroupPrefs`, `defaultBusinessGroupPrefs`, `toGroupPreferences`, record literals); re-attach unknowns in the 4 group `set*` helpers.
- `Library/Commands.hs` (decision 1): in `APIUpdateGroupProfile` (`:3132`), re-inject stored unknown pref keys into the caller's profile before `runUpdateGroupProfile`.
- `tests/ProtocolTests.hs`: round-trip keeps an unknown key; all-known ⇒ `unknownFields == mempty`; `setGroupPreference` keeps an unknown key; values differing only by an unknown key are `/=`.

## Open decisions

1. **Robustness vs. the lossy mobile save path.**
   (A, recommended) core JSON + re-inject unknown keys in `APIUpdateGroupProfile` — makes the normal mobile save loss-free, zero UI changes.
   (B) core JSON only — dormant survival guaranteed; an old-version owner editing prefs via mobile still strips.
   (C) A + also preserve unknown prefs in the Kotlin/Swift models — belt-and-suspenders, unnecessary once A is in, touches two UI codebases.
2. **Depth.** (recommended) new top-level features only; or also unknown sub-fields inside a leaf pref type (same mechanism, more types).
