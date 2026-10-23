# Channel SignMessages preference (recipient side, stage 1)

## Problem

Per-message opt-in signing (the current channel-message-signing PR) puts the signing decision on each sender, so a channel cannot express or enforce an expectation that its content is signed. The chosen direction is a channel-wide preference under which all content in the channel is expected to be signed. This is a two-stage rollout so clients have time to update and process the preference. Stage 1 ships **recipient support only**, default off, channel-only. Stage 2 (later) makes the preference affect sending. The existing Part-A signing/verification/§7 enforcement stays as-is (used for testing the recipient side until sending is built).

## Design

New group feature `GFSignMessages` (on/off, no-role, default off), channel-only. When on in a channel, the recipient marks a received **new content item** whose signature is absent as "signature required but missing" rather than plain unsigned.

`CIMeta.msgSigned :: Maybe MsgSigStatus` becomes `msgVerified :: MsgVerified`:

```
data MsgVerified = MVSigned MsgSigStatus | MVSigMissing | MVUnsigned
```

- `MVSigned s` — a signature is present (verified or no-key), as today.
- `MVSigMissing` — pref requires a signature but it's absent (red warning in meta).
- `MVUnsigned` — pref doesn't require a signature and it's absent (legacy unsigned).

Computed at content-item creation from `(RcvMessage.msgSigned, channel SignMessages pref)`:
- `Just s` → `MVSigned s`
- `Nothing` → `MVSigMissing` if the channel requires signing, else `MVUnsigned`

Only **new content items** get this; edits/deletes keep today's behavior. `RcvMessage.msgSigned :: Maybe MsgSigStatus` (raw signature result) is unchanged. Sent items: `MVSigned MSSVerified` if signed, else `MVUnsigned` (no `MVSigMissing` for own items in stage 1).

### Encoding

- **DB (`msg_signed`, unchanged column):** `MVSigned MSSVerified`→`"verified"`, `MVSigned MSSSignedNoKey`→`"no_key"`, `MVSigMissing`→`"sig_missing"`, `MVUnsigned`→`"unsigned"`. Decode: those strings, plus **NULL → `MVUnsigned`** (legacy rows). No migration.
- **JSON (API→UI):** tagged encoding; `omittedField = MVUnsigned` for forward-compat (older clients / missing field).

### Feature exclusion from regular groups

`GFSignMessages` added with `groupFeatureInChannel = True`. New predicate `groupFeatureInRegularGroup :: GroupFeature -> Bool` (False for `GFSignMessages`, True otherwise); a `regularGroupFeatures = filter groupFeatureInRegularGroup allGroupFeatures` used where regular groups generate feature items / list features, so `SignMessages` is channel-only (no group items, not in group preference UI).

### UI

- `msgVerified` field + `MsgVerified` type on Kotlin/iOS.
- Meta badge: `MVSigned MSSVerified` → signature badge (as now, gated on file loaded); `MVSigMissing` → red `exclamationmark.triangle`, tap → alert "signature required but missing" (mirror the AUTH-error alert on the status X); `MVUnsigned` → nothing.
- Add the `SignMessages` channel preference to the channel preferences UI (will be default-off / hidden at release; shown for now for testing).

## Steps

1. `Types/Shared.hs`: `MsgVerified` + TextEncoding/ToField/FromField (NULL→MVUnsigned) + JSON (+omittedField).
2. `Types/Preferences.hs`: `GFSignMessages` (enum, SGADT, name, instances, allGroupFeatures, `groupFeatureInChannel`=True, `groupFeatureInRegularGroup`, preference plumbing, default off); update regular-group feature usage to `regularGroupFeatures`.
3. `Messages.hs`: rename `msgSigned`→`msgVerified`, type `MsgVerified`; `CIMeta` + `JCIMeta` + `mkCIMeta`.
4. Store: `createNewChatItem_` takes `MsgVerified`; write/read `msg_signed`. `createNewSndChatItem`/`createNewRcvChatItem` compute it; thread a `signMessagesRequired :: Bool` where needed.
5. `Subscriber.hs` `newGroupContentMessage`: compute the required flag from the channel pref; adapt §7 enforcement to `MsgVerified`.
6. `View.hs` + other Haskell usages of `msgSigned`.
7. Build core; iterate.
8. UI (Kotlin + iOS): type, field, badge + warning + alert, preference.
9. Tests (channel: pref on + unsigned content → MVSigMissing; pref off → MVUnsigned; signed → MVSigned).

## Progress / divergences (2026-07-09)

- **Haskell core: DONE, library builds clean.** Type `MsgVerified` + encodings; `GFSignMessages` feature (channel-only via `groupFeatureInRegularGroup`/`regularGroupFeatures`); `signMessagesRequired` + `toMsgVerified` threaded through send/receive/store; §7 enforcement adapted; CLI renderer `msgVerifiedStr`.
- **Kotlin: DONE (review-verified; gradle not run).** `MsgVerified` sealed class; `CIMeta.msgVerified`; badge + red `ic_warning` for `SigMissing`; tap-alert via `sigMissingInfo`; `GroupFeature.SignMessages` + all switches; `FullGroupPreferences`/`GroupPreferences.signMessages`; channel-gated toggle in `GroupPreferences.kt`; strings.
- **iOS: delegated (in progress).**
- **Divergence 1 (scope):** requirement applies to BOTH as-channel posts (`CDChannelRcv`) and member posts (`CDGroupRcv`); regular groups are never affected because the preference is off there by construction (default off, excluded from the group UI, business chats set it off). Rationale: "everything in the channel signed." Confirm if as-channel-only is wanted.
- **Divergence 2 (Kotlin pref placement):** toggle gated on `groupInfo.isChannel` at the top of the preferences screen (existing feature toggles there are gated on `!useRelays`, and relay-channels showed none). Confirm placement.
- **No migration:** `msg_signed` column reused. `MVUnsigned`→`"unsigned"`; decode `"unsigned"` and NULL → `MVUnsigned`.
- **Remaining:** iOS review; Haskell recipient test; product/spec doc updates (multiplatform + iOS doc protocol).
