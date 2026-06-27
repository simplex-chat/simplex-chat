# SimpleX name UI: display + verify + set (iOS + Android/desktop)

Branch: `sh/namespace-ui` (rebased onto core `fc0582cf0` — the finalized verify API). This pass adds
displaying a contact's / channel's SimpleX name with verification state, verifying names (manual + auto),
and two screens for setting the user's own name and a channel's name.

**Upstream sync (2026-06-27):** core `sh/namespace` is at `fc0582cf0` ("split verify API for contacts
and public groups"). UI branch rebased onto it (backup tag `backup-namespace-ui-pre-rebase2`). The 5 UI
commits are pure frontend; the rebase was clean.

## Scope

Ships (both iOS and Android/desktop):
- Models: decode name / proof / verification fields, add `NameClaimProof` + `SimplexNameInfo` helpers.
- Name display + 3-state verification indicator on contact info and channel info.
- Verify API calls + new response handling.
- "Verify SimpleX names" privacy toggle (default ON).
- Two "Set SimpleX name" screens (own name; channel name).

Out: no change to the core verify algorithm — the UI only triggers it and renders the result.

## Decisions

Single source of truth. The UX walkthrough shows the visuals; the implementation sections give file:line.

### Product / UX (confirmed)
- **A. Title** — rename to "SimpleX address and name".
- **B. Screen body copy** — placeholders for now (final copy TBD):
  - Own: *"Set a SimpleX name so people can connect to you using @yourname instead of a link. The name
    must already be registered to your address."*
  - Channel: *"Set a SimpleX name so people can find this channel as #name. The name must be registered
    to this channel's address."*
- **C. Own name read-only** — No; the current own name appears only inside the set screen.
- **D. Rename scope** — rename the shared string everywhere (settings-menu row + screen title).
- **E. Auto-verify trigger** — with the toggle ON, auto-verify on open only if stored state is `null`;
  verified/failed shows the stored result, and tapping the indicator re-verifies.
- **F. Failure reason** — shown on tap of the red cross (alert); an inconclusive result shows a brief
  alert on completion. Not shown inline.

### Technical approach
- **T1. Show the name only when a proof exists** (`contactDomainProof` / `groupDomainProof` != nil).
- **T2. One formatter + one parser.** `SimplexNameInfo.shortName` (display, mirrors `shortNameInfoStr`),
  `editDomain` (prefix-less, prefills set fields), `SimplexNameInfo(parsing:)` (decode the encoded
  `groupDomain` string). Contact display uses the decoded object; channel display parses `groupDomain`.
- **T3. Set screens send a name string; UI fixes only the type prefix.** `strP` reads `@`->contact /
  `#`->group first, so the UI prepends `@` (own) / `#` (channel); the backend canonicalises the domain.
  Empty input clears.
- **T4. Channel uses a raw `APIUpdateGroupProfile`** (documented at the call site): core has
  `APISetUserName` but no `APISetGroupName`, so the channel name is set by re-sending the cloned
  `GroupProfile` with `publicGroup.publicGroupAccess.groupDomain` updated.
- **T5. Gating.** Channel "Set SimpleX name" only when `useRelays && isOwner && publicGroup?.publicGroupAccess != nil`;
  own "Set SimpleX name" lives in the existing-address branch (inherently requires an address).

Minor defaults (flag if wrong): iOS toggle in the first "Chats" Section (PrivacySettings.swift:85);
spinner delay ~300ms; channel name cleared via empty input; set-name fields rely on backend rejection
for validation (+ helper text).

## UX walkthrough

Same on both platforms. Nothing existing moves.

### Name + verification indicator (contact info / channel info)

A new line under the name, above the description, shown per T1.

```
   Contact info                  Channel info
   +------------------+          +------------------+
   |     [ photo ]    |          |     [ photo ]    |
   |      Alice       |          |     My Team      |
   |  @alice.simplex v|  <- new  |  #myteam  Verify |  <- new
   |  "description..."|          |  "description..."|
   +------------------+          +------------------+
      (v = check)                  (Verify = action)
```

Name on the left, indicator on the right; the indicator depends on state:

| State (`*Verification`) | Name style | Indicator |
|---|---|---|
| Verified (`true`) | accent color | check mark in regular color |
| Failed (`false`) | code style | cross mark in red (tap -> failure reason, per F) |
| Not verified (`null`), toggle OFF | code style | "Verify name" action (accent) |
| Not verified (`null`), toggle ON | code style | auto-verify on open (per E) -> spinner -> result |
| Verifying (in-flight) | code style | delayed spinner (~300ms, so a fast result doesn't flash) |

### Set screens (2 new buttons -> 2 new screens)

1. **Own name** — on the "SimpleX address and name" screen, a new "Set SimpleX name" button just above
   the "Or to share privately" section:
   ```
     [ QR code ]
     Share address
     Business address (toggle)
     Address settings
     ------------------
     Set SimpleX name   ->     <- new
     ------------------
     Or to share privately
     Create 1-time link
   ```
2. **Channel name** — on channel info, in the existing "Advanced options" section (owners only):
   ```
     Advanced options
       Web access
       Set SimpleX name   ->     <- new
   ```

Each opens the same view (parameterised by prefix / body text / save action): explanation + a text field
with a fixed prefix adornment (`@` own -> user types `alice.simplex`; `#` channel -> user types `myteam`)
+ Save. Empty + Save clears the name.

### Settings toggle
"Verify SimpleX names" toggle, default ON, in the privacy settings "Chats" section (iOS:
PrivacySettings.swift:85, no `MorePrivacyView`; Kotlin: `MorePrivacyView` Chats section, PrivacySettings.kt:118).

## Backend reference (core @ fc0582cf0)

Single source for wire/JSON facts.

- Display: `shortNameInfoStr` (simplexmq Protocol.hs:1594) — public group on default `.simplex` TLD with
  empty subdomain -> `#myteam`; else prefix (`@`/`#`) + full domain (`@alice.simplex`, `#myteam.testing`).
- Encoded form: `strEncode SimplexNameInfo = "simplex:/name" <> ("@"|"#") <> fullDomain` (Protocol.hs:1565).
- JSON shapes: `LocalProfile.contactDomain :: Maybe SimplexNameInfo` -> JSON **object**;
  `PublicGroupAccess.groupDomain :: Maybe (StrJSON SimplexNameInfo)` -> JSON **string** (encoded form).
- Verification: `LocalProfile.contactDomainVerification` / `GroupInfo.groupDomainVerification :: Maybe Bool`
  -> JSON `true`/`false`/absent (decodes as Swift `Bool?` / Kotlin `Boolean?`); null=not attempted,
  false=failed, true=verified.
- Proof: `LocalProfile.contactDomainProof` / `PublicGroupAccess.groupDomainProof :: Maybe NameClaimProof`
  -> JSON object `{linkOwnerId?: string, presHeader: string, signature: string}` (all strings).
- Verify commands (manual; network/resolver errors are retryable `ChatErrorAgent`):
  - `APIVerifyContactName {contactId}` -> `/_verify name @<contactId>`.
  - `APIVerifyPublicGroupName {groupId}` -> `/_verify name #<groupId>`.
  - Outcome `NVOVerified | NVOFailed Text | NVOInconclusive Text` -> persists `Just True` / `Just False` /
    leaves unchanged. Responses `CRContactNameVerified {user, contact, verificationResult :: Maybe Text}` /
    `CRGroupNameVerified {user, groupInfo, verificationResult :: Maybe Text}` return the **updated** entity
    plus `Nothing`=verified / `Just reason`=failure-or-inconclusive text.
- Set own name: `APISetUserName userId (Maybe SimplexNameInfo)` -> `/_set_name <userId> [<name>]` (parsed by
  `strP`, NOT json; Commands.hs:5438). Rejects `name is not registered to your address`. Response
  `CRUserProfileUpdated` / `CRUserProfileNoChange`.
- Set channel name: `APIUpdateGroupProfile` (`/_group_profile #<id> <json>`, Commands.hs:5571) with
  `groupDomain` set. Rejects `name is not registered to this channel`. Response `CRGroupUpdated`.

## Gotchas

1. **iOS Codable (compile-blocking).** `SimplexNameInfo`/`SimplexNameDomain`/`SimplexTLD`/`SimplexNameType`
   are `Decodable`-only (ChatTypes.swift:5261-5281); adding `contactDomain` to the `Codable` `LocalProfile`
   breaks `Encodable` synthesis -> make all four `Codable`. (Kotlin already `@Serializable`.)
2. **`groupDomain` string carries the `simplex:/name` prefix** -> strip it in `parsing`.
3. **`NameClaimProof` decoded for presence only.** UI just checks `!= nil`; if any field's JSON shape is
   uncertain at implementation, decode permissively.
4. **Delayed spinner — no existing helper.** iOS: `@State var verifying` set true after `Task.sleep(~300ms)`,
   guarded by an `inFlight` flag. Android: a small `LaunchedEffect(inFlight){ delay(300); show=true }` composable.
5. **Enum JSON parity** — backend `enumJSON (dropPrefix "TLD"/"NT")` lowercases -> matches the Swift/Kotlin
   enum raw values.
6. **Navigation.** iOS `NavigationLink`; Android `ModalManager.start.showModalCloseable { ... }`
   (template: PrivacySettings.kt:162).

---

## iOS implementation

### Models — `apps/ios/SimpleXChat/ChatTypes.swift`
- Make `SimplexNameInfo`/`SimplexNameDomain`/`SimplexTLD`/`SimplexNameType` (5261-5281) `Codable`.
- Add `NameClaimProof: Codable, Hashable { presHeader: String; signature: String; linkOwnerId: String? }`.
- `SimplexNameInfo` (5261): add `init?(parsing: String)`, `var shortName: String`, `var editDomain: String`.
- `LocalProfile` (153): add `contactDomain: SimplexNameInfo?`, `contactDomainProof: NameClaimProof?`,
  `contactDomainVerification: Bool?`.
- `GroupInfo` (2506): add `groupDomainVerification: Bool?`.
- `PublicGroupAccess` (2616): keep `groupDomain: String?`; add `groupDomainProof: NameClaimProof?`.

### API — `apps/ios/Shared/Model/{AppAPITypes,SimpleXAPI}.swift`
- `ChatCommand`: `apiSetUserName(userId: Int64, name: String?)` -> `"/_set_name \(userId)" + (name.map{" "+$0} ?? "")`;
  `apiVerifyContactName(contactId: Int64)` -> `"/_verify name @\(contactId)"`;
  `apiVerifyPublicGroupName(groupId: Int64)` -> `"/_verify name #\(groupId)"`.
- `ChatResponse1`: add `contactNameVerified(user:contact:verificationResult:)` /
  `groupNameVerified(user:groupInfo:verificationResult:)` cases (+ `responseType`/`details` entries).
  Templates: `contactUpdated` AppAPITypes.swift:1114 / responseType:1193 / details:1267; `groupUpdated`:1140.
- Wrappers: `apiSetUserName(_:)`; `apiVerifyContactName(_:)` (updated `Contact` + reason);
  `apiVerifyPublicGroupName(_:)` (updated `GroupInfo` + reason). Channel set reuses `apiUpdateGroup(_:_:)`.

### Views
- Reusable `SimplexNameView` subview rendering the 5-row state table (incl. delayed spinner + verify action).
- Contact: `ChatInfoView.swift` `contactInfoHeader()` — before the `shortDescr` block (~395), per T1 render
  `SimplexNameView` from `contactDomain`/`contactDomainProof`/`contactDomainVerification`; onAppear auto-verify (E).
- Channel: `GroupChatInfoView.swift` `groupInfoHeader()` — before webPage (~334), same from parsed
  `groupDomain` + `groupDomainProof` + `groupDomainVerification`; "Set SimpleX name" button in Advanced options (~248).
- Address view: `UserAddressView.swift` — "Set SimpleX name" Section above "Or to share privately" (194).
- New `SetSimplexNameView.swift` (own + channel modes).
- Title rename (A/D): the address screen's title is set by the presenter, not `UserAddressView`
  (literal also at UserAddressLearnMore.swift:71) — locate the title source and rename everywhere.
- Toggle: `PrivacySettings.swift` main "Chats" Section (~85) `Toggle("Verify SimpleX names", isOn:)`
  bound to `@AppStorage(DEFAULT_PRIVACY_VERIFY_SIMPLEX_NAMES)`; declare the constant + default `true` in
  the `appDefaults` dict (SettingsView.swift:88-105).

## Android/desktop implementation (multiplatform Kotlin)

### Models — `model/ChatModel.kt`
- Add `@Serializable data class NameClaimProof(presHeader: String, signature: String, linkOwnerId: String? = null)`.
- `SimplexNameInfo` (4875): add `companion fun parse(encoded): SimplexNameInfo?`, `val shortName`, `val editDomain`.
- `LocalProfile` (2061): add `contactDomain: SimplexNameInfo? = null`, `contactDomainProof: NameClaimProof? = null`,
  `contactDomainVerification: Boolean? = null`.
- `GroupInfo` (2181): add `groupDomainVerification: Boolean? = null`.
- `PublicGroupAccess` (2323): keep `groupDomain: String?`; add `groupDomainProof: NameClaimProof? = null`.

### API — `model/SimpleXAPI.kt`
- `CC`: `ApiSetUserName(userId, name: String?)`, `ApiVerifyContactName(contactId)` -> `"/_verify name @$contactId"`,
  `ApiVerifyPublicGroupName(groupId)` -> `"/_verify name #$groupId"` (+ cmdString entries).
- `CR`: `@SerialName("contactNameVerified") ContactNameVerified(user, contact, verificationResult: String?)`,
  `GroupNameVerified(user, groupInfo, verificationResult: String?)` (+ `responseType`). Templates:
  `ContactUpdated` SimpleXAPI.kt:6454 / responseType:6646; `GroupUpdated`:6500.
- Wrappers `apiSetUserName`, `apiVerifyContactName`, `apiVerifyPublicGroupName`; channel set reuses `apiUpdateGroupProfile`.
- Pref: `val privacyVerifySimplexNames = mkBoolPreference(SHARED_PREFS_PRIVACY_VERIFY_SIMPLEX_NAMES, true)` (~123).

### Views
- Reusable `SimplexNameView` composable (the 5-row state table).
- Contact: `views/chat/ChatInfoView.kt` — after `ChatInfoDescription(...)` (~759), per T1; onAppear auto-verify (E).
- Channel: `views/chat/group/GroupChatInfoView.kt` — after `ChatInfoDescription(...)` (~947), per T1;
  "Set SimpleX name" button in Advanced options `SectionView` (~805).
- New `views/usersettings/SetSimplexNameView.kt` (own + channel modes).
- `UserAddressView.kt`: "Set SimpleX name" button above the one-time-link section (~347).
- Toggle: `PrivacySettings.kt` `MorePrivacyView` "Chats" Section (~118) `SettingsPreferenceItem(..., appPrefs.privacyVerifySimplexNames)`.
- `strings.xml`: `set_simplex_name`, `verify_simplex_name`, `verify_simplex_names`, screen titles/body;
  rename `simplex_address` -> "SimpleX address and name" (D).

## Build / verify
- iOS: Xcode build of SimpleXChat + app (or swift build of the package targets).
- Android/desktop: `./gradlew` compile of `:common` (desktop target fastest).
- Manual: a peer with a verified name shows accent + check; tampered/failed shows red cross + reason on tap;
  toggle OFF shows "Verify name"; toggle ON auto-verifies on open with a delayed spinner. Set own/channel
  name; clearing works; core rejection surfaces as an alert.

## Commit plan (conventional)
1. `feat(names): decode name/proof/verification; add SimplexNameInfo helpers + NameClaimProof` (models, both)
2. `feat(names): verify API (contact/group) + response handling` (CC/CR, wrappers)
3. `feat(names): show name + verification state on contact and channel info` (SimplexNameView + headers)
4. `feat(names): "Verify SimpleX names" privacy toggle + auto-verify on open`
5. `feat(names): set user and channel SimpleX name screens`
