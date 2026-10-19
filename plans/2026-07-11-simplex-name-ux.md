# SimpleX name — UX improvements plan

Date: 2026-07-11. Scope: **client-only** (iOS SwiftUI + Kotlin/Compose for Android & desktop); no core / simplexmq change.
Adversarially verified against the code; fixes are merged inline below.

The contact-address and channel name editors are the **same shared view** per platform
(iOS `SetSimplexDomainView` in `apps/ios/.../UserSettings/UserAddressView.swift:718`; Kotlin `SetSimplexDomainView` in
`apps/multiplatform/.../usersettings/SetSimplexNameView.kt:22`), so fixing the editor once covers both call sites.

Reusable pieces:
- Badge: iOS `SimplexNameView` (`ChatInfoView.swift:1377`), Kotlin `SimplexNameView` (`SimplexNameView.kt:28`).
- Inline name-error pattern: iOS `CreateProfile.profileNameField` (`CreateProfile.swift:310-333` — red `exclamationmark.circle`
  in the field when invalid + disabled action button); Kotlin `ProfileNameField` (`WelcomeView.kt:413`, warning `IconButton` +
  `isValid` predicate). See Task 3 for the reuse caveats.
- Warnings: iOS `showAlert` free function; Kotlin `AlertManager.shared`. String "Profile update will be sent to your SimpleX
  contacts" exists on both platforms.
- `apiSetUserDomain` broadcasts the updated profile to contacts (same update+notify path as `APIUpdateProfile`) — CONFIRMED.

## Resolved design decisions (maintainer)
- **Banner name = interactive** (reuse `SimplexNameView`). The iOS banner observes `chat` (`@ObservedObject`), so a verify
  updates the model and refreshes via the observed `chat` — no real caveat.
- **Remove button = clears the input field only**; no save, no confirm. Saving an empty field is what removes the name.
- **Validation = disable Save while invalid + the inline warning-icon pattern**, with a SimpleX-name predicate (below).
- **Save-on-close = prompt Save / Don't save, ONLY when the name is valid AND changed** (invalid or unchanged → silent close).
- **Row display**: actual name (`@name.simplex` / `#name`) when set; original label ("Your SimpleX name" for the address) when
  unset. [address rows DONE]
- **Task 5 warning**: pre-save confirm using the existing string, only when the name actually changed; user/contact path only.
- Copy copies the shown prefixed name. Kotlin editor title stays "Set SimpleX name".

## Task 1 — SimpleX name in the chat-start banner (contact / channel / business) — *safe to build*
Reuse `SimplexNameView`, switching on chat type.
- **iOS** `ChatView.swift` `struct ChatBannerView` (1004-1065): insert between the shortDescr block (~1038) and the
  `chatContext` block (~1040). `switch chat.chatInfo`:
  - `.direct(let contact)` → contact block (mirror `ChatInfoView.swift:396-414`). NOT verbatim: the banner has no
    `@State contact`, so the verify closure uses `ChatModel.shared.updateContact(ct)` and drops `contact = ct`.
  - `.group(let groupInfo, _)` → nested here (both need `groupInfo`): `businessChat == nil` → channel block
    (`GroupChatInfoView.swift:335-354`). NOT verbatim: the banner binds an immutable `let groupInfo`, so — like the contact
    case — the verify closure uses `ChatModel.shared.updateGroup(gInfo)` and drops the `groupInfo = gInfo` reassignment
    (`GroupChatInfoView.swift:346`, which only compiles at the source because `groupInfo` there is an `@Binding`). Else business
    block (`:356-366`, verify `{ nil }`).
  - `default` → `EmptyView()` (keep the switch exhaustive).
- **Kotlin** `ChatView.kt` `ChatBannerView` (2227-2358): insert between the descr `MarkdownText` (~2344) and `chatContext()`
  (~2346). `when (chatInfo)`: `Direct` → contact block (`ChatInfoView.kt:760-773`); `Group` + `businessChat==null` → channel
  (`GroupChatInfoView.kt:976-990`); `Group` + `businessChat!=null` → business (`:991-1001`). `remoteHostId` + `chatModel` are in
  scope, so the verify bodies copy verbatim.

## Task 2 — row shows the name; Copy + Remove buttons in the editor
- **Row (address)**: DONE (iOS `UserAddressView.swift`; Kotlin `UserAddressView.kt`).
- **Row (channel)**: show `#name` when set, keep the existing label when unset. iOS `GroupChatInfoView.swift:734` (value from
  `:712`, already `#`-prefixed). Kotlin `GroupChatInfoView.kt:641`; the value at `:183` is NOT `#`-prefixed — render `"#$name"`.
- **Editor buttons** (shared editor, shown only when the ORIGINAL prefill is non-empty). Each platform gates on its own original:
  iOS reuses the `original` captured in Task 4; Kotlin has no `original` var — it gates on the `simplexName` param, which IS the
  immutable original (Task 4, `SetSimplexNameView.kt:26`).
  - iOS `SetSimplexDomainView` (`UserAddressView.swift:718`): in the Save `Section`, gate on `if !original.isEmpty` and add
    `Button("Copy") { UIPasteboard.general.string = <shown name> }` and `Button("Remove") { simplexName = "" }` — clears the
    existing `@State var simplexName` (NOT a var named `name`); no save, no confirm.
  - Kotlin `SetSimplexNameView.kt` (63-75): below the Save `SectionItemView`, `if (simplexName.isNotBlank())` add a Copy
    `SectionItemView` (clipboard + copied toast) and a Remove `SectionItemView { name.value = "" }`.

## Task 3 — validate; block Save on invalid
Add a SimpleX-name `isValid` predicate that **normalizes internally** (trim, strip a leading `@`/`#`, add `.simplex` via
`addSimplexTLD`) then checks the grammar (dot-separated ASCII `[a-zA-Z0-9]` labels + internal hyphens, ≤63 bytes/label, ≤253
total, TLD label present), mirroring simplexmq `SimplexName.hs` `nameLabelP` — note `isNameLetter` (`SimplexName.hs:71`) accepts
`A-Z` as well as `a-z` and the parser lowercases on accept (`:90`), so `isValid` MUST accept uppercase (or, equivalently,
lowercase the input inside `normalized()` before the grammar check); a literal `[a-z0-9]` predicate would flag a valid uppercase
entry that the core accepts-and-lowercases, wrongly disabling Save. **`isValid` returns true for empty** (a cleared field is
valid — it means "remove"), so the warning icon doesn't flash on Remove.
- **iOS**: mirror `profileNameField` — red `exclamationmark.circle` in the field when invalid; `Save.disabled(saving || !isValid || unchanged)`
  (also fixes iOS not disabling Save when unchanged/empty).
- **Kotlin**: the editor field is currently `PlainTextEditor(name, placeholder)` (`SetSimplexNameView.kt:64`), NOT `ProfileNameField`,
  so the warning icon is not there yet and both options below change which field the editor renders. `ProfileNameField` is also NOT
  a clean drop-in — its invalid-tap hardcodes `showInvalidNameAlert(mkValidName(name.value), name)` (`WelcomeView.kt:454`, the
  display-name correction, wrong for `@name.simplex`) and passes the RAW `name.value` to `isValid` (`:473`). Either (a) REPLACE
  `PlainTextEditor` with `ProfileNameField` — passing a NON-EMPTY `placeholder` (its `trailingIcon` is gated on `!valid && placeholder != ""`,
  `WelcomeView.kt:452`, so an empty placeholder hides the warning) AND adding an invalid-tap/correction-callback param to
  `ProfileNameField` so it takes the SimpleX `isValid` and correction instead of the hardcoded display-name one — or (b) keep
  `PlainTextEditor` but wrap it (Row/Box) with a separately-rendered warning icon computed from the SimpleX `isValid`. Either way,
  `Save.disabled = unchanged || saving.value || !isValid`.

## Task 4 — save-on-close prompt (valid && changed), consistent across contact + channel — *largest fix*
The editor currently stores no baseline, so first **capture the original**, then compute `changed` by comparing the normalized
entered value against the normalized original. But the existing `normalized()` takes NO argument — iOS `normalized()`
(`UserAddressView.swift:759`) reads `self.simplexName`, Kotlin `normalized()` (`SetSimplexNameView.kt:38`) reads `name.value` —
so each normalizes only the entered value and there is no way to normalize the original. **Refactor `normalized()` to accept the
string as a parameter** (iOS `private func normalized(_ s: String) -> String?`; Kotlin `fun normalized(s: String): String?`),
update its one existing call in `doSave` to pass the entered value, then reuse it for both sides:
`changed = normalized(entered) != normalized(original)`.
- iOS: add `@State private var original` set in `.onAppear` to the prefill; also `@State private var didSave = false`.
  Compute `changed = normalized(simplexName) != normalized(original)` (and `unchanged = !changed`).
- Kotlin: the `simplexName` param IS the immutable original; redefine `unchanged` (`SetSimplexNameView.kt:32`) as
  `normalized(name.value) == normalized(simplexName)`, comparing normalized values rather than raw-trimmed, so it matches iOS,
  and add `val changed = !unchanged` alongside it (Kotlin parallel to the iOS `changed`, referenced by the close-prompt gate below).
  **Ordering caveat**: `unchanged` is a `val` at `:32`, but the local `fun normalized` and its helper `fun addSimplexTLD`
  are declared *below* it (`:34` and `:38`), and Kotlin does NOT hoist local functions — a `:32` initializer referencing
  `normalized` fails to compile ("unresolved reference: normalized"). So first MOVE the `addSimplexTLD` + `normalized`
  function declarations above the `unchanged` line (keeping their order, `addSimplexTLD` before `normalized`), then
  redefine `unchanged`. (iOS is unaffected: there `normalized` is a struct member function, visible regardless of textual
  order.)
Prompt only when `changed && isValid` (on the contact path this Save action carries the Task 5 broadcast warning and saves with
the nested confirm suppressed — see Task 5 "Close-prompt interaction"):
- **iOS**: `.onDisappear { if !didSave && changed && isValid { showAlert("Save SimpleX name?", Save/Don't-save) } }`.
  **CRITICAL**: set `didSave = true` on a successful Save — Save calls `dismiss()` (`UserAddressView.swift:746`) which fires
  `.onDisappear` with the edited value still set, so without `didSave` the prompt double-fires right after saving (cf.
  `UserProfile.swift:157` `getCurrentProfile()` which resets its baseline instead).
- **Kotlin**: `ModalView(close = { onClose(close) })`; `onClose` shows the prompt only when `changed && isValid`. The desktop
  background-click bypasses `ModalView.close` (`ModalView.kt:41,61`; cf. `UserAddressView.kt:527`), so on the **contact /
  start-panel path** the close must also route through `onClose` via `chatModel.centerPanelBackgroundClickHandler`. Because
  `SetSimplexDomainView` (`SetSimplexNameView.kt:22-58`) is ONE shared function serving both call sites, it cannot infer which
  path it is on — so give it the signal explicitly: add a `registerBackgroundClose: Boolean = false` param. The contact call
  site (`UserAddressView.kt`) passes `true`; the channel call site (`GroupChatInfoView.kt:182`) leaves it `false`. In a
  `LaunchedEffect(Unit)` the editor registers the handler (→ `onClose(close)`) ONLY when `registerBackgroundClose` is true. Do
  NOT register it for the channel editor: that editor opens via `ModalManager.end` (`GroupChatInfoView.kt:182`) while the desktop
  background-click overlay is gated on start-panel modals and closes only `ModalManager.start` (`App.kt:449-456`), so registering
  there is both a dead no-op for the channel path (already covered by the app-bar back button = `ModalView.close`) AND, because
  `centerPanelBackgroundClickHandler` is a single global slot on `chatModel` (`ChatModel.kt:238`), it would cross-wire a
  start-panel background click to the channel editor's close logic. The handler MUST be cleared to `null` on EVERY close path —
  the save path (`doSave`, `SetSimplexNameView.kt:47-58`), don't-save/revert, and direct close. Clearing to `null` is idempotent,
  so the editor clears it unconditionally on close regardless of `registerBackgroundClose` (a safe no-op on the channel path,
  which never set it); cf. the precedent (`UserAddressView.kt:507,514,518`). The existing `showUnsavedChangesAlert`
  (`UserAddressView.kt:786`) hardcodes auto-accept strings — write a local prompt / new SimpleX-name strings, don't call it directly.
Same pass: unify the editor prefill form (contact prefills full `@name.simplex`, channel prefills short `#name` — pick one).

## Task 5 — warn when saving a *contact* name (profile broadcast)
Gate in the caller's save closure (shared editor; the channel path is behaviorally untouched — no broadcast confirm — though its
closure signature changes with the `confirmBroadcast` flag below; channel uses `apiSetPublicGroupAccess`). The save
closure is `(String?) async -> Bool` (iOS) / `suspend (String?) -> Boolean` (Kotlin) and the confirm is callback-based, so
**bridge the dialog** so the closure awaits the user's choice before calling `apiSetUserDomain`:
- **iOS** `UserAddressView.swift ~202-210`: `withCheckedContinuation` around `showAlert`. **MUST use the actions-based
  `showAlert` overload** (`ShareSheet.swift:61`), building BOTH the confirm and the cancel action with handlers that resume the
  continuation **exactly once** — the default `showAlert(title:message:buttonTitle:buttonAction:)` overload's Cancel action
  (`cancelAlertAction`, `ShareSheet.swift:130`) has NO handler, so tapping Cancel would never resume and the async save closure
  would hang forever with `saving` stuck true. Gate on a **client-side compare** (entered name vs
  `currentUser.profile.contactDomain?.domain`) — not the response — because `apiSetUserDomain` collapses both changed and NoChange
  to `return user` (`SimpleXAPI.swift:1380`). Confirm text = the existing "Profile update will be sent to your SimpleX contacts".
- **Kotlin** `UserAddressView.kt ~378-387`: `suspendCancellableCoroutine` around `AlertManager.shared.showAlertDialog`; string
  exists. Wire `onConfirm`, `onDismiss`, AND `onDismissRequest` (`AlertManager.kt:126-133`) to each resume the continuation
  **exactly once** — otherwise a dismissal (tap-outside / back) leaves the suspended save coroutine hung. (The editor already
  disables Save when unchanged, so the "only when changed" gate is largely covered here.)

**Close-prompt interaction (Task 4 ↔ Task 5).** Tapping *Save* in the Task 4 "Save SimpleX name?" close prompt runs the same
contact save closure that carries this Task 5 broadcast confirm, so without a decision two dialogs stack (Task 4 prompt →
then Task 5 confirm). Decision: the close prompt already captured intent to save, so the nested Task 5 confirm is SUPPRESSED
when the save originates from the close prompt, and the warning is surfaced exactly once by giving the **contact-path** close
prompt the broadcast-warning text ("Profile update will be sent to your SimpleX contacts") as its message body (the plain
"Save SimpleX name?" wording stays for the channel path, which has no broadcast). Mechanism: thread a `confirmBroadcast` flag
(default `true`) into the save call — the normal in-editor Save leaves it `true` (shows this confirm), the close-prompt Save
passes `false`. Note this CHANGES the shared save-closure signature — Kotlin `suspend (String?) -> Boolean` →
`suspend (String?, Boolean) -> Boolean`, iOS `(String?) async -> Bool` → `(String?, Bool) async -> Bool` — so BOTH call sites'
closure literals must accept the flag or the code won't compile (mismatched closure types). The contact call site acts on it;
the channel call site (`GroupChatInfoView`) must be updated to accept and IGNORE it (its closure has no broadcast to suppress).
The contact vs channel distinction is the same signal Task 4 already carries (`registerBackgroundClose` on
Kotlin; pass the equivalent flag to iOS's `SetSimplexDomainView`), so the close prompt picks its message from it.

## Status
- DONE: address row shows dynamic `@name.simplex` when set / "Your SimpleX name" when unset (iOS + Kotlin).
- Task 1 is safe to build as-is. Tasks 2–5 have the verification fixes merged above (Task 4 is the largest; do the Task-4
  `original`/`didSave`/close plumbing first since Tasks 2 and 3 reuse the `original` field and the disabled-Save state).
