# Open Member Profile Without Waiting for the Core

## Context

`#7388` removed `apiListMembers` from the member profile tap, which was O(group
size) and took seconds in a group with 10000 members. It did not remove the
dependency on the core.

`showMemberInfo` (ChatView.kt:499) still awaited two calls before the modal was
created:

```kotlin
val r = chatModel.controller.apiGroupMemberInfo(...)          // awaited
val (updatedMember, code) = if (...) {
  val memCode = chatModel.controller.apiGetGroupMemberCode(...) // awaited
  ...
}
...
ModalManager.end.showModalCloseable(...) { ... }               // only now
```

Both are single-row queries, 1-2 ms when the core is idle, which is why the tap
looks instant in testing. But `sendCmd` is serialized against everything else the
core is doing, so while it is busy - startup, a batch of incoming events, a long
database operation - the tap produces nothing at all until the core drains. The
profile opens at the speed of the core rather than the speed of the UI.

The same two calls were duplicated at three call sites: `ChatView.kt:499`,
`GroupChatInfoView.kt:125` and `MemberSupportChatView.kt:78`.

## Root Cause

The knowledge of how to prepare a `GroupMemberInfoView` lived at every call site
rather than in the view. The view's signature required it:

```kotlin
fun GroupMemberInfoView(
  ...
  connectionStats: ConnectionStats?,
  connectionCode: String?,
  ...
)
```

Two pre-fetched parameters mean the caller cannot construct the view without
talking to the core first. The premise "the caller has already waited for the
core" was encoded in the interface.

Each call site also resolved the member through the model and rendered nothing
when it was absent:

```kotlin
remember { derivedStateOf { chatModel.getGroupMember(member.groupMemberId) } }.value?.let { mem -> ... }
```

In a large group where members were never loaded, that is an empty card - which
is why `#7388` had to write the member into the model *before* showing the modal.

The duplication had already drifted. `#7255` widened the condition for requesting
the security code so that channel members - who are `memberCurrent` but not
`memberActive` - are covered. It updated `GroupChatInfoView` and
`MemberSupportChatView`, and iOS, where the fetch lives in the view and one line
covered every entry point. `ChatView.kt` is not in that PR's diff at all, so
tapping a member avatar in a channel offered no "Verify security code" while
opening the same member from the members list did.

## Solution Summary

**Prerequisite (#7486).** Align the condition in `ChatView.kt` with the other two
call sites and iOS. Collapsing three call sites into one derivation forces a
choice between the two conditions that exist today, so this change cannot be
behaviour-preserving on its own; making that one line explicit first leaves the
restructure below with no behaviour change hidden in it. It is a separate PR
because it is a different bug, and it applies to `stable` too, where the gap is
live in shipped 7.0.0-7.0.2.

**This change.** Move the load into the view, as the iOS app has done since
`#5008` ("ios: fix group member sheet load animation"). The profile is shown from the member
that is already known at the tap - from the chat item, or from the members list -
and the view loads the connection info itself:

```kotlin
// the passed member is shown until the loaded one is added to the model, so that the profile opens without waiting for the core
val member = remember(groupMember.groupMemberId) {
  derivedStateOf { chatModel.getGroupMember(groupMember.groupMemberId) ?: groupMember }
}.value
```

The `?: groupMember` fallback is what actually removes the dependency: the card
no longer needs the model to contain the member, so nothing has to happen before
the modal opens. All three call sites collapse to the modal call alone.

## Technical Design

### No loading gate is needed

iOS hides its whole body below the action buttons behind `connectionLoaded`
(GroupMemberInfoView.swift:123). The Kotlin layout already tests for null at each
connection-dependent point - `canVerifyCode` (:566), `canSyncConn` (:567) and the
Servers section (:645) - so those rows simply appear when the data arrives and
everything else renders immediately. A blanket gate would hide content that is
already available locally.

iOS needs its gate for a reason Kotlin does not have: `newRole` is `@State`
initialised to a placeholder `.member` and corrected inside `.task`
(GroupMemberInfoView.swift:30, :297), so the role picker must not render before
that runs. Kotlin seeds it from the member inline -
`remember { mutableStateOf(member.memberRole) }` (:104) - and is correct on the
first frame.

### The loaded state lives in ModalData

`ModalManager.showInView` drives `AnimatedContent` from `modalCount` alone
(ModalView.kt:207), so the card is disposed while a sub-screen is open and
re-composed on return. With `remember` the connection rows would disappear and
the two queries would repeat on every return from the security code screen.
`stateGetOrPutNullable` keeps them in the modal's own `ModalData`, and a
`connectionLoaded` flag loads once per opened profile - the same number of calls
as before this change.

### The member is added to the model before the rows are shown

`verifyClicked` resolves the member through `chatModel.getGroupMember` (:229).
Setting `connectionCode` first would reveal "Verify security code" while the
model could still be missing the member, and a click landing in that window would
open an empty modal. The upsert happens first.

### The chat-change guard is not needed

`#7388` added `chatModel.chatId.value != groupInfo.id` because `upsertGroupMember`
is a no-op when the open chat changed, which would leave `getGroupMember` null and
open an empty card. The fallback removes that hazard, and `KeyChangeEffect`
(:56) already closes the card on chat change.

`groupMembersJob.cancel()` is kept in the tap handler: `info` (ChatView.kt:390)
preloads members *before* showing its modal, so without the cancel a pending
chat info load would open its modal on top of the member card.

## Consequences

- `GroupChatInfoView` and `MemberSupportChatView` now write the loaded member to
  the model, where they previously discarded it (`val (_, code)`).
  `APIGetGroupMemberCode` clears verification in the database when the peer's
  code no longer matches and returns the updated member, so those two paths could
  show a stale verified shield. Unavoidable once the load is unified, and correct.
- The card renders from the member known at the tap, so it can be marginally
  staler for the duration of the load. iOS has the same window - it renders the
  header before `connectionLoaded`.
- "Send message" for a member with `!sendMsgEnabled` does nothing if tapped
  before the stats arrive. iOS is identical: `createMemberContactButton` has the
  same `else if let connStats` with no else, and renders the action buttons before
  `connectionLoaded`.
- The modal is now opened from the click handler on the main thread instead of
  from `Dispatchers.Default` / `withBGApi`. All three callers are UI click
  handlers, so this is strictly safer - `modalViews` is a plain `ArrayList`.
- The role picker initialises from the member known at open time rather than from
  a freshly fetched one. `upsertGroupMember` rewrites chat items when a role
  changes, so it tracks the model, and iOS sets it the same way.

## Alternatives Rejected

- **Add `connectionLoaded` over the whole body, for exact iOS parity** - no
  content shift, but the card sits nearly empty while the core is busy, which is
  the situation this change exists to fix.
- **Reserve the space with disabled placeholder rows** - removes the shift
  without the empty card, but invents UI that neither platform has. Seeding
  `connStats` from `member.activeConn?.connectionStats` to fill it would also
  offer "Fix connection" on stale data, and that button calls
  `apiSyncGroupMemberRatchet` with no confirmation.
- **Fix only `ChatView.kt`** - smallest diff, but leaves three copies of the
  fetch and a fourth variant of the condition that `#7255` already drifted.

## Out of Scope

- Message *Info* (ChatView.kt:706) still loads all members; it needs them to
  resolve delivery recipients. A narrower core API would be required.
- Opening a second member's profile while one is already open does not switch the
  card: `closeModals()` sets `modalCount` to 0 and `showCustomModal` back to 1, so
  `AnimatedContent`'s target never changes. The close and open are back to back in
  both the old and the new code, so this is neither introduced nor worsened.
