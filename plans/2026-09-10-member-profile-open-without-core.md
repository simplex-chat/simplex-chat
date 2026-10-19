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

The same wait is in `GroupChatInfoView.kt:125` and `MemberSupportChatView.kt:78`.

## Root Cause

The modal was created inside the coroutine, after both awaits, and the loaded
values were passed into the view as plain parameters:

```kotlin
fun GroupMemberInfoView(
  ...
  connectionStats: ConnectionStats?,
  connectionCode: String?,
  ...
)
```

A plain parameter can only be supplied by a caller that has already waited.

Each call site also resolved the member through the model and rendered nothing
when it was absent:

```kotlin
remember { derivedStateOf { chatModel.getGroupMember(member.groupMemberId) } }.value?.let { mem -> ... }
```

In a large group where members were never loaded, that is an empty card - which
is why `#7388` had to write the member into the model *before* showing the modal.

## Solution

Open the modal first and let the loaded values arrive afterwards. The two
parameters become state the caller creates at the tap and fills in when the
queries return:

```kotlin
val connStats = mutableStateOf<ConnectionStats?>(null)
val connectionCode = mutableStateOf<String?>(null)
ModalManager.end.showModalCloseable(...) { close ->
  GroupMemberInfoView(chatRh, groupInfo, member, scrollToItemId, connStats, connectionCode, ...)
}
groupMembersJob = scope.launch(Dispatchers.Default) {
  ...unchanged...
  connStats.value = r?.second
  connectionCode.value = code
}
```

The card is shown from the member that is already known at the tap - from the
chat item, or from the members list - with the model's copy preferred once it is
there:

```kotlin
val member = remember(groupMember.groupMemberId) {
  derivedStateOf { chatModel.getGroupMember(groupMember.groupMemberId) ?: groupMember }
}.value
```

The `?: groupMember` fallback is what removes the dependency: the card no longer
needs the model to contain the member, so nothing has to happen before the modal
opens.

## Technical Design

### The state is created at the tap, not in the view

`ModalManager.showInView` drives `AnimatedContent` from `modalCount` alone
(ModalView.kt:207), so the card is disposed while a sub-screen is open and
re-composed on return. State held by `remember` inside the view would lose the
loaded values there, and the rows would disappear. A `MutableState` created in
the click handler is captured by the modal lambda and outlives every
recomposition of the card, with no storage of its own.

### The loaded code cannot come from the model

`APIGetGroupMemberCode` returns the live code beside the member
(Commands.hs:2047); the member it returns carries only the verification record,
which the core *clears* when the code no longer matches. So the code has to be
held by the UI - it cannot be read back off `member.activeConn`.

### The rows are reserved, not gated and not inserted

iOS wraps everything below the action buttons in `if connectionLoaded`
(GroupMemberInfoView.swift:123-288) - the action section, Address, Member,
Servers, Connection failed and For console. Until both queries return its card is
a header and three buttons over blank space. That is tolerable when the core
answers in 1-2 ms; it is the wrong trade for the case this change exists for,
where the core is busy and the wait is what the user sees.

Letting each row appear as its data arrives is no better: the card is complete
from the first frame but grows under the finger.

So the rows that depend on the load are rendered from the first frame in their
final positions, disabled, and enabled in place when the data arrives:

```kotlin
val canVerifyCode = member.memberRole != GroupMemberRole.Relay && (connectionCode != null || !connectionLoaded)
...
VerifyCodeButton(member.verified, verifyClicked, disabled = connectionCode == null)
```

`SectionItemView` already drops the `clickable` modifier when `disabled`
(Section.kt:213), so a disabled row is inert and identical in size to the enabled
one - nothing moves when it is enabled.

Only rows that can be predicted locally are reserved:

- **Verify security code.** The enclosing condition is already
  `memberActive || (useRelays && memberCurrent)`, so inside it the loader's
  `getCode` reduces to `memberRole != Relay` - known from the member at the tap.
- **Servers.** Reserved as a whole section with a disabled "Change receiving
  address" and empty Receiving/Sending rows, for a member the same condition says
  is connected.
- **Fix connection is not reserved.** `canSyncConn` needs
  `cStats.ratchetSyncAllowed`, which is false except during ratchet desync, so a
  placeholder for it would disappear on nearly every open - the same jump in the
  opposite direction.

A third state, `connectionLoaded`, is created at the tap beside the other two and
set when the queries return, including on failure. Without it a failed load would
leave the row disabled forever, because "no code yet" and "no code at all" look
the same.

Network status is reserved with them. It looks like agent state that cannot be
predicted, but `connSubStatus` (simplexmq Agent.hs:2736) returns `Just` whenever
`rcvQueuesInfo` is non-empty and `Nothing` only when it is empty - the same list
that decides whether "Receiving via" is rendered. The two rows appear together,
so reserving one and not the other is what leaves a gap.

The remaining shift is "Abort changing address", which renders only while
`rcvSwitchStatus != null` on a receiving queue. That is a switch the user
started, so it is absent in normal use and reserving it would add a row that
disappears on nearly every open.

iOS needs its gate for a reason Kotlin does not have: `newRole` is `@State`
initialised to a placeholder `.member` and corrected inside `.task`
(GroupMemberInfoView.swift:30, :297), so the role picker must not render before
that runs. Kotlin seeds it from the member inline -
`remember { mutableStateOf(member.memberRole) }` - and is correct on the first
frame.

### The chat-change guard is kept

`#7388` added `chatModel.chatId.value != groupInfo.id` so that a load finishing
after the chat changed does not write to the model. The modal now opens before
the guard is reached, and `KeyChangeEffect` (:56) closes the card on chat change
as before, so the guard is left exactly as it was.

`groupMembersJob.cancel()` is kept in the tap handler: `info` (ChatView.kt:390)
preloads members *before* showing its modal, so without the cancel a pending chat
info load would open its modal on top of the member card.

## Consequences

- The card renders from the member known at the tap, so it can be marginally
  staler for the duration of the load. iOS has the same window - it renders the
  header before `connectionLoaded`.
- "Verify security code" and the Servers section are shown disabled until the
  queries return, so the card does not change height as they arrive. This is a
  third behaviour, matching neither the old Kotlin (rows inserted as data landed)
  nor iOS (whole body hidden until loaded); porting it to iOS would be the way to
  converge them.
- "Fix connection" still appears on arrival when the ratchet needs syncing. It is
  rare enough that reserving space for it would move the card more often than it
  saves.
- "Send message" for a member with `!sendMsgEnabled` does nothing if tapped
  before the stats arrive. iOS is identical: `createMemberContactButton` has the
  same `else if let connStats` with no else, and renders the action buttons
  before `connectionLoaded`.
- The modal is now opened from the click handler on the main thread instead of
  from `Dispatchers.Default` / `withBGApi`. All three callers are UI click
  handlers, so this is strictly safer - `modalViews` is a plain `ArrayList`.

## Out of Scope

- **Collapsing the three copies of the fetch.** The same two queries and the same
  condition are still written out at all three call sites. That duplication has
  already drifted once - `#7255` widened the condition for channel members in two
  of the three and in iOS, and `ChatView.kt` was left behind until `#7486` - and
  moving the load into the view, as iOS has done since `#5008`, would remove it.
  It is a restructure rather than a fix, so it belongs in its own change.
- Message *Info* (ChatView.kt:706) still loads all members; it needs them to
  resolve delivery recipients. A narrower core API would be required.
- Opening a second member's profile while one is already open does not switch the
  card: `closeModals()` sets `modalCount` to 0 and `showCustomModal` back to 1, so
  `AnimatedContent`'s target never changes. The close and open are back to back in
  both the old and the new code, so this is neither introduced nor worsened.
