# Accepting an invitation twice

Two unrelated failures with the same shape - a second accept or join reaching the core - plus the
client state that let users produce them.

## Problem

### Contact request

Accepting a contact request that was already accepted fails with an agent error:

```
Error accepting contact request:
error agent: error agent CMD PROHIBITED SEInvitationNotFound acceptContact, invitationId = "..."
```

Reported after accepting from a notification and then, while the request still looked pending,
opening the chat and accepting again. The first accept completed normally, so the contact connected
regardless — the second accept only produced the error.

### Group

Joining a group via a link twice fails with a raw SQLite error, reported far more often:

```
chat db error: SEDBException {message = "SQLite3 returned ErrorConstraint while attempting to
perform step: UNIQUE constraint failed: group_members.group_id, group_members.member_id"}
```

The user does not have to tap twice. Joining a channel whose relays are unreachable throws a
`BROKER` error, the client offers Retry, and the retry re-sends the same command - see fix 2.
Connecting twice on a group link that is not relayed fails the same way on
`connections.group_member_id`.

## Cause

`acceptContactRequest` (`Library/Internal.hs`) unconditionally ends with the agent call, using the
invitation id stored on the request:

```haskell
(ct,conn,) <$> withAgent (\a -> acceptContact a nm (aUserId user) (aConnId conn) True invId dm pqSup' subMode)
```

Its `Just conn` branch already recognises that the contact has a connection and skips creating one,
but still calls the agent with the invitation the first accept consumed.

Nothing marks the request as accepted at the chat level:

- `deleteContactRequestRec` was removed from the accept path by `cc643e5ae`
  ("core: rework contact requests so that they are always created with entity", #6011), so the
  `contact_requests` row now survives acceptance;
- `deleteContactRequest` is called only from `APIRejectContact`;
- `createContactFromRequest` only sets `contact_requests.contact_id`;
- there is no accepted flag on the table.

So a repeat accept passes every chat-side check and reaches the agent. Before #6011 it failed early
with `SEContactRequestNotFound`.

On the client, nothing recorded that an accept was under way. The only in-progress state was private
to `ComposeContextContactRequestActionsView` — a `rememberSaveable` flag that is additionally reset
on chat change — while the notification action and the chat list menu passed none. Opening the chat
after accepting from a notification therefore composed a fresh, fully enabled Accept button.

## Fix

**1. Core: report an invitation the agent already used** (`Library/Internal.hs`).

The chat layer cannot tell an accepted request from an accept that failed. `acceptContactRequest`
commits the contact connection with status `ConnNew` (`createAcceptedContactConn`) and only then
calls `acceptContact`, so a connection existing means the accept was *attempted*; and nothing
advances `ConnNew` on success either - it stays `ConnNew` until the peer's `CONF` arrives. A guard
on the connection would reject retrying an accept that failed, which
`retry connecting via contact link` covers.

The agent can tell, because it owns the invitation and flips the flag in the right order
(`acceptContact'`):

```haskell
Invitation {connReq} <- withStore c $ \db -> getInvitation db "acceptContact'" invId  -- WHERE accepted = 0
r <- joinConn c nm userId connId enableNtfs connReq ownConnInfo pqSupport subMode      -- fallible
withStore' c $ \db -> acceptInvitation db invId ownConnInfo                            -- accepted = 1
```

So `SEInvitationNotFound` from `acceptContact` means the request was accepted, and a join that
failed leaves `accepted = 0` for the retry to find. The error it surfaces as - `CMD PROHIBITED`,
mapped in `Agent/Client.hs` - is translated to a chat command error instead of reaching the user as
`error agent CMD PROHIBITED SEInvitationNotFound acceptContact, invitationId = "..."`. Within
`acceptContact'` that is the only source of `CMD PROHIBITED`: every other one in the agent belongs
to a command not reachable from here, and `joinConn` fails with `BROKER` or `SMP`.

`throwCmdError` is used rather than a new `ChatErrorType` constructor: it matches the other command
preconditions in this handler and avoids fanning a new error type through Kotlin, Swift, the
TypeScript/python SDKs and the generated bot docs. A structured, localizable error can replace it
later if wanted.

**2. Core: joining a prepared group is idempotent** (`Library/Commands.hs`, `Store/Groups.hs`).

This is a different failure from the contact request one, and it is the one behind the reports of a
duplicate SQL error for groups. `APIConnectPreparedGroup` creates the link owner members inside
`withFastStore` **before** the relay loop, and throws when every relay fails. That error is an agent
`BROKER` error, which `retryableNetworkErrorAlert` matches, so the client offers Retry and
`sendCmdWithRetry` re-sends the identical command — which re-inserted the same owner members:

```
UNIQUE constraint failed: group_members.group_id, group_members.member_id
```

`createLinkOwnerMember` is now reached through `getCreateLinkOwnerMember`, matching
`getCreateRelayForMember` in the same module, whose comment already says "Save relayLink to re-use
relay member record on retry". The relay loop and the direct join were already retry-safe:
`joinContact` advances the connection past `ConnPrepared` only after the agent call returns, so a
join that failed is resumed by the `ConnPrepared -> joinPreparedConn'` branch rather than repeated.
Owner member creation was the only step in the command that was not.

Reaching that branch with a connection **past** `ConnPrepared` therefore means the join already
succeeded, and `connect'` would create a second connection for the same member:

```
UNIQUE constraint failed: connections.group_member_id
```

That is now `throwCmdError "group is already being joined"`, replacing a `connect'` call the code
itself questioned (`-- why not "already connected" for host member?`). It cannot be reached by a
retry, only by connecting again after success — which is what fix 3 stops the UI from offering.

**3. Client: do not offer to accept or join while accepting or joining** (android, desktop, ios).

An invitation being accepted is a fact about the invitation, not about the view that shows it, but
every view kept its own flag: `ComposeContextContactRequestActionsView` and
`CIGroupInvitationView` a `rememberSaveable`, `ChatListNavLinkView` a third that was threaded as a
`MutableState<Boolean>` parameter through `groupChatAction` → `acceptGroupInvitationAlertDialog` →
`JoinGroupAction`, and the notification action, the chat list menu and the accept alert none at all.
So the chat list Join and the in-chat "tap to join" for the same group could not see each other, and
accepting from a notification left the in-chat banner fully enabled.

`ChatModel.acceptingContactRequests` and `ChatModel.joiningGroups` now hold the ids, keyed by
contact request id and group id — not by chat id, because the same request appears under
`ChatInfo.ContactRequest` (`<@N`) in the chat list and under `ChatInfo.Direct` (`@N`) in the chat,
and those two views must agree. They are maintained by the shared `acceptContactRequest` and by a
new shared `joinGroup`, so every caller participates without changing its call site and a failed
accept re-enables the buttons. Every view derives from the record instead of owning a flag, which
deletes the parameter chain, the per-view flags and the `KeyChangeEffect` that existed only to reset
them on chat change.

The record is taken before the request is sent rather than inside the background coroutine, so a
second tap in the same view is blocked too — the previous per-view flags were set synchronously at
the click site and moving to the model must not lose that.

The chat list long-press menu offers no Accept while an accept is in flight, matching what
`GroupMenuItems` already did for Join. Reject stays, so the menu is never empty.

**4. Client: the connect composer does not render in a group's support scope** (`ComposeView.kt`).

`ChatView` owns its `composeState`, and `MemberSupportChatView` opens a second `ChatView` for the
same group in `ModalManager.end`. In the desktop two-pane layout both composers are visible at once,
and the condition at `ComposeView.kt` had no scope check, so a prepared group showed two independent
Join buttons. The connect composer belongs to the chat itself, not to a scope within it.

**5. Client: the progress indicator appears after 0.5s instead of 1s** (android, desktop, ios).

The effect behind it was copied into 5 Kotlin and 4 Swift views; it is now one helper per platform,
which is also what makes the threshold a single constant. Two of the Swift copies had drifted — the
deferred write captured the value that started the delay rather than reading the current one, so an
operation that finished before the delay elapsed left the indicator on with nothing to turn it off.

## Alternatives considered and rejected

**Guarding the accept on the contact connection** (reject when the contact reached via a request
already has a connection that is not `ConnPrepared`). This was the first shape of the fix and it is
wrong: the connection is committed before the agent call that can fail, so the guard also rejects
retrying an accept that failed with a network error, which `retry connecting via contact link`
exercises. Nothing on the connection distinguishes the two - a successful accept leaves it at
`ConnNew` as well. The same mistake produced the group half of this plan.


**Reordering the notification action to accept before switching profile.** Would have removed the
window in the cross-profile case entirely, since during the accept the user is still on the other
profile where the request is not displayed. Rejected: it requires `APIAcceptContact` to work for a
non-active user (reverted in #5978, see PR #7316), it delays the visible profile switch by the
accept round trip, and it does nothing for the active-profile case that the in-flight guard covers
anyway.

**Fixing `nextAcceptContactRequest` so the button disappears when the accept lands.** The predicate
treats `connStatus == New` as "not yet accepted", but a non-`sqSecured` accept leaves the connection
at `ConnNew` until the peer's `CONF` arrives, so the button can stay offered after a fully
successful accept. Deferred, not rejected: the window requires a peer whose invitation cannot be
secured immediately (`sendWelcomeMsg` only runs when `sqSecured`, and in the added test the contact
is ready the moment the accept returns), while the change spans Kotlin and Swift, ~8 read sites per
platform, and is shared with the group direct invitation flow whose pre-accept connection state
would need verifying first.

**Blocking the UI for the duration of the accept** (holding the switch-user progress overlay across
the accept). Rejected: it makes a full-screen, input-blocking spinner depend on a network round
trip.

## Not covered

**`sendCmdWithRetry` re-sends the command.** On a retryable `BROKER` error it shows a Retry alert and
re-issues the same command (`SimpleXAPI.kt`). `apiAcceptContactRequest`, `apiJoinGroup` and
`apiAcceptMemberContact` pass no `inProgress`, unlike `apiConnectPlan`, so a retry can re-send an
accept whose first attempt may have partially landed. No button is involved, so nothing in this
change addresses it.

**`acceptMemberContact` and `apiAcceptMember`** keep their per-view flags; they are the same shape
but were left out of scope.

## Testing

- `connecting to channel twice fails` (`tests/ChatTests/Groups.hs`) and `prepare group and connect
  twice` (`tests/ChatTests/Profiles.hs`): join, then `/_connect group #1` again and expect
  `bad chat command: group is already being joined`, then exchange messages to show the connection
  from the first join is intact. Both reproduce the reported SQL errors without the fix — the
  channel test fails on `group_members.group_id, group_members.member_id` (owner members) and the
  direct one on `connections.group_member_id` — and both reach the new guard only because owner
  member creation became idempotent.
- `retry connecting to group via short link` and the rest of the `short links` group (34 examples)
  pass unchanged, which is what shows a real retry after a connection failure still completes.
- `accepting contact request twice fails` (`tests/ChatTests/Profiles.hs`): accept, wait for the
  contact to connect, then `/_accept 1` again and expect
  `bad chat command: contact request already accepted`, then exchange messages to show the
  connection established by the first accept is intact. The expected string exists only in the guard
  and the test, so the test cannot pass without the fix.
- `cabal build lib:simplex-chat` and `:common:compileKotlinDesktop` build clean.
- The in-flight state is not covered by an automated test — it is UI state, reachable only by
  holding two views of the same invitation open at once.

### Manual check

1. Receive a contact request. Open the chat and the chat list side by side (desktop). Accept from
   the chat list menu — the composer banner greys out at the same moment, and the menu stops
   offering Accept.
2. Accept a contact request from a notification, then open the chat: the banner is already greyed,
   where before it was fully enabled.
3. Receive a group invitation. Open the direct chat with the inviter and the chat list side by side.
   Tap to join in the chat item — the chat list row greys out too, and its long-press menu stops
   offering Join.
4. In each case the progress indicator appears after 0.5s rather than 1s.
