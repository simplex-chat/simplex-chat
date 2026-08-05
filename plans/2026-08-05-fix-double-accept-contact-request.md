# Accepting a contact request twice

## Problem

Accepting a contact request that was already accepted fails with an agent error:

```
Error accepting contact request:
error agent: error agent CMD PROHIBITED SEInvitationNotFound acceptContact, invitationId = "..."
```

Reported after accepting from a notification and then, while the request still looked pending,
opening the chat and accepting again. The first accept completed normally, so the contact connected
regardless — the second accept only produced the error.

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

**1. Core: reject accepting a request that was already accepted** (`Library/Internal.hs`).

`createAcceptedContactConn` (`Store/Direct.hs`) is the only code that creates a `ConnContact`
connection with status `ConnNew`, and all three of its call sites are in the accept path
(`createContactFromRequest`, `acceptContactRequest`, `acceptContactRequestAsync`). So a contact
reached via a contact request having a connection at all means an accept already ran — manual or
automatic — with `ConnPrepared` as the one legitimate pre-accept case (prepared short-link
contacts). The guard reads state the accept path itself creates; it stores nothing, changes no
connection status semantics and emits no new events.

`APIAcceptContact` runs under `withContactRequestLock`, so two accepts of the same request
serialize and the second one observes the state left by the first — the guard is reliable for
concurrent taps, not only for sequential ones.

`throwCmdError` is used rather than a new `ChatErrorType` constructor: it matches the other command
preconditions in this handler and avoids fanning a new error type through Kotlin, Swift, the
TypeScript/python SDKs and the generated bot docs. A structured, localizable error can replace it
later if wanted.

**2. Client: do not offer to accept a request that is being accepted**
(`ChatModel.kt`, `ChatListNavLinkView.kt`, `ComposeContextContactRequestActionsView.kt`).

`ChatModel.acceptingContactRequests` holds the ids of requests currently being accepted. It is
maintained inside the shared `acceptContactRequest` in a `try`/`finally`, so every caller —
notification action, chat list, compose banner — participates without changing its call site, and a
failed accept re-enables the buttons. The compose banner derives its in-progress state from both its
own flag and the shared set, keyed on `contactRequestId` so the derived state is not reused across
request chats. The existing rendering does the rest: both buttons grey out and lose their
`clickable` modifier, and after one second the banner dims with a progress indicator.

## Alternatives considered and rejected

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

## Known gap

The chat list long-press menu still offers Accept while an accept is in flight — the in-flight guard
covers the compose banner only. The outcome is safe (the core guard rejects the second accept with a
clear message instead of the agent error), but the item is still tappable. Closing it cleanly needs
a disabled state on `ItemAction`, which has none today; making `onClick` a no-op would leave a dead
control, and hiding the group would leave an empty menu.

## Testing

- `accepting contact request twice fails` (`tests/ChatTests/Profiles.hs`): accept, wait for the
  contact to connect, then `/_accept 1` again and expect
  `bad chat command: contact request already accepted`, then exchange messages to show the
  connection established by the first accept is intact. The expected string exists only in the guard
  and the test, so the test cannot pass without the fix.
- `cabal build lib:simplex-chat` and `:common:compileKotlinDesktop` build clean.
- The in-flight greying is not covered by an automated test — it is UI state on a path reachable
  only from a notification or a second view.
