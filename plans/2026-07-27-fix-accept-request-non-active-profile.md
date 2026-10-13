# Accepting a contact request from a notification for a non-active profile

## Problem

With two profiles on one device, a contact request that arrives for the profile that is **not**
currently active shows a notification, and tapping **Accept** in it fails with:

```
ERROR accepting contact request: error store: error store userContactLinkNotFound
```

Repro: create an address in profile 1, create profile 2, connect to profile 2's address from
profile 1, then accept the resulting request from the notification while profile 1 is active.

## Cause

`APIAcceptContact` is scoped to the **active** user (`Library/Commands.hs`):

```haskell
APIAcceptContact incognito connReqId -> withUser $ \user@User {userId} -> do
  uclData_ <- withFastStore $ \db -> do
    uclId_ <- getUserContactLinkIdByCReq db connReqId   -- NOT user-scoped
    forM uclId_ $ \uclId -> do
      uclGLinkInfo <- getUserContactLinkById db userId uclId  -- user-scoped -> throws
```

`getUserContactLinkIdByCReq` (`Store/Direct.hs`) has no `user_id` filter, so it returns the address
id of the *other* profile; `getUserContactLinkById` (`Store/Profiles.hs`) then filters on
`user_id = ?` and throws `SEUserContactLinkNotFound`. Every chat-preview/chat-item query is
unaffected — only the accept fails.

The client never compensated: `NtfManager.acceptContactRequestAction` computed `isCurrentUser`
only to decide whether to update the chat model, and called the API without switching profile —
unlike its neighbours `openChatAction` and `showChatsAction`, which both call `changeActiveUser`.
iOS is not affected: `processNotificationResponse` has switched the active user since
`06a0dbd0f` (2023).

The core-side scoping is itself a regression. `7dd4dc3b4` ("core: support accepting contact
requests for non active users (for accepting via notification)", #1809) deliberately made this
command use the request's own user via `getContactRequest'`. `7f6bc3089` (#5978, first released in
v6.4.0-beta.4) reverted it to `withUser $ \user@User {userId}` + user-scoped `getContactRequest`
as a side effect of unrelated short-link work, leaving `getUserByContactRequestId`
(`Store/Direct.hs`) as dead code. So the bug predates the v7 line.

## Fix

Client-side, in `NtfManager.acceptContactRequestAction`: switch to the profile the request was
sent to before calling the API, mirroring `openChatAction`/`showChatsAction` and iOS.

- `changeActiveUser` is called only when the target profile differs from the active one; the
  accept then runs in the right profile, and `isCurrentUser` — computed *after* the switch — is
  true, so the accepted contact is inserted into the chat list the user is now looking at instead
  of being silently dropped.
- The body moves into `withLongRunningApi` and gains `awaitChatStartedIfNeeded`, which the two
  sibling actions already had. This is required, not incidental: `APISetActiveUser` starts with
  `unlessM (lift chatStarted) $ throwChatError CEChatNotStarted`, so without the wait a tap during
  cold start would fail the switch, and the switch is the whole fix.
- `clearOverlays` is set when a switch happened, so a modal left open by the previous profile does
  not end up rendering the new profile's data. It is scoped to the switch branch on purpose: the
  siblings clear unconditionally because they *navigate*, which accepting does not.

## Alternative considered and rejected

Restoring the core behaviour — deriving the user from the request via the already-present
`getUserByContactRequestId` instead of `withUser` — was implemented, built, and covered by a test
(`accept contact request for non active user`, passing), then dropped. It fixes the API for all
callers (terminal `/_accept`, bots, the python/nodejs SDKs) and does not depend on a client-side
switch that swallows its own errors. It was rejected for this fix because the UI has to switch
profiles anyway for the result to be visible, so the core capability would never be exercised by
the app, and the client change alone resolves every path reachable from a notification. The core
API therefore remains active-user-scoped, and `getUserByContactRequestId` remains unused.

## Known gaps not addressed here

- `acceptContactRequestAction` passes `rhId = null` (its own long-standing TODO), so accepting from
  a notification always targets the local core. A request that arrived on a *remote host* produces
  a notification carrying a remote user id, and the new `changeActiveUser(null, userId, null)` will
  switch the **local** profile. `openChatAction` — the notification's default tap action — already
  has this flaw, so this extends an existing pattern rather than introducing one; the real fix is
  carrying the remote host id in the notification.
- On desktop the Accept action is not clickable: `NtfManager.desktop.kt` passes the action to
  two-slices, whose Linux backend does not render action buttons, and it passes
  `NotificationAction.ACCEPT_CONTACT_REQUEST.name` as the label instead of
  `generalGetString(MR.strings.accept)`. The bug is therefore Android-only in practice.
- Accepting from a notification still does not open the new contact's chat, whereas iOS dismisses
  sheets and calls `loadOpenChat` from inside `acceptContactRequest` when `contact.sndReady`. The
  Kotlin equivalent is the existing `close` callback of `acceptContactRequest`, which the
  notification path passes as `null`.
- `APIRejectContact` is also active-user-scoped and fails the same way. It is left alone: unlike
  accept, it never supported non-active users (#1809 changed only accept), and the notification has
  no Reject action.

## Testing

- `:common:compileKotlinDesktop` and `:android:assembleDebug` build clean.
- Manual, Android: profile 1 active, request lands on inactive profile 2, tap Accept in the
  notification — the app switches to profile 2, the contact appears in the list, no error.
- No automated coverage: the changed path is reachable only from a notification action.
