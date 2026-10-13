# Auto-accept Group Invitations — Plan

## Table of Contents
1. [Context](#1-context)
2. [Why This Belongs in the Core](#2-why-this-belongs-in-the-core)
3. [Design](#3-design)
4. [Decisions and Justification](#4-decisions-and-justification)
5. [Scope](#5-scope)
6. [Verification](#6-verification)

---

## 1. Context

**Problem**: Every group invitation requires the user to tap accept, even when they have
already decided they want to join groups from their contacts. Users in active communities
accumulate invitations that are pure friction — the decision was made when they added the
contact, not when the invitation arrived.

**Precedent**: Privacy & security already carries a per-profile "Contact requests from
groups / Auto-accept" toggle (`users.auto_accept_member_contacts`, added in
`M20250729_member_contact_requests`). This change adds the equivalent for group
invitations — per-profile flag, same command pair — and regroups both under a single
**Auto-accept** section, so the two rows name what is being accepted rather than repeating
"Auto-accept" as two adjacent section headers:

```
Auto-accept
  Contact requests in groups     [ ]
  Group invitations              [ ]
  These settings are for your current profile <name>.
```

---

## 2. Why This Belongs in the Core

The naive implementation is client-side: observe `CEvtReceivedGroupInvitation`, then call
`APIJoinGroup`. That is wrong here for three reasons.

1. **It does not work when the app is closed.** Invitations arrive through the notification
   extension and background message processing. A client-side rule cannot run there.
2. **`APIJoinGroup` blocks.** It takes `withGroupLock`, calls the agent's `joinConnection`
   synchronously, and rolls member status back to `GSMemInvited` via `catchAllErrors` on
   failure. Driving that from an event handler couples message processing to a network
   round trip.
3. **It would be reimplemented per client.** Android, desktop and iOS would each carry the
   decision, and they would drift.

The flag therefore lives on `users`, and the decision is made where the invitation is
received.

---

## 3. Design

`processGroupInvitation` already contained an async accept path, used when an invitation
matches a group link the user opened:

```
prepareAgentJoin -> createMemberConnectionAsync -> joinAgentConnectionAsync
```

The outcome is reported later against the `CFJoinConn` command id, so nothing blocks. This
change does not add a second mechanism — it turns the existing two-way branch into three,
and auto-accept takes the path that already exists:

| Condition | Behaviour |
|---|---|
| invitation matches an opened group link | join async, no chat item (unchanged) |
| profile auto-accepts, membership still `GSMemInvited` | join async, record accepted item |
| otherwise | create pending invitation item, notify (unchanged) |

The shared sequence is extracted as `joinGroupAsync`; the invitation item as
`createInvitationItem`, parameterised by `CIGroupInvitationStatus` rather than a boolean.

---

## 4. Decisions and Justification

**Per-profile, not global.** Matches the sibling toggle and the user's mental model: a
profile is an identity, and willingness to auto-join groups is a property of that identity.
An incognito or work profile should not inherit a personal profile's setting.

**A chat item is still recorded.** The group-link branch creates no item because the user
initiated the join. Auto-accept is not user-initiated, so a `CIRcvGroupInvitation` with
status `CIGISAccepted` is written to the chat with the inviting contact — a durable record
of who added the user to what. It is deliberately left counting as unread
(`ciRequiresAttention`), so an auto-join is noticed rather than silent.

**`hostContact` is reported only for group links.** Clients respond to `hostContact` on
`CEvtUserAcceptedGroupSent` by replacing the transient host connection view with the group
and removing that chat. That is right for a group link, where the contact is a placeholder
created to join. For a plain invitation the contact is a real one, and removing their chat
would be destructive — so auto-accept passes `Nothing`, matching `APIJoinGroup`.

**The join only runs while membership is `GSMemInvited`.** `createGroupInvitation` is
idempotent on `inv_queue_info`: a resent invitation returns the existing group rather than
failing. Without the guard, every resend would open another agent connection, which a
hostile or buggy host could drive indefinitely. A resend after joining is ignored.

**UI strings reuse legacy keys deliberately.** The section header, the contact-requests row
and the footer use `auto_accept_contact`, `settings_section_title_contact_requests_from_groups`
and `receipts_section_description` — key names that no longer describe where they are used.
This is intentional: those keys are already translated in 35, 20 and 28 of 41 locales
respectively, while any newly added key ships English everywhere until translators catch up,
which would have put an English header and footer around a translated row. Only
`group_invitations` is genuinely new, so the feature adds exactly one string. Renaming these
keys to match their new use would discard the existing translations. All 28 translations of
`receipts_section_description` were checked and none mention receipts, so the reuse is safe.

**Security note.** This converts a user-gated action into an automatic one: a contact can
cause the client to open group connections and fetch history without a prompt. It is
opt-in and per-profile, and channels cannot be used for it — `processGroupInvitation`
rejects `publicGroup` invitations outright. No rate cap is applied; the setting is
explicit.

**Known behaviour.** Clients drop events for non-active profiles (`active(user)` guard), so
a group auto-accepted on a background profile becomes visible when switching to that
profile. The join itself happens on arrival: `subscribeUsers` passes the agent the active
user's id rather than the user list, and the agent enumerates every user's servers from its
own store — the active id orders subscriptions, it does not filter them.

---

## 5. Scope

| Layer | Change |
|---|---|
| Schema | `users.auto_accept_group_invitations` (`M20260813`, SQLite + Postgres) |
| Core | `User` field; `APISetUserAutoAcceptGroupInvitations` / `SetUserAutoAcceptGroupInvitations`; third branch in `processGroupInvitation` |
| Bot API | documented command; regenerated TypeScript/Python bindings and markdown |
| Android/desktop | Privacy & security: two per-profile toggles regrouped under one **Auto-accept** section; the contact-requests row relabelled "Contact requests in groups" |
| iOS | same section in `PrivacySettings.swift` |

---

## 6. Verification

- Schema dump, `.lint`, strict tables, and **down-migration round-trip** pass; the down
  migration restores the original DDL byte-for-byte, so no skip-list entry is needed.
- JSON fixtures and all Bot API doc/codegen specs pass with no regenerated drift.
- `testGroupCheckMessages` and `testGroupLink` pass — the manual and group-link branches
  are behaviour-preserving through the refactor.
- Three new tests: auto-accept on the active profile, on a second profile, and on an
  inactive profile while another is active.

**Not verified**: iOS is not compiled (no toolchain available); the Postgres schema test is
gated behind `#if defined(dbPostgres)` and was not run.
