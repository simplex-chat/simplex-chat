# Fix: directory bot admits members past admission review (`APIAcceptMember` intent conflation)

Status: **root cause confirmed and fix validated.** The fix is a new
`screeningApproval` flag on `APIAcceptMember`; the regression test
`testScreeningApprovalKeepsReview` reproduces the bug (fails without the fix) and
passes with it.

## 1. Problem (as reported)

A plain **p2p** group has member admission `review = all`. A member joining via
the **SimpleX Directory** bot's invite link was **fully admitted without admin
review** — visible to everyone and able to send messages. It happened
**intermittently** (one member bypassed, the next were correctly held, then
another bypassed), **self-healed**, and stopped recently. Throughout, the group's
stored `member_admission` was **correctly `review=all`**, and the group had as few
as **one owner** on a current client.

These facts jointly rule out any "`member_admission` was wiped" explanation (a
wipe would be permanent and affect everyone, not intermittent with the setting
staying correct). The setting was right; it simply wasn't *enforced* for some
approvals.

## 2. Root cause (confirmed, file:line)

`APIAcceptMember` (`Commands.hs`) performs **two different operations**, chosen
only by the member's **current** status:

- `GSMemPendingApproval` (+ `GCInviteeMember`): apply the group's admission policy
  — `review=all` ⇒ `introduceToModerators` + set `GSMemPendingReview` (advance to
  admin review); no review ⇒ full admit.
- `GSMemPendingReview` (+ `GCInviteeMember`): `introduceToRemaining` + set
  `GSMemConnected` — **full admission** (introduced to everyone → can send). This
  is the explicit *moderator-approves-past-review* action.

Authorization is only `assertUserGroupRole gInfo (max GRModerator role)`. The
**directory bot is a group admin**, so it is allowed to hit *either* branch. The
bot's `approvePendingMember` (`Directory/Service.hs`) calls the **same**
`APIAcceptMember` that human moderators use. The core re-reads the member fresh by
id, so if the member is **already `GSMemPendingReview`** at the instant the bot's
approval executes (e.g. a prior approval — the bot's own from a duplicate/queued
event, or a human owner's — already advanced them), the bot's *captcha* approval
silently runs the **full-admit branch**.

This matches every reported fact: no wipe (setting stays `all`), p2p, one owner,
intermittent by timing, the **bot** is the actor, self-healing per member.

Why it "stopped recently": upstream **#7180** added `--knocking`, which routes
joiners straight to `GSMemPendingReview` at join (`acceptMemberHook`), skipping the
`GSMemPendingApproval` stage — so the bot never issues the captcha approval that
could land on the full-admit branch. #7180 **masks** the defect operationally; it
does not fix the core conflation.

## 3. The fix — intent separation

Add `screeningApproval :: Bool` to `APIAcceptMember`:

- **Bot passes `True`** (`Directory/Service.hs`). A screening approval may advance
  a `GSMemPendingApproval` member per policy, but on a `GSMemPendingReview` member
  it is a **no-op** — returns the member unchanged (still pending review). The bot
  can therefore **never** admit past review.
- **Humans pass `False`** (CLI `/_accept member`, the `AcceptMember` shorthand,
  the mobile/desktop apps). Full behaviour unchanged — a moderator can still admit
  a pending-review member.

Backward compatible: apps send `/_accept member #g m role` as a **string**
(`AppAPITypes.swift`), and the parser flag is optional (`screening=on`, default
off) — mirrors the existing `withMessages` pattern on `APIRemoveMembers`.

Handler change (`Commands.hs`), one guarded alternative:
```haskell
GSMemPendingReview | screeningApproval -> pure $ CRMemberAccepted user gInfo m
GSMemPendingReview -> do ...  -- unchanged full-admit for human moderators
```

This makes the bot **structurally incapable** of the bypass, independent of the
exact race that leaves a member at `GSMemPendingReview` before the bot's call, and
leaves human moderator approval untouched.

## 4. Verification (done)

`tests/ChatTests/Groups.hs` — `testScreeningApprovalKeepsReview`: a `review=all`
group, a joiner held pending review, then `/_accept member … screening=on`.
- **Fixed:** the member stays pending review — cannot send (`bad chat command:
  not current member`) and is not introduced to the regular member (`bob`). `[✔]`
- **Reverted (guard removed):** the member is fully admitted — `bob` receives
  `alice added eve to the group`, and the accept prints `eve accepted` instead of
  `pending review`. `[✘]` — i.e. the test reproduces the reported bug.

Then `/_accept member … ` (screening off) admits the member normally, confirming
the human path is unchanged. Existing review/accept flows
(`only moderators review`, `host approval then moderators review`, `introduces
member for moderator review`, `should hold captcha-passing member`) stay green.

## 5. Files touched

- `src/Simplex/Chat/Controller.hs` — `screeningApproval :: Bool` on `APIAcceptMember`.
- `src/Simplex/Chat/Library/Commands.hs` — handler guard on the pending-review
  branch; `AcceptMember` shorthand passes `False`; parser adds optional
  `screening=on` (default `False`).
- `apps/simplex-directory-service/src/Directory/Service.hs` — bot passes `True`.
- `tests/ChatTests/Groups.hs` — `testScreeningApprovalKeepsReview`.

## 6. Investigation history — theories raised and ruled out

Recorded so the reasoning isn't repeated. Each was proposed under partial evidence
and eliminated by a later fact:

- **Command-path lost-update race** (concurrent profile edits clobber
  `member_admission`). Real race; fix = `withGroupLock` on `APIAcceptMember` and
  the profile-update commands (proven by `testConcurrentProfileUpdateKeepsReview`
  via a test-only `groupProfileUpdateTestHook` seam). **Not this bug** — it would
  wipe the setting permanently; the setting stayed correct. Kept as hardening.
- **Stale link-data overwrite** (`updateGroupFromLinkData` copying a stale link
  snapshot over `member_admission`). Real for a *member* resolving another's stale
  link; **ruled out for the bot** — the bot resolving its own link short-circuits
  to `GLPOwnLink` and never calls `updateGroupFromLinkData`. Reverted.
- **Old-client owner clobber** (an owner on a pre-`groupKnockingVersion` client
  broadcasting profiles without the field). Real gap; guarded in `xGrpInfo`
  (`testOldOwnerCannotRevertReview`). **Ruled out for this incident** — one current
  owner. Kept/dropped per PR scope.
- **Propagation lag / enable-time window.** Inherent to async messaging; one-time,
  not recurring — doesn't match the intermittent pattern.

## 7. Related, out of scope (separate work)

- **TOB-35 (High): relay/channel pending-review subscribers receive history before
  approval.** Same admission-enforcement area, different defect: the `useRelays'`
  join branch (`Subscriber.hs`) runs `sendHistory` regardless of pending status.
  Non-relay p2p is not affected (the p2p branch has explicit pending cases).
  Separate security PR: guard `sendHistory` at the relay call site (and,
  defensively, inside `sendHistory`, after reordering the approval-time callers).
- **Concurrent same-version owner edits** to the whole-profile blob remain
  last-writer-wins (protocol-level field versioning, tracked separately).
