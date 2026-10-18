# Fix: directory bot admits members past admission review (`APIAcceptMember` intent conflation)

Status: root cause inferred from the code (no logs from the reported incident); a
concrete trigger is described in §2. The fix is a new `screeningApproval` flag on
`APIAcceptMember`; by reading the code, the regression test `testScreeningApprovalKeepsReview` fails
without the guard and passes with it (the Haskell tests run in CI).

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

## 2. Root cause

`APIAcceptMember` (`Commands.hs`) performs **two different operations**, chosen
only by the member's **current** status:

- `GSMemPendingApproval` (+ `GCInviteeMember`): apply the group's admission policy
  — `review=all` ⇒ `introduceToModerators` + set `GSMemPendingReview` (advance to
  admin review); no review ⇒ full admit.
- `GSMemPendingReview`: for any moderator, notify and set `GSMemConnected` (or
  `GSMemAnnounced` before the connection is ready); for the host
  (`GCInviteeMember`), additionally `introduceToRemaining` — **full admission** (introduced to everyone → can send). This
  is the explicit *moderator-approves-past-review* action.

Authorization is only `assertUserGroupRole gInfo (roleRequiredToChange GRMember role)`.
The **directory bot is a group admin**, so it is allowed to hit *either* branch. The
bot's `approvePendingMember` (`Directory/Service.hs`) calls the **same**
`APIAcceptMember` that human moderators use. The core re-reads the member fresh by
id, so if the member is **already `GSMemPendingReview`** at the instant the bot's
approval executes (a prior approval by the bot itself, from a queued event, already
advanced them — only the bot, as the member's host, can approve a pending-approval
member), the bot's approval silently runs the **full-admit branch**. This needs a
path where captcha is not required: a captcha answer cannot reach a pending-review
member, because the first approval removes the member's pending captcha.

A concrete trigger: the bot handles its events sequentially, and each event carries
the member status from when it was emitted (`Directory/Events.hs`: a message from a
member counts as "pending" when its status was `GSMemPendingApproval`). A member is
only held at `GSMemPendingApproval` when captcha is required at join; otherwise, in a review=all group, they
go straight to `GSMemPendingReview`. If captcha then stops being required for them
(the owner turned captcha off, or the "no image" filter and the member added an
image) and two of their pending events (join or message) are queued before the bot
handles the first, both events call `approvePendingMember`. The first moves
the member to `GSMemPendingReview`; the second finds them pending review and admits
them. With captcha required, the second event only re-sends the captcha.

This is consistent with the reports if the captcha requirement changed while members
were pending: no wipe (setting stays `all`), p2p, one owner, intermittent, the
**bot** is the actor, self-healing per member.

Why it may have "stopped recently": upstream **#7180** added the bot options
`--knocking`, which accepts joiners straight into `GSMemPendingReview` at join
(`acceptMemberHook`), skipping the `GSMemPendingApproval` stage, and
`--always-captcha`, which makes every stale queued message re-send a captcha
instead of approving — with either, the bot never issues the approval that could
land on the full-admit branch (whether the production bot uses them is not known
here). They would mask the defect, not fix it.

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
off) — the same optional `key=on|off` parser form as `withMessages` on `APIRemoveMembers`.

Handler change (`Commands.hs`), one guarded alternative:
```haskell
GSMemPendingReview | screeningApproval -> pure $ CRMemberAccepted user gInfo m
GSMemPendingReview -> do ...  -- unchanged full-admit for human moderators
```

This makes the bot **structurally incapable** of the bypass, independent of the
exact race that leaves a member at `GSMemPendingReview` before the bot's call, and
leaves human moderator approval untouched.

Considered: a bot-only fix — re-read the member in `approvePendingMember` and skip
unless it is still `GSMemPendingApproval`. It closes the same hole without a core
API change, but leaves any other screening bot exposed and relies on the bot never
acting on a stale status; the core flag makes the rule hold whatever the caller does.

## 4. Verification

`tests/ChatTests/Groups.hs` — `testScreeningApprovalKeepsReview`: a `review=all`
group, a joiner held pending review, then `/_accept member … screening=on`. The
member stays pending review: cannot send (`bad chat command: not current member`)
and is not introduced to the regular member (`bob`). Then `/_accept member …`
(screening off) admits the member normally, so the moderator path is unchanged.
With the guard removed, the screening approval admits the member (by reading the code).

`tests/Bots/DirectoryTests.hs` — `testCaptchaWithReview`: the bot's normal captcha
approval in a `review=all` group still moves the member to pending review (by
reading the code it passes on master too; it guards the bot's single-approval path).

The Bot API docs hide `screeningApproval` (as `APIJoinGroup` hides `enableNtfs`), so
the generated `COMMANDS.md`, TypeScript and Python command types are unchanged.

## 5. Files touched

- `src/Simplex/Chat/Controller.hs` — `screeningApproval :: Bool` on `APIAcceptMember`.
- `src/Simplex/Chat/Library/Commands.hs` — handler guard on the pending-review
  branch; `AcceptMember` shorthand passes `False`; parser adds optional
  `screening=on` (default `False`).
- `apps/simplex-directory-service/src/Directory/Service.hs` — bot passes `True`.
- `bots/src/API/Docs/Commands.hs` — `screeningApproval` hidden from the Bot API docs.
- `tests/ChatTests/Groups.hs`, `tests/Bots/DirectoryTests.hs` — the two tests above.

## 6. Investigation history — theories raised and ruled out

Recorded so the reasoning isn't repeated. Each was proposed under partial evidence
and eliminated by a later fact:

- **Command-path lost-update race** (concurrent profile edits clobber
  `member_admission`). Real race; fix = `withGroupLock` on `APIAcceptMember` and
  the profile-update commands. **Not this bug** — it would wipe the setting
  permanently; the setting stayed correct. Split out of this PR (it also holds the
  group lock across network calls, which needs its own review).
- **Stale link-data overwrite** (`updateGroupFromLinkData` copying a stale link
  snapshot over `member_admission`). Real for a *member* resolving another's stale
  link; **ruled out for the bot in p2p groups** — it resolves p2p group links
  only with `PRMNever` (its own link gives `GLPOwnLink`), so
  `updateGroupFromLinkData` is never called. That fix was dropped.
- **Old-client owner clobber** (an owner on a pre-`groupKnockingVersion` client
  broadcasting profiles without the field). Real gap; **ruled out for this incident** — one current owner.
  Not addressed here.
- **Propagation lag / enable-time window.** Inherent to async messaging; one-time,
  not recurring — doesn't match the intermittent pattern.

## 7. Related, out of scope (separate work)

- **Third-party screening bots.** `screeningApproval` is hidden from the Bot API
  docs, so the typed Node.js and Python SDK command helpers do not expose it; a bot built on them
  that approves from stale events has the same bypass. Exposing the flag is a
  maintainer decision.

- **Concurrent same-version owner edits** to the whole-profile blob remain
  last-writer-wins (protocol-level field versioning, tracked separately).
