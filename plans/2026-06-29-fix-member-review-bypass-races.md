# Fix: member admission `review=all` bypassed under concurrency

Status: **implemented + verified.** Fixes applied to
`src/Simplex/Chat/Library/Commands.hs`; builds clean. Tests green: directory
member-admission 8/8 (incl. regression `testCaptchaWithReview`), core member
screening 6/6 (exercises the locked `updateGroupProfileByName` via
`/set admission review` and the `APIAcceptMember` review branches under the new
lock), profile-update + public-group sweep 10/10 (`APIUpdateGroupProfile`,
`SetPublicGroupAccess`). The defect is purely concurrency — the sequential path
was already correct.

## 1. Problem

A group has member admission `review = all` (every joiner must be reviewed by
admins / "knocking"), yet some members join fully (visible to all, can send)
without review. Observed on live `directory.simplex.chat`: the SimpleX Directory
bot admitted members it should have left for admin review.

## 2. Grounded diagnosis (facts, file:line)

**2a. The sequential logic is correct.** `acceptanceToStatus` (Types.hs:1377)
forces `GSMemPendingReview` for a `GAAccepted` join when `review == Just MCAll`;
`APIAcceptMember` (Commands.hs:2692) converts the bot's `GSMemPendingApproval`
approval to `GSMemPendingReview`. The regression test confirms a captcha-screened
joiner into a `review=all` group is held. So the bypass is a race.

**2b. Concurrency model.** One inbound thread `agentSubscriber`
(Commands.hs:4816) drains the agent queue **sequentially**, each message under
`withEntityLock` (Subscriber.hs:132). Commands run on **caller** threads
(`sendChatCmd` → `execChatCommand'`, Core.hs:89), concurrent with the inbound
thread and each other. All cross-thread safety therefore depends on each command
taking the correct per-entity lock (`withGroupLock` → `CLGroup`,
Internal.hs:146; lock map is per-entity, non-reentrant, Internal.hs:127-131).
Lock entity by connection: group-link join → `CLUserContact`, group message →
`CLGroup` (Connections.hs:65-68).

**2c. The review decision reads `memberAdmission`; the only writer is a group
profile update.** Decision points: `acceptanceToStatus` in
`acceptGroupJoinRequestAsync` (Internal.hs:956) and `APIAcceptMember`
(Commands.hs:2689). Writer: `updateGroupProfile` (Groups.hs:2646 writes
`member_admission`) via the profile-update commands.

**2d. Two in-scope race conditions — missing `withGroupLock`:**

- **Race B — `APIAcceptMember` (Commands.hs:2684) is the only group-member
  command without `withGroupLock`.** Every sibling locks (`APIAddMember` 2621,
  `APIMembersRole` 2755, `APIRemoveMembers` 2886, `joinGroup` 2652). The bot's
  captcha approval reads `memberAdmission` + member status (2685) and mutates
  member state, unserialized against the group-locked inbound thread.

- **Race A — group-profile updates take no `withGroupLock`.**
  `updateGroupProfileByName` (Commands.hs:3943-3947) is a read-modify-write
  (`getGroupInfo` → `update p` → `runUpdateGroupProfile`); `APIUpdateGroupProfile`
  (3086) and `runUpdateGroupProfile` (3862) likewise unlocked.
  `SetGroupMemberAdmissionReview` (3387) goes through `updateGroupProfileByName`.
  Inbound `XGrpInfo` *does* hold the group lock, so a command-path update races
  it (and other command-path updates). Classic lost update: a writer that read a
  pre-`review` snapshot writes the full profile back, **clobbering
  `member_admission` to NULL**. Review is then silently off and *every* later
  join bypasses it until re-set — this matches an ongoing live bypass.

## 3. The fix (minimal, idiomatic — add the missing locks)

The codebase already serializes all group state via the group lock; the bug is
that these entry points skip it. Restore parity. **This is a core-library fix —
it ships to all clients (owner apps + the bot), which matters for Fix 2 below.**

**Fix 2 (PRIMARY — fixes the persistent bypass) — make every group-profile
update an atomic, group-locked read-modify-write** (closes Race A's lost update).
The persistent NULL originates on whichever client edits the profile (typically
the **owner**: enabling `review=all` concurrently with another profile edit such
as welcome/name → two unlocked whole-profile writes interleave → the later write,
derived from a pre-review snapshot, drops `member_admission` → that no-review
profile broadcasts to everyone *including the bot*). The bot never issues these
commands, so the fix lands on the editing client and the bot inherits the correct
profile.

There are **three** unlocked `runUpdateGroupProfile` callers. Wrap each one's
read-modify-write in `withGroupLock`, inline, the way every other group command
already does (`APICreateGroupLink`, `APIMembersRole`, …) — **no shared helper, no
change to each command's existing read connection** (surgical):
- `updateGroupProfileByName` (covers `SetGroupMemberAdmissionReview`,
  `UpdateGroupNames`, `UpdateGroupDescription`, `SetGroupTimedMessages`, …):
  resolve the id, then lock and read-modify-write inside it (read stays `withStore`).
  ```haskell
  updateGroupProfileByName gName update = withUser $ \user -> do
    gId <- withStore $ \db -> getGroupIdByName db user gName
    withGroupLock "updateGroupProfile" gId $ do
      gInfo@GroupInfo {groupProfile = p} <- withStore $ \db -> getGroupInfo db cxt user gId
      runUpdateGroupProfile user gInfo $ update p
  ```
- `APIUpdateGroupProfile` — wrap the existing body in `withGroupLock`; read stays
  `withFastStore`, `runUpdateGroupProfile user gInfo p'` unchanged.
- `SetPublicGroupAccess` (**was missing from the first draft; found in adversarial
  review**) — resolve the id, then lock; the `Just pg`/`Nothing` branch and the
  "not a public group" throw are unchanged (read stays `withStore`).

The only structural change beyond adding the lock is splitting the combined
`getGroupIdByName >>= getGroupInfo` of the two name-based commands into
id-first / locked-read — intrinsic to locking by group id.

**Fix 1 (closes the bot's approval window + restores lock parity) — lock
`APIAcceptMember`.** Wrap the body in `withGroupLock "acceptMember" groupId`
(mirrors `APIMembersRole` etc.). Safe: top-level command (never nested inside a
group lock), and its callees (`introduceToModerators`, `introduceToRemaining`,
`sendGroupMessage`) take no group lock.
```haskell
APIAcceptMember groupId gmId role -> withUser $ \user@User {userId} ->
  withGroupLock "acceptMember" groupId $ do
    (gInfo, m) <- withFastStore $ \db -> (,) <$> getGroupInfo ... <*> getGroupMemberById ...
    ...
```

Both fixes are pure additions of the existing `withGroupLock` wrapper; no logic,
schema, or protocol change.

## 4. Why this is sufficient (and minimal) — both needed, neither redundant

- **Fix 2 necessary interleaving:** owner `SetGroupMemberAdmissionReview` (reads
  P0) ∥ `UpdateGroupDescription` (reads P0) → both write the whole profile → the
  description write (derived from P0, no review) lands last → review lost →
  propagates to the bot → *persistent* bypass. Fix 1 cannot help once
  `member_admission` is NULL (the locked read still reads NULL).
- **Fix 1 necessary interleaving:** the bot's `APIAcceptMember` reads `gInfo`
  (2685) at the instant inbound `XGrpInfo(review=all)` is mid-apply (group lock)
  → unlocked reads `Nothing` → admits; locked → serialized → reads `review=all` →
  holds. Fix 2 alone leaves this bot-local window open.

With review kept stable (Fix 2) and the approval serialized (Fix 1), there is no
remaining window in the admit path.

## 5. Deliberately excluded (analyzed, out of scope)

- **Race C — join-time decision under the user-contact lock** (not group lock):
  a member joining in the sub-second window while review is *first* enabled
  adopts the prior policy. This is benign (they genuinely joined before the
  policy took effect) and not persistent once Fix 2 keeps review stable. For the
  directory bot it cannot apply: captcha joins are `GAPendingApproval`, so the
  review decision is entirely at `APIAcceptMember` (Fix 1). Closing it would
  require taking the group lock while holding the user-contact lock — a
  lock-ordering change with deadlock risk, not justified by a benign window.
- **Distributed / old-client clobber:** an `XGrpInfo` carrying a profile without
  `member_admission` (an old client, or a client whose own read-modify-write
  raced) overwrites stored review. `Nothing` is indistinguishable from "review
  turned off" (only `MCAll`/absent exist), so the receiver cannot defensively
  preserve it without a protocol change. Out of scope; track separately.
  **Long-term hardening direction:** carry `member_admission` out of the
  broadcast whole-`GroupProfile` last-writer-wins (its own
  versioned/field-level update), so no profile edit — local or remote, current
  or old — can clobber it. This is the genuinely more robust design but is a
  protocol change, hence not part of this minimal fix.

## 5b. Adversarial review outcome

An independent adversarial pass (full code context) tried to break this plan and
find a better fix. Verdict: **the lock-based fix is the best *minimal* approach —
no alternative is strictly better at minimal scope.** It produced two corrections,
both folded in above: (1) re-framed Fix 2 as the persistent-bug fix that must ship
to owner clients (Fix 1 is window + parity); (2) found the missed third caller
`SetPublicGroupAccess`, now also group-locked. A later surgical pass removed an
over-abstraction (a shared `updateGroupProfileById` helper) and an incidental
`withFastStore`→`withStore` change it introduced — each command now just gets an
inline `withGroupLock` with its original read connection preserved. Alternatives
were evaluated and rejected for minimal scope:
- Re-check review at `introduceToAll/Remaining` under the lock — reads the *same*
  possibly-NULL `member_admission`; doesn't help the clobber; more invasive.
- Enforce at the CON handler (`Subscriber.hs:931-962`) — captcha joiners are
  `GSMemPendingApproval` at CON (`pure ()`); the decision is later at
  `APIAcceptMember`, so CON enforcement misses it.
- Fail-safe "hold when uncertain" — not viable; `Nothing` is indistinguishable
  from legitimately-off.
- Separate `member_admission` from the broadcast profile — more robust against the
  out-of-scope distributed clobber, but a protocol change (see hardening note).

Confirmed by the review: deadlock-safe (all `runUpdateGroupProfile` callers are
command paths; inbound `xGrpInfo` uses the store fn `updateGroupProfile db`
directly under its own group lock, `Subscriber.hs:3644`, never the command
helpers); `APIAcceptMember` is the only unlocked introduction path; both fixes are
necessary and neither subsumes the other.

## 6. Verification (done)

- Builds clean. Green: directory member-admission 8/8 (incl.
  `testCaptchaWithReview`), core "member screening" 6/6, profile-update +
  public-group sweep 10/10. The "member screening" block runs `/set admission
  review` on a group **with a short link**, so it exercises the locked
  `updateGroupProfileByName` → `runUpdateGroupProfile` → `setGroupLinkData'` path
  under the new lock with no deadlock or sequential-behavior change.
- **Gap (acknowledged):** the lost-update fix (Fix 2's core value) is verified by
  lock-parity reasoning, not by a test — a deterministic concurrency test is
  flaky. The sequential regression test only proves the held-behavior baseline.
- **Behavioral note:** `runUpdateGroupProfile` now holds the group lock across the
  synchronous `setGroupLinkData' → setConnShortLink` network round-trip, so a
  profile edit briefly stalls that group's inbound processing on the same client.
  This is the same pattern `APICreateGroupLink` (Commands.hs:3103) already uses
  (`withAgent createConnection` under `withGroupLock`), so it's not a new class of
  risk; profile edits are infrequent. Optional future mitigation (out of scope):
  switch to the async `setGroupLinkDataAsync` inside the locked section.

## 8. Adjacent issue (flagged, not fixed — out of scope)

`updateRelayGroupKeys` (Groups.hs:2222) and the prepared-host write (Groups.hs:721)
do partial `UPDATE group_profiles` of *other* columns (group_type / group_link /
public_group_id, and user_id / local_display_name) **without** the group lock.
They do not touch `member_admission`, so they don't affect review, but under
concurrency with the now-locked full-profile write they could lose those columns.
Pre-existing and unrelated to this fix; track separately if it matters.

## 7. Files touched

- `src/Simplex/Chat/Library/Commands.hs` — add an inline `withGroupLock` (Fix 2
  group lock + Fix 1) to four commands, each keeping its original read connection:
  - `APIAcceptMember` — `withGroupLock "acceptMember"` (Fix 1).
  - `updateGroupProfileByName` — id-then-locked read-modify-write (covers
    `UpdateGroupNames`/`UpdateGroupDescription`/`SetGroupMemberAdmissionReview`/
    `SetGroupTimedMessages`).
  - `APIUpdateGroupProfile` — wrap existing body (`withFastStore` read preserved).
  - `SetPublicGroupAccess` — id-then-locked read; branch/throw unchanged.
- `tests/Bots/DirectoryTests.hs` — regression test (already added).
