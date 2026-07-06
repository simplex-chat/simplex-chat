# Fix: SimpleX directory re-approves a channel every 30 min after a blockchain name is added

Status: proposed — **fix gated on a diagnostic step (see §5); root-cause delta not yet observed**
Branch: `nd/fix-directory-names`
Base: `origin/master` (`b38015c7b`)

## 1. Problem

After an owner adds a SimpleX name (blockchain domain claim) to a public
channel, the SimpleX Directory service asks the owner to re-approve the channel
**every 30 minutes, indefinitely**, with no changes on the owner's side. The
channel keeps flipping to "hidden until approved."

## 2. What is confirmed (facts, file:line)

1. **The 30-min cadence is the directory's link-check timer.** `linkCheckInterval`
   defaults to **1800 s** (`apps/simplex-directory-service/src/Directory/Options.hs:181`);
   `linkCheckThread_` sleeps that long and enqueues a `DEGroupLinkCheck` for every
   non-removed registered group (`.../Directory/Service.hs:199-211`).

2. **`DEGroupLinkCheck` re-approves iff `groupUpdated` is true.** `deGroupLinkCheck`
   runs `APIConnectPlan` against the channel link and, on a `GLPKnown` plan, does
   `when groupUpdated $ reapprove …` (`Service.hs:815-854`, trigger at `:824`),
   which sets `GRSPendingApproval` and re-sends for admin approval.

3. **`groupUpdated` is a whole-profile byte inequality.** It is the `Bool` returned
   by `updateGroupFromLinkData`: `profileChanged = p /= groupProfile`
   (`src/Simplex/Chat/Library/Internal.hs:1466-1482`), where `p` is the directory's
   **stored** `GroupProfile` and `groupProfile` is the profile in the **fetched
   link data**.

4. **`groupUpdated` is behaviorally directory-only.** The mobile/desktop clients do
   not even deserialize it: the client `GroupLinkPlan.Known` variant carries only
   `groupInfo` — `class Known(val groupInfo: GroupInfo)`
   (`apps/multiplatform/.../model/SimpleXAPI.kt:7158`; iOS
   `apps/ios/Shared/Model/AppAPITypes.swift`, `enum GroupLinkPlan`). The only
   consumers of the flag are in the directory service: re-approval (`Service.hs:824`),
   listing refresh (`:825`), and owner re-registration (`:988` →
   `deReregistration:1043,1046`). (All other `groupUpdated` matches in the tree are
   an unrelated chat event, `RcvGroupEvent`/`CR.GroupUpdated`.)

## 3. What is NOT yet established

**Why the two profiles fail to converge every cycle is unproven.** An ordinary
profile change self-corrects: when `profileChanged` is true,
`updateGroupFromLinkData` writes the link profile to the DB (`Internal.hs:1471`),
and the store round-trips the domain claim + proof faithfully
(`group_domain_proof` column; symmetric `publicGroupAccessRow` /
`toPublicGroupAccess`, `src/Simplex/Chat/Store/Shared.hs:721`/`:727`). So a
one-time change re-approves **once** and then converges. A perpetual 30-min
re-approval requires the directory's stored profile to be **permanently unequal**
to the fetched link profile — and which field differs has **not been observed**.

Two hypotheses were considered and both are insufficient as stated:

- **Relay drops the domain claim from the served link data**
  (`src/Simplex/Chat/Library/Commands.hs:4326-4328` handles this case). But this
  **converges after one cycle**: the directory stores the claim-less link profile,
  and with no further owner change nothing restores the claim, so the next check
  matches. It explains a single reapproval, not a repeat.
- **A re-presented proof with a fresh nonce.** This does **not** apply to domain
  claims. `PHTest`/`badgeProof` (fresh nonce per presentation) is the separate
  *badge* feature, attached to `Profile.badge` (`Internal.hs:2085-2086`), not to
  the domain claim. A `SimplexDomainProof`'s `presHeader` is **inside the signed
  payload** (`Commands.hs:4869`, verified at `:4867`), so it cannot be re-presented
  with a new nonce without re-signing by the owner — it is static. The group claim
  is also frequently created with `proof = Nothing` (`mkDomainClaim`,
  `src/Simplex/Chat/Names.hs:56`; used at `Commands.hs:5693`, `:1510`).

So the correlation ("started right after adding a name") strongly implicates the
domain claim as the differing field, but the **exact field and the reason it
never converges are not yet demonstrated.** This plan therefore treats the fix as
gated on a diagnostic.

## 4. Likely mechanism (hypothesis, to be confirmed)

The differing field is the group profile's `publicGroup.publicGroupAccess.groupDomainClaim`
(or its `proof`): the directory's stored copy and the fetched link copy carry
different claim representations on every fetch, in a way that storing the link
copy does not reconcile. Because the domain claim is verified **independently** —
cryptographically + on-chain by `verifyEntityDomain` (`Commands.hs:4840`), with
verification tracked in a *separate* field `group_domain_verified`
(`Store/Groups.hs:2710-2713`) — it is not admin-moderated content and should never
gate re-approval. The bug is that `groupUpdated` conflates "profile changed at
all" (used to decide whether to *store* the fresh claim) with "profile changed in
a way needing admin *re-moderation*" (used to decide re-approval).

## 5. Required diagnostic (gate before the fix)

Add a temporary log at the one comparison that flips everything,
`Internal.hs:1482`, dumping the two profiles' domain claims when `profileChanged`
fires, and run it against the affected channel across ≥2 link-check cycles:

```haskell
-- temporary:
when (p /= groupProfile) $ logInfo $ "linkcheck delta: stored=" <> tshow (claimOf p)
                                   <> " link=" <> tshow (claimOf groupProfile)
```

Decision:
- **Delta is the domain claim, identical each cycle** → structural asymmetry;
  apply §6 (proof redaction covers it if the delta is the proof; if the delta is
  the whole claim, widen the redaction to the claim — decide from the log).
- **Delta changes each cycle** → identify the varying field from the log; if it is
  not the claim, this plan's fix does not apply and the diagnosis restarts here.

The fix in §6 is only justified once the log shows the per-cycle delta is confined
to the domain claim/proof.

## 6. The fix (conditional on §5)

In `updateGroupFromLinkData` (`Internal.hs`), keep storing on the full
`profileChanged` (so the claim/proof and verification stay current), but report
`groupUpdated` with the domain-claim **proof** redacted — reusing the codebase's
existing proof-redaction idiom (`Types.hs:750` `clearProofs`; `Internal.hs:1265`
`redactedDomain`), rather than new helpers:

```haskell
        pure (g'', moderationChanged)      -- was: profileChanged
  ...
  where
    profileChanged = p /= groupProfile     -- keep: still gates storing the fresh claim
    -- A domain-claim change is verified independently (on-chain + owner signature)
    -- and is not admin-moderated, so it must not trigger directory re-approval.
    -- Redact the proof before comparing (same idiom as Types.clearProofs).
    moderationChanged = redactProof p /= redactProof groupProfile
    redactProof gp@GroupProfile {publicGroup} = gp {publicGroup = redactPg <$> publicGroup}
    redactPg pg@PublicGroupProfile {publicGroupAccess} = pg {publicGroupAccess = redactAccess <$> publicGroupAccess}
    redactAccess a@PublicGroupAccess {groupDomainClaim} =
      a {groupDomainClaim = (\d -> d {proof = Nothing} :: SimplexDomainClaim) <$> groupDomainClaim}
```

Redacting **only the proof** (not the whole claim) is the surgical choice: it kills
any proof-representation delta while still letting a genuine domain add/change
register as a one-time change (which then converges). If §5 shows the delta is the
whole claim (e.g. claim present vs absent every cycle), replace `redactAccess` with
`groupDomainClaim = Nothing`; note this also suppresses the legitimate one-time
reapproval on a name add.

Storage keys off the full `profileChanged`, so the claim/proof and
`setGroupDomainVerified` still update — by-name lookups and the verified badge stay
correct.

## 7. Why this is safe (given §5 confirms the delta)

- **Directory-only blast radius** (fact, §2.4). All three consumers behave
  correctly with the proof/claim excluded:
  - `Service.hs:824` periodic re-approval → **fixed**.
  - `Service.hs:988` → `deReregistration` (`:1043`/`:1046`) — an owner re-submitting a
    name-only change gets "already listed" instead of forced re-approval.
  - `Service.hs:825` `listingsUpdated` — minor trade-off: a name-only change no longer
    *immediately* refreshes web-listing files (still refreshes on any real change or
    member-count change). If prompt refresh matters, keep `:825` on the raw
    `profileChanged` while `:824` uses `moderationChanged`.
- **Moderation not weakened.** Re-approval still fires on any moderatable field
  (name, description, image); only proof/claim deltas are excluded, and those are
  independently verified.

## 8. Residual behaviour (accepted)

If the claim/proof genuinely differs each cycle, the store branch re-writes the
`group_profiles` row every 30 min. This is idempotent and does not thrash
verification (`claimChanged` in `updateGroupProfile` compares the *domain*, not the
proof — `Groups.hs:2708-2709`). A follow-up could skip the store on proof-only
deltas; out of scope.

## 9. Verification plan

1. Land the §5 diagnostic, capture the delta on the affected channel, confirm it is
   the domain claim/proof, and choose proof-vs-whole-claim redaction from the log.
2. Apply §6.
3. **Regression test** in `tests/Bots/DirectoryTests.hs`: register a channel →
   approve → add a SimpleX name → trigger ≥2 link checks → assert the registration
   stays `GRSActive` (no `GRSPendingApproval`, no owner re-approval message).
4. Build the directory service; run the directory test suite.

## 10. Files touched

- `src/Simplex/Chat/Library/Internal.hs` — `updateGroupFromLinkData`: return a
  claim/proof-insensitive change flag (store logic unchanged). Temporary diagnostic
  log removed before merge.
- `tests/Bots/DirectoryTests.hs` — add the name-added-no-reapproval regression test.

No schema, wire-format, or API changes. `GLPKnown.groupUpdated`'s type is unchanged;
only its value semantics (directory-only consumer) are refined.
