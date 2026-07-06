# Fix: SimpleX directory re-approves a channel every 30 min after a blockchain name is added

Status: proposed
Branch: `nd/fix-directory-names`
Base: `origin/master` (`b38015c7b`)

## 1. Problem

After an owner adds a SimpleX name (blockchain domain claim) to a public
channel, the SimpleX Directory service repeatedly asks the owner to re-approve
the channel **every 30 minutes, indefinitely**, with no changes on the owner's
side. The channel keeps flipping to "hidden until approved."

The 30-minute cadence is not a coincidence — it is the directory's own periodic
link-check interval.

## 2. Grounded diagnosis (facts, file:line)

1. **The 30-min repeat is the directory's link-check timer.**
   `linkCheckInterval` defaults to **1800 s** (`apps/simplex-directory-service/src/Directory/Options.hs:181`).
   `linkCheckThread_` sleeps that long and enqueues a `DEGroupLinkCheck` event for
   every non-removed registered group
   (`apps/simplex-directory-service/src/Directory/Service.hs:199-211`).

2. **`DEGroupLinkCheck` re-approves whenever `groupUpdated` is true.**
   `deGroupLinkCheck` runs `APIConnectPlan` against the channel's link and, on a
   `GLPKnown` plan, does `when groupUpdated $ reapprove …`
   (`Service.hs:815-854`, trigger at `:824`), which sets `GRSPendingApproval` and
   re-sends the channel for admin approval.

3. **`groupUpdated` is a whole-profile byte inequality.**
   It is the `Bool` returned by `updateGroupFromLinkData`, defined as
   `profileChanged = p /= groupProfile`
   (`src/Simplex/Chat/Library/Internal.hs:1466-1482`), where `p` is the
   directory's **stored** `GroupProfile` and `groupProfile` is the profile inside
   the **fetched link data**. This equality includes
   `publicGroup.publicGroupAccess.groupDomainClaim` and its `proof`.

4. **A normal profile change self-corrects after one cycle — so this is a
   non-convergence bug specific to the domain claim.**
   When `profileChanged` is true, `updateGroupFromLinkData` also writes the link
   profile to the DB (`Internal.hs:1471`), and the store persists the domain claim
   *and its proof* (`group_domain_proof` column,
   `src/Simplex/Chat/Store/Groups.hs:2724`; write/read are symmetric —
   `publicGroupAccessRow` / `toPublicGroupAccess`,
   `src/Simplex/Chat/Store/Shared.hs:721` and `:727`). So an ordinary field change
   (name/description/image) re-approves **once** and then converges. For the loop
   to persist, the directory's stored profile must be **permanently unequal** to
   the fetched link profile — and the only field that behaves this way after a
   name is added is the domain claim/proof.

5. **The domain claim/proof legitimately differs between "stored" and "link"
   representations.** Two documented mechanisms, either of which reproduces the
   symptom (we do not need to distinguish them — see §4):
   - A relay can **drop the domain claim** from the served link data — the code
     explicitly handles this case (`src/Simplex/Chat/Library/Commands.hs:4326-4328`,
     "an un-upgraded relay dropped the claim").
   - The proof carries a `ProofPresHeader` that supports a **fresh
     per-presentation nonce** (`PHTest nonce`, `src/Simplex/Chat/Badges.hs:197-221`),
     so a re-presented proof is not byte-identical to the stored one.

6. **The domain claim is verified independently and is not admin-moderated
   content.** Ownership is proven cryptographically and on-chain by
   `verifyEntityDomain` (`Commands.hs:4845`: the on-chain link must match the
   connection link, and the proof must be signed by the address owner's key), and
   verification state is tracked in a **separate** field, `group_domain_verified`
   (`Groups.hs:2710-2713`, `setGroupDomainVerified`). Nothing about the name
   requires directory-admin re-approval.

## 3. Root cause

The directory's re-approval trigger (`groupUpdated`) is a byte-comparison of the
**entire** group profile, including the independently-verified domain claim and
its proof. Adding a name introduces a claim/proof whose link-side representation
never byte-matches the directory's stored copy (relay-stripped claim and/or
re-presented proof nonce), so `profileChanged` is true on **every** 30-minute
link check → re-approval on repeat.

The conceptual error: `groupUpdated` conflates two different questions —
"did the profile change at all?" (used to decide whether to *store* the fresh
claim/proof) and "did the profile change in a way that requires admin
*re-moderation*?" (used to decide re-approval). These must be separated for the
domain claim.

## 4. The fix (minimal, idiomatic)

In `src/Simplex/Chat/Library/Internal.hs`, `updateGroupFromLinkData`:
keep storing on the full `profileChanged` (so the claim/proof and verification
stay current), but **report `groupUpdated` with the domain claim excluded**.

```haskell
        -- store branch unchanged; only the returned flag changes:
        pure (g'', moderationChanged)      -- was: profileChanged
  ...
  where
    profileChanged = p /= groupProfile     -- keep: still gates storing fresh claim/proof
    -- The domain claim is verified independently (on-chain + owner signature) and
    -- is not admin-moderated; its link vs stored representation can differ (a relay
    -- dropping the claim, or a re-presented proof nonce), which otherwise re-triggers
    -- directory approval on every link check. Exclude it from the reported change.
    moderationChanged = withoutClaim p /= withoutClaim groupProfile
    withoutClaim gp@GroupProfile {publicGroup = pgm} = gp {publicGroup = clearClaim <$> pgm}
    clearClaim pg@PublicGroupProfile {publicGroupAccess = a} =
      pg {publicGroupAccess = (\acc -> acc {groupDomainClaim = Nothing}) <$> a}
```

This is a semantics fix, not a convergence hack: a domain-claim change should
**never** trigger re-approval, independently of *why* the two representations
differ. It is therefore robust to both mechanisms in §2.5 — we do not need to
resolve which one occurs in production.

## 5. Why this is correct, sufficient, and safe

**Blast radius is contained to the directory.** `GLPKnown.groupUpdated`
(`src/Simplex/Chat/Controller.hs:1131`) is consumed **only** by the directory
service. (The many other `groupUpdated` matches in the tree are an unrelated
chat event — `RcvGroupEvent.GroupUpdated` / `CR.GroupUpdated` — not this plan
flag.) The three directory consumers all behave correctly with the claim
excluded:

- `Service.hs:824` — periodic re-approval → **fixed** (the 30-min loop stops).
- `Service.hs:988` → `deReregistration` (`:1018`, uses the flag at `:1043` and
  `:1046`) — an owner re-submitting a **name-only** change now correctly gets
  "already listed" instead of being forced back into approval.
- `Service.hs:825` — `listingsUpdated` on `groupUpdated || summary changed`. The
  only trade-off: a name-only change no longer *immediately* refreshes the web
  listing files (it still refreshes on any real profile change or member-count
  change). See §6 for the variant that preserves this.

**Storage and verification stay correct.** Because the store branch still keys
off the full `profileChanged`, the directory continues to persist a genuinely
new claim/proof, and `verifyChanged`/`setGroupDomainVerified` still run — so
by-name lookups and the verified badge remain accurate.

**Moderation is not weakened.** Re-approval exists to re-check *moderatable*
fields (display name, description, image) that an owner might change after
approval. The SimpleX name is not such a field: it is cryptographically verified
and, if name-content moderation is ever desired, that is a separate feature
operating on the resolved name value — not on byte-equality of a proof.

## 6. Alternatives considered

- **Fix in the directory (`deGroupLinkCheck`) instead of core.** Rejected: the
  directory only receives the `Bool`, not the profile diff, so it cannot tell a
  claim-only change from a real one without the core change anyway. The core is
  the correct single locus.
- **Make the profiles converge (stop the relay stripping / freeze the proof
  nonce).** Larger, mechanism-specific, and touches link-data serving and proof
  presentation. The semantics fix in §4 is smaller and correct regardless.
- **Preserve prompt listing refresh on name changes.** If desired, keep the
  `listingsUpdated` check (`Service.hs:825`) driven by the raw `profileChanged`
  while re-approval (`:824`) uses the new `moderationChanged`. This needs the
  plan to carry both signals (or the directory to recompute); recommended only
  if the web listing renders the verified name and staleness is a concern.

## 7. Residual behaviour (accepted)

If the claim/proof is genuinely volatile (per-presentation nonce), the store
branch will re-write the group_profiles row on each 30-min check (same domain →
`claimChanged` is false, so verification is **not** thrashed;
`Groups.hs:2708-2713` compares domain, not proof). This is a harmless idempotent
write and is vastly preferable to the owner-facing re-approval spam. A follow-up
could gate the store to skip proof-only deltas; out of scope here.

## 8. Verification plan

1. **Regression test** in `tests/Bots/DirectoryTests.hs` (the member-review
   branch already added directory concurrency coverage there): register a
   channel → approve → add a SimpleX name (domain claim) → trigger a link check →
   assert the registration status **stays `GRSActive`** (no `GRSPendingApproval`,
   no re-approval message to the owner).
2. **Optional diagnostic** before/after: a one-line log at `Internal.hs:1482`
   dumping the two `groupDomainClaim` values confirms which §2.5 mechanism occurs
   in a live setup — informative, not required for the fix.
3. Build the directory service and run the directory test suite.

## 9. Files touched

- `src/Simplex/Chat/Library/Internal.hs` — `updateGroupFromLinkData`: return a
  claim-insensitive change flag (store logic unchanged).
- `tests/Bots/DirectoryTests.hs` — add the name-added-no-reapproval regression test.

No schema, wire-format, or API changes. `GLPKnown.groupUpdated`'s type is
unchanged; only its value semantics (directory-only consumer) are refined.
