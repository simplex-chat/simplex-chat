# Fix: SimpleX directory re-approves a channel every 30 min after a blockchain name is added

Status: proposed — **root cause deduced from code (see §3); fix in core**
Branch: `nd/fix-directory-names`
Base: `origin/master` (`b38015c7b`)

> This supersedes two earlier drafts of this plan that blamed the domain claim/proof.
> Deep investigation showed that was a **red herring** — the claim/proof round-trips
> and converges. The real non-convergent field is **`publicGroupId`**. See §3.

## 1. Problem

After an owner adds a SimpleX name (blockchain domain claim) to a public channel,
the SimpleX Directory service asks the owner to re-approve the channel **every
30 minutes, indefinitely**, with no changes on the owner's side. The channel keeps
flipping to "hidden until approved."

## 2. Confirmed trigger (facts, file:line)

1. **30-min cadence = the directory's link-check timer.** `linkCheckInterval`
   defaults to **1800 s** (`apps/simplex-directory-service/src/Directory/Options.hs:181`);
   `linkCheckThread_` sleeps that interval and enqueues a `DEGroupLinkCheck` for
   every non-removed registered group (`.../Directory/Service.hs:199-211`).
2. **`deGroupLinkCheck` re-approves iff `groupUpdated` is true** —
   `when groupUpdated $ reapprove …` (`Service.hs:824`), flipping `GRSActive →
   GRSPendingApproval` and re-sending for admin approval (`:843-854`).
3. **`groupUpdated` is a whole-profile byte inequality** — the `Bool` returned by
   `updateGroupFromLinkData`: `profileChanged = p /= groupProfile`
   (`src/Simplex/Chat/Library/Internal.hs:1466-1482`), `p` = directory's *stored*
   `GroupProfile`, `groupProfile` = profile from the *fetched* link short-link data.
4. **The flag is behaviorally directory-only** — the mobile/desktop/iOS
   `GroupLinkPlan.Known` variant does not carry it (`SimpleXAPI.kt:7158`; iOS
   `AppAPITypes.swift`); only the directory consumes it (`Service.hs:824`, `:825`,
   `:988`→`deReregistration:1043/1046`).

## 3. Root cause (deduced by elimination — code-confirmed)

Three independent traces close the case without needing a runtime diagnostic:

- **The fetched link profile is byte-identical across fetches.** It is served
  verbatim from the owner's published, owner-authorized `LSET` blob via `LGET`
  (`simplexmq` agent `getConnShortLink` → `decryptLinkData`); there is no
  timestamp, per-fetch nonce, relay reconstruction, or server-computed member
  count. Member count lives in `GroupShortLinkData.publicGroupData` (a sibling of
  `groupProfile`) and drives `countChanged`, not `groupUpdated`. So the link side
  cannot make `groupUpdated` flip on its own.
- **Every `Eq`-relevant `GroupProfile` field except one is persisted by
  `updateGroupProfile` and therefore converges** after the first store
  (`Internal.hs:1471`). The single exception is **`publicGroupId`**
  (`Types.hs:858`, part of `PublicGroupProfile`, `deriving Eq`): the UPDATE in
  `updateGroupProfile` (`src/Simplex/Chat/Store/Groups.hs:2721-2732`) writes
  `group_type, group_link, group_web_page, group_domain, domain_web_page,
  allow_embedding, group_domain_proof, …` but **omits `public_group_id`**. It is
  read back from the stored column (`Shared.hs:699`, `Groups.hs:2779`), and
  `toPublicGroupProfile` returns `publicGroup = Nothing` unless `group_type` **and**
  `group_link` **and** `public_group_id` are all present (`Shared.hs:713-716`).
- **The domain claim/proof is not the cause.** `group_domain`/`group_domain_proof`
  *are* persisted and round-trip symmetrically (`Shared.hs:721`/`:727`), so they
  converge. In the group flow the proof is `Nothing` anyway (`SimplexDomainProof`
  is never constructed in `src/`; group claims use `mkDomainClaim`, `proof = Nothing`,
  `Names.hs:56`, `Commands.hs:5693`), and group verification is a separate scalar
  column `group_domain_verified` (`Groups.hs:2740-2746`).

**Deduction:** a *perpetual* re-approval requires a field that never reconciles.
The link side is deterministic, and every field except `publicGroupId` converges
after one store — so the only field that can hold `p /= groupProfile` forever is
`publicGroupId`. It does so because the directory's stored `public_group_id` is
**NULL / stale** and `updateGroupProfile` — the only function that syncs a group's
profile from link data or `XGrpInfo` — cannot write that column.

**Why the directory's `public_group_id` is NULL, and why it correlates with the
name.** `public_group_id` is written in only three places, all *creation-time*:
the two group INSERTs (`Groups.hs:405`, `:912`, from the profile's `publicGroup`)
and `updateRelayGroupKeys` (`:2293`). If the directory joined the channel while it
had no `publicGroupId` in its link profile (an older channel, or one that became a
public/named channel later), its `public_group_id` was inserted NULL and can never
be populated afterward. When the owner adds the SimpleX name, the owner republishes
the link with a fully-populated `publicGroup` (`publicGroupId` + `publicGroupAccess`),
so the fetched profile's `publicGroup` becomes `Just{…}` while the directory's
stored copy stays `Nothing` — a permanent inequality, re-approved every 30 min.

**Confirmation step (cheap, not blocking):** on the affected directory,
`SELECT public_group_id FROM group_profiles` joined to the channel's group should
show NULL, while the link's `publicGroupId` is present.

## 4. The fix (core, minimal): let `updateGroupProfile` populate `public_group_id`

`updateGroupProfile` is missing a profile column that its sibling INSERT paths
already persist. Add `public_group_id` to its UPDATE, **guarded by `COALESCE` so it
only populates a currently-NULL value and never changes or erases an existing
identity** (respecting `publicGroupId`'s immutability):

```sql
-- src/Simplex/Chat/Store/Groups.hs, updateGroupProfile_ UPDATE:
SET display_name = ?, full_name = ?, short_descr = ?, description = ?, image = ?,
    group_type = ?, group_link = ?, public_group_id = COALESCE(public_group_id, ?),
    group_web_page = ?, group_domain = ?, domain_web_page = ?, allow_embedding = ?,
    group_domain_proof = ?, preferences = ?, member_admission = ?, updated_at = ?
```

with the new `?` sourced exactly like the INSERT paths (`Groups.hs:384-385`):

```haskell
publicGroupId_ = case publicGroup of
  Just PublicGroupProfile {publicGroupId} -> Just publicGroupId
  Nothing -> Nothing
```

Effect: on the next link check, the directory's NULL `public_group_id` is populated
from the authoritative link profile → its stored `publicGroup` reconstructs as
`Just{…}` equal to the link's → `profileChanged` becomes false → re-approval stops.

## 5. Why this is the most correct fix

- **Fixes the actual root cause, and converges.** After one sync the profiles are
  equal, so `groupUpdated` correctly reports "no change." Symptom and cause both go.
- **It is a plain completeness fix.** The two INSERT paths and `updateRelayGroupKeys`
  already persist `public_group_id`; `updateGroupProfile` omitting it is the bug.
- **Safe under `COALESCE(public_group_id, ?)`** — it *only* fills a NULL. It never
  changes a set identity, so it cannot corrupt a correct `publicGroupId`, and it
  cannot be erased by a claim-less `XGrpInfo` (new value `Nothing` → keep old).
  This preserves the "immutable identity" invariant while repairing missing data.
- **General, not a directory workaround.** Any client whose `public_group_id` is
  missing gets it repaired on the next profile sync; the fix is in shared core, as
  intended, and needs no judgment about which fields are "moderatable."
- **Blast radius is safe.** `updateGroupProfile` is widely called, but the only
  behavioral change is *populating a NULL column when a profile carrying a
  `publicGroupId` is stored* — a pure improvement for every caller.

## 6. Alternative considered — and why not

**Scope the re-approval comparison** (return `groupUpdated` from
`updateGroupFromLinkData` computed over only the moderatable display fields,
excluding `publicGroup`). This mirrors the contact path's `clearProofs` /
`sameProfileContent` redaction (`Types.hs:747-750`, `:803`) and the existing
`DirectoryTests` expectation that link-only changes must not re-approve
(`tests/Bots/DirectoryTests.hs:296-326`). It would stop the re-approval — **but it
only hides the symptom**: the stored `public_group_id` stays NULL, the profiles
stay unequal, so `updateGroupFromLinkData` keeps calling `updateGroupProfile` every
30 min to store a profile that never fully matches (idempotent churn), and any
other consumer of the difference stays broken. §4 is preferred because it removes
the inequality itself. (This scoping could still be added later as defense in depth
for genuinely non-moderatable fields, but it is not needed for this bug.)

## 7. Verification plan

1. Confirm on the affected directory that the channel's `public_group_id` is NULL
   while the link carries a `publicGroupId` (validates §3; one SELECT).
2. Apply §4.
3. **Regression test** in `tests/Bots/DirectoryTests.hs` (sits alongside the
   existing "link-only / whitespace-only must not re-approve" tests at `:296-326`):
   register a channel whose stored `public_group_id` is NULL → add a SimpleX name /
   populate the link `publicGroupId` → run ≥2 link checks → assert the registration
   stays `GRSActive` (no `GRSPendingApproval`, no owner re-approval message). Also
   assert the group's `public_group_id` is populated after the first check.
4. Build the directory service (which statically links core); run the directory
   test suite.

## 8. Files touched

- `src/Simplex/Chat/Store/Groups.hs` — `updateGroupProfile`: add
  `public_group_id = COALESCE(public_group_id, ?)` to the UPDATE and thread the
  `publicGroupId_` parameter (as the INSERT paths do). No other logic change.
- `tests/Bots/DirectoryTests.hs` — add the name-added / NULL-`public_group_id`
  no-reapproval regression test.

No schema, wire-format, or API changes. `updateGroupProfile`'s signature is
unchanged; it simply stops dropping a column it is already given.
