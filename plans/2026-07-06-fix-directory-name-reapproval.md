# Fix: SimpleX directory re-approves a channel every 30 min after a blockchain name is added

Status: proposed — **root cause confirmed by code (deduction, §3); fix in core (§4)**
Branch: `nd/fix-directory-names`
Base: `origin/master` (`b38015c7b`)

> Supersedes earlier drafts that blamed the domain claim/proof, and a draft that
> proposed `COALESCE(public_group_id, ?)`. Both were wrong. Deep investigation
> (three traces + elimination) pins the cause to **`publicGroupId`** and the fix to
> a content-comparison in `updateGroupFromLinkData`. See §3–§4.

## 1. Problem

After an owner adds a SimpleX name (blockchain domain claim) to a public channel,
the SimpleX Directory service asks the owner to re-approve the channel **every
30 minutes, indefinitely**, with no changes on the owner's side.

## 2. Confirmed trigger (facts, file:line)

1. **30-min cadence = the directory's link-check timer.** `linkCheckInterval`
   defaults to 1800 s (`apps/simplex-directory-service/src/Directory/Options.hs:181`);
   `linkCheckThread_` enqueues `DEGroupLinkCheck` per registered group every interval
   (`.../Directory/Service.hs:199-211`).
2. **`deGroupLinkCheck` re-approves iff `groupUpdated`** — `when groupUpdated $
   reapprove …` (`Service.hs:824`), which flips the reg to `GRSPendingApproval` and
   re-sends for approval (`:843-854`). It only runs when the stored `publicGroup` is
   `Just` (`forM_ pg_ …`, `:817`).
3. **`groupUpdated` = full structural inequality.** It is the `Bool` from
   `updateGroupFromLinkData`: `profileChanged = p /= groupProfile`
   (`src/Simplex/Chat/Library/Internal.hs:1466-1482`), `p` = directory's *stored*
   `GroupProfile`, `groupProfile` = the *fetched link* profile.
4. **The flag is behaviorally directory-only.** The mobile/desktop/iOS
   `GroupLinkPlan.Known` variant does not carry it (`SimpleXAPI.kt:7158`; iOS
   `AppAPITypes.swift`); consumers are all in the directory (`Service.hs:824`, `:825`,
   `:988`→`deReregistration:1043/1046`).

## 3. Root cause — `publicGroupId`, confirmed by elimination (no diagnostic needed)

Three facts close the case deductively:

- **`p` reflects every persisted column.** The directory's CTLink plan resolves via
  `getGroupToConnect` → `getGroupViaShortLinkToConnect` (`Groups.hs:1084`) →
  **`getGroupInfo`** (`:2841`) — the standard projection reading all `group_profiles`
  columns.
- **`updateGroupProfile` persists every `Eq`-relevant `GroupProfile` field except
  one.** Its UPDATE (`Groups.hs:2721-2732`) writes `display_name, full_name,
  short_descr, description, image, group_type, group_link, group_web_page,
  group_domain, domain_web_page, allow_embedding, group_domain_proof, preferences,
  member_admission` — but **omits `public_group_id`**. That column is written only at
  group creation (INSERTs, `:405`/`:912`) and by `updateRelayGroupKeys` (`:2293`).
- **The fetched link profile is byte-identical across fetches** — served verbatim
  from the owner's `LSET` blob via `LGET` (no timestamp/nonce/reconstruction).

**Deduction.** When `profileChanged` is true, `updateGroupFromLinkData` stores the
link profile (`Internal.hs:1471`); on the next check `getGroupInfo` re-reads a `p`
equal to the link profile on **every field except `public_group_id`**, which the
store cannot change. The link side is deterministic. Therefore a *perpetual*
inequality is possible **iff** the stored `public_group_id` differs from the link's
— it is the only field that cannot converge. This is deductively certain.

**Why it is non-NULL-but-stale (not NULL).** The re-approval only runs when the
stored `publicGroup` is `Just`, which requires `public_group_id` non-NULL. So the
directory's value is present but **stale**: set once at join time (or via
`updateRelayGroupKeys`) and never re-synced, because `updateGroupProfile` — the
function that syncs the profile from link data / `XGrpInfo` on every update — never
writes that column. When adding the name causes the owner to (re)publish a
`publicGroupId` that differs from the directory's stale stored value, the mismatch
is permanent → `groupUpdated` true every cycle → re-approval on repeat.

**Ruled out (earlier drafts):** the domain claim/proof **is** persisted
(`group_domain`/`group_domain_proof`, symmetric `publicGroupAccessRow`/
`toPublicGroupAccess`) and therefore converges; in the group flow the proof is
`Nothing` anyway (`mkDomainClaim`, `Names.hs:56`). The claim is a red herring — it
is merely the *occasion* (the owner republishing the link) that surfaces the
pre-existing `publicGroupId` staleness.

## 4. The fix (core, minimal): compare *content*, ignoring immutable identity

`publicGroupId` is immutable cryptographic identity (`sha256(genesis root key)`,
`Types.hs:858`), not moderatable content. `updateGroupFromLinkData` should not treat
an identity difference as a profile change. Compare with `publicGroupId` normalized
out, and use that for **both** the store guard **and** the returned flag (so there is
neither re-approval nor per-cycle store churn):

```haskell
-- src/Simplex/Chat/Library/Internal.hs, updateGroupFromLinkData:
  | contentChanged || countChanged || verifyChanged = do
      cxt <- chatStoreCxt
      withStore $ \db -> do
        g   <- if contentChanged then updateGroupProfile db user gInfo groupProfile else pure gInfo
        g'  <- …count…
        g'' <- if verifyChanged then …setGroupDomainVerified… else pure g'
        pure (g'', contentChanged)
  | otherwise = pure (gInfo, False)
  where
    -- publicGroupId is immutable identity, not content; updateGroupProfile never
    -- re-syncs it, so a stale stored value would make a plain (/=) re-trigger
    -- approval forever. Compare content with it normalized out.
    contentChanged = clearId p /= clearId groupProfile
    clearId gp@GroupProfile {publicGroup} =
      gp {publicGroup = (\pg -> pg {publicGroupId = mempty}) <$> publicGroup}
    …
```

(`profileChanged = p /= groupProfile` is removed; `countChanged`/`verifyChanged`
unchanged.)

## 5. Why core, not the directory

The defect is that a shared primitive — "did the group profile change?" — answers
"yes" when only immutable identity differs. That is the primitive's correctness,
not a directory policy, so it belongs where the comparison lives:

- **Robust.** Compares the exact two profiles at the update site — no reliance on a
  directory-side pre/post snapshot with an enqueue-vs-process timing window.
- **Correct for any consumer.** The flag is directory-only today, but fixing it here
  keeps it right for any future consumer at no extra cost.
- **Isolation is illusory anyway.** The directory statically links core, so it is
  rebuilt either way, and the behavioral change is directory-scoped either way.

## 6. Why this is correct, safe, and minimal

- **Fixes the confirmed cause and converges** the re-approval decision: an
  identity-only difference no longer reads as a change, so `groupUpdated` is false
  and re-approval stops.
- **No data mutation.** It does *not* write `public_group_id`, so it cannot violate
  any "immutable identity" assumption elsewhere; the stale stored value simply stops
  driving moderation.
- **No store churn.** Because `contentChanged` also gates the store, an identity-only
  delta no longer triggers a pointless `updateGroupProfile` every 30 min.
- **Moderation intact.** Any moderatable change (display name, full name, short
  descr, description, image, prefs, admission, or the domain claim) still differs
  under `clearId` and still re-approves. Only immutable identity is excluded —
  consistent with the contact path's content-comparison (`Types.hs:747-750`
  `clearProofs`) and the existing `DirectoryTests` expectation that link/identity
  changes must not re-approve (`tests/Bots/DirectoryTests.hs:296-326`).

## 7. Alternatives considered — and why not

- **Persist the column** (`public_group_id = COALESCE(?, public_group_id)`, adopt the
  link's value). Also converges, but it **mutates an "immutable" identity** on every
  profile sync and risks assumptions elsewhere; identity repair is a bigger, riskier
  change than simply not moderating on identity.
- **Directory-only** (compare `gInfo` vs `g'` at `Service.hs:824` instead of
  `groupUpdated`). Works — both are stored-side, so the stale `publicGroupId` is
  equal on both and never fires — but it relies on `gInfo` being a faithful
  pre-update snapshot (enqueue-vs-process window) and leaves `groupUpdated`
  misleading for any future consumer. Core is preferred per §5.

## 8. Verification plan

1. **Regression test** in `tests/Bots/DirectoryTests.hs` (beside the existing
   "link/whitespace-only must not re-approve" tests, `:296-326`): register a channel
   with one `public_group_id`, then make the fetched link carry a *different*
   `publicGroupId` (all other fields equal), run ≥2 link checks, and assert the reg
   stays `GRSActive` (no `GRSPendingApproval`, no owner re-approval message). Add a
   positive control: a display-name change *does* re-approve.
2. Apply §4.
3. Build the directory service (statically links core); run the directory suite.

## 9. Files touched

- `src/Simplex/Chat/Library/Internal.hs` — `updateGroupFromLinkData`: replace
  `profileChanged` (store guard + returned flag) with a `publicGroupId`-insensitive
  `contentChanged`. No store, schema, wire, or API change.
- `tests/Bots/DirectoryTests.hs` — add the identity-difference no-reapproval
  regression test (+ display-name positive control).
