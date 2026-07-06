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

The claim's round-trip fidelity — the one field that *changes* when a name is
added, hence the last assumption behind the elimination — was verified: `ToField
SimplexDomain = decodeLatin1 . strEncode` and `FromField = strDecode . encodeUtf8`
(`simplexmq SimplexName.hs:127-129`); domains are ASCII, so the stored value is
`strDecode (strEncode d)`, which equals `d` for a canonical (lower-cased) domain,
and the link's domain is already canonical (it arrives via `StrJSON`/`strDecode`).
Because the name sets `group_domain`, `toPublicGroupAccess` returns `Just` (not the
degenerate empty-access→`Nothing` at `Shared.hs:726`), so the claim reconstructs
equal. So every field except `publicGroupId` provably converges — the elimination
is airtight, not "assuming fidelity". (The empty-access→`Nothing` degeneracy at
`Shared.hs:726` is a separate latent asymmetry, unreachable here since a claim makes
the access non-empty; out of scope.)

## 4. The fix (core, minimal): compare *content*, ignoring immutable identity

`publicGroupId` is immutable cryptographic identity (`sha256(genesis root key)`,
`Types.hs:858`), not moderatable content. `updateGroupFromLinkData` should not treat
an identity difference as a profile change.

`profileChanged` is a local `where`-binding already used in all three relevant
sites — the guard, the `if … then updateGroupProfile`, and the returned flag — so
**redefining only its definition** fixes all three at once with **no body edits**
(so there is neither re-approval nor per-cycle store churn). The entire change:

```haskell
-- src/Simplex/Chat/Library/Internal.hs, updateGroupFromLinkData `where` block:
    -- was: profileChanged = p /= groupProfile
    -- publicGroupId is immutable identity, not content; updateGroupProfile never
    -- re-syncs it, so a stale stored value makes a plain (/=) re-trigger approval
    -- forever. Compare content with it normalized out (mirrors sameGroupProfileInfo,
    -- Internal.hs:2986, which normalizes groupPreferences the same way).
    profileChanged = clearId p /= clearId groupProfile
    clearId gp@GroupProfile {publicGroup} =
      gp {publicGroup = (\pg -> pg {publicGroupId = B64UrlByteString ""}) <$> publicGroup}
```

The `| profileChanged || countChanged || verifyChanged` guard, the
`if profileChanged then updateGroupProfile …`, and the `pure (g'', profileChanged)`
return are all **unchanged** — they keep referencing `profileChanged`, whose meaning
is now "content changed, ignoring identity". `countChanged`/`verifyChanged`
unchanged. Net diff: one redefined `where` line + one helper line, zero body edits.

Notes:
- `B64UrlByteString` has no `Monoid` instance (`Types.hs:161-163`), so use
  `B64UrlByteString ""`, **not** `mempty`. The value is only used inside the local
  `/=` and discarded — nothing is written.
- **Do not** reuse/extend `sameGroupProfileInfo` for this: it also drops
  `groupPreferences`, which would change a shared helper (its other caller is
  `Commands.hs:3939`) and silently stop the directory re-approving on preference
  changes — scope creep beyond this bug. Normalize `publicGroupId` only.

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
- **Correctly scoped, not a workaround.** `publicGroupId` is *not* a field
  `updateGroupProfile` manages — it is owned by the group INSERTs and
  `updateRelayGroupKeys` (`Groups.hs:2293`); genuine identity/key changes flow
  through that path, never through profile sync. So `updateGroupFromLinkData`
  reacting to it was the defect: it compared a field it neither syncs nor should.
  Normalizing it out aligns the comparison with exactly the columns
  `updateGroupProfile` actually writes (and `publicGroupId` is the *only* `Eq`-field
  it omits — so this is precisely "compare the managed fields").
- **No data mutation.** It does *not* write `public_group_id`, so it cannot violate
  any "immutable identity" assumption elsewhere.
- **The residual stored staleness is inert.** The directory reads its stored
  `publicGroupId` in *only* this comparison: `generateListing`
  (`Directory/Listing.hs:146`) names files by `listingFileName`/`promotedFileName`,
  not `publicGroupId`; the directory does not use `Simplex.Chat.Web` /
  `webPreviewWorker` (the per-`publicGroupId` web-file naming is a channel-owner
  feature); and the `publicGroupId`↔entity-id validations (`Commands.hs:2190-2193`,
  `:4315`) use the *fetched link* value, not the stored one. So leaving the stored
  value stale changes nothing else the directory does.
- **No store churn.** Because the redefined `profileChanged` also gates the store, an
  identity-only delta no longer triggers a pointless `updateGroupProfile` every 30 min.
- **Moderation intact.** Any moderatable change (display name, full name, short
  descr, description, image, prefs, admission, or the domain claim) still differs
  under `clearId` and still re-approves. Only immutable identity is excluded —
  consistent with the contact path's content-comparison (`Types.hs:747-750`
  `clearProofs`) and the existing `DirectoryTests` expectation that link/identity
  changes must not re-approve (`tests/Bots/DirectoryTests.hs:296-326`).

## 7. Alternatives considered — and why not

- **Persist the column** (`public_group_id = COALESCE(?, public_group_id)`, adopt the
  link's value). Also converges, and it *additionally* repairs the stored staleness —
  but that staleness is inert for the directory (§6), so this fixes a separate,
  non-reported concern. It **mutates an "immutable" identity** on every profile sync
  (a field `updateGroupProfile` deliberately does not manage) and risks assumptions
  in its many other callers — a bigger, riskier change for no benefit to this bug.
- **Directory-only** (compare `gInfo` vs `g'` at `Service.hs:824` instead of
  `groupUpdated`). Works — both are stored-side, so the stale `publicGroupId` is
  equal on both and never fires — but it relies on `gInfo` being a faithful
  pre-update snapshot (enqueue-vs-process window) and leaves `groupUpdated`
  misleading for any future consumer. Core is preferred per §5.

## 7b. Regressions checked (none found)

- **Compiles.** `Internal.hs` has `{-# LANGUAGE OverloadedStrings #-}`, so
  `B64UrlByteString ""` type-checks. (`B64UrlByteString mempty` — inner
  `ByteString` `mempty` — is an equivalent pragma-independent form.)
- **Existing "link/whitespace-only must not re-approve" test is untouched**
  (`DirectoryTests.hs:296-326`). That test changes a URL inside the *welcome
  message* text and exercises the owner-update path (`deGroupUpdated`) — not
  `publicGroup.groupLink`, not `publicGroupId`, and not the link-check path. The
  fix normalizes only `publicGroupId`, so every non-`publicGroupId` field
  (including `groupLink`, description, image) compares exactly as before.
- **No missed re-approval.** Only `publicGroupId` is excluded; any moderatable
  field still differs under the comparison and still re-approves.
- **Non-directory clients unaffected.** They don't read the returned flag (the
  mobile/desktop `GroupLinkPlan.Known` variant omits it, §2.4); the only other
  effect is skipping an `updateGroupProfile` call when *only* `publicGroupId`
  differs — a no-op, since that column is never written there anyway.
- **Other `updateGroupFromLinkData` callers safe.** `Commands.hs:4331` (CTName
  path) ignores the returned `Bool` and uses `g'`; its domain check reads the
  claim *domain*, which still converges (a real claim change keeps
  `profileChanged` true and stores). `Commands.hs:4358` is the directory path this
  fix targets.

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

- `src/Simplex/Chat/Library/Internal.hs` — `updateGroupFromLinkData`: redefine the
  local `profileChanged` `where`-binding to normalize `publicGroupId` out before
  comparing (one redefined line + one `clearId` helper line; no body edits). No
  store, schema, wire, or API change.
- `tests/Bots/DirectoryTests.hs` — add the identity-difference no-reapproval
  regression test (+ display-name positive control).
