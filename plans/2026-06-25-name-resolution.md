# SimpleX names — simplification plan

Status: design agreed while reviewing branch `sh/namespace`. This supersedes the
name handling currently on that branch. Line refs are to the working tree at
review time; re-check before editing.

## 1. Goal

Reduce the feature to the minimum coherent, secure shape:

- **One** name per entity, on the **profile** (the entity's claimed identity),
  typed `Maybe SimplexNameInfo`. No second "locally-known" copy on the entity row.
- A local **verification status** as a 3-state `Maybe Bool` (not a timestamp):
  not-attempted / failed / verified. A failed check never blocks connecting.
- Connect-by-name gated by **consent** and **born verified**.
- The **proof** (address-key signature, context-bound) is **in scope for link
  contexts** — connect-by-name, 1-time invitations, contact addresses, channel
  join links — so those names are verifiable in this release. Only a name with
  **no link context** (a group member, or a name on an `XInfo` profile over an
  established connection) is deferred: stored, but not shown until that gap closes.

Removed from the current branch: the `*.simplex_name` entity columns (the
"locally-known/ct" copy), the `connections.simplex_name` carrier, the four
partial UNIQUE indexes + "newer-claim-wins" clearing, and the `_verified_at`
timestamps.

## 2. Why (trust model)

A name resolves to a link via the agent (`resolveSimplexName`); a `NameRecord`
carries **lists** of links (`nrSimplexContact`, `nrSimplexChannel`), so
verification matches against **any** of them. "Match any" is safe **only because
the namespace is an on-chain, ENS-style registry**: `nrOwner`/`nrResolver` are
Ethereum addresses (`Names/Record.hs:22-23,36`), so each name has a **single
owner** who sets all its links. An attacker can register *their* name → your
address (the offensive-name case, handled by the consent gate below) but **cannot
add a link to your name** — on-chain ownership blocks impersonation. This premise
is load-bearing; if names were not single-owner, "match any" would be exploitable.

The registrant of a name controls what it points to, with no proof that the
target address consents — anyone can publish `@offensive → your address` or
`#offensive → your channel`. Therefore:

- One-directional verification ("does `resolve(name)` equal a stored link") is
  insufficient: it confirms the registrant's assertion, not the address owner's.
- A profile **claiming** a name (and even a link) proves nothing — anyone can
  copy a link into a profile. **Control of a link is proven only by an actual
  connection/join through it.**
- Sound verification is the intersection: `resolve(name) → link`, **and** the
  entity advertises that name in its profile (consent), **and** that link is one
  we connected/joined through (control-proven).
- Verifying a name without having connected through its link needs a **signature
  by the address key over the name, bound to the presentation context** (§4.8).
  This is **in scope** for link contexts: a 1-time invitation carries such a proof
  bound to the invite, so resolving the name → address → address key verifies it.

Consequence: names are verifiable in this release for **channels** (join link),
**contacts connected via their address/name** (the link connected through), and
**1-time-invite contacts** (the in-scope address-key proof). The only case still
deferred is a name with **no link context at all** — a group member, or a name on
an `XInfo` profile over an established connection — which stays stored-but-not-shown.

## 3. Current branch state (to be changed)

Migration `M20260603_simplex_name` currently adds, on both SQLite and Postgres:

- `contacts.simplex_name`, `contacts.simplex_name_verified_at`
- `groups.simplex_name`, `groups.simplex_name_verified_at`
- `connections.simplex_name`
- `contact_profiles.simplex_name`, `group_profiles.simplex_name`
- UNIQUE indexes `idx_contacts_simplex_name`, `idx_groups_simplex_name`,
  `idx_contact_profiles_simplex_name`, `idx_group_profiles_simplex_name`
- `server_operators.smp_role_names` (= 1 for `'simplex'`)

…and threads two names per entity through the types: `Contact.simplexName` /
`GroupInfo.simplexName` (from `*.simplex_name`, "locally known"), and
`LocalProfile.simplexName` (from `*_profiles.simplex_name`, "peer claim"), plus
a `verified_at` timestamp on the entity. `apiVerifySimplexName` verifies the
entity (ct) name against the **profile's self-asserted `contactLink`** for
contacts (wrong link), and against `preparedGroup.connLinkToConnect` for groups
(correct). The `connections.simplex_name` carrier is plumbed through
`createConnection_` but **never written** (all callers pass `Nothing`).

## 4. Target design

### 4.1 Name type and JSON encoding

The name type is **`SimplexNameInfo`** (simplexmq `Simplex/Messaging/SimplexName.hs:37`),
which has:

- `StrEncoding` (`SimplexName.hs:89`) — canonical `simplex:/name@…` / `#…` string,
- a text `ToField` (`SimplexName.hs:146`) — stores as TEXT,
- a JSON **object** instance: `$(J.deriveJSON defaultJSON ''SimplexNameInfo)`
  (`SimplexName.hs:154`) → `{nameType, nameDomain}`.

**Keep the object JSON** — the UI/API wants the structured form (it reads the name
off `LocalProfile`, `CRSimplexNameVerified`, …). The conflict is only on the
**wire**: `PublicGroupAccess.groupDomain` is a **released** field typed
`Maybe Text` (a JSON string), so the wire form of the name must stay a string.

Resolve by wrapping **only the wire fields** with simplexmq's generic newtype
`StrJSON` (`Simplex/Messaging/Encoding/String.hs:265`), whose `ToJSON`/`FromJSON`
go through `StrEncoding` → a JSON string (`String.hs:267–272`; idiom
`deriving (ToJSON, FromJSON) via (StrJSON "X" T)`):

- wire (`Profile`, `GroupProfile`/`PublicGroupAccess`):
  `Maybe (StrJSON "SimplexName" SimplexNameInfo)` → JSON **string**, byte-identical
  on the wire to the released `Maybe Text`.
- local/UI (`LocalProfile`): `Maybe SimplexNameInfo`, **unwrapped** → JSON **object**.

Wrap/unwrap at the `Profile` ⇄ `LocalProfile` boundary (`toLocalProfile` /
`fromLocalProfile`). `SimplexNameInfo`'s own JSON instance is untouched, so
`CRSimplexNameVerified` and any other UI-facing use stay object.

Inherent asymmetry: there is no `LocalGroupProfile`, so the channel name reaches
the UI as a **string** (via `GroupProfile`, which is `StrJSON`), while the contact
name reaches it as an **object** (via `LocalProfile`). If the UI needs the channel
name as an object too, add a decoded field on `GroupInfo` (follow-up). The bot-API
binding generator must render `StrJSON`-wrapped fields as `string`.

DB stays TEXT: `ToField` (`SimplexName.hs:146`) on write; on read a **hard**
`FromField SimplexNameInfo` (add it — `SimplexName.hs:141-145` says to define it
"when a consumer wants the row-fail behaviour"), so an invalid stored name fails the
row — matching the wire, where a name that won't `strDecode` fails the profile. **No
soft-decode** (`decodeSimplexName` dropped for the name columns).

### 4.2 Types (field changes)

- `Profile.contactDomain :: Maybe (StrJSON "SimplexName" SimplexNameInfo)` — NEW;
  JSON string (§4.1). The entity's advertised contact name. (Replaces the branch's
  `Profile.simplexName`.)
- `Profile.contactDomainProof :: Maybe ClaimProof` — NEW; a flat sibling of
  `contactDomain`/`contactLink`, a **wire profile field like `Profile.badge`**. The
  **stored** own profile (`contact_profiles`) carries the name but **no proof**; the
  proof is generated and **added to the outgoing profile at send/save** (§4.8), exactly
  as the badge is. `PHSimplexLink` (verifiable) when the profile is saved to a contact
  address / 1-time invite; `PHTest` over an established connection (the gap).
- `LocalProfile.contactDomain :: Maybe SimplexNameInfo` (unwrapped → object JSON)
  and `LocalProfile.contactDomainVerification :: Maybe Bool` — local status, **not**
  on wire `Profile`. `toLocalProfile` unwraps the `StrJSON`; `fromLocalProfile`
  re-wraps `contactDomain` and drops the status (mirrors how `localBadge`'s status
  is dropped).
- `PublicGroupAccess.groupDomain :: Maybe (StrJSON "SimplexName" SimplexNameInfo)`
  — RETYPE from `Maybe Text` (`Types.hs:857`); JSON string, wire-identical to the
  released text (§4.1). Owner-set, broadcast in `XGrpInfo`. **Only public, relay-backed
  groups** (`groups.use_relays`) can carry a name — they have a channel join link to
  resolve to and an owner key (`groups.member_priv_key`, `chat_schema.sql:193`) to sign
  with; p2p groups have neither. (`contactDomain` on a member is a contact attribute, unaffected.)
- `GroupInfo.groupDomainVerification :: Maybe Bool` — local status, read from the
  `groups` table (there is no `LocalGroupProfile`, and `GroupProfile` is the wire
  type, so the status cannot ride the profile).
- DROP `Contact.simplexName`, `GroupInfo.simplexName`, `Connection.simplexName`,
  and both `*VerifiedAt` timestamps.

Asymmetry, intentional: contact name + status both live on `contact_profiles`
(surfaced via `LocalProfile`); the group name lives on `group_profiles`
(surfaced via `GroupProfile`/`PublicGroupAccess`) but its status lives on
`groups` (surfaced via `GroupInfo`). This is forced — `group_profiles` is the
shared wire profile with no local columns, while `contact_profiles` already
carries local state (`local_alias`).

### 4.3 Schema — rewrite `M20260603_simplex_name` (branch unreleased)

Add:
- `contact_profiles`: `contact_domain TEXT`, `contact_domain_verification`
  (nullable `INTEGER` SQLite / `SMALLINT` Postgres).
- `groups`: `group_domain_verification` (nullable `INTEGER`/`SMALLINT`).
- `server_operators.smp_role_names` (= 1 for `'simplex'`) — keep.
- `user_contact_links`: the contact-address **root signing key** (BLOB, mirroring
  `groups.root_priv_key`) — captured from the 2-step short-link creation — so the
  contact-address / 1-time-invite proof can be signed chat-side. (Not present today.)

No DB change for `group_profiles.group_domain` — it already exists (from
`M20260515_public_group_access`); only the Haskell type and JSON change.

Remove (vs the current branch migration): `contacts.simplex_name`,
`contacts.simplex_name_verified_at`, `groups.simplex_name`,
`groups.simplex_name_verified_at`, `connections.simplex_name`,
`contact_profiles.simplex_name`, `group_profiles.simplex_name`, and all four
UNIQUE indexes. No name uniqueness is enforced at the DB level — identity comes
from verification, not a constraint.

Verification column decode: `Maybe BoolInt → Maybe Bool` (`NULL` = not attempted,
`0` = failed, `1` = verified).

### 4.4 Storage functions (`Store/Shared.hs`, `Store/Direct.hs`, `Store/Groups.hs`)

- Read the name columns as `Maybe SimplexNameInfo` via the **hard** `FromField`
  (§4.1) — an invalid name fails the row; no soft `decodeSimplexName`.
- `toContact` / `toGroupInfo`: read `contact_domain` → `LocalProfile.contactDomain`
  and `contact_domain_verification` → `LocalProfile.contactDomainVerification`;
  `group_profiles.group_domain` → `PublicGroupAccess.groupDomain` and
  `groups.group_domain_verification` → `GroupInfo.groupDomainVerification`. Delete
  the ct/cp split and the entity-`simplex_name` reads.
- `createContact_` / `createGroup_` / `createPreparedContact` / `createPreparedGroup`:
  set the name on the **profile** columns only; drop the entity-`simplex_name`
  argument and the `connections.simplex_name` carrier param on `createConnection_`.
- `updateContactProfile` / `updateGroupProfile`: write `contact_domain` /
  `group_domain` from the received profile. Reset the verification to `NULL`
  (not-attempted) **only when the name changes**; an XInfo/XGrpInfo carrying the
  **same** name keeps the existing status (a verified name stays verified), exactly as
  a badge does. No conflict clearing (no UNIQUE index).
- `getContactBySimplexName` / `getGroupIdBySimplexName`: look up by the **verified**
  profile name (the name column joined with verification = `Just True`); on a miss
  or unverified, fall through to resolve-and-connect. Consistent with the existing
  by-address lookup `getContactViaShortLinkToConnect` (`Direct.hs:963`), which
  matches the link with no verified gate — a link is the identity, a name is a
  claim that only becomes a usable pointer once verified.

### 4.5 Redaction (`Library/Internal.hs:1246`)

```haskell
redactedMemberProfile :: GroupInfo -> GroupMember -> Profile -> Profile
redactedMemberProfile g m Profile {…, contactLink, contactDomain} =
  let allowDirect       = groupFeatureMemberAllowed SGFDirectMessages m g
      allowSimplexLinks = groupFeatureMemberAllowed SGFSimplexLinks  m g && allowDirect
   in Profile { …
              , shortDescr         = removeSimplexLink =<< shortDescr       -- via allowSimplexLinks
              , contactLink        = if allowSimplexLinks then contactLink   else Nothing
              , contactDomain      = if allowDirect       then contactDomain else Nothing
              , contactDomainProof = Nothing }  -- member profiles are contextless; never carry a proof
```

- `allowDirect` is the single primitive (the `DirectMessages` permission); it
  gates the name and is reused inside `allowSimplexLinks`. No `allowName` flag,
  no second lookup.
- **Behavior changes vs current:** `contactLink` flips from unconditionally
  dropped to gated on `allowSimplexLinks` (a member's contact address becomes
  visible whenever links+DMs are allowed — the meaning of "links allowed"); the
  name follows the looser `allowDirect`. Rationale: a link is one-tap-to-connect
  (low friction), a name only resolves if the recipient deliberately looks it up
  (higher friction), so a group can forbid links yet allow name discovery, with
  "DMs allowed" the floor for both.
- Signature takes `(GroupInfo, GroupMember)` and derives both flags inside, so
  the rule lives in one place. Callers pass `(g, m)`; the own-profile path passes
  `(g, membership g)` — behavior-preserving because
  `groupFeatureUserAllowed f g ≡ groupFeatureMemberAllowed f (membership g) g`
  (both reduce to `groupFeatureMemberAllowed' f (memberRole (membership g))
  (fullGroupPreferences g)`, `Types.hs:646–652`).
- Collapses `groupUserAllowSimplexLinks` (`Types.hs:656`) and the pre-computed
  `allowSimplexLinks` plumbing at the call sites (`Internal.hs:1244`,
  `Subscriber.hs:842,2817,3239`, `Commands.hs:3748,4057`). `Commands.hs:3748`
  has `Maybe GroupInfo` → "no group ⇒ no redaction" at the call site.

### 4.6 Resolution + verification (`Library/Commands.hs`)

Two regimes that differ in whether verification can *fail*:

**Connect-by-name** (`connectPlanName` / `dispatchResolvedRecord`) — verification is
a **precondition** of connecting. After decoding the resolved short link's embedded
profile, **add the consent gate**: require that profile's `contactDomain` /
`groupDomain` to equal the resolved name, else fail with `CESimplexNameNotFound`
("name unknown") and **do not connect**. (The current branch decodes the profile but
never compares the name — this is the missing check.) On success the prepared
contact/group is created with the name on the profile, `connLinkToConnect` = the
resolved link, verification = `Just True` (**born verified**). There is no "failed"
outcome here — failing to resolve/consent just means no connection.

**Connected NOT by name** (via an address link or a 1-time link) — the peer's profile
may *claim* a name; it starts **unverified** (`Nothing`). Verification is **post-hoc
and non-blocking**: keep `apiVerifySimplexName` (`/_verify simplex name`).
  - **Contacts** verify by a **single path** — check `contactDomainProof` (§4.8):
    resolve the claimed name → its link, validate the owner chain and **select the key by
    the proof's `linkOwnerId`** (that owner's `ownerKey`, or the root key if `Nothing` —
    the usual contact-address case), check the `ClaimProof` signature over
    `name <> presHeader`, and check the proof's `presHeader` link ==
    `preparedContact.connLinkToConnect` (**not** `profile.contactLink` — the branch bug).
    Contact addresses and 1-time invites both carry the proof.
  - **Channels** verify by **presence / link-match**: `resolve(#name)` includes
    `preparedGroup.connLinkToConnect` (the join link), whose owner-signed data already
    carries `groupDomain` (no `ClaimProof` — §4.8).
  Result is `Just True` (holds) or `Just False` (fails) — and **`Just False` must NOT
  prevent the connection**, exactly as a failed *badge* verification doesn't. **This is
  why the status must be 3-state** (`Nothing` not attempted / `Just False` failed /
  `Just True` verified). Names that stay `Nothing` have **no proof/link context** —
  group members, and names on an `XInfo` profile over an established connection.

Open option: **auto-verify on connect** (run the same non-blocking check
automatically when connecting via address/1-time link), instead of only on demand.
Either way the status stays 3-state — auto-verification can fail without blocking.

Keep the pure helpers `firstNameLink` (per-type link pick, cross-type rejection) and
`linksMatch` (scheme-normalized compare).

### 4.7 Display (`View.hs`)

A name is shown **when there is a proof to verify**, together with its status —
verified, failed, or not-yet-verified. All three states are surfaced (a failed or
pending check still shows the name, flagged), so the user sees the name and its
trust level rather than a silent omission. Rendering those three states is **out of
scope for this PR** (UI work), but the core stores the data — the 3-state status and
the proof — to drive it. A name with **no proof to verify** — a group member, or a
name on an `XInfo` profile over an established connection — is **not shown**.

### 4.8 Proof — a flat profile field, signed by the address key, context-bound

A name resolves to a **contact address** (the persistent identity link). The proof
asserts "the owner of that address asserts this name", so it is **signed by the
address's key** (the key the resolved address carries) — **not** by the
per-connection or 1-time-link key. Every proof is **bound to a presentation
context** (anti-replay), so a proof minted for one context cannot be replayed in
another; these are distinct proofs. Carry the context in a presentation header:
**rename `BadgePresHeader` (`Badges.hs:212`) → `ProofPresHeader`** (now shared by
badge and name proofs) and add a **`PHSimplexLink AConnShortLink`** constructor.
`AConnShortLink` (`Agent.Protocol.hs:1536`,
`forall m. ConnectionModeI m => ACSL (SConnectionMode m) (ConnShortLink m)`) is the
existing existential, so the context is either a 1-time invitation or a contact
address. The signed payload includes this header, and the **verifier compares the
header's link against the link the proof is presented through** (the current
invite/address) — that comparison is what makes a proof non-replayable across
links. The tag enum (`Badges.hs:200`), the `StrEncoding` (`:216`), and
`badgePresHeaderAccepted` (`:226`, → `proofPresHeaderAccepted`) each gain the new
variant.

**Type & wire** — a JSON object like `BadgeProof` (`Badges.hs:393`):

    data ClaimProof = ClaimProof
      { linkOwnerId :: Maybe OwnerId,          -- which owner signed; Nothing = root key (see below)
        presHeader  :: ProofPresHeader,         -- context: PHSimplexLink <this invite> | PHTest
        signature   :: C.Signature 'C.Ed25519   -- by that owner's key, over: smpEncode name <> smpEncode presHeader
      }

The signature is by the key of the signer's **owner identity** in the link's owner
chain (`OwnerAuth`, `Agent/Protocol.hs:1829`); `ClaimProof` carries
**`linkOwnerId :: Maybe OwnerId`** (`OwnerId`, `:1827`) to name it, so the verifier
checks exactly that key (no iterating the owner list):

- **Channels** (which can have delegated owners) sign with the user's **owner key**,
  `linkOwnerId = Just oid` — **not** the root key — `groups.member_priv_key`
  (`chat_schema.sql:193`).
- A **contact address** has a single owner = its creator, so it signs with the **root
  key**, `linkOwnerId = Nothing`.

The root key (`ShortLinkCreds.linkPrivSigKey`/`linkRootSigKey`, `:1482-83`) otherwise
only *authorizes* owners (`validateOwners`/`validateLinkOwners`, `:1846–1859`);
`Nothing` (root) is **allowed at validation**. Both keys live **chat-side** so signing
is in the chat layer — **but only channels store theirs today** (`groups`); a contact
address (`user_contact_links`, `:386`) has **no key column**, so we must add one
(mirroring `groups.root_priv_key`), captured from the 2-step short-link creation, to
sign the contact-address / 1-time-invite proofs. Signed payload =
`smpEncode name <> smpEncode presHeader` (`name` from the profile's `contactDomain`).
`presHeader` serialises as its `StrEncoding` string, `signature` base64url.

Per presentation context:

- **Contact address** (the name's own resolved link): presence in the address link's
  owner-signed data already proves the name — but we **include the explicit `ClaimProof`
  here too** (bound to the address) so contact verification is
  one uniform path (always check the proof) rather than presence-for-addresses /
  proof-for-invites. The address can also serve as the **badge** proof's context.
- **1-time invitation**: include a proof **signed by the address key, bound to the
  1-time link** as context. Feasible **now** — the 1-time link is a unique,
  single-use anti-replay context; no general mechanism required. This is what makes
  a 1-time-invite contact verifiable.
- **`XInfo` over an established connection**: no link context → the augmentation stamps
  `PHTest` (unbound) → unverifiable → not shown. The deferred gap, same as the badge's
  `PHTest`. (For **group members** the proof is dropped entirely by redaction, §4.5.)

Home: **inside the profile** — `Profile.contactDomainProof :: Maybe ClaimProof`, a flat
sibling of `contactDomain`/`contactLink`, exactly like `Profile.badge`. It is **not**
stored on the own profile; like the badge, it is **generated fresh and added to the
outgoing profile at send/save** — when the profile is sent to a peer (`XInfo`) or saved
to a link — by signing `name <> presHeader` with the address root key and stamping the
**destination as the `presHeader` context**. Because `presentUserBadge`
(`Internal.hs:2037`) already augments the outgoing profile with the badge proof at *both*
peer-sends and link-data writes, the name-proof augmentation belongs in the **same
function**. The context decides whether a proof is useful: saving to a contact address /
1-time invite stamps `PHSimplexLink(that link)` (verifiable — the in-scope cases); an
established-connection peer-send stamps `PHTest` (unbound — the deferred gap, same as the
badge). The receiver verifies and stores the 3-state status on
`LocalProfile.contactDomainVerification`, as `localBadge` carries the badge status.
Connect-by-name is **born verified**.

**Scope:** the **useful** (`PHSimplexLink`) proofs — stamped when the profile is saved to
a contact address or 1-time invite — are in this change; the verifier resolves the name →
address → address key → checks the signature and the `presHeader` link. Channels use
presence (below). The contextless established-connection case (a `PHTest` name proof) and
group members are the deferred gap.

Badges stay **in the profile** — person-scoped, presented per connection
(`presentUserBadge`, `Internal.hs:2037`), including over established connections via
`XInfo`. They share the presentation-header context type with name proofs but sign
with the **badge credential key**, not the address key. Two scopes, two homes.

The shared augmentation stamps **both** proof kinds the same way: `PHSimplexLink(link)`
when the destination is a link (contact address, 1-time invite — verifiable), `PHTest`
over an established connection (unbound — the gap). So name proofs and badges both use
`PHTest` only for the contextless established-connection case.

No group proof field: a channel is always joined via its **join link** (its own
address), whose owner-signed data already carries `groupDomain`, and there is no
"carrying link ≠ resolved address" case for channels — so channels verify by
presence/link-match and need no `ClaimProof`. (If full symmetry is wanted later, add
`GroupProfile.groupDomainProof` the same way.)

### 4.9 Set-name API (in scope — currently missing)

Names are **pre-registered out of band** — the app does **not** call RNAME. The API
only **verifies** the name and **adds it to the profile**. The user must be able to
add/change/remove their own name from the UI; the branch has no command for the
**contact** name.

- **Contact name** — add `APISetUserName :: Maybe SimplexNameInfo -> ChatCommand`
  (`Nothing` clears). On set:
  1. Require an **address** — fail if none (the UI won't offer the action without one).
  2. Require it to be a **short link** — if only a long link exists, create the short
     link (the name resolves to a short link, and the check below is short-link-based).
  3. Ensure that short link is **in the profile** (`contactLink`) — add it if missing.
  4. **Verify**: resolve the name and compare the short link it points to against the
     profile's `contactLink`; fail if they don't match (name not preregistered to this
     address).
  5. Set `LocalProfile.contactDomain` and **re-publish the contact-address link data**.
     The `ClaimProof` is added when the profile is saved to that link (the send/save
     augmentation, §4.8) — **not** produced by the API itself. (Steps 1–4 are the verify;
     this step is set + re-publish.)
  Needs a `ChatCommand` constructor + parser + handler.
- **Channel name** (public, relay-backed groups only) — a **separate**
  `APISetPublicGroupName`, parallel to the contact one (not folded into
  `SetPublicGroupAccess`): same require-address / require-short-link
  / link-in-profile / verify / set-`groupDomain` flow against the channel's **join
  link**. Rationale: it mirrors the contact API, and the name's fail-able verify +
  preconditions don't mix cleanly with the plain `web=`/`embed=`/`domain_page=` writes.
  **Drop `domain=` from `SetPublicGroupAccess`** (`Commands.hs:5461`) so there's a single
  verified path; factor out the shared `GroupProfile`-update + `XGrpInfo` broadcast so
  both commands reuse it. Verify is **TLD-dependent**: resolve+compare for `TLDSimplex`,
  a different/no check for `TLDWeb` (web domains don't resolve through the namespace).
- The own name carries **no stored verification status** — the verify step checks it at
  add time; the 3-state status is only for peers' names. No RNAME plumbing (out of scope).

## 5. Removal checklist (from the current branch)

- `Contact.simplexName`, `GroupInfo.simplexName`, `Connection.simplexName`.
- `*VerifiedAt` timestamps (→ `Maybe Bool` status fields).
- `connections.simplex_name` column + the `createConnection_` carrier param + the
  `XInfo` carrier consumption in `Subscriber.hs`.
- `contacts.simplex_name`, `groups.simplex_name` columns.
- The four partial UNIQUE indexes + `clearConflictingContactProfileSimplexName_`
  / `clearConflictingGroupProfileSimplexName_` + their call sites.
- `getContactBySimplexName` / `getGroupIdBySimplexName` against entity columns
  (re-point or remove per 6.b).

## 6. Resolved decisions

a. **Wire-string encoding (§4.1):** `SimplexNameInfo` keeps object JSON; wire
   fields are wrapped in `StrJSON` (string), `LocalProfile` stays unwrapped
   (object). Channel name reaches the UI as a **string** (via `GroupProfile`) and
   that is fine — no decoded-object field on `GroupInfo` (no reason to re-connect
   to a channel you're in; channel names are only shown verified). The object form
   matters for **contacts** (connect-from-groups, sharing), which `LocalProfile`
   provides. Residual chore: teach the bot-API binding generator to emit `string`
   for `StrJSON` fields and pick the `StrJSON` `name` Symbol.
b. **Lookup (§4.4):** re-point `getContactBySimplexName` (its one caller is
   connect-by-name, `Commands.hs:4241`) to the **verified** `contact_profiles.
   contact_domain`; miss/unverified ⇒ resolve-and-connect. Keeps the no-network
   shortcut for already-known contacts. (`getGroupIdBySimplexName` has no external
   caller — drop it.)
c. **Proof (§4.8):** a flat **`Profile.contactDomainProof :: Maybe ClaimProof`**, a wire
   profile field like `Profile.badge`. Not stored on the own profile; **generated fresh and
   added to the outgoing profile at send/save** (peer `XInfo` or save-to-link) by the
   **same augmentation function** as the badge (`presentUserBadge`, which already runs at
   peer-sends *and* link-data writes). Signed by the signer's **owner-identity key** — a
   channel's delegated **owner key** (`groups.member_priv_key`, `linkOwnerId = Just oid`)
   or a contact address's **root key** (sole owner, `linkOwnerId = Nothing`) — over
   `name <> presHeader`; `linkOwnerId` selects the verification key (`Nothing` = root,
   allowed at validation). `PHSimplexLink` for address/invite saves, `PHTest` over
   established connections (the gap). Verify checks the signature **and `presHeader`'s link
   == the link actually used** (`connLinkToConnect`). Channels use presence (owner-signed
   link data). **Gap:** the contact-address key isn't stored chat-side today
   (`user_contact_links` has no key column) — add one to sign chat-side. Receiver status
   on `LocalProfile.contactDomainVerification`.
d. **`redactedMemberProfile` contactLink exposure (§4.5):** intended — a member's
   contact address becomes group-visible when links + DMs are allowed.
e. **Verify command + status (§4.6):** keep `apiVerifySimplexName` — required to
   verify a claimed name for entities connected **not** by name (address / 1-time
   link). Status is **3-state** because a failed name (or badge) verification must
   **not** block the connection; connect-by-name is the only born-verified path
   (and there a resolve/consent failure means no connection, not a failed state).
   **Auto-verify on connect** (vs. on-demand only) is left open; it doesn't change
   the 3-state requirement.

## 7. Rollout & scope

The **consent gate** (§4.6) — a name resolves only if the **resolved link's own
data claims it** — is the anti-stray-names protection and **must ship regardless**:
a name someone registers against a real address must not "work" without that
address owner's consent. It needs no signature (the address-case presence suffices),
so it lands with the core change.

The signed proof (§4.8) can ship in the **same** release (add + verify together) or
be **staged** — implemented at the core level with the user-facing name-addition
hidden in the UI until ready. Either way it stays a *core* change.

The `ProofPresHeader` rename + `PHSimplexLink` + letting badges opt into the link
context ripples through the badge code — accepted, and bounded:

- `Badges.hs`: `BadgePresHeader` → `ProofPresHeader`, `BadgePresHeaderTag` →
  `ProofPresHeaderTag`, `badgePresHeaderAccepted` → `proofPresHeaderAccepted`
  (`:200, :212, :216, :226`); add the `PHSimplexLink AConnShortLink`
  tag/constructor/`StrEncoding`/accepted-case; `badgeProof` (`:314`) takes the
  renamed type.
- `presentUserBadge` (`Internal.hs:2037`) + its ~15 call sites — the **shared profile-
  augmentation** point (already runs at peer-sends *and* link-data writes): generalize it
  to stamp **both** the badge proof and the name `ClaimProof` onto the outgoing profile,
  using `PHSimplexLink link` where the destination is a link, `PHTest` otherwise.

The pure rename can land as a **standalone prep commit** ahead of the proof; the
`PHSimplexLink` plumbing + name proof land with the proof work.
