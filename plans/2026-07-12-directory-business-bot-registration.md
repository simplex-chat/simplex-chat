# Directory registration of businesses and service bots (via signed contact card)

Status: draft plan for review. Grounded against the current tree (branch `ep/improve-names-2`).

## 1. Goal

Let a business or a service (chat bot) operator register their **contact `/a` address** in
the directory by **forwarding a signed contact card** to the directory bot — exactly the
UX we already have for channels (`/share chat #ch @'SimpleX Directory'`), but for a contact
address instead of a channel link.

**Guiding principle: the flow is the channel registration flow verbatim — owner-signed card, admin
approval, re-approval on any profile change, the same periodic link-check loop — differing only in
the listing type (a contact `peerType`, not a group). Where a detail is unspecified here, the answer
is "whatever channels do."**

Product decisions from the discussion, baked into this plan:

- **The owner sends the card, and the signature is the authorization.** The `ownerSig` (signed
  with the address key) is what proves the address owner authorized the listing — only the key
  holder can produce it. We deliberately do NOT use a "directory connects and asks the owner to
  confirm" double opt-in (it is a spam vector, like any mailing-list signup). "Submitter ≠ owner"
  is handled not by letting non-owners submit, but by giving the owner's tooling a way to send (the
  support-bot entry point that would cover the headless case is deferred — §B.4).
- **The directory does NOT connect to / join these addresses.** They are not groups. It verifies
  the signature via a link-data *fetch* (not a connection) and records the address in a new table.
- **One table for both businesses and bots** — they are all contact `/a` addresses. The MVP types
  each accepted registration by `peerType`: a **bot** requires `peerType == CPTBot`; a **business**
  requires `peerType ∈ {CPTHuman, CPTBusiness}` (an unset `peerType` counts as `CPTHuman`); an
  unrecognized `CPTUnknown` is rejected. The admin then manually verifies a business before
  approving — as for channels.
- **Listing type = `ChatPeerType`.** Extend `ChatPeerType` (today `CPTHuman | CPTBot`, `Types.hs:710`)
  with **`CPTBusiness`** and **`CPTUnknown Text`** (forward-compat, like `GTUnknown`), and make the
  decoder **lenient** (unknown tag → `CPTUnknown`) so this version won't choke on future tags.
  **Wire-compat caveat (verified):** `ChatPeerType` decodes strictly today
  (`textDecode … _ -> Nothing`, `Profile` via `deriveJSON`), so a present-but-unknown `peerType`
  makes an *already-deployed* app fail to parse the whole profile — it does **not** downgrade to
  human. So a business must **not** publish `CPTBusiness` yet (old apps couldn't reach it); a
  business's profile stays `CPTHuman` in practice, with `CPTBusiness` reserved for later. The MVP
  types a **bot** from `peerType == CPTBot` and a **business** from `peerType ∈ {CPTHuman,
  CPTBusiness}` (unset ≙ `CPTHuman`), rejecting `CPTUnknown`; it stores the resolved type
  (`CPTBot`/`CPTBusiness`) on the listing. When the lenient version is broadly adopted, businesses
  can publish `CPTBusiness` directly.
- **`peerType` and `businessAddress` are orthogonal, and the directory ignores `businessAddress`.**
  `businessAddress` chooses the *conversation type* a connector gets (a business chat / group vs a
  direct 1:1); it can be set by non-businesses, and a real business may run a plain direct-chat
  address. The directory does **not** use it to classify — the type comes from the profile's
  `peerType` (bot vs human/business, above), not from `businessAddress`. (App-side only, unchanged:
  the connect-preview briefcase shows when **either** `businessAddress` or `peerType == CPTBusiness`;
  bot cube from `peerType`, else person — in the MVP that briefcase comes from `businessAddress`.
  Separate from directory classification.)
- **Description lives on the contact `Profile`** (new `description` field, parallel to
  `GroupProfile.description`). In group-member profiles it is **redacted per the group's policy —
  the same treatment `shortDescr` gets** (links/names stripped when the group prohibits them), not
  removed wholesale. It is carried **full** in the direct contact view, the address link preview,
  and the directory. See §G.
- **`peerType` + `description` are visible in the app independent of the directory** (that is why
  owners will set them). `peerType` drives the type icon in the pre-connect alert
  (`ConnectPlan.kt:698-713`) and a marker in the chat list / chat banner. The compact surfaces (the
  alert, the shared-link card) are too small for the large `description`, so it appears via a
  **"Read more"** affordance in the **chat banner** (`ChatView.kt` `ChatBannerView`) and the
  **contact info page** (`ChatInfoView.kt:778`) that opens the full text in a sheet (iOS) / alert
  (Kotlin). These are NOT the welcome/auto-reply message (`AddressSettings.autoReply`, transient
  on-connect). Full details in §H.

Deliverables: (a) an API + CLI to prepare and share the signed contact card; (b) the
`Profile.description` field; (c) directory handling that verifies and stores the address; (d) admin
approval, web listing, and search. (A support-bot entry point for headless businesses is out of
scope for now — §B.4.)

## 2. End-to-end flow

```
Operator's client                         Directory bot
-----------------                         -------------
/share address @'SimpleX Directory'
  -> get own /a address (short link,
     businessAddress flag, root key)
  -> build MCChat { chatLink =
        MCLContact {connLink, profile, business},
        ownerSig = sign(rootPrivKey,
                        chatBinding <> connLink) }   ── card ──▶  DEChatLinkReceived (MCLContact, ownerSig)
                                                                    -> APIConnectPlan (PLAN only, no connect)
                                                                       fetches link data (opaque) + verifies sig
                                                                       => CPContactAddress (CAPOk {ownerVerification})
                                                                    -> if OVVerified:
                                                                         addContactReg (bot if CPTBot,
                                                                           else business), status pending
                                                                         notify admins with profile (admin verifies)
admins: /approve ...                                              -> status active -> listingsUpdated
                                                                    -> web listing.json + bot search include it
```

Nothing is connected or joined. The only network action on the directory side is a one-time,
opaque link-data fetch for signature verification (consistent with the established rule that
the directory may fetch link data, only name *resolution* leaks membership).

## 3. What already exists (reuse map)

All grounded in the current tree:

- **Chat-link card type** — `MCLContact {connLink :: ShortLinkContact, profile :: Profile, business :: Bool}`
  already exists (`src/Simplex/Chat/Protocol.hs:769`). `MCChat {text, chatLink, ownerSig}` and
  `LinkOwnerSig {ownerId, chatBinding, ownerSig}` at `Protocol.hs:764,774`.
- **Owner-signature verification for contact addresses is already wired.** `connectPlan`'s
  `CTShortContact CCTContact` path fetches `FixedLinkData {rootKey}` + `UserContactData {owners}`
  and computes `ov = verifyLinkOwner rootKey owners l' sig_`, surfaced as
  `CPContactAddress (CAPOk {contactSLinkData_, ownerVerification})`
  (`src/Simplex/Chat/Library/Commands.hs:4287-4289,4518-4527`; `Controller.hs:1114-1121,1139-1142`).
  For plain/business addresses `owners == []`, so `ownerId = Nothing` and verification uses the
  link **root key** (`verifyLinkOwner` fallback).
- **The directory already receives any `MCChat` card as `DEChatLinkReceived`** — `Directory/Events.hs:108`
  turns `(MCChat {chatLink, ownerSig}, Nothing)` into `DEChatLinkReceived`. Today `deChatLinkReceived`
  only matches `MCLGroup` and otherwise replies "Only channels can be added to directory via link."
  (`Directory/Service.hs:964-979`). We add an `MCLContact` case.
- **Card-sharing UI + API + signing** — `/share chat #g @to` → `SharePublicGroup`
  (`Commands.hs:2437-2449`, parser `Commands.hs:5551`) → `APIShareChatMsgContent`
  (`Commands.hs:1136-1170`) which builds the `MCChat` and signs with `mkLinkOwnerSig` +
  `shareChatBinding` (binds the card to the recipient connection, anti-replay).
- **Address key + business flag storage** — `link_priv_sig_key` (the address root private key,
  Ed25519) is stored in `user_contact_links` by `createUserContactLink`
  (`src/Simplex/Chat/Store/Profiles.hs:429-439`); `businessAddress` lives in `AddressSettings`
  (`Profiles.hs:497-502`) and is published as `ContactShortLinkData.business`
  (`Commands.hs:4528-4533`, `Protocol.hs:1584-1592`). Note: `getUserAddress`/`UserContactLink`
  do **not** currently read `link_priv_sig_key` back (`Profiles.hs:479-524`).
- **Directory store / listing / web infra** — `sx_directory_group_regs` table
  (`Directory/Store/{SQLite,Postgres}/Migrations.hs`), `GroupReg`/`GroupRegStatus`
  (`Directory/Store.hs:116-226`), `getAllListedGroups_` (`Store.hs:354-363`), `generateListing`
  (`Directory/Listing.hs:148-170`), `DirectoryEntry`/`DirectoryEntryType = DETGroup`
  (`Listing.hs:55-86`), website renderer `website/src/js/directory.jsc`.

## 4. Work items

### A. Protocol / types

- `MCLContact` exists; no new protocol message for the card itself.
- **Extend `ChatPeerType`** (`Types.hs:710`, today `CPTHuman | CPTBot`) with `CPTBusiness` and
  `CPTUnknown Text` (forward-compat, like `GTUnknown`). Update the `TextEncoding`/JSON instances
  (`Types.hs:724-731`): encode `CPTBusiness` as `"business"` and `CPTUnknown t` back to `t`
  (round-trips the original tag); make `textDecode` **lenient** — an unrecognized tag becomes
  `CPTUnknown t` instead of `Nothing`, so this version never fails to parse a profile with a future
  tag. **Verified constraint:** the *current* decoder is strict (`_ -> Nothing`) and `Profile`
  is `deriveJSON`-parsed, so an already-deployed app fails the whole profile on an unknown `peerType`;
  therefore `CPTBusiness` must not be published on profiles until the lenient version is broadly
  adopted. **MVP:** the directory types a **bot** from `peerType == CPTBot` and a **business** from
  `peerType ∈ {CPTHuman, CPTBusiness}` (unset ≙ `CPTHuman`; stored as `CPTBusiness`), and **rejects
  `CPTUnknown`**. Businesses are then admin-verified — the admin is the gate, as for channels.
- **New optional `description :: Maybe Text` on `Profile`** (`Types.hs:693`), parallel to
  `GroupProfile.description` (`Types.hs:867`). Additive/nullable — only businesses/bots set it.
  It rides into the address link data automatically (`ContactShortLinkData` embeds the whole
  `Profile`, `Protocol.hs:1584`), so the directory reads it from the fetched link data. It is
  redacted per group policy in group-member profiles, on **both send and receive** (see §G). No
  version bump is needed — `Profile` is `deriveJSON`-parsed and aeson ignores unknown keys, so old
  apps just drop `description` (same as when `peerType`/`badge`/`contactDomain` were added).
- **Setting `peerType`/`description` (for tests + eventual UI).** Both are plain `Profile` fields, so
  they ride through the existing profile-update path (`APIUpdateProfile` / the `/p` command); tests
  drive them via `/_profile`. A small dedicated setter for the multi-line `description` is worth
  adding for CLI ergonomics. The app-UI toggle to set `peerType = CPTBusiness` is deferred (per the
  wire-compat caveat above).

### B. Client: prepare + share the contact-address card

1. **Signing key — from the agent, not the chat DB.** Sign the card with the address short-link key
   via `getConnLinkPrivKey (aConnId addressConn)` (already in the agent, used at `Subscriber.hs:1649`;
   `getUserAddressConnection` gives the connection). This is the authoritative key — the private half
   of the short link's root key the directory verifies against — and it exists whenever the short link
   does, **including right after an upgrade** (`setConnShortLink` provisions it). Do **not** read the
   chat-DB `link_priv_sig_key` for signing: it is written only at `createUserContactLink` and never on
   upgrade. *(Separate cleanup, off the signing path: persist `link_priv_sig_key` on upgrade too —
   `setMyAddressData`/`setUserContactLinkShortLink` — reading it back via `getConnLinkPrivKey` so the
   column stops being stale.)*
2. **Card-builder API — `APIShareMyAddress {toSendRef :: SendRef}`** (Controller) + handler in
   `Commands.hs`, mirroring the group-share case (`APIShareChatMsgContent`, `Commands.hs:1136`):
   - `getUserAddress` → `connLinkContact` (short link) + profile + `businessAddress`.
   - `getUserAddressConnection` → conn; `getConnLinkPrivKey (aConnId conn)` → `rootPrivKey`
     (`Nothing` ⇒ not upgraded → error; the UI pre-empts this via §B.5).
   - hoist `shareChatBinding` to top-level; `binding <- shareChatBinding user toSendRef`.
   - `ownerSig = LinkOwnerSig {ownerId = Nothing, chatBinding = B64UrlByteString cb,
     ownerSig = C.sign' rootPrivKey (cb <> smpEncode connShortLink)}` (contact variant of
     `mkLinkOwnerSig`, `ownerId = Nothing` so the directory verifies against the link root key).
   - return `CRChatMsgContent user (MCChat {text, chatLink = MCLContact {connLink, profile, business}, ownerSig})`.
   `SendRef` covers direct **and** group/channel targets.
3. **CLI command — `ShareMyAddress {toChatName}`**, parser `/share address @to` / `/share address #to`
   (`Commands.hs:5551` neighborhood), handler mirroring `SharePublicGroup` (`Commands.hs:2437-2449`):
   resolve `toChatName` → `SendRef` → `APIShareMyAddress` → `APISendMessages`. Shares to contacts and
   groups/channels alike.
4. **Support-bot entry point — OUT OF SCOPE (deferred).** A headless business running
   `apps/simplex-support-bot` (TypeScript, no app UI) will eventually need a way to trigger the
   share — a bot admin/config command that calls `APIShareMyAddress` against the directory contact
   once connected. Deferred; the core `APIShareMyAddress`/`/share address` path built here is exactly
   what it will call.
5. **App UI — "Share via chat" (Phase 1; mirrors the channel share).** The receiving/rendering half
   already exists from the channel work (`MsgChatLink.Contact`, `CIChatLinkHeader`, the compose
   preview, and the `SharedContent → ShareListView → ComposeView` picker). New pieces: the entry
   point, a `SharedContent.AddressLink` case, the `apiShareMyAddress` call, and the upgrade branch.
   - **Entry point:** a **"Share via chat"** button (reuse the channel string) in the user's own
     address screen (`UserAddressView.kt`), beside the existing OS-share "Share" button. Address
     creation lands on this same screen (`createAddress` sets `userAddress`, `UserAddressView.kt:73-84`
     — verified), so the button is visible immediately after creating an address.
   - **Flow:** tap → if `userAddress.shouldBeUpgraded` (old full address) show an **upgrade alert**
     ("To share your address in a chat it will be upgraded to a short link. All your contacts stay
     connected."), buttons **[Upgrade & share]** / **[Cancel]** — on confirm: spinner →
     `apiAddMyAddressShortLink`, **then** continue (two separate API calls, cleaner errors); no
     "share old" option. Then set `SharedContent.AddressLink` → `ShareListView` (contacts +
     groups/channels, with the simplex-link prohibition filtering) → pick destination → `ComposeView`
     `LaunchedEffect` calls `apiShareMyAddress` → sets the existing `ChatLinkPreview` → optional
     message text (same UX as the channel share) → **Send** → the recipient sees the existing
     `CIChatLinkHeader` card and taps to connect.
   - iOS mirrors this via the existing channel-share flow (`f49d98511`); Kotlin per
     `plans/2026-04-17-kotlin-share-channel-link.md`.

### C. Directory: verify + store (no connect)

1. **`deChatLinkReceived` — add the `MCLContact` case** (`Directory/Service.hs:964`).
   **No new verification code** — reuse the exact plan path channels use (resolved, Q1):
   - `deChatLinkReceived ct (MCLContact {connLink, profile, business}) (Just ownerSig)`:
     - `APIConnectPlan userId (contact link) PRMAll (Just ownerSig)` — **plan only**, no connect
       (rename `PRMAllGroups` → `PRMAll` and make it work for contact links too, not just groups).
       `connectPlan`'s contact-address path already computes `ov = verifyLinkOwner rootKey owners l' sig_`
       (`Commands.hs:4288`), identical to the channel path at `Commands.hs:4346`; for a plain/business
       address `owners == []`, so the card's `ownerId = Nothing` makes `verifyLinkOwner` verify against
       the link **root key**. Expect `CPContactAddress (CAPOk {contactSLinkData_ = Just csld, ownerVerification})`.
       Use the **fetched** `csld.profile` (`peerType`, `description`, name claim, …) as authoritative
       — the card's copies are display-only / potentially stale; `csld.business` is not used for
       typing.
     - `OVVerified` → type from the fetched profile's `peerType`: **bot** if `CPTBot`, **business**
       if `CPTHuman`/`CPTBusiness` (unset ≙ `CPTHuman`; stored as `CPTBusiness`) → `addContactReg`
       status pending; **reject `CPTUnknown`** ("unsupported account type"). A business is then
       admin-verified before approving (as for channels). `OVFailed reason` → "ownership verification
       failed". `CAPKnown`/other → appropriate message.
   - The fetch is intrinsic (the root public key isn't in the card, same as channels) and is an
     opaque link-data read, not a connection — consistent with the established directory rule.
   - Keep the existing `MCLGroup` and fall-through cases unchanged.
2. **New store: `sx_directory_contact_regs`.** Add a migration to
   `Directory/Store/SQLite/Migrations.hs` and `Directory/Store/Postgres/Migrations.hs` (new named
   migration appended to `schemaMigrations`). Proposed columns:
   ```
   contact_reg_id       PK autoincrement
   user_contact_reg_id  INTEGER            -- per-submitter sequence (cf. user_group_reg_id)
   submitter_contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE
   conn_short_link      TEXT    NOT NULL    -- the contact link; the LISTING IDENTITY (stable for
                                            --   contacts). A link change ⇒ unlist + re-register.
                                            --   Must be present in the fetched profile (contactLink).
   display_name         TEXT    NOT NULL
   full_name            TEXT
   short_descr          TEXT
   description          TEXT                -- long description (Profile.description)
   image                TEXT                -- base64, optional
   peer_type            TEXT    NOT NULL    -- resolved listing type: "bot" or "business" (ChatPeerType)
   simplex_name         TEXT                -- verified SimpleX name, optional (see Q5)
   reg_status           TEXT    NOT NULL
   promoted             INTEGER NOT NULL DEFAULT 0
   created_at, updated_at TEXT
   UNIQUE(conn_short_link); UNIQUE(submitter_contact_id, user_contact_reg_id)
   ```
   Records store the profile inline (`display_name`, `description`, `image`, `peerType`, …) —
   self-contained, no FK to a joined contact, since we never connect. Reuse **`GroupRegStatus`**
   (resolved, Q3) for `reg_status` — same states as channels. New `Directory/Store.hs` data +
   functions mirroring the `GroupReg` ones: `ContactReg`, `addContactReg`, `setContactRegStatus`,
   `deleteContactReg`, `getContactRegBy{Id,Link}`, `getAllListedContacts`, and a search query.
3. **Registration lifecycle mirrors channels.** `proposed → pending approval → active`, plus
   `suspended/removed`. On submission, notify admins with the profile + an approve command. The
   directory **re-reads the address links in the same periodic loop as channels** (resolved, Q6 —
   `deGroupLinkCheck`, `Service.hs:832`): re-fetch the link data, refresh the stored profile, and
   a profile change triggers **re-approval** (hidden until re-approved), exactly like a channel
   profile change (`reapprove`, `Service.hs:858`). The loop is the same as channels; a contact
   address has a single key and no group membership, so the channel `checkValidOwner` owner-list
   re-check has no analog and doesn't run — which also means the empty-`linkOwners` false-delist bug
   can't arise, and `UNIQUE(conn_short_link)` (below) prevents the duplicate-row class that triggered
   it. **Re-submission of an already-registered link (to research + propose):** mirror the channel
   re-registration path (`deReregistration`) — upsert the existing reg and send it back to admin
   review on any change, rather than erroring.
4. **Admin & user commands — same commands, extended with a chat type** (resolved, Q4). Reuse the
   existing command constructors and syntax; carry a chat-type discriminator on the id token — `#`
   for a group (existing), `@` for a contact address — e.g. `/approve @<id>:<name> <version>`,
   `/list @...`, `/suspend @...`, mirroring the group forms. The `@`/`#` prefix disambiguates the
   overlapping id spaces (a `group_id` and a `contact_reg_id` both start at 1), so no parallel
   command names are needed. Extend the command constructors with the chat type, extend
   `Directory/Events.hs` `directoryCmdP` to parse the prefix, and branch on it in `Service.hs`
   `deSuperUserCommand`/`deUserCommand`.
5. **Link identity + verified SimpleX names (resolved).** The contact **link is the listing
   identity** (for contacts the link is expected to be stable). Two conditions:
   - **Link present in the profile.** For listing, the fetched profile must declare this link —
     `Profile.contactLink` present and equal to the registered link. If the link **changes** (or the
     profile stops declaring it), the address is **unlisted** and must be re-registered — the link is
     the anchor, not a mutable attribute.
   - **Name↔link consistency, verified inline.** If the profile claims a SimpleX name
     (`Profile.contactDomain`), resolve that name and confirm it points to **this** link, comparing
     inline — the reverse direction of the existing by-name plan path (`Commands.hs:4272-4281`,
     `contactDomain`/`nameResolvesTo`). A **name change** re-runs this check. On success, populate
     `sx_directory_contact_regs.simplex_name` → flows to `DirectoryEntry.simplexName` and bot/web
     search. Reuse `plans/2026-06-25-name-resolution.md` and the group-names work.
   *(To research + propose: how the directory detects a link change on re-read, whether an address's
   published profile actually carries `contactLink == its own link`, and the exact resolve-and-compare
   call for the link → name-claim → link round-trip.)*

### D. Listing + web

1. **`DirectoryEntryType`** (`Listing.hs:55`): add `DETContact {peerType :: ChatPeerType}`. The
   `taggedObjectJSON`/`dropPrefix "DET"` derivation already emits `{"type":"contact", ...}` for a new
   constructor for free (single→multi constructor is transparent); `peerType` serializes as
   `"business"`/`"bot"`/etc.
2. **`contactDirectoryEntry`** builder (analogue of `groupDirectoryEntry`, `Listing.hs:100`), from a
   `ContactReg` row: `DirectoryEntry {entryType = DETContact peerType, displayName, simplexName,
   groupLink = PublicLink Nothing (Just connShortLink), shortDescr` (from `Profile.shortDescr`)`,
   welcomeMessage` (from the new `Profile.description`)`, imageFile, activeAt, createdAt}`.
   `PublicLink` already models contact links (`Listing.hs:63-68`). Store the profile fields (incl.
   `description`, `peerType`) on the reg row at registration so the entry is self-contained.
3. **`generateListing`** (`Listing.hs:148`): merge group entries + contact entries into the single
   `DirectoryListing`. Feed the contact rows from `getAllListedContacts` (status active); build
   `DirectoryEntry`s from both sources and serialize together. `listingsUpdated` triggers stay as-is,
   plus fire on contact-reg status changes.
4. **Website `directory.jsc`**: branch `displayEntries` on `entryType.type` and, for contacts, on
   `entryType.peerType`:
   - business vs bot label/avatar from `peerType` (`business`/`bot`); non-group avatar fallback
     instead of `/img/group.svg`;
   - "Connect"/"Chat" affordance instead of the "N members/subscribers" line (`entryMemberCount`
     already returns 0 for non-group — `directory.jsc:183-193`);
   - join URI already works via `connShortLink` (`directory.jsc:331-348`).
   Search/filter already reads generic fields (`displayName`, `shortDescr`, `welcomeMessage`,
   `simplexName`), so text search works unchanged.

### E. Bot search

Include active contact regs in the bot's search results (`DCSearchGroup` path,
`Service.hs:1115`, backed by `searchListedGroups` in `Store.hs`) as **one unified result set** (not a
separate contact search); match on display name and SimpleX name.

### F. Tests

- **Client** (`tests/ChatTests/`): `/share address` produces an `MCChat`/`MCLContact` card with a
  valid `ownerSig` (`ownerId = Nothing`); parser test for `/share address`.
- **Directory** (`tests/Bots/DirectoryTests.hs`, mirroring `testRegisterChannelViaCard`
  `:2050` and `testDirectoryChannelName` `:2129`): register a business and a bot via card
  (verified → pending → admin approve → listed), reject on bad/absent signature, search finds it,
  and the generated `listing.json` contains a `"type":"contact"` entry with the right `peerType`
  (`business`/`bot`). Wire under the names/SMP test harness as needed.
- **Profile description** (§G): a member's `description` is **redacted per the group's policy** in
  the profile others receive in a group (send side) and when stored from an incoming member profile
  (receive side) — links/names stripped when the group prohibits them, clean prose passing through;
  a direct contact / address preview keeps it full.

### G. `Profile.description` field + member-profile redaction (resolved)

`description :: Maybe Text` is added to `Profile` (§A). In group-member profiles it is **redacted
per the group's policy — the same treatment `shortDescr` gets today** (not removed wholesale):
links and SimpleX names are stripped when the group prohibits them.

1. **Send side** — in `redactedMemberProfile` (`Internal.hs:1266-1277`, which already redacts
   `shortDescr`/`contactLink`/name-proof under the group's `SGFSimplexLinks`/`SGFDirectMessages`),
   also redact `description` — with a **new inline-strip helper** (per G.3), not `shortDescr`'s
   drop-whole `removeSimplexLink`. Adding `description` to `Profile` forces this output record to be
   rebuilt here anyway. (Used on every member-profile-out path — `Internal.hs:1254,1262`,
   `Subscriber.hs:851,3273`, `Commands.hs:4134`.)
2. **Receive side** — apply the same redaction when ingesting a member profile from the network, so
   a peer can't inject a link/name-laden description. Chokepoints: `updateMemberProfile`
   (`Store/Groups.hs:3407`) and member creation (`Store/Groups.hs:2510`, `1395`); prefer a single
   helper mirroring the send-side redaction.
3. **Redaction granularity (RESOLVED).** **Inline-strip links and names** — drop the
   `Uri`/`HyperLink`/`SimplexLink`/`SimplexName` (the `isLink` set, `Markdown.hs:184`) and `Mention`
   spans via `parseMaybeMarkdownList`, re-concat the remaining `FormattedText`, keep the prose (empty
   result ⇒ `Nothing`). **Exception:** if `hasObfuscatedSimplexLink` matches (a link that can't be
   cleanly isolated as a token), drop the **whole** description.
4. **Kept full where wanted** — the address link data (`ContactShortLinkData` embeds the full,
   unredacted profile), the direct contact profile view, and the directory listing all carry the
   full `description`. Group redaction applies only to member-profile *delivery into a group*, a
   separate code path. For the **directory** page, abuse is gated by **admin review** (Q7), not an
   automatic filter.
5. **UI/UX** — add a multi-line "Description" field to the profile/address editor (app UI, follow-on
   with §B.5). Because the field can carry into groups (redacted), an edit-time hint that links and
   names won't show where a group prohibits them is worthwhile, mirroring `shortDescr`.

### H. App visibility of `peerType` + `description` (why owners will set them)

These are persistent profile identity shown to everyone who reaches the address — independent of the
directory. That is the reason to fill them in; the directory is a bonus channel. Existing surfaces
(multiplatform paths; iOS/Android mirror them):

**`peerType` — type icon / badge** (small, already-present surfaces):
- Pre-connect "Open chat?" alert (`newchat/ConnectPlan.kt:698-713`) — type icon + verification;
  briefcase when **either** the address `business` flag or `peerType == CPTBusiness`, bot cube from
  `peerType`, else person (see §1). The alert holds no description (too small — `AlertManager.kt:289`).
- Chat list (`chatlist/ChatPreviewView.kt:188`, `isBot`) and the chat banner
  (`chat/ChatView.kt:2227` `ChatBannerView`, which already has per-type captions — bot / business /
  contact) — extend to a business marker from `peerType`.

**`description` — shown via a "Read more" affordance, NOT inline** (the alert and the in-chat link
card `CIChatLinkHeader.kt` are too small — they carry only the short teaser). Rendered in **two
surfaces: the chat banner (`ChatBannerView`) and the contact info page (`ChatInfoView`, `:778`)**:
- Teaser text: if `shortDescr` is present → show `shortDescr`, then a clickable **"Read more"**; if
  `shortDescr` is absent → show the first line of `description` truncated to 100 chars with ellipsis
  (up to the first line break), then **"Read more"**. "Read more" appears only when a `description`
  exists to reveal.
- **"Read more" is a general, extensible `Modal` markdown element.** Add an inline `Format` variant
  `Modal {modalName :: Text}` to `Markdown.hs:51` (sibling to `Command`/`Mention`/`SimplexLink`) —
  **no `showText`**: the app resolves both the tappable label and the modal content from the current
  chat by `modalName` (e.g. `modalName = "description"` → renders "Read more", opens the contact's
  `description`). `Format`'s existing `Unknown` fallback (`parseJSON … <|> pure (Unknown v)`,
  `Markdown.hs:533`) makes it forward-compat — old apps decode it as `Unknown`. The **teaser is built
  app-side** from the profile fields (d3), so the Haskell core just adds the variant + JSON so the
  app's mirrored enum matches; each client renders the tap (iOS sheet / Android modal), reusing the
  existing tappable-markdown mechanism (no iOS multiline-hit-test hack).
- This is NOT the welcome/auto-reply message (`AddressSettings.autoReply`, a transient on-connect
  message), and NOT shown in the pre-connect alert or the shared-link card.

**Profile editor** (`usersettings/UserProfileView.kt`) — add the multi-line description field and a
way to set the account type (`peerType`). Note: the editor exposes two separate "business" concepts
— `peerType` (identity) and the `businessAddress` conversation-type setting — which must use distinct
labels, since both otherwise read as "business."

Note: before connecting, the only surface with room to read the full description is the directory web
page; in-app it is the banner/info "Read more" once the (prepared) chat is open.

## 5. Files to touch (summary)

- `src/Simplex/Chat/Types.hs` — extend `ChatPeerType` (`CPTBusiness`, `CPTUnknown`, lenient decode);
  add `Profile.description`; JSON/TextEncoding derivations.
- `src/Simplex/Chat/Markdown.hs` — add the `Modal {modalName}` `Format` variant + JSON (§H).
- App views (Phase 1, §B.5/§H) — `UserAddressView.kt` ("Share via chat" button + upgrade branch),
  `ChatInfoView.kt` + `ChatView.kt` `ChatBannerView` (description teaser + `Modal` "Read more"), the
  Kotlin/Swift `Format` mirror (`Modal` case + tap → sheet/alert) (+ iOS equivalents). The `peerType`
  badge/editor UI is deferred.
- `src/Simplex/Chat/Controller.hs` — `APIShareMyAddress`, `ShareMyAddress` command constructors.
- `src/Simplex/Chat/Library/Commands.hs` — handlers + parsers for the two new commands; reuse
  `shareChatBinding`.
- `src/Simplex/Chat/Library/Internal.hs` — redact `description` per group policy in `redactedMemberProfile` (send side, §G).
- `src/Simplex/Chat/Store/Groups.hs` — redact `description` when ingesting a member profile (receive side, §G).
- `src/Simplex/Chat/Store/Profiles.hs` — persist `link_priv_sig_key` on short-link upgrade
  (`setUserContactLinkShortLink`/`setMyAddressData`); card signing uses the agent's
  `getConnLinkPrivKey`, not this column.
- `apps/simplex-directory-service/src/Directory/Service.hs` — `MCLContact` case in
  `deChatLinkReceived`; contact-reg lifecycle + admin/user commands; listing trigger.
- `apps/simplex-directory-service/src/Directory/Store.hs` — `ContactReg` model + queries.
- `apps/simplex-directory-service/src/Directory/Store/{SQLite,Postgres}/Migrations.hs` — new table.
- `apps/simplex-directory-service/src/Directory/Events.hs` — extend `directoryCmdP` to parse the
  `@`/`#` chat-type prefix and thread the chat type into the (shared) command constructors.
- `apps/simplex-directory-service/src/Directory/Listing.hs` — `DETContact`, `contactDirectoryEntry`,
  merge in `generateListing`.
- `website/src/js/directory.jsc` (+ a contact/bot avatar asset) — non-group card rendering.
- `tests/Bots/DirectoryTests.hs`, `tests/ChatTests/*` — tests.

## 6. Design decisions

Resolved:

- **Submission model (RESOLVED: identical to channels).** Submission is by the **link owner**,
  **signed with the address key** — exactly the channel card flow, no extra requirement. The
  `ownerSig` (only the key-holder can produce it) is the authorization. Admins then decide to list;
  any profile change sends it back to admin review; the address is re-read on the same periodic loop
  — all as for channels. The only "submitter ≠ owner" accommodation is giving the headless support
  bot a way to send (§B.4). *(Earlier we explored open submission with the verified SimpleX name as
  the authenticity signal, and an opt-in flag in the address link data; both dropped — the channel
  model already answers authorization, and name-verification proves identity, not consent to list.)*
- **Description home (RESOLVED: `Profile.description`, redacted per group policy).** New profile
  field. In group-member profiles it is redacted the same way `shortDescr` is (links/names stripped
  under the group's policy, §G), not removed wholesale; carried full in the address link data /
  direct view / directory. Directory abuse is gated by admin review (Q7).
- **Q1 — Verification (RESOLVED: reuse the plan).** Already verifiable via `APIConnectPlan` — the
  same `verifyLinkOwner` path channels use, no new code. The intrinsic link-data fetch (to get the
  root public key) is opaque and not a connection. No card/protocol extension.
- **Q4 — Command surface (RESOLVED: same commands, extended with chat type).** Reuse the existing
  command constructors and syntax with a chat-type discriminator on the id token — `#` group
  (existing), `@` contact (new) — e.g. `/approve @<id>:<name> <version>`. The prefix disambiguates
  the overlapping `group_id`/`contact_reg_id` spaces, so no parallel command names are needed.
- **Q5 — SimpleX names (RESOLVED: support now).** Verify name↔link consistency for addresses and
  populate `simplex_name`; flows through to listing + search (see §C.5).

- **Q2 — Entry type (RESOLVED: `ChatPeerType`, typed + admin-verified).** The listing type is a
  `ChatPeerType`: **bot** from `peerType == CPTBot`; **business** from `peerType ∈ {CPTHuman,
  CPTBusiness}` (unset ≙ `CPTHuman`; stored as `CPTBusiness`); `CPTUnknown` is rejected. Because
  `CPTBusiness` can't be published on profiles yet (wire-compat, §A), a business's profile is
  `CPTHuman` in practice; the admin verifies it. When profiles can carry `CPTBusiness`, it's read
  directly.
- **Q3 — Reg status type (RESOLVED: reuse the group/channel type).** Use the same `GroupRegStatus`
  the channel registrations use — no separate `ContactRegStatus`. The lifecycle mirrors channels.
- **Q6 — Updates (RESOLVED: re-read in the same loop as channels).** The directory re-reads the
  registered address links in the same periodic link-check loop it runs for channels
  (`deGroupLinkCheck`, `Service.hs:832`), re-fetching the address link data to pick up profile/link
  changes and re-verify the name. Opaque fetch, no connection.
- **Q7 — Description screening (RESOLVED: two surfaces, two mechanisms).** *Directory page:* admin
  approval is the gate — a profile change (incl. description) triggers re-approval, hiding the
  address until re-approved, exactly like a channel profile change; no separate automatic content
  filter on the directory description. *Group member profiles:* the group's own policy redacts the
  description on delivery (links/names stripped like `shortDescr`, §G). The two are independent.

## 7. Suggested sequencing

**Phase 1 — UX prerequisites (self-contained; do these first — no registration work until they
land).**

1. **`Profile.description` field** (§A) + member-profile redaction on send and receive (§G) + a test
   that a member's description is redacted per group policy.
2. **Show the description in the app** — banner + contact-info "Read more" via the `Modal` markdown
   element (§H). This is the "see how it looks" step; iterate on the UX here.
3. **`ChatPeerType` extension** (`CPTBusiness`, `CPTUnknown`, lenient decoder) (§A) — the type only,
   **no UI** to set or display it yet.
4. **Share a contact link via chat** — core (`getUserAddressSignKey`, `APIShareAddress`,
   `/share address`, §B.1–3) + the app share UI mirroring the channel share (§B.5) + a client test on
   the signed `MCLContact` card.

**Phase 2 — directory (only after Phase 1).**

5. Directory store: migration + `ContactReg` model/queries (link-keyed).
6. `deChatLinkReceived` `MCLContact` case (verify + derive type + link-in-profile check +
   `addContactReg`) + name↔link verification + admin approval + directory test through to "listed".
7. Listing merge (`DETContact` + `contactDirectoryEntry` + `generateListing`) + one unified
   group+contact search + website rendering.

**Deferred:** peerType setting/badge UI; the support-bot entry point (§B.4).
