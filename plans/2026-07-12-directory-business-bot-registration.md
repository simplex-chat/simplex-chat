# Directory registration of businesses and service bots (via signed contact card)

Status: draft plan for review. Grounded against the current tree (branch `ep/dir-contacts`).

## 1. Goal

Let a business or a service (chat bot) operator register their **contact `/a` address** in the
directory by **sending a signed contact card** to the directory bot — the UX already used for
channels (`/share chat #ch @'SimpleX Directory'`), for a contact address instead of a channel link.

**Guiding principle: the flow matches channel registration — owner-signed card, admin approval,
re-approval on any profile change — differing in that the directory does not connect to or prepare
the address, and the listing type is a contact `peerType`.**

Product decisions from the discussion, baked into this plan:

- **The owner sends the card; the signature is the authorization.** The `ownerSig`, signed with the
  address root key, proves the sender owns the address, and only the key holder can produce it. The
  signature is bound to the sending conversation (§3), so a forwarded card carries no signature.
- **The directory neither connects to nor prepares the address.** The operator is already a connected
  contact, because it sends the card over its own conversation with the directory. The registration
  references that existing contact by `contact_id`. The listing profile is refreshed by resolving the
  published address link and applying `updateContactFromLinkData` — a library function the directory
  calls directly, not a connection and not `connectPlan` (§A, §C.3).
- **The operator must publish the registered address in its own profile** (`Profile.contactLink`), so
  a user reading the listing can connect. The directory checks that the card's `connLink` equals the
  sending contact's `profile.contactLink`; if absent or different, it asks the operator to publish it.
  This publication is a listing requirement for user convenience, not the ownership proof.
- **Short links only.** A registration card carries a `ShortLinkContact` (`MCLContact.connLink`, §3).
  A raw or full link receives the same response channels give — a request to send the address as a
  card.
- **One table for businesses and bots** — both are contact `/a` addresses. The registration is typed
  from the fetched profile's `peerType`: a **bot** when `peerType == CPTBot`, a **business**
  otherwise. `CPTUnknown` is rejected. An admin verifies a business before approving, as for channels.
- **Listing type = `ChatPeerType`.** `ChatPeerType` (`Types.hs:711`, now
  `CPTHuman | CPTBot | CPTBusiness | CPTUnknown Text` — implemented) with a **lenient** decoder
  (unknown tag → `CPTUnknown`) so this version never fails to parse a profile with a future tag.
  **Wire-compat caveat (verified):** the *current* decoder is strict and `Profile` is
  `deriveJSON`-parsed, so an already-deployed app fails the whole profile on an unknown `peerType`;
  therefore `CPTBusiness` is not published on profiles until the lenient version is broadly adopted.
  Classification does not depend on this — a business profile stays `CPTHuman` in practice, and the
  directory types it a business because its `peerType` is not `CPTBot`.
- **`peerType` and `businessAddress` are orthogonal, and the directory ignores `businessAddress`.**
  `businessAddress` chooses the conversation type a connector gets; the directory does not use it to
  classify.
- **Description lives on the contact `Profile`** (new `description` field, parallel to
  `GroupProfile.description`). In group-member profiles it is redacted per the group's policy, as
  `shortDescr` is; it is carried full in the direct contact view, the address link preview, and the
  directory. See §G.
- **`peerType` + `description` are visible in the app independent of the directory.** Full details in
  §H.

Deliverables: (a) an API + CLI to prepare and share the signed contact card; (b) the
`Profile.description` field; (c) directory handling that verifies and stores the address; (d) admin
approval, web listing, and search.

## 2. End-to-end flow

```
Operator's client                         Directory bot
-----------------                         -------------
(already a connected contact of the directory)
/share address @'SimpleX Directory'
  -> own /a short address + root key
  -> MCChat { chatLink = MCLContact {connLink, profile, business},
              ownerSig = sign(rootPrivKey,
                              chatBinding <> connLink) }  ── card ──▶  DEChatLinkReceived (MCLContact, ownerSig)
                                                                        -> APIConnectPlan (plan only): resolves link,
                                                                           verifies ownerSig => CAPOk {csld, ownerVerification}
                                                                        -> if OVVerified:
                                                                             require connLink == ct.profile.contactLink
                                                                             peerType: CPTBot => bot, else business
                                                                             addContactReg (existing ct.contactId),
                                                                               status pending approval
                                                                             updateKnownContactFromLink ct  (listed profile)
                                                                             notify admins (admin verifies a business)
admins: /approve @<contactId>:<name> <n>                                -> status active -> listingsUpdated
                                                                        -> web listing.json + bot search include it

periodic loop, per registered contact:
  updateKnownContactFromLink ct   (resolve link + updateContactFromLinkData)
    change => suspend / re-approval; address removed => suspend; address replaced => remove
DEContactUpdated (message profile change) => same transitions
```

No conversation is opened. The directory's only network action for a registration is the opaque
link-data fetch performed by `APIConnectPlan` at registration and by `updateKnownContactFromLink` on
each refresh.

## 3. What already exists (reuse map)

All grounded in the current tree:

- **Chat-link card type** — `MCLContact {connLink :: ShortLinkContact, profile :: Profile, business :: Bool}`
  (`src/Simplex/Chat/Protocol.hs:730`). `MCChat {text, chatLink, ownerSig}` and
  `LinkOwnerSig {ownerId, chatBinding, ownerSig}` at `Protocol.hs:726,736`.
- **Owner-signature verification for contact addresses is already wired.** `connectPlan`'s
  `CTShortContact CCTContact` path fetches `FixedLinkData {rootKey}` + `UserContactData {owners}` and
  computes `ov = verifyLinkOwner rootKey owners l' sig_`, surfaced as
  `CPContactAddress (CAPOk {contactSLinkData_, ownerVerification})`
  (`src/Simplex/Chat/Library/Commands.hs:4360-4393`, `verifyLinkOwner` def `4634`;
  `Controller.hs:1116`). For plain/business addresses `owners == []`, so `ownerId = Nothing` and
  verification uses the link **root key**.
- **The signature is bound to the sending conversation.** `mkLinkOwnerSig` signs
  `chatBinding <> connLink` (`Commands.hs:4619`); for a direct conversation `shareChatBinding` sets the
  binding to that connection's ratchet association-data hash (`Commands.hs:4626`). On receipt the core
  recomputes the receiving connection's hash and drops the signature when it differs
  (`Subscriber.hs:1855-1861`), so a forwarded or replayed card arrives without a signature.
- **The directory already receives any `MCChat` card as `DEChatLinkReceived`** —
  `Directory/Events.hs:108` turns `(MCChat {chatLink, ownerSig}, Nothing)` into `DEChatLinkReceived`.
  Today `deChatLinkReceived` matches `MCLGroup` only and otherwise replies "Only channels can be added
  to directory via link." (`Directory/Service.hs:964-979`). An `MCLContact` case is added.
- **A connected contact carries its published address.** `Profile.contactLink` (`Types.hs:699`) is
  stored in `contact_profiles.contact_link`; `getContactWithoutConnViaShortAddress`
  (`Store/Profiles.hs:608`) already matches `cp.contact_link = ?` (filtered to unconnected contacts).
- **The core emits contact profile updates.** `CEvtContactUpdated {fromContact, toContact}`
  (`Controller.hs:940`), currently unmapped in `crDirectoryEvent_` (`Events.hs:80-115`).
- **Card-sharing UI + API + signing** — `/share chat #g @to` → `SharePublicGroup`
  (`Commands.hs:2492`) → `APIShareChatMsgContent` (`Commands.hs:1185`), which builds the `MCChat` and
  signs with `mkLinkOwnerSig` + `shareChatBinding`.
- **Address root key storage** — `link_priv_sig_key` (the address root private key, Ed25519) is stored
  in `user_contact_links` by `createUserContactLink` (`Store/Profiles.hs:429-439`); `businessAddress`
  lives in `AddressSettings` (`Profiles.hs:497-502`) and is published as `ContactShortLinkData.business`
  (`Commands.hs:4648`).
- **Directory store / listing / web infra** — `sx_directory_group_regs` table
  (`Directory/Store/{SQLite,Postgres}/Migrations.hs`), `GroupReg`/`GroupRegStatus`
  (`Directory/Store.hs:116-194`), `getAllListedGroups_` (`Store.hs:354`), `generateListing`
  (`Directory/Listing.hs:148`), `DirectoryEntry`/`DirectoryEntryType = DETGroup` (`Listing.hs:55-86`),
  `verifiedGroupDomain` (`Store.hs:228`), website renderer `website/src/js/directory.jsc`.

## 4. Work items

### A. Protocol / types

- `MCLContact` exists; no new protocol message for the card itself.
- **`ChatPeerType`** (`Types.hs:711`, implemented) with `CPTBusiness` and `CPTUnknown Text`
  (forward-compat), and a **lenient** decoder (`Types.hs:724-731`). Classification is by
  `peerType == CPTBot` (bot) versus otherwise (business); `CPTUnknown` is rejected. Because the
  current decoder is strict, `CPTBusiness` is not published on profiles until the lenient version is
  broadly adopted (a business profile stays `CPTHuman`, and the directory still types it a business).
- **`description :: Maybe Text` on `Profile`** — already merged (`Types.hs`, #7256). It rides into the
  address link data (`ContactShortLinkData` embeds the `Profile`, `Protocol.hs:1553`). Send-side
  redaction in group-member profiles is done (`redactedMemberProfile`, `Internal.hs:1261`); the
  receive-side redaction (§G.2) is **not** implemented (verified: no `redactedMemberProfile`/
  `removeSimplexLink` in `Store/Groups.hs`).
- **Setting `peerType`/`description`.** Both are plain `Profile` fields carried by the existing
  profile-update path (`APIUpdateProfile` / `/p`); tests drive them via `/_profile`. A dedicated
  setter for the multi-line `description` is worth adding for CLI ergonomics.
- **Refresh function for the directory (§C.3).** Add
  `updateKnownContactFromLink :: User -> Contact -> CM (Contact, Bool)`: resolve the contact's
  published address (`Contact.profile.contactLink`) via `getShortLinkConnReq'` (`Internal.hs:1557`),
  decode the `ContactShortLinkData`, and apply `updateContactFromLinkData`; the `Bool` reports whether
  the listed profile or the verification changed. `updateContactFromLinkData` (`Internal.hs:1523`)
  changes to return `(Contact, Bool)`, adapting its sole existing caller (`Commands.hs:4374`). The
  directory calls the new function directly via `runReaderT … cc` (as `sendChatCmd` does,
  `Core.hs:103`) — no `connectPlan`, `PRMAll`, `resolveKnownContact`, or new `ChatCommand`.

### B. Client: prepare + share the contact-address card

**Status: B.1–B.3 are merged** — `APIShareMyAddress`/`ShareMyAddress` with handlers and parsers
(`Controller.hs:401,567`; `Commands.hs:1200,2505,5472,5670`). B.5 (app UI) is the outstanding piece.

1. **Signing key — from the agent, not the chat DB.** Sign the card with the address short-link key
   via `getConnLinkPrivKey (aConnId addressConn)` (already in the agent, used at `Subscriber.hs:1597`;
   `getUserAddressConnection` gives the connection). This is the authoritative key — the private half
   of the short link's root key the directory verifies against — and it exists whenever the short link
   does, including right after an upgrade (`setConnShortLink` provisions it). Do not read the chat-DB
   `link_priv_sig_key` for signing: it is written only at `createUserContactLink` and never on upgrade.
   *(Separate cleanup, off the signing path: persist `link_priv_sig_key` on upgrade too —
   `setMyAddressData`/`setUserContactLinkShortLink` — reading it back via `getConnLinkPrivKey` so the
   column stops being stale.)*
2. **Card-builder API — `APIShareMyAddress {toSendRef :: SendRef}`** (Controller) + handler in
   `Commands.hs`, mirroring `APIShareChatMsgContent` (`Commands.hs:1185`):
   - `getUserAddress` → `connLinkContact` (short link) + profile + `businessAddress`.
   - `getUserAddressConnection` → conn; `getConnLinkPrivKey (aConnId conn)` → `rootPrivKey`
     (`Nothing` ⇒ not upgraded → error; the UI pre-empts this via §B.5).
   - hoist `shareChatBinding` to top-level; `binding <- shareChatBinding user toSendRef`.
   - `ownerSig = LinkOwnerSig {ownerId = Nothing, chatBinding = B64UrlByteString cb,
     ownerSig = C.sign' rootPrivKey (cb <> smpEncode connShortLink)}` (contact variant of
     `mkLinkOwnerSig`, `ownerId = Nothing` so the directory verifies against the link root key).
   - return `CRChatMsgContent user (MCChat {text, chatLink = MCLContact {connLink, profile, business}, ownerSig})`.
   `SendRef` covers direct and group/channel targets.
3. **CLI command — `ShareMyAddress {toChatName}`**, parser `/share address @to` / `/share address #to`
   (`Commands.hs:5670` neighborhood), handler mirroring `SharePublicGroup` (`Commands.hs:2492`):
   resolve `toChatName` → `SendRef` → `APIShareMyAddress` → `APISendMessages`.
4. **Support-bot entry point — OUT OF SCOPE (deferred).** A headless business running
   `apps/simplex-support-bot` will eventually need a bot command that calls `APIShareMyAddress` against
   the directory contact once connected. The core path built here is what it will call.
5. **App UI — "Share via chat" (Phase 1; mirrors the channel share).** The receiving/rendering half
   already exists from the channel work (`MsgChatLink.Contact`, `CIChatLinkHeader`, the compose
   preview, and the `SharedContent → ShareListView → ComposeView` picker). New pieces: the entry
   point, a `SharedContent.AddressLink` case, the `apiShareMyAddress` call, and the upgrade branch.
   - **Entry point:** a "Share via chat" button in the user's own address screen (`UserAddressView.kt`),
     beside the OS-share "Share" button. Address creation lands on this same screen
     (`createAddress` sets `userAddress`, `UserAddressView.kt:73-84`), so the button is visible
     immediately after creating an address.
   - **Flow:** tap → if `userAddress.shouldBeUpgraded` show an upgrade alert ("To share your address in
     a chat it will be upgraded to a short link. All your contacts stay connected."), buttons
     **[Upgrade & share]** / **[Cancel]** — on confirm: spinner → `apiAddMyAddressShortLink`, then
     continue. Then set `SharedContent.AddressLink` → `ShareListView` (contacts + groups/channels, with
     the simplex-link prohibition filtering) → pick destination → `ComposeView` `LaunchedEffect` calls
     `apiShareMyAddress` → sets the existing `ChatLinkPreview` → optional message text → **Send**.
   - iOS mirrors this via the existing channel-share flow (`f49d98511`); Kotlin per
     `plans/2026-04-17-kotlin-share-channel-link.md`.

### C. Directory: verify + store (no connect)

1. **`deChatLinkReceived` — add the `MCLContact` case** (`Directory/Service.hs:964`).
   - `deChatLinkReceived ct (MCLContact {connLink, business}) (Just ownerSig)` — `business` is retained
     (unused for classification, kept for possible later use):
     - `APIConnectPlan userId (contact link) PRMAllGroups (Just ownerSig)` — plan only. Because the
       operator connected to the directory (its own address is not the connection's key), the plan
       resolves the link and returns `CPContactAddress (CAPOk {contactSLinkData_ = Just csld,
       ownerVerification})` (`contactRequestPlan:4565`). If the directory had instead connected to the
       operator via that address, the plan returns `CAPKnown ct'`/`CAPContactViaAddress ct'`; treat
       those the same — verified and registered against `ct'`. `verifyLinkOwner rootKey [] connLink
       (Just ownerSig)` runs on this path (`Commands.hs:4379`); verification uses the link root key.
     - `OVVerified`:
       - require the short link within `csld.profile.contactLink` to equal `connLink` — the operator
         has published this address in the profile the link advertises. `Profile.contactLink` is a
         `ConnLinkContact` (`ConnectionLink 'CMContact`, full or short), so extract its short link and
         compare. If it is absent, or a full (non-short) link, reply asking the operator to publish the
         short address in its profile, and stop.
       - resolved `peerType` from `csld.profile`: `CPTBot` → bot; `CPTUnknown` → reject ("unsupported
         account type"); otherwise business.
       - `addContactRegStore cc ct peerType (GRSPendingApproval 1)` referencing `ct.contactId`.
       - `updateKnownContactFromLink user ct` to set the listed profile from the link (§C.3).
       - notify admins with the profile and the approve command.
     - `OVFailed reason` → "ownership verification failed".
     - An already-registered address (found via `getContactRegByContactId ct.contactId`) → re-approval
       (§C.3).
   - Keep the existing `MCLGroup` and fall-through cases; a non-card link still replies with a request
     to send a card.

   Both the address check and `peerType` read the **link** profile `csld.profile`, which is also what
   updates the stored `Contact.profile` (§C.3), not the message-side `ct.profile`.
2. **New store table `sx_directory_contact_regs`** — named migration in
   `Directory/Store/SQLite/Migrations.hs` and `Directory/Store/Postgres/Migrations.hs`. The
   registration is one-to-one with the existing contact.

   ```sql
   CREATE TABLE sx_directory_contact_regs(
     contact_reg_id INTEGER PRIMARY KEY AUTOINCREMENT,
     contact_id INTEGER REFERENCES contacts(contact_id) ON DELETE CASCADE,
     peer_type TEXT NOT NULL,
     contact_reg_status TEXT NOT NULL,
     contact_promoted INTEGER NOT NULL DEFAULT 0,
     created_at TEXT NOT NULL DEFAULT(datetime('now')),
     updated_at TEXT NOT NULL DEFAULT(datetime('now'))
   );
   CREATE UNIQUE INDEX idx_sx_directory_contact_regs_contact_id ON sx_directory_contact_regs(contact_id);
   ```

   Column roles (Postgres mirrors with `BIGSERIAL` + `TIMESTAMPTZ`):
   - `contact_reg_id` — the registration's own autoincrement key; the global admin-facing id, as
     `group_id` is for groups.
   - `contact_id` — the registered contact; nullable and unique, so at most one address per contact
     now, and addresses without a contact stay possible later. Its `Contact.profile` is the listed
     profile, refreshed from the link (§C.3).
   - `peer_type` — resolved listing type (`bot`/`business`); not recoverable from the profile alone
     (`peerType` is `human` for a business, §A).
   - `contact_reg_status` reuses the `GroupRegStatus` encoding (Q3); `contact_promoted` as
     `group_promoted`.

   No submitter column (the sender is the owner, §5) and no per-operator numbering (each contact
   registers one address). Types + functions in `Directory/Store.hs`, mirroring the `GroupReg` set;
   queries join `contacts`/`contact_profiles` and return `(Contact, ContactReg)`:

   ```haskell
   type ContactRegId = Int64

   data ContactReg = ContactReg
     { contactRegId :: ContactRegId,
       contactId :: Maybe ContactId,
       peerType :: ChatPeerType,
       contactRegStatus :: GroupRegStatus,
       promoted :: Bool,
       createdAt :: UTCTime
     }

   addContactRegStore       :: ChatController -> Contact -> ChatPeerType -> GroupRegStatus -> IO (Either String ContactReg)
   getContactReg            :: ChatController -> User -> ContactId -> IO (Either String (Contact, ContactReg))
   getContactRegByContactId :: ChatController -> ContactId -> IO (Either String (Maybe ContactReg))
   setContactRegStatus      :: ChatController -> ContactRegId -> GroupRegStatus -> IO (Either String (GroupRegStatus, ContactReg))
   setContactPromoted       :: ChatController -> ContactRegId -> Bool -> IO (Either String (DirectoryStatus, Bool))
   deleteContactReg         :: ChatController -> ContactRegId -> IO (Either String ())
   getAllListedContacts     :: ChatController -> User -> IO (Either String [(Contact, ContactReg)])
   ```

   `getContactReg` and admin commands look up by `contact_id`. `contact_reg_status` reuses
   `GroupRegStatus`; the reachable subset for contacts is `GRSPendingApproval`, `GRSActive`,
   `GRSSuspended`, `GRSRemoved` (the join states `GRSProposed`/`GRSPendingUpdate` and the role state
   `GRSSuspendedBadRoles` never arise). Registrations are stored in the database only — the
   append-only log (`Store.hs:476-505`, written for groups only when `--directory-file` is given,
   Options.hs:139) gets no contact records.
3. **Refresh and re-approval.**

   - **Refresh** is scheduled by `linkCheckThread_` (`Service.hs:200`), which every `linkCheckInterval`
     enqueues one event per registered entity. Add a `DEContactLinkCheck ct` event alongside
     `DEGroupLinkCheck` and enqueue it over the contact regs; its handler (a `deGroupLinkCheck` analog,
     `Service.hs:828`) calls `runReaderT (updateKnownContactFromLink user ct) cc` (§A). That resolves
     the published address and applies `updateContactFromLinkData`, which overwrites `Contact.profile`
     with the link-advertised profile and reconciles `contactDomain`/`contactDomainVerified`. The
     `Bool` result drives re-approval.
   - There is one `Contact.profile`. Both a message update (`updateContactProfile`) and the link
     refresh write it, and either change requires re-approval, so a divergent profile is hidden until
     re-approved. No separate link-profile snapshot is kept; the listing reads `Contact.profile`.
   - **Message-side changes.** Map `CEvtContactUpdated` to a new `DEContactUpdated {fromContact,
     toContact}` (`Events.hs:80`). Its handler compares the visible fields of a registered contact
     (`displayName`, `fullName`, `shortDescr`, `description`, `image`, `contactLink`, `peerType`).
   - **Transitions**, each communicated to the operator:
     - a change to a visible field (name, description, image) → `GRSPendingApproval`, hidden until
       re-approved, as `reapprove` (`Service.hs:858`) does for channels;
     - the address removed from the profile → `GRSSuspended`, with a request to add it back;
     - the address replaced by a different one → `GRSRemoved` permanently, with a request to
       re-register.
   - Re-submission of an already-registered address (`getContactRegByContactId`) re-verifies ownership
     from the card and routes through the same transitions (`deReregistration` analog).
4. **Admin & user commands — a directory-local target sum type.** Replace the group-id field of the
   shared commands with:

   ```haskell
   data DirectoryRef
     = DRGroup UserGroupRegId (Maybe GroupName)
     | DRAddress (Maybe (ContactId, ContactName))
   ```

   - `@`, `address`, or `addr` parses to `DRAddress Nothing` — the operator's own address.
   - `@ID:name` parses to `DRAddress (Just (ContactId, ContactName))`, matching the group `ID:name`
     form; the admin reference is `contact_id`, as `group_id` is for groups.
   - A non-admin command carrying `Just` is an error; an admin command carrying `Nothing` is an error.

   Shared commands gain contact support, dispatching on the `DirectoryRef` constructor: `DCApprove`,
   `DCReject`, `DCSuspend`, `DCResume`, `DCDelete`, `DCPromote`, `DCSendToOwner`, the admin `DCListLast`
   / `DCListPending`, `DCExecuteCommand`, `DCHelp`, and search. Contact approval notifies the owner and
   lists the entry; it sends no join link and runs no captcha.

   Group-membership commands stay group-only: `DCMemberRole`, `DCGroupFilter`,
   `DCShowUpgradeGroupLink`, `DCInviteOwnerToGroup`, and the `DCSubmitGroup`/`DCConfirmDuplicateGroup`
   pair (contacts begin via the card).

   `/list` shows the operator's group registrations by their numbers followed by the single address
   line, referenced as `address`. Admin approval and similar messages carry the pre-filled
   `@<contactId>:<name>`.
5. **Listing identity + verified SimpleX names.** The listing identity is `contact_id`, one-to-one with
   the address. Name↔link verification is inherited from the core: `updateContactFromLinkData` sets
   `contactDomainVerified` on each refresh (`Internal.hs:1529`). A new
   `verifiedContactDomain :: Contact -> Maybe SimplexDomain` (counterpart of `verifiedGroupDomain`,
   `Store.hs:228`) reads it into `DirectoryEntry.simplexName` and bot/web search.

### D. Listing + web

1. **`DirectoryEntryType`** (`Listing.hs:55`): add `DETContact {peerType :: ChatPeerType}`. The
   `taggedObjectJSON`/`dropPrefix "DET"` derivation emits `{"type":"contact", …}` for the new
   constructor; `peerType` serializes as `"business"`/`"bot"`.
2. **`contactDirectoryEntry`** builder (analogue of `groupDirectoryEntry`, `Listing.hs:100`), from
   `(Contact, ContactReg)`: `DirectoryEntry {entryType = DETContact peerType, displayName, simplexName,
   groupLink = PublicLink Nothing (Just connShortLink), shortDescr, welcomeMessage, imageFile, activeAt,
   createdAt}`. The profile fields (`displayName`, `shortDescr`, `description` → `welcomeMessage`,
   `image`), the link, and the verified domain → `simplexName` are read from `Contact.profile` (the
   link-advertised profile, §C.3); `peerType` from the `ContactReg`. `PublicLink` already models contact
   links (`Listing.hs:63-68`). Like `groupDirectoryEntry`, it returns
   `Maybe (DirectoryEntry, Maybe (FilePath, ImageFileData))`, the image filename hashed from the contact
   link (`imgFileData`, `Listing.hs:133`), so `generateListing` writes the image.
3. **Export pipeline.** `generateListing` (`Listing.hs:148`,
   `FilePath -> [(GroupInfo, GroupReg, Maybe GroupLink)] -> IO ()`) builds entries via
   `groupDirectoryEntry`, writes each image into `listingImageFolder`, and saves `listing.json` +
   `promoted.json` (filtered by `promoted`) into a timestamped directory swapped in atomically via a
   symlink. Extend its signature to also take `[(Contact, ContactReg)]`, build and write those via
   `contactDirectoryEntry` alongside the group rows, and merge into the one `DirectoryListing {entries}`
   array; `promoted.json` includes promoted contacts (`contact_promoted`). The caller
   `updateGroupListingFiles` (`Service.hs:1557`) fetches `getAllListedContacts` beside
   `getAllListedGroups` and passes both; `listingsUpdated` fires on contact-reg status/promotion
   changes as it does for groups.
4. **Website `directory.jsc`**: branch `displayEntries` on `entryType.type` and, for contacts, on
   `entryType.peerType`:
   - business vs bot label/avatar from `peerType`; non-group avatar fallback instead of
     `/img/group.svg`;
   - a "Connect"/"Chat" affordance instead of the "N members/subscribers" line (`entryMemberCount`
     already returns 0 for non-group — `directory.jsc:183-193`);
   - join URI already works via `connShortLink` (`directory.jsc:331-348`).
   Search/filter already reads generic fields (`displayName`, `shortDescr`, `welcomeMessage`,
   `simplexName`), so text search works unchanged.

### E. Bot search

Include active contact regs in the bot's search results (`DCSearchGroup` path, `Service.hs:1115`,
backed by `searchListedGroups`) as one unified result set; match on display name and SimpleX name.

### F. Tests

- **Client** (`tests/ChatTests/`): `/share address` produces an `MCChat`/`MCLContact` card with a valid
  `ownerSig` (`ownerId = Nothing`); parser test for `/share address`.
- **Directory** (`tests/Bots/DirectoryTests.hs`, mirroring `testRegisterChannelViaCard` `:2050`):
  register a business and a bot via card (verified → pending → admin approve → listed), reject on
  bad/absent signature, reject when the address is not in the sender's profile, search finds it, a
  profile change de-lists per §C.3, and the generated `listing.json` contains a `"type":"contact"`
  entry with the right `peerType`.
- **Profile description** (§G): a member's `description` is redacted per the group's policy on both
  send and receive; a direct contact / address preview keeps it full.

### G. `Profile.description` field + member-profile redaction (resolved)

`description :: Maybe Text` is added to `Profile` (§A). In group-member profiles it is redacted per the
group's policy — the same treatment `shortDescr` gets — not removed wholesale: links and SimpleX names
are stripped when the group prohibits them. Implemented in `redactedMemberProfile`
(`Internal.hs:1259-1273`).

1. **Send side** — in `redactedMemberProfile` (which already redacts `shortDescr`/`contactLink`/name
   proof under the group's `SGFSimplexLinks`/`SGFDirectMessages`), redact `description` with an inline
   strip helper (per G.3). Adding `description` to `Profile` forces this output record to be rebuilt
   here anyway. (Used on every member-profile-out path — `Internal.hs:1247,1255`, `Subscriber.hs:803,3220`,
   `Commands.hs:4230`.)
2. **Receive side** — apply the same redaction when ingesting a member profile. Chokepoints:
   `updateMemberProfile` (`Store/Groups.hs:3388`) and member creation (`Store/Groups.hs:2510,1395`).
3. **Redaction granularity.** Inline-strip links and names — drop the
   `Uri`/`HyperLink`/`SimplexLink`/`SimplexName`/`Mention` spans via `parseMaybeMarkdownList`, re-concat
   the remaining `FormattedText`, keep the prose (empty result ⇒ `Nothing`). Exception: if
   `hasObfuscatedSimplexLink` matches, drop the whole description.
4. **Kept full where wanted** — the address link data, the direct contact profile view, and the
   directory listing all carry the full `description`. Directory abuse is gated by admin review (Q7).
5. **UI/UX** — add a multi-line "Description" field to the profile/address editor (app UI, follow-on with
   §B.5), with an edit-time hint that links and names won't show where a group prohibits them.

### H. App visibility of `peerType` + `description` (why owners will set them)

These are persistent profile identity shown to everyone who reaches the address — independent of the
directory.

**`peerType` — type icon / badge**:
- Pre-connect "Open chat?" alert (`newchat/ConnectPlan.kt:698-713`) — type icon + verification;
  briefcase when either the address `business` flag or `peerType == CPTBusiness`, bot cube from
  `peerType`, else person.
- Chat list (`chatlist/ChatPreviewView.kt:188`, `isBot`) and the chat banner
  (`chat/ChatView.kt:2234` `ChatBannerView`) — extend to a business marker from `peerType`.

**`description` — shown via a "Read more" affordance, NOT inline** (the alert and the in-chat link card
`CIChatLinkHeader.kt` carry only the short teaser). Rendered in the chat banner (`ChatBannerView`) and
the contact info page (`ChatInfoView`, `:778`):
- Teaser: `shortDescr` if present, then a clickable "Read more"; otherwise the first line of
  `description` truncated to 100 chars, then "Read more".
- "Read more" is a client-only `Format` span (implemented). The `Modal {modalName}` variant lives only
  in the app's mirrored `Format` enum (Kotlin/Swift), not in Haskell `Markdown.hs`. Each client resolves
  the label and modal content by `modalName`, rendering the tap (iOS sheet / Android modal). No Haskell
  change.
- This is NOT the welcome/auto-reply message (`AddressSettings.autoReply`), and NOT shown in the
  pre-connect alert or the shared-link card.

**Profile editor** (`usersettings/UserProfileView.kt`) — add the multi-line description field and a way
to set the account type (`peerType`). The editor exposes two distinct "business" concepts — `peerType`
(identity) and the `businessAddress` conversation-type setting — which must use distinct labels.

## 5. Files to touch (summary)

- `src/Simplex/Chat/Types.hs` — extend `ChatPeerType` (`CPTBusiness`, `CPTUnknown`, lenient decode);
  add `Profile.description`; JSON/TextEncoding derivations.
- `src/Simplex/Chat/Library/Internal.hs` — `updateKnownContactFromLink`;
  `updateContactFromLinkData → (Contact, Bool)`; redact `description` in `redactedMemberProfile` (§G).
- `src/Simplex/Chat/Library/Commands.hs` — adapt the `updateContactFromLinkData` caller (`:4374`);
  `APIShareMyAddress` + `ShareMyAddress` handlers + parsers; reuse `shareChatBinding`.
- `src/Simplex/Chat/Controller.hs` — `APIShareMyAddress`, `ShareMyAddress` command constructors.
- `src/Simplex/Chat/Store/Groups.hs` — redact `description` when ingesting a member profile (§G).
- `src/Simplex/Chat/Store/Profiles.hs` — persist `link_priv_sig_key` on short-link upgrade
  (cleanup, off the signing path).
- `apps/simplex-directory-service/src/Directory/Service.hs` — `MCLContact` case in
  `deChatLinkReceived`; `DEContactUpdated` handling; contact-reg lifecycle + admin/user commands;
  listing trigger.
- `apps/simplex-directory-service/src/Directory/Events.hs` — map `CEvtContactUpdated`; the
  `DirectoryRef` target type in `directoryCmdP` and the shared command constructors.
- `apps/simplex-directory-service/src/Directory/Store.hs` — `ContactReg` model + queries;
  `verifiedContactDomain`.
- `apps/simplex-directory-service/src/Directory/Store/{SQLite,Postgres}/Migrations.hs` — new table.
- `apps/simplex-directory-service/src/Directory/Listing.hs` — `DETContact`, `contactDirectoryEntry`,
  merge in `generateListing`.
- App views (Phase 1, §B.5/§H) — `UserAddressView.kt`, `ChatInfoView.kt` + `ChatView.kt`
  `ChatBannerView`, the Kotlin/Swift `Format` mirror (+ iOS equivalents). The `peerType` badge/editor UI
  is deferred.
- `website/src/js/directory.jsc` (+ a contact/bot avatar asset) — non-group card rendering.
- `tests/Bots/DirectoryTests.hs`, `tests/ChatTests/*` — tests.

## 6. Design decisions

Resolved:

- **Submission model (RESOLVED: owner-signed card).** Submission is by the address owner, signed with
  the address root key, bound to the sending conversation. The directory neither connects to nor
  prepares the address; it references the sender's existing contact. Admins decide to list; any profile
  change returns it to review; the address is re-read on the periodic loop.
- **Ownership proof (RESOLVED: sufficient).** The card signs the sending connection's ratchet-ad-hash
  plus the link, under the address root key; a forwarded card loses its signature on receipt
  (`Subscriber.hs:1856`), and `verifyLinkOwner` checks it against the resolved root key. The published
  address in the profile is a listing requirement for user convenience, not the proof.
- **Refresh (RESOLVED: direct function, no plan).** `updateKnownContactFromLink` resolves the published
  address and applies `updateContactFromLinkData`, called directly via `runReaderT … cc`. `connectPlan`
  is the wrong tool: it finds contacts only by `conn_short_link_to_connect`, never set for an operator
  that connected to the directory. The listed profile is the link-advertised one, which guards against
  a divergent advertised profile.
- **Entry type (RESOLVED: `ChatPeerType`).** Bot when `peerType == CPTBot`; business otherwise;
  `CPTUnknown` rejected. A business profile is `CPTHuman` in practice (wire-compat, §A), and the admin
  verifies it.
- **Reg status (RESOLVED: reuse `GroupRegStatus`).** The reachable subset for contacts is
  `GRSPendingApproval`/`GRSActive`/`GRSSuspended`/`GRSRemoved`.
- **Identity + numbering (RESOLVED: one contact, one address).** The registration is one-to-one with the
  contact, so there is no submitter and no per-operator number. `contact_reg_id` is the admin-facing id;
  admins reference `contact_id`, as they reference `group_id` for groups.
- **Command surface (RESOLVED: `DirectoryRef`).** A directory-local sum type distinguishing a group
  (by number) from an address (`@`/`address`, with `@ID:name` for admins). No core `ChatRef`.
- **SimpleX names (RESOLVED: support now).** `verifiedContactDomain` reads the verified domain into the
  listing and search.
- **Description (RESOLVED: `Profile.description`, redacted per group policy).** Implemented in
  `redactedMemberProfile` (§G). Directory abuse is gated by admin review.

## 7. Suggested sequencing

**Phase 1 — UX prerequisites.** Largely merged; outstanding items marked.

1. `Profile.description` field — merged. Send-side member redaction — merged (`redactedMemberProfile`,
   `Internal.hs:1261`). **Outstanding: receive-side redaction (§G.2)** + a test that a member's
   description is redacted per group policy.
2. `ChatPeerType` extension (`CPTBusiness`, `CPTUnknown`, lenient decoder) — merged (`Types.hs:711`).
3. Core share path (`APIShareMyAddress`, `/share address`, §B.1-3) — merged. **Outstanding: the app
   "Share via chat" UI (§B.5)** + a client test on the signed `MCLContact` card.
4. **Outstanding (verify against the app before treating as done): in-app description display** —
   banner + contact-info "Read more" via the `Modal` element (§H).

**Phase 2 — directory (only after Phase 1).**

5. Core refresh function (§A: `updateKnownContactFromLink`, `updateContactFromLinkData → (Contact,
   Bool)`).
6. Directory store: migration (`sx_directory_contact_regs`) + `ContactReg` model/queries +
   `verifiedContactDomain`.
7. `deChatLinkReceived` `MCLContact` case (verify → `addContactRegStore` → initial refresh) +
   `DEContactUpdated` handling + the re-approval/suspend/remove transitions + admin approval + directory
   test through to "listed".
8. Listing merge (`DETContact` + `contactDirectoryEntry` + `generateListing`) + one unified group+contact
   search + website rendering.

**Deferred:** peerType setting/badge UI; the support-bot entry point (§B.4).
