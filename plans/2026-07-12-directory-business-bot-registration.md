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
- **The directory prepares an unconnected entity; it does not open a conversation.** On a verified card it calls `APIPrepareContact` to create a prepared contact or, for a business-chat address, a prepared business group, from the fetched `ContactShortLinkData`, and stores a registration referencing it. The periodic loop re-runs `APIConnectPlan … PRMAll` on the address link; under `PRMAll` a new `resolveKnownContact` (contact) or `resolveKnownGroup` (business group) refreshes the entity's profile and domain verification via `updateContactFromLinkData`/`updateGroupFromLinkData` — the analog of `deGroupLinkCheck` for channels (core change, §A/§C.3). No connection is established.
- **One table for both businesses and bots** — all are contact `/a` addresses. The registration is typed by the fetched profile's `peerType`: a **bot** from `CPTBot`; a **business** from `CPTHuman` or `CPTBusiness` (unset ≙ `CPTHuman`), stored `CPTBusiness`; `CPTUnknown` is rejected. An admin verifies a business before approving, as for channels.
- **Listing type = `ChatPeerType`.** `ChatPeerType` (`Types.hs:711`, now `CPTHuman | CPTBot | CPTBusiness | CPTUnknown Text` — implemented)
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
                                                                       => CPContactAddress (CAPOk {contactSLinkData_})
                                                                    -> if OVVerified:
                                                                         APIPrepareContact, then addContactReg (bot if CPTBot,
                                                                           else business), status pending
                                                                         notify admins with profile (admin verifies)
admins: /approve ...                                              -> status active -> listingsUpdated
                                                                    -> web listing.json + bot search include it
```

No conversation is opened. The prepared address contact holds the profile without a connection; the directory's only network action is the opaque link-data fetch performed by `APIConnectPlan` at registration and on each refresh.

## 3. What already exists (reuse map)

All grounded in the current tree:

- **Chat-link card type** — `MCLContact {connLink :: ShortLinkContact, profile :: Profile, business :: Bool}`
  already exists (`src/Simplex/Chat/Protocol.hs:731`). `MCChat {text, chatLink, ownerSig}` and
  `LinkOwnerSig {ownerId, chatBinding, ownerSig}` at `Protocol.hs:726,736`.
- **Owner-signature verification for contact addresses is already wired.** `connectPlan`'s
  `CTShortContact CCTContact` path fetches `FixedLinkData {rootKey}` + `UserContactData {owners}`
  and computes `ov = verifyLinkOwner rootKey owners l' sig_`, surfaced as
  `CPContactAddress (CAPOk {contactSLinkData_, ownerVerification})`
  (`src/Simplex/Chat/Library/Commands.hs:4356-4389`, `verifyLinkOwner` def `4634`; `Controller.hs:1104,1116-1124`).
  For plain/business addresses `owners == []`, so `ownerId = Nothing` and verification uses the
  link **root key** (`verifyLinkOwner` fallback).
- **The directory already receives any `MCChat` card as `DEChatLinkReceived`** — `Directory/Events.hs:108`
  turns `(MCChat {chatLink, ownerSig}, Nothing)` into `DEChatLinkReceived`. Today `deChatLinkReceived`
  only matches `MCLGroup` and otherwise replies "Only channels can be added to directory via link."
  (`Directory/Service.hs:965-979`). We add an `MCLContact` case.
- **Card-sharing UI + API + signing** — `/share chat #g @to` → `SharePublicGroup`
  (`Commands.hs:2492`, parser `Commands.hs:5670`) → `APIShareChatMsgContent`
  (`Commands.hs:1185`) which builds the `MCChat` and signs with `mkLinkOwnerSig` +
  `shareChatBinding` (binds the card to the recipient connection, anti-replay).
- **Address key + business flag storage** — `link_priv_sig_key` (the address root private key,
  Ed25519) is stored in `user_contact_links` by `createUserContactLink`
  (`src/Simplex/Chat/Store/Profiles.hs:429-439`); `businessAddress` lives in `AddressSettings`
  (`Profiles.hs:497-502`) and is published as `ContactShortLinkData.business`
  (`Commands.hs:4648`, `Protocol.hs:1553`). Note: `getUserAddress`/`UserContactLink`
  do **not** currently read `link_priv_sig_key` back (`Profiles.hs:479-524`).
- **Directory store / listing / web infra** — `sx_directory_group_regs` table
  (`Directory/Store/{SQLite,Postgres}/Migrations.hs`), `GroupReg`/`GroupRegStatus`
  (`Directory/Store.hs:116-226`), `getAllListedGroups_` (`Store.hs:354-363`), `generateListing`
  (`Directory/Listing.hs:148-170`), `DirectoryEntry`/`DirectoryEntryType = DETGroup`
  (`Listing.hs:55-86`), website renderer `website/src/js/directory.jsc`.

## 4. Work items

### A. Protocol / types

- `MCLContact` exists; no new protocol message for the card itself.
- **`ChatPeerType`** (`Types.hs:711`, implemented — now `CPTHuman | CPTBot | CPTBusiness | CPTUnknown Text`) with `CPTBusiness` and
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
  `GroupProfile.description` (`Types.hs:872`). Additive/nullable — only businesses/bots set it.
  It rides into the address link data automatically (`ContactShortLinkData` embeds the whole
  `Profile`, `Protocol.hs:1553`), so the directory reads it from the fetched link data. It is
  redacted per group policy in group-member profiles, on **both send and receive** (see §G). No
  version bump is needed — `Profile` is `deriveJSON`-parsed and aeson ignores unknown keys, so old
  apps just drop `description` (same as when `peerType`/`badge`/`contactDomain` were added).
- **Setting `peerType`/`description` (for tests + eventual UI).** Both are plain `Profile` fields, so
  they ride through the existing profile-update path (`APIUpdateProfile` / the `/p` command); tests
  drive them via `/_profile`. A small dedicated setter for the multi-line `description` is worth
  adding for CLI ergonomics. The app-UI toggle to set `peerType = CPTBusiness` is deferred (per the
  wire-compat caveat above).
- **Core connect-plan changes for the directory refresh (§C.3).** Rename `PRMAllGroups → PRMAll`
  (`Controller.hs:677`; uses at `Commands.hs:4419`, `Service.hs:572/834/972`) and resolve known
  contacts under it. `updateContactFromLinkData` (`Internal.hs:1523`) returns `(Contact, Bool)` — the
  change flag — adapting its existing call at `Commands.hs:4374`. `CAPKnown` (`Controller.hs:1121`)
  gains `updated :: Bool` and `ownerVerification :: Maybe OwnerVerification`, so a by-link re-plan of a
  prepared contact surfaces a change flag and ownership, as `GLPKnown` does for groups. New
  `resolveKnownContact` mirrors `resolveKnownGroup` (`Commands.hs:4471`).

### B. Client: prepare + share the contact-address card

1. **Signing key — from the agent, not the chat DB.** Sign the card with the address short-link key
   via `getConnLinkPrivKey (aConnId addressConn)` (already in the agent, used at `Subscriber.hs:1597`;
   `getUserAddressConnection` gives the connection). This is the authoritative key — the private half
   of the short link's root key the directory verifies against — and it exists whenever the short link
   does, **including right after an upgrade** (`setConnShortLink` provisions it). Do **not** read the
   chat-DB `link_priv_sig_key` for signing: it is written only at `createUserContactLink` and never on
   upgrade. *(Separate cleanup, off the signing path: persist `link_priv_sig_key` on upgrade too —
   `setMyAddressData`/`setUserContactLinkShortLink` — reading it back via `getConnLinkPrivKey` so the
   column stops being stale.)*
2. **Card-builder API — `APIShareMyAddress {toSendRef :: SendRef}`** (Controller) + handler in
   `Commands.hs`, mirroring the group-share case (`APIShareChatMsgContent`, `Commands.hs:1185`):
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
   (`Commands.hs:5670` neighborhood), handler mirroring `SharePublicGroup` (`Commands.hs:2492`):
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

1. **`deChatLinkReceived` — add the `MCLContact` case** (`Directory/Service.hs:965`).
   - `deChatLinkReceived ct (MCLContact {connLink, business}) (Just ownerSig)`:
     - `APIConnectPlan userId (contact link) PRMAll (Just ownerSig)` — plan only (rename `PRMAllGroups` → `PRMAll`, extend to contact links). Returns `CPContactAddress (CAPOk {contactSLinkData_ = Just csld, ownerVerification})`. `verifyLinkOwner rootKey owners l' sig_` runs on this path (`Commands.hs:4379`, def `4634`); a plain/business address has `owners == []` and card `ownerId = Nothing`, so verification uses the link root key.
     - `OVVerified`:
       - resolved `peerType` from `csld.profile`: `CPTBot` → bot; `CPTHuman`/`CPTBusiness` (unset ≙ `CPTHuman`) → business, stored `CPTBusiness`; `CPTUnknown` → reject ("unsupported account type").
       - `APIPrepareContact userId ccLink verifiedDomain csld` → the prepared entity: `Contact` (`business = False`, `createPreparedContact` → `SCTDirect`) or business `GroupInfo` (`business = True`, `createPreparedGroup` → `SCTGroup`) (`Commands.hs:2129`) — `business` is a link-data flag, orthogonal to `peerType`.
       - `addContactRegStore` referencing that entity (`contact_id` or `group_id`), resolved `peerType`, status `GRSProposed`.
       - notify admins with the profile and the approve command.
     - `OVFailed reason` → "ownership verification failed".
     - `CAPKnown`/`GLPKnown` (already prepared) → re-registration path (§C.3), matched via `getContactRegByEntity`.
   - Keep the existing `MCLGroup` and fall-through cases unchanged.
2. **New store table `sx_directory_contact_regs`** — named migration in `Directory/Store/SQLite/Migrations.hs` and `Directory/Store/Postgres/Migrations.hs`. The prepared entity is a contact (direct/bot) or a business group (`business = True`), so the row references one or the other.

   ```sql
   CREATE TABLE sx_directory_contact_regs(
     contact_reg_id INTEGER PRIMARY KEY AUTOINCREMENT,
     contact_id INTEGER REFERENCES contacts(contact_id) ON UPDATE RESTRICT ON DELETE CASCADE,
     group_id INTEGER REFERENCES groups(group_id) ON UPDATE RESTRICT ON DELETE CASCADE,
     user_contact_reg_id INTEGER NOT NULL,
     submitter_contact_id INTEGER NOT NULL REFERENCES contacts(contact_id) ON UPDATE RESTRICT ON DELETE CASCADE,
     peer_type TEXT NOT NULL,
     contact_reg_status TEXT NOT NULL,
     contact_promoted INTEGER NOT NULL DEFAULT 0,
     created_at TEXT NOT NULL DEFAULT(datetime('now')),
     updated_at TEXT NOT NULL DEFAULT(datetime('now')),
     CHECK ((contact_id IS NULL) <> (group_id IS NULL))
   );
   CREATE UNIQUE INDEX idx_sx_directory_contact_regs_contact_id ON sx_directory_contact_regs(contact_id);
   CREATE UNIQUE INDEX idx_sx_directory_contact_regs_group_id ON sx_directory_contact_regs(group_id);
   CREATE UNIQUE INDEX idx_sx_directory_contact_regs_submitter_user_reg_id ON sx_directory_contact_regs(submitter_contact_id, user_contact_reg_id);
   ```

   Column roles (Postgres mirrors with `BIGSERIAL` + `TIMESTAMPTZ`):
   - `contact_reg_id` — global PK; the admin/superuser id.
   - `contact_id` / `group_id` — the prepared entity, exactly one set (`CHECK`): `contact_id` for a direct/bot address (`createPreparedContact`), `group_id` for a business-chat address (`createPreparedGroup`, `business = True`, `Commands.hs:2129`). Both `UNIQUE`; SQLite and Postgres treat NULLs as distinct, so the many-null side is unconstrained. Profile/`contactLink`/`contactDomain`/verification are read from the joined contact or group.
   - `user_contact_reg_id` — the user-facing id, from the shared per-submitter sequence (below).
   - `submitter_contact_id` — the submitter (forwarded the card); FK `contacts`, `ON DELETE CASCADE`; this is `dbContactId`, checked by `isOwner`.
   - `peer_type` — resolved listing type (`bot`/`business`); not recoverable from the profile (`peerType = human` for a business, §A).
   - `contact_reg_status` reuses the `GroupRegStatus` encoding (Q3); `contact_promoted` as `group_promoted`.

   **Shared per-submitter sequence.** `user_contact_reg_id` and `user_group_reg_id` draw from one per-submitter series, so `/list` numbers channels + bots + businesses uniquely. Allocated as `1 + MAX over both tables for the submitter` (COALESCE each sub-max to 0); race-free — inserts run only in the sequential event loop (`Service.hs:175-181`). `addGroupRegStore` (`Store.hs:252`) gains the second sub-select against `sx_directory_contact_regs`.

   Types + functions in `Directory/Store.hs`; the joined entity is a contact or a business group, so queries return `(DirectoryContactEntity, ContactReg)`:

   ```haskell
   type ContactRegId = Int64
   type UserContactRegId = Int64

   data DirectoryContactEntity = DCEContact Contact | DCEGroup GroupInfo

   data ContactReg = ContactReg
     { contactRegId :: ContactRegId,
       dbContactId :: ContactId,
       userContactRegId :: UserContactRegId,
       contactId :: Maybe ContactId,
       groupId :: Maybe GroupId,
       peerType :: ChatPeerType,
       contactRegStatus :: GroupRegStatus,
       promoted :: Bool,
       createdAt :: UTCTime
     }

   addContactRegStore    :: ChatController -> Contact -> DirectoryContactEntity -> ChatPeerType -> GroupRegStatus -> IO (Either String ContactReg)
   getContactAndReg      :: ChatController -> User -> ContactRegId -> IO (Either String (DirectoryContactEntity, ContactReg))
   getUserContactReg     :: ChatController -> User -> ContactId -> UserContactRegId -> IO (Either String (DirectoryContactEntity, ContactReg))
   getUserContactRegs    :: ChatController -> User -> ContactId -> IO (Either String [(DirectoryContactEntity, ContactReg)])
   getContactRegByEntity :: ChatController -> DirectoryContactEntity -> IO (Either String (Maybe ContactReg))
   setContactRegStatus   :: ChatController -> ContactRegId -> GroupRegStatus -> IO (Either String (GroupRegStatus, ContactReg))
   setContactPromoted    :: ChatController -> ContactRegId -> Bool -> IO (Either String (DirectoryStatus, Bool))
   deleteContactReg      :: ChatController -> ContactRegId -> IO (Either String ())
   getAllListedContacts  :: ChatController -> User -> IO (Either String [(DirectoryContactEntity, ContactReg)])
   ```

   `getContactRegByEntity` resolves re-registration — `getContactWithoutConnViaShortAddress` for a contact, `getGroupViaShortLinkToConnect` for a business group. `deleteContactReg` also deletes the prepared entity. `contact_reg_status` reuses `GroupRegStatus` including `GRSPendingApproval GroupApprovalId`, so the approval-version check is unchanged. Open decision: contacts as DB-only vs mirrored `CR*` append-only log records (`Store.hs:475`); the live read path uses the DB (`getAllListedGroups_`).
3. **Registration lifecycle mirrors channels.** `proposed → pending approval → active`, plus `suspended/removed`. On submission, notify admins with the profile and an approve command.

   Refresh requires core changes (§A). The `CCTContact` plan path (`Commands.hs:4362`) does **not** refresh a known entity today: `refreshContact` runs `updateContactFromLinkData` only for a by-name plan (`planDomain = Just`, `4372`), and there is no `resolveKnownContact`. Add, under `resolveMode == PRMAll`: `CAPKnown ct -> resolveKnownContact ct` (new, mirrors `resolveKnownGroup`, `4471`), and — for a business group found via the contact link (`getGroupToConnect`, `4402`) — `GLPKnown g -> resolveKnownGroup g` (existing). The periodic loop (`deGroupLinkCheck` analog, `Service.hs:828`) runs `APIConnectPlan … PRMAll` on each registered address link and refreshes the prepared contact or business group.

   Re-approval on change: `updateContactFromLinkData` (`Internal.hs:1523`) gains a change `Bool` (returns `(Contact, Bool)`, mirroring `updateGroupFromLinkData`), and `CAPKnown` carries it plus `ownerVerification` (§A) — the current constructor carries only `Contact`. A change transitions the registration to pending approval (hidden until re-approved), as `reapprove` (`Service.hs:858`) for channels. The channel `checkValidOwner` owner-list re-check has no contact analog.

   Re-submission of an already-prepared address routes through the same plan (`CAPKnown`/`GLPKnown`, matched via `getContactRegByEntity`) and re-verifies ownership from the plan's `ownerVerification` (`deReregistration` analog).
4. **Admin & user commands — shared commands take a `ChatRef`; group-only commands unchanged** (Q4, from the full `Service.hs` read).

   The directory id resolves **by caller role**, not a fixed type: user-run commands use the per-user local id via `getUserGroupReg` (`deUserCommand:1158` — `if isAdmin then withGroupAndReg else withUserGroupReg`), admin/superuser commands the global id via `getGroupAndReg`, and listings print `if isAdmin then groupId else userGroupRegId` (`:1504`). This split is preserved verbatim for contacts, against the contact table.

   **Shared commands switch their id field to `ChatRef`** (`Directory/Events.hs`, `DirectoryCmd` GADT):
   - `DCApproveGroup {groupId,..}` → `DCApprove {chatRef :: ChatRef, displayName :: Text, approvalId :: GroupApprovalId, promote :: Maybe Bool}`
   - `DCRejectGroup` → `DCReject ChatRef Text`; `DCSuspendGroup` → `DCSuspend ChatRef Text`; `DCResumeGroup` → `DCResume ChatRef Text`
   - `DCDeleteGroup` → `DCDelete ChatRef Text`; `DCConfirmDuplicateGroup` → `DCConfirmDuplicate ChatRef Text`
   - `DCSendToGroupOwner` → `DCSendToOwner ChatRef Text Text`; `DCPromoteGroup` → `DCPromote ChatRef Text Bool`
   - `DCListUserGroups` keeps no id; its handler lists group + contact regs together (merged last/pending too).

   **Group-only commands are not modified** — they keep the bare group-id parser (`gc`/`gc_`): `DCMemberRole`, `DCGroupFilter`, `DCShowUpgradeGroupLink`, `DCInviteOwnerToGroup`. A `@` id there is not a valid decimal, so it falls through to `DCCommandError` — parser-level rejection, no handler type check.

   **Parser** (`directoryCmdP`): a ref parser `('@' $> CTDirect) <|> ('#' $> CTGroup) <|> pure CTGroup` then `A.decimal`, building `ChatRef {chatType, chatId, chatScope = Nothing}`; used only by the shared commands. Bare = `CTGroup`, so `/approve 5:Name 1`, `/delete 5:Name` are unchanged.

   **Handlers** (`de{User,Admin,SuperUser}Command`): each shared command branches on `chatType chatRef` — `CTGroup` = existing group logic with `chatId chatRef` (per-role `getUserGroupReg`/`getGroupAndReg`); `CTDirect` = mirrored contact logic (`getUserContactReg`/`getContactReg`); any other `chatType` = error. The `if isAdmin` local/global choice is shared, unchanged.

   **Emitted command strings** carry the prefix — `/approve @<contactRegId>:<name> <n>` for contacts (bare for groups; `sendToApprove:822`), and `/list`/pending shows `@<id>` for contact rows (`sendGroupsInfo:1504`).
5. **Listing identity + verified SimpleX names.** The listing identity is the prepared entity (`contact_id` or `group_id`), 1:1 with the link. Name↔link verification is inherited from the core: `updateContactFromLinkData` (contact) / `updateGroupFromLinkData` (business group) reconciles the domain claim and sets the verified flag on each refresh (`Internal.hs:1524`); no directory-side resolve-and-compare. The verified name is read from the joined entity (when verified) into `DirectoryEntry.simplexName` and bot/web search. A link that stops resolving to the claimed name clears the verified flag through the same path.

### D. Listing + web

1. **`DirectoryEntryType`** (`Listing.hs:55`): add `DETContact {peerType :: ChatPeerType}`. The
   `taggedObjectJSON`/`dropPrefix "DET"` derivation already emits `{"type":"contact", ...}` for a new
   constructor for free (single→multi constructor is transparent); `peerType` serializes as
   `"business"`/`"bot"`/etc.
2. **`contactDirectoryEntry`** builder (analogue of `groupDirectoryEntry`, `Listing.hs:100`), from `(DirectoryContactEntity, ContactReg)`: `DirectoryEntry {entryType = DETContact peerType, displayName, simplexName, groupLink = PublicLink Nothing (Just connShortLink), shortDescr, welcomeMessage, imageFile, activeAt, createdAt}`. Profile fields (`displayName`, `shortDescr`, `description` → `welcomeMessage`, `image`), the link, and the domain → `simplexName` are read from the joined entity — a `Contact`'s `Profile` (`DCEContact`) or a business group's `GroupProfile` (`DCEGroup`); `peerType` from the `ContactReg`. `PublicLink` already models contact links (`Listing.hs:63-68`).
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

1. **Send side** — in `redactedMemberProfile` (`Internal.hs:1259`, which already redacts
   `shortDescr`/`contactLink`/name-proof under the group's `SGFSimplexLinks`/`SGFDirectMessages`),
   also redact `description` — with a **new inline-strip helper** (per G.3), not `shortDescr`'s
   drop-whole `removeSimplexLink`. Adding `description` to `Profile` forces this output record to be
   rebuilt here anyway. (Used on every member-profile-out path — `Internal.hs:1247,1255`,
   `Subscriber.hs:803,3220`, `Commands.hs:4230`.)
2. **Receive side** — apply the same redaction when ingesting a member profile from the network, so
   a peer can't inject a link/name-laden description. Chokepoints: `updateMemberProfile`
   (`Store/Groups.hs:3388`) and member creation (`Store/Groups.hs:2510`, `1395`); prefer a single
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
  (`chat/ChatView.kt:2234` `ChatBannerView`, which already has per-type captions — bot / business /
  contact) — extend to a business marker from `peerType`.

**`description` — shown via a "Read more" affordance, NOT inline** (the alert and the in-chat link
card `CIChatLinkHeader.kt` are too small — they carry only the short teaser). Rendered in **two
surfaces: the chat banner (`ChatBannerView`) and the contact info page (`ChatInfoView`, `:778`)**:
- Teaser text: if `shortDescr` is present → show `shortDescr`, then a clickable **"Read more"**; if
  `shortDescr` is absent → show the first line of `description` truncated to 100 chars with ellipsis
  (up to the first line break), then **"Read more"**. "Read more" appears only when a `description`
  exists to reveal.
- **"Read more" is a client-only `Format` span (Phase 1, implemented).** The `Modal {modalName}`
  variant lives only in the app's mirrored `Format` enum (Kotlin/Swift); it is **not** in Haskell
  `Markdown.hs`. The teaser is built app-side from the profile fields, and each client resolves the
  label and modal content from the current chat by `modalName`, rendering the tap (iOS sheet / Android
  modal). No Haskell core change.
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
- `src/Simplex/Chat/Controller.hs` / `Library/Commands.hs` / `Library/Internal.hs` — refresh core changes (§A/§C.3): `PRMAllGroups → PRMAll` + `resolveKnownContact`, `CAPKnown` gains `updated`/`ownerVerification`, `updateContactFromLinkData → (Contact, Bool)`.
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
- **Q6 — Updates (RESOLVED: prepared contact refreshed on the channel loop).** The periodic loop runs `APIConnectPlan … PRMAll` on each registered address link (`deGroupLinkCheck` analog, `Service.hs:828`); under `PRMAll` a new `resolveKnownContact` (mirroring `resolveKnownGroup`) invokes `updateContactFromLinkData` to refresh the prepared contact and reconcile `contactDomain`/`contactDomainVerified`; a business group refreshes via `resolveKnownGroup`. No connection (§A/§C.3).
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

5. Directory store: migration (two nullable ids + `CHECK`) + `ContactReg` model/queries + shared per-submitter sequence.
6. Core refresh changes (§A: `PRMAll`/`resolveKnownContact`, `CAPKnown.ownerVerification`, `updateContactFromLinkData` change flag), then `deChatLinkReceived` `MCLContact` case (verify → `APIPrepareContact` → `addContactRegStore`) + admin approval + directory test through to "listed".
7. Listing merge (`DETContact` + `contactDirectoryEntry` + `generateListing`) + one unified
   group+contact search + website rendering.

**Deferred:** peerType setting/badge UI; the support-bot entry point (§B.4).
