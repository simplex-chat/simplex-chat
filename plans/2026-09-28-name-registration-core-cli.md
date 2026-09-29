# Name registration: core and CLI (#7530)

Plan for the core and CLI half of phase 3a name registration, before any UI work. The UX canvases (`plans/sketches/2026-09-18-names-phase3a-registration.excalidraw`, `2026-09-23-names-backup-restore.excalidraw`) are tentatively approved. Price: **$100 for 2 years**.

Decisions answered by the owner on 2026-09-28 are in §3. Those still open are in §4, each with a recommendation. Nothing that depends on an open decision should be built before it is answered.

## Table of contents

1. Scope
2. Starting point
3. Decisions taken
4. Decisions still open
5. ShopService: widening and renaming the badge service
6. Name codes
7. Registration in core
8. CLI commands
9. The service side
10. Chain access over JSON-RPC
11. Changes to #7530's declarations
12. Tests
13. Order of work
14. Done means
15. Canvas fixes and open UX questions

## Executive summary

A user registers a SimpleX name from the CLI with a code. The name ends up owned by a key of the device wallet (#7475), pointing at the user's address and/or channel, which claim it at once.

- **ShopService.** The badge service is widened to register names and renamed ShopService (§5). Wire tags need no change.
- **Core drives registration.** The core computes the commitment over the contract's `Registration` struct and keeps the secret. The ShopService submits commit and reveal itself over Ethereum JSON-RPC, to the reth node the resolver deployment already runs. It dry-runs every transaction and sends only what succeeds (§7, §9, §10). The core resumes after a restart.
- **Table codes like badges**, whose text says what they cover: shortest name and years, e.g. `SN6-2Y-4K2P7-TQ9M1-ZX3RB-8HJ5W` (§6). Codes only in phase 3a; in-app purchase comes later.
- **CLI:** user-facing `/name …` commands for the active user, plus the `/_name … <userId> …` API forms the apps call, as `/contacts` and `/_contacts <userId>` are paired (§8).
- **#7530 keeps** the commit/reveal protocol and the code, state, retry and cancel commands. It **loses** purchase, renewal and link changes until they are needed (§11).
- **Stacked PRs** (§13): rename; the wallet lands; transaction signing in simplexmq; #7530's implementation with a fake JSON-RPC node; then a test chain, in-app purchase, the wallet's follow-ups, and the UI.

---

## 1. Scope

**In**, from the registration canvas (1–11) and the backup canvas (1–11):

- check a code (4, 4a, 4b)
- register a name with a code on any build (5, 5d, 5e, 5g, 5h)
- the recovery key before the first registration (7, backup 1). This is the wallet's command, used here.
- progress: committing, waiting, registering (9)
- registration survives the app closing (9a)
- taken, with the payment kept for another name (10a)
- paid but not registered, retried without paying twice (10b)
- success (10)
- the address or channel claims the name at once (11)
- listing the user's names (backup 2), and refusing hidden profiles (backup 9)

**Search is not new work.** Registration's search (3, 3b) is the lookup API of #7525 (`/_connect plan <userId> <name>`), called with `resolve=all`. The default mode answers a known chat from the store for up to a day without asking the registry, so a lapsed name would read as "taken". This is decision 4 of the lookup plan (`plans/2026-09-22-name-lookup-core-api.md`, as #7530 edits it); this plan only fixes the mode.

**Out:**

- everything the canvas footer excludes: "Not in phase 3a: owning a name (the hub with names, changing where a name points, renewing), transfers, subnames, and expiry reminders by message";
- in-app purchase;
- the UI;
- the wallet's scan improvements drawn on backup 7, 8 and 10 (D16).

## 2. Starting point

**#7530 today** declares types only, in `Controller.hs` (`APIPurchaseName`, `APIRedeemNameCode`, `APICheckNameCode`, `APIGetNameState`, `APIRetryName`, `APICancelName`, `APISetNameLinks`, `APIRenewName`, `APIRenewNameCode`, `CRNameState`, `CRNameCode`, `CEvtNameChanged`, `CENameError`, `NameLinks`, `NameState`, `NamePurchaseStatus`, `NameError`) and in `Badges/Service.hs` (`BSCPurchaseName`, `BSCRedeemNameCode`, `BSCCommitName`, `BSCRevealName`, `BSCRenewName`, `BSCSetNameLinks`, `BSPNameCredit`, `BSPNameCommitted`, `BSPName`, `NameCredit`, `NameReveal`, `SignedNameLinks`, `namesBadgeServiceVersion = 2`).

Nothing handles them, and the branch does not compile under `-Werror=incomplete-patterns`. The client already sends version 2 on every badge request, so the service must be deployed before clients.

**The badge service** (`apps/simplex-badge-service`, core `Simplex.Chat.Badges.*`, `Simplex.Chat.PaymentService`):

- a chat bot answering SMP service requests;
- versioned envelope `BadgeServiceRequest {version, purchaseKey, request}`, signed by `purchaseKey`;
- one worker per user in core;
- "SB-" table codes (`Badges/Code.hs`: 20 Crockford base32 characters, the last a Luhn mod-32 check);
- BTCPay and Stripe providers;
- address in `ChatConfig.badgeServiceAddress`.

**The wallet (#7475, `ab/wallet`)** is not merged:

- one BIP-39 key per device (24 words, no passphrase);
- one hardened account per name at `m/44'/60'/n'/0/0`, bound to a profile in `wallet_accounts`;
- an allocation counter starting at 1;
- explicit creation only; hidden profiles refused;
- `/_wallet …` API commands.

**The contract** (`SimplexController.sol`, based on ENS):

- `makeCommitment` returns `keccak256(abi.encode(Registration {label, owner, duration, secret, resolver, data, reverseRecord, referrer}))`, where `data` is the resolver calls applied at registration;
- `minCommitmentAge` and `maxCommitmentAge` are set per deployment;
- a credited path, `registerWithCredit(Registration)`, lets a registrar submit on the buyer's behalf. It draws on the registrar's allowance in attoUSD, which the owner sets with `setRegistrarAllowance`; zeroing it is the contract's "kill switch for a compromised registrar".

**The SNRC resolver** (simplexmq `scripts/resolver/service/snrc-resolve.py`, branch `ab/snrc-resolver-owned-by`) is a Python HTTP service. It serves `/resolve/<name>` and `/owned-by/<address>` to SMP relays through `Server/Names/HttpResolver.hs`. Its `docker-compose.yml` runs `reth --minimal` with nimbus (about 260 GB on mainnet), which serves JSON-RPC (`--http.api eth,net`) on `127.0.0.1:8545`. The resolver reads the chain through that node, and ShopService will send through it too.

**Crypto.** simplexmq's ETH crypto (#1843) covers key derivation, keccak and EIP-55 addresses. It has no ABI encoder and no signing. Registration needs no signature from the owner, because ShopService submits both transactions from its own relayer key. That key's transactions do need recoverable signing, which is not there yet (D20).

## 3. Decisions taken

**Answered by the owner on 2026-09-28**

- **D1 Name:** ShopService.
- **D4 Registration** is driven by core, as #7530 declares (commit, wait, reveal).
- **D5 Chain access:** ShopService calls Ethereum JSON-RPC directly on the resolver host's reth node, with no Python in its path. Every transaction is dry-run before it is sent, so an invalid intent costs nothing (§10). This also removes the earlier question of who may call relayer endpoints.
- **D6 Codes:** table codes like badges, whose text shows the shortest name covered and the duration (§6).
- **D7 Payment:** codes first; in-app purchase after badges' store receipts (#7590).
- **D8 Ownership:** names belong to the profile that registered them, through the wallet's account binding.
- **D9 Claiming:** core claims the name for the address and channel on success.
- **D11 CLI:** user-facing `/name …` commands, plus the API forms with a userId (§8).
- **D12 Declarations:** remove whatever can be done later (§11).
- **D13 Links:** `NameLinks` points at the calling profile's own address (a Bool), not at another profile's.
- **D17 Encoding:** the `Registration` ABI encoding and commitment live in one chat module, used by both core and ShopService.
- **D18 Expiry:** a code's text carries no expiry. The service reports an expired code at redemption.
- **D20 Signing:** recoverable secp256k1 signing (re-enabling the vendored library's recovery module), RLP and EIP-1559 transactions go into simplexmq, in a PR stacked on #1843 (`ab/eth-crypto`), under EP's FFI rules (IO, `ScrubbedBytes`, randomness passed in). The JSON-RPC client stays in the ShopService app.
- **D21 Key custody:** the relayer key lives in ShopService's ini, like the badge issuer secret. The relayer holds only gas and a small allowance, topped up as needed; zeroing the allowance stops a stolen key.
- **D22 Confirmations:** a reveal counts as registered at its receipt plus 3 blocks. A commit only needs inclusion, since its age counts from its block.

**Carried over from reviews and prototypes**

- Transport is SMP service requests, with type-tagged JSON whose constructor prefix is dropped. Error codes are snake_case text with an `Unknown` pass-through (badges, #7390).
- Versioning uses `VersionRange` as badges do. Names are version 2.
- The wallet works as in #7475. #7390's lazy seed, `/0/k` paths and `users.wallet_*` columns are dropped.
- Constructors follow `APIVerbObject` naming (EP on #7475).
- One command, one response; progress arrives as events; core never prompts (#7390 RFC).
- Mutating service calls carry a request id, and the service replays settled answers (#7390).
- Client and service use the same `validLabel`: a–z, 0–9, inner hyphens, no `xx--` prefix (#7390).
- Names never gate app features (#7390 RFC).

## 4. Decisions still open

| # | Question | Options | Recommendation |
|---|---|---|---|
| D2 | How deep the rename goes | **Rename:** internal Haskell, the executable, scripts, Docker and compose names, core error texts and SDK type names. **Keep:** wire tags (product-named) and persisted names (`sx_badge_service_` prefix, its migrations table, database name, ini path), since the bot's address, hard-coded into every client, lives in that database. Alternative: rename persisted names too, with migrations and a database move. | Keep persisted names, rename everything else. Mobile strings change with the UI. |
| D3 | Where the rename lands | (a) its own PR off master; (b) inside #7530 | **(a)**: a pure refactor of master's code, reviewed on its own. |
| D10 | After a restart mid-registration | (a) the worker carries on by itself: it reveals when due and commits again if `maxCommitmentAge` passed. The UI shows progress with Cancel. (b) The worker waits for Continue or Cancel (canvas 9a as drawn). | **(a)**: with (b) a registration stalls until the app is opened, and its commitment can age out. |
| D14 | Background work in core | (a) widen the per-user badge worker, renamed with the service; (b) a separate names worker | **(a)**: one service, one worker per user, one wake schedule. |
| D15 | Bot API docs | (a) list the name commands, responses and events as undocumented, as badges and wallet are; (b) document them now | **(a)**: prior art, and it keeps the generated SDK stable. |
| D16 | Wallet needs from the backup canvas | Scan progress, stop and start-from (7, 8), the counter reset when an older database is imported (10), a device-wide name count (9), a "written down" flag (2, 4). (a) in the wallet stack; (b) in this plan. | **(a)**: wallet behaviour; this plan only binds accounts. |

## 5. ShopService: widening and renaming the badge service

**The rename PR** is off master with no behaviour change, if D3(a):

- `Simplex.Chat.Badges.Service` becomes `Simplex.Chat.ShopService`, flat like `Simplex.Chat.PaymentService`.
- `BadgeServiceRequest`/`Command`/`Response`/`Version`/`ErrorCode` become `ShopService…`.
- The version values and range are renamed to match.
- `ChatConfig.badgeServiceAddress` becomes `shopServiceAddress`.
- **Constructor prefixes** `BSC`/`BSP`/`BSE` become `SHC`/`SHP`/`SHE`. `SSC` and `SSP` are taken (`SSCMemory`, `SSPComplete`). Every rename updates its `dropPrefix` string in the same commit, or the wire tags silently change. The error texts are written by hand and do not move.
- **The app:** `apps/simplex-badge-service` becomes `apps/simplex-shop-service`, its `BadgeService.*` modules become `ShopService.*`, `BadgeServiceOpts`/`Env` become `ShopService…`, and the executable becomes `simplex-shop-service`. The same goes for `scripts/badge-service`, the Docker and compose names, test modules, hspec labels, and the core error texts ("shop service not configured").
- Badge-domain modules keep their names: `Simplex.Chat.Badges`, `Badges.Types`, `Badges.Code`, `Badges.Ledger`, `Store.Badges`.
- A JSON test pins every command and response tag before the rename, not just the three in `tests/BadgeTests.hs` ("service protocol JSON"). The rename then provably leaves the wire unchanged.
- Persisted names stay, if D2 is answered as recommended.

**Widening** (in #7530): the envelope, purchase key and versioning are shared, and the name commands arrive at version 2. The service's dispatch must accept a creating command under a fresh purchase key. Today only `BSCRedeemBadgeCode` and `BSCIssueBadge` are handled. Any other command gets `unsupported_version` for an absent or known key and `unknown_purchase_key` for an unknown one.

## 6. Name codes

**Proposed format, for confirmation:** `SN6-2Y-4K2P7-TQ9M1-ZX3RB-8HJ5W`.

- `SN` is the kind, as `SB` is for badges.
- `6` is the shortest name the code covers.
- `2Y` is the registration's term in years.
- The rest is the badge code's body: 20 Crockford base32 characters, the last a Luhn mod-32 check. The check is taken over the parameters as well, so `SN6` edited to `SN3` fails.

Reading folds case and the look-alike characters, as `Badges/Code.hs` does. That module grows a code kind rather than being copied.

The service stores each code with its parameters and looks it up by the hash of the whole code, so edited parameters never match a real code. The parameters in the text are for the device: canvas 4's "Your code covers names of 8 characters or more" and 3a's length check need no request.

Codes are issued by the web store (5f) and the operator's `//issue`, into the same code table as badge codes, with a kind column. Whether a code is spent or expired is known only to the service (D18).

## 7. Registration in core

**Flow**

1. `APICheckNameCode userId code` parses the code on the device and returns `CRNameCode {credit = NameCredit {minLength, years}}`. It sends nothing.
2. `APIRedeemNameCode userId domain nameLinks code` does the following:
   - refuses hidden profiles (the wallet refuses to bind for them);
   - checks `validLabel` and the code's minimum length;
   - binds the next wallet account to the profile;
   - generates a purchase key and a random 32-byte secret;
   - stores a `name_purchases` row;
   - returns `CRNameState`, with progress following as `CEvtNameChanged`.
3. The worker then does the following:
   - sends `redeemNameCode {code}`, which yields `BSPNameCredit`, carrying the credit and the registration parameters the service will submit (resolver address, duration; `reverseRecord = 0`, `referrer` zero);
   - computes `commitment = keccak256(abi.encode(Registration …))`, with `data` holding the resolver calls that set `simplex.contact` and/or `simplex.channel` (D17);
   - sends `commitName {requestId, commitment}` and gets `BSPNameCommitted {revealAfter}` (status waiting);
   - at `revealAfter`, sends `revealName {requestId, nameReveal}` and gets `BSPName {registration}` (status registered);
   - on success, claims the name for the address (the `/_set domain` logic, creating the address and its short-link data if missing) and/or the channel (D9).
4. **Taken** (the reveal returns `name_taken`): the credit stays on the purchase key. `APIRetryName userId namePurchaseId (Just domain')` commits the new name with a new secret, the same credit and the same account, since no name landed on it.
5. **Failed:** `APIRetryName … Nothing` retries from the step that failed. Request ids make every step idempotent, so nothing is paid twice.
6. **Cancel:** `APICancelName` works until the reveal is sent; the credit stays.
7. **Restart:** per D10.
8. `APIGetNameState userId` lists the profile's names and registrations in progress.

**Status.** `NamePurchaseStatus` keeps #7530's shape: committing, waiting until a time, revealing, registered, taken, failed, cancelled.

**Persistence** (one migration per backend). A `name_purchases` table modelled on `badge_purchases`:

- `name_purchase_id`, `user_id`;
- `purchase_priv_key` (Ed25519, as for badges);
- `wallet_account_index`;
- `domain`, `contact_address` (bool), `channel_group_id`;
- `min_length`, `years`;
- `commit_secret`, `commitment`, `request_id`, `reveal_after`;
- `status`, `registration` (JSON as the service wrote it), `service_error`, `next_wake_at`, and timestamps.

A registered name's account is also recorded in the wallet's per-account table, so the wallet and this table agree.

**Errors.** `CENameError` with `NameError`:

- `NEInvalidCode`, now meaning not a well-formed name code;
- `NEServiceNotConfigured`;
- `NEServiceError {serviceError :: ShopServiceErrorCode}`, which covers spent and expired codes;
- `NEInvalidResponse`.

Hidden profiles surface as the wallet's `CEWallet WEHiddenProfile`.

**Command fields.** Domains in command records use `StrJSON "SimplexDomain" SimplexDomain`, as master's `APISetUserDomain` does.

## 8. CLI commands

The prior art pairs API forms with a userId and user-facing forms without one, which act on the active user and delegate to the API forms:

- `/_contacts <userId>` and `/contacts`;
- `/_badge state <userId>` for the API side;
- bare plurals list things (`/contacts`, `/groups`);
- options go before the last argument (`/_prepare contact <userId> <link>[ domain=<d>] <json>`);
- free text goes last.

User-facing forms refer to a channel by name, as `#team` elsewhere; the API forms use ids.

```
API (apps)                                                                  user-facing (CLI)
/_name code <userId> <code>                                                 /name code <code>
/_name redeem <userId> <domain>[ address=on|off][ channel=<groupId>] <code> /name redeem <domain>[ address=on|off][ channel=#<group>] <code>
/_name state <userId>                                                       /names
/_name retry <userId> <namePurchaseId>[ domain=<domain>]                    /name retry <namePurchaseId>[ domain=<domain>]
/_name cancel <userId> <namePurchaseId>                                     /name cancel <namePurchaseId>
```

`address` defaults to on, as screen 5 proposes the profile's address. `address=off` with no channel registers a name that points nowhere (5d).

Search and the name's current state stay `/_connect plan <userId> <name> resolve=all` (#7525). The view follows `viewUserBadgeState`, one line per name: `1: dynamis.simplex, registered, expires 2028-12-12, points at your address`.

## 9. The service side

**Name commands**

- `redeemNameCode {code}` looks the code up by hash, refuses spent or expired codes, and credits the purchase key with one name of the code's tier, at most once.
- `commitName {requestId, commitment}` checks there is an unspent credit, dry-runs and sends `commit(commitment)` (§10), and returns `revealAfter`: the commit block's timestamp plus `minCommitmentAge`.
- `revealName {requestId, nameReveal}` does the following:
  - rebuilds the `Registration` with the shared module (D17);
  - checks it against the commitment, `validLabel`, the reserved set and the credit's minimum length;
  - dry-runs and sends `registerWithCredit` (§10);
  - spends the credit only on success, and answers `name_taken` if someone else registered the name first.

**State.** A table of commitments and reveals by request id: phase, transaction hashes and result, under the existing prefix if D2 is answered as recommended. A restart re-reads it, and settled answers are replayed.

**Codes** live in the code table with a kind (§6). The web store and `//issue` gain the name kind.

## 10. Chain access over JSON-RPC

**Endpoint.** ShopService talks to the reth node the resolver deployment runs (§2), which already exposes the `eth` namespace. It runs either on that host against `127.0.0.1:8545`, or with the RPC exposed to ShopService on a private network. Its ini gains a `[chain]` section: the RPC URL, the chain id, the controller and resolver addresses, the relayer key (D21) and the confirmation depth, 3 blocks (D22).

**Dry run first, always.** Before any transaction is sent, ShopService does the following:

- executes it with `eth_call`, from the relayer's address, with the exact calldata and value, against the pending block, and estimates its gas with `eth_estimateGas`;
- for a reveal, also checks with `eth_call` that `makeCommitment(registration)` equals the commitment it committed;
- sends only if all of this succeeds.

A failed dry run sends nothing and spends no gas or credit. Its revert is decoded from the contract's custom errors and answered as a service error:

- `NameNotAvailable` becomes `name_taken`;
- the others (`UnexpiredCommitmentExists`, `CommitmentTooNew`, `CommitmentTooOld`, `NameTooShort`, `NameReserved`, an exhausted allowance) map to their own codes.

Only a purchase key with an unspent credit can cause a transaction at all, with at most one in flight per credit. So a stream of invalid intents costs the service RPC calls, never gas.

**Sending.**

- One relayer key sends serially, taking nonces from `eth_getTransactionCount` on the pending block.
- EIP-1559 fees come from `eth_feeHistory`, and transactions go out through `eth_sendRawTransaction`.
- Receipts are polled. A transaction not mined within a few blocks is replaced with a higher fee.
- "Registered" follows the reveal's receipt plus 3 blocks (D22).
- State can still change between the dry run and inclusion, e.g. another registration landing first. The transaction then reverts and costs its gas; it is reported as taken or failed, and the credit stays.

**Signing** comes from simplexmq (D20). The `Registration` calldata comes from the shared module (D17), so what the core committed to and what is sent come from one encoder.

**Tests** use a fake JSON-RPC node, like `FakeBTCPay`. It answers the calls above, scripts reverts, and advances blocks and time. The hardhat node in `simplex-namespace-contract` serves local end-to-end runs.

## 11. Changes to #7530's declarations

- **Keep and rename to `ShopService`:** `APIRedeemNameCode`, `APICheckNameCode`, `APIGetNameState`, `APIRetryName`, `APICancelName`, `CRNameState`, `CRNameCode`, `CEvtNameChanged`, `CENameError`, `NameState`, `NamePurchaseStatus`, `NameError`, `NameCredit` (without `expiresAt`, D18), `NameReveal`, `BSCRedeemNameCode`, `BSCCommitName`, `BSCRevealName`, `BSPNameCredit` (plus the registration parameters), `BSPNameCommitted`, `BSPName`, `BSENameTaken`, `BSENameNotCovered`, and version 2.
- **Change:**
  - `NameLinks.contactUserId :: Maybe UserId` becomes `contactAddress :: Bool` (D13);
  - domains become `StrJSON`;
  - commit and reveal carry request ids.
- **Remove until needed:**
  - `APIPurchaseName` and `BSCPurchaseName` (in-app purchase);
  - `APISetNameLinks`, `BSCSetNameLinks` and `SignedNameLinks` (changing where a name points);
  - `APIRenewName`, `APIRenewNameCode` and `BSCRenewName` (renewal).

## 12. Tests

Following `tests/ChatTests/Names.hs`, `tests/Bots/BadgeService/*` and `tests/NameResolver.hs`:

- **Tag pinning:** JSON tests for every command and response tag, before and after the rename.
- **Codes:**
  - parse and format;
  - folding of look-alike characters;
  - the check character catching an edited parameter;
  - kind and parameters read on the device.
- **Commitment:** matches `SimplexController.makeCommitment` for fixed vectors, taken from the hardhat deployment in `simplex-namespace-contract`.
- **Dry run:** a scripted revert sends nothing and keeps the credit; a reveal whose `makeCommitment` differs is refused before sending; a revert after a passing dry run is reported and keeps the credit; concurrent registrations get consecutive nonces; a stuck transaction is replaced.
- **End to end,** with ShopService and the fake JSON-RPC node:
  - redeem, commit, wait and reveal reach registered, and the address and the channel carry the claim;
  - `/_connect plan … resolve=all` then shows the name as own;
  - taken, then retry with another name, keeps the credit;
  - cancel before reveal keeps the credit;
  - failure, then retry, is not charged twice;
  - a restart in the waiting phase completes (D10), and one past `maxCommitmentAge` commits again;
  - hidden profiles, spent codes, expired codes and codes too short for the name are refused;
  - a service without names support (version 1) yields a clear error.
- **Regeneration:** the schema dump, query plans and API docs, run from tmpfs.

## 13. Order of work

1. **Rename PR off master** (D3): a pure refactor with the tag-pinning test. Done when every existing badge test passes, with only labels changed.
2. **#7475 wallet lands**, and #7530 rebases on it.
3. **simplexmq, stacked on #1843 (`ab/eth-crypto`):** recoverable secp256k1 signing, RLP and EIP-1559 transactions (D20).
4. **#7530:**
   - trim and rename the declarations (§11);
   - the code kind (§6), the `Registration` module (D17), the core flow and store (§7), the CLI (§8), the service side (§9), and the JSON-RPC client with its dry run (§10), against the fake node;
   - tests (§12).
   - Done when §14 holds.
5. **Against a test chain:** the hardhat node, then the Sepolia or testing deployment, with the resolver host's reth for the latter.
6. **In-app purchase for names**, after badges' #7590.
7. **Wallet follow-ups** in the wallet stack (D16).
8. **UI.**

## 14. Done means

- A CLI user with a code registers a name through ShopService and the fake JSON-RPC node, with progress events. Their address and/or channel claim the name at once.
- Every transaction is dry-run first; a failing dry run sends nothing and keeps the credit.
- The commitment matches the contract's for fixed vectors.
- Registration survives a restart, taken names keep the credit for another name, and nothing is paid twice.
- Hidden profiles cannot register.
- The service is renamed and badges behave as before, wire tags included.
- #7530 compiles with no unhandled declarations.
- The schema dump, query plans and API docs are regenerated.

## 15. Canvas fixes and open UX questions

- **Price.** $100 for 2 years. These still say $20:
  - registration 1d ("$20 for 2 years"), which the lookup plan's decision 1 quotes;
  - 5h;
  - the store sheet in 8 ("$20.00");
  - the arrow "Register for $20";
  - 5f ("$20.00").
- **Canvas 4's example** `SMPX-4K2P-7T` becomes the §6 format. 4b's "expired on …" appears after the redeem request (D18).
- **9a** depends on D10.
- **Scope conflict:** the registration canvas puts "the hub with names" out of phase 3a, while backup 2 draws one. `/names` and `APIGetNameState` serve either.
