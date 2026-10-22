# StoreService names backend: tasks for stages 2–4

This is the implementation plan for the names backend in StoreService: stages 2, 3 and 4 of [`2026-10-09-names-web-store-plan.md`](2026-10-09-names-web-store-plan.md), and the service side of stages 6 and 7. Every endpoint must answer as [`2026-10-21-store-api.md`](2026-10-21-store-api.md) says; the plan says how. Tasks are numbered T1–T21 (T7 split into T7a and T7b), each with its goal, files, acceptance, tests and dependencies. The owner's answers and the decisions taken here close the document.

## Where and how

- **Branch:** `sh/store-service` in the worktree `/project/git/simplex-chat-store`, from Alain's `origin/ab/names-registration-primitives-sponsored` (`f06910698`). PRs target `names`; the branch is rebased when Alain's lands there. Nothing is pushed without the owner.
- **Stage 1 first.** The StoreService rename (workstream W3) lands before T1. Paths below use the new names:

  | Before | After |
  |---|---|
  | `apps/simplex-badge-service/`, `BadgeService.*` | `apps/simplex-store-service/`, `StoreService.*` |
  | `Simplex.Chat.Badges.Service`, `BSC`/`BSP`/`BSE` | `Simplex.Chat.StoreService`, `STC`/`STP`/`STE` (wire tags unchanged) |
  | `tests/Bots/BadgeService/` | `tests/Bots/StoreService/` (as W3 names it) |
  | `scripts/badge-service/` | `scripts/store-service/` |

  `src/Simplex/Chat/Badges/Code.hs` and `Types.hs` keep their names. Persisted names stay: the `sx_badge_service_` prefix, the migrations table, the database.
- **simplexmq.** The branch pins simplexmq `4e6f5586` ("add EIP712 (#1941)") in `cabal.project` (`packages: .`), with its hash in `scripts/nix/sha256map.nix`. Verified 2026-10-21 in the store worktree's cabal source cache (`dist-newstyle/src/simplexmq-*`):
  - `Simplex.Messaging.Eth.Address` exports `Address` (no constructor), `unAddress`, `addressFromPublicKey`, `addressFromPrivateKey`, `recoverAddress`, `deriveAddress`. `checksumAddress` is not exported; it is `strEncode` of an `Address`. `strP` accepts all-lower or all-upper hex unchecked and checks EIP-55 only on mixed case.
  - `Eth.EIP712`: `hashTypedData`, `Eip712Value` (`VUint | VAddress | VBytes | VArray`), `serializeSignature`. A `bytes32` field is `VUint` of its 32 bytes.
  - `Eth.Transaction`: `signEip1559Tx` (raw bytes; the hash is keccak256 of them). `Eth.RLP`.
  - `Crypto.Secp256k1`: `recoverPublicKey`, which accepts a high `s`. `Crypto.BIP39`.
  - `SimplexName`: `SimplexDomain {nameTLD, domain, subDomain}`, `labelHash`. `nameLabelP` is not exported, and `strDecode` of a bare label fails ("domain requires TLD").
  - No EIP-191 helper; T3 adds it. No pin change is needed.
- **One worktree per task.** Every task of a parallel wave works in its own worktree and branch, made from `sh/store-service` at the wave's start:

  ```
  git -C /project/git/simplex-chat-store worktree add /project/git/simplex-chat-store-t5 -b sh/store-t5 sh/store-service
  ```

  Each worktree has its own `dist-newstyle`. At the wave's start the manager builds `sh/store-service` (the merged head) once in `/project/git/simplex-chat-store`; each new task worktree then starts from a copy of that build, so a task rebuilds only what it changes:

  ```
  cp -a --reflink=auto /project/git/simplex-chat-store/dist-newstyle /project/git/simplex-chat-store-t5/
  ```

  The manager reviews each task branch and merges them into `sh/store-service` at the end of the wave, one at a time, building and testing after each merge.
- **One cabal build at a time, across all worktrees.** The container runs out of memory with two. Every cabal command, by any agent, in any worktree, runs under one global lock:

  ```
  flock /project/git/.store-cabal.lock cabal build -j1 exe:simplex-store-service
  flock /project/git/.store-cabal.lock cabal build -j1 simplex-chat-test
  flock /project/git/.store-cabal.lock cabal test -j1 simplex-chat-test --test-options='-m "StoreService names"'
  ```

  Run it in the foreground with a timeout of at most 600000 ms, or in the background and wait for its exit notification. Never poll with `sleep`, `tail` or `ps` loops.
- **Postgres.** The service's tests run on SQLite by default. The Postgres build uses its own build directory, so it does not reconfigure the SQLite one: `flock /project/git/.store-cabal.lock cabal build -j1 --flags=client_postgres --builddir dist-postgres simplex-chat-test`. Run it once per wave after the merges, where a Postgres server is available; else leave it to CI and say so.
- **Test labels.** Each new spec sits under `describe "StoreService names: <part>"`, so `-m "StoreService names"` runs them all and `-m "StoreService names: <part>"` one task's. Badge specs keep their labels and must stay green.
- **Shared files.**
  - *Registration lists*, where each task adds only its own lines: `simplex-chat.cabal` (each task adds its own modules to the executable's and the test suite's `other-modules` once they compile), `tests/Test.hs` (its specs), `StoreService/Web/Names.hs` (its handler module's routes), `StoreService/Names/Env.hs` (its part's field and its construction in `newNamesEnv`). The manager resolves the line conflicts at merge.
  - *Owned files*: within a wave, each of these has one owner task, given in the wave table; no other task of the wave edits it: `StoreService/Web/Server.hs`, `StoreService/Web/Http.hs`, `StoreService/Service.hs`, `StoreService/Names/Store.hs`, `StoreService/Names/Turns.hs`, the two migration modules, `StoreService/Names/Orders.hs`, `StoreService/Store/Invoices.hs`, `StoreService/Orders.hs`, `StoreService/Poller.hs`, `StoreService/Web/Names/Registration.hs`, `StoreService/Names/Worker.hs`, `tests/Bots/StoreService/NameRegistrationTests.hs`, `tests/Bots/StoreService/FakeEthNode.hs`, `apps/simplex-store-service/README.md`.
  - A task that needs a change in a file it does not own stops and tells the manager, who moves the change into the owner's task or the next wave.
- **Fakes.** `tests/Bots/StoreService/FakeEthNode.hs` (96 lines on the branch) is extended by T5 into a scriptable JSON-RPC node. `FakeBTCPay` and `FakeStripe` are in-process test fixtures for paid orders. The local anvil chain and deploy script are the webapp's (`simplex.domains/webapp/dev/start.sh`, `dev/deploy.ts`; see memory "names local chain").

## Tasks

### Stage 2: core and primitives

#### T1. Name codes: one sum type for owner and prepaid codes

- **Goal:** a code's kind in its type, parsed from one text field, as Check your code needs.
- **Files:** `src/Simplex/Chat/Badges/Code.hs`; `tests/BadgeTests.hs`; the owner-code users (`tests/Bots/StoreService/WebUIMock.hs`, and any other `parseNameCode`/`formatNameCode` caller, found with `rg`).
- **Do:**
  - `data NameCode = NCOwner NameOwnerCode | NCPrepaid NamePrepaidCode`. The branch's `NameCode` becomes `NameOwnerCode` (`randomNameOwnerCode`, `nameOwnerCodeEntropy`, `parseNameOwnerCode`, `formatNameOwnerCode`).
  - `parseNameCode :: Text -> Maybe NameCode` dispatches on the body's length after `SN`, separators removed: 30 is an owner code, 21 or 22 a prepaid code. The 20-character body folds as badge codes do (case, `I`/`L` → `1`, `O` → `0`); the length must be ASCII digits, as the webapp's `parsePrepaid` requires.
  - `NamePrepaidCode`: the length digits (1–2, value 6–63) and 20 characters, the last a Luhn mod-32 check over the length digits' values and the 19 payload values. `randomNamePrepaidCode :: TVar ChaChaDRG -> Int -> IO NamePrepaidCode`, `namePrepaidCodeText` (canonical, `SN<len><20>`), `formatNameCode` (display, `SN<len>-XXXXX-…`), `namePrepaidCodeHash` (raw SHA-256 bytes of the canonical text, stored as a blob as `badgeCodeHash` is; unpadded base64url only on the wire), `namePrepaidCodeLength`.
- **Acceptance:** an owner code never parses as prepaid and the reverse; a single-character change fails the check, and so does an adjacent swap except `0`↔`Z` (Luhn mod 32's blind spot, as for badge codes); the base64url of the hash equals the webapp's `codeHash` for the same code.
- **Tests** (`describe "StoreService names: codes"` in `BadgeTests.hs`): the branch's owner-code vectors; prepaid vectors copied from `simplex.domains/webapp/test/crypto.test.ts` (parse, canonical, display, hash); the taken-order code derivation vector (`takenOrderCode`) as a parse vector; random codes round-trip; a letter in the length refused.
- **Depends on:** stage 1.

#### T2. The label rule

- **Goal:** one label check, the relays' own.
- **Files:** new `apps/simplex-store-service/src/StoreService/Names/Label.hs`; new `tests/Bots/StoreService/NamesLabelTests.hs`.
- **Do:** `validLabel :: SimplexTLD -> Text -> Either LabelError Text`:
  1. the length, 6–63, first (`LELength`; search answers `invalid_name` for it), since `nameLabelP` also fails above 63;
  2. then `strDecode` of `label <> "." <> tld` as `SimplexDomain` (which runs `nameLabelP` on each part), requiring `subDomain == []`, `domain == label`, and the text already lower case (`LEChars`).
- **Acceptance:** agrees with the webapp's `labelProblem` (`src/names.ts`) on its vectors.
- **Tests:** `a-b-c-d-e-f` accepted; `--` anywhere, a leading or trailing hyphen, upper case, a dot, 5 and 64 characters refused (64 as `LELength`); 63 accepted.
- **Depends on:** stage 1.

#### T3. Signatures and ABI helpers

- **Goal:** everything the service verifies from the page, with vectors from viem.
- **Files:** new `StoreService/Names/Abi.hs` (the ABI helpers now private in `Registration.hs`, plus decoders for `uint256`, `address`, `bool`, `bytes32`, `string`, `bytes`, and the oracle's `(uint256, (uint256,uint256)[])`); changed `StoreService/Names/Registration.hs` (imports `Abi`; adds `resolverDomain`, `setTextDigest`, `clearRecordsDigest`; at most one link per key); new `StoreService/Names/Signatures.hs`; `tests/Bots/StoreService/NameRegistrationTests.hs` (its vectors regenerated with one link per key); new `tests/Bots/StoreService/NamesSignatureTests.hs`.
- **Do (`Signatures`):**
  - `parseSignature :: Text -> Either SignatureError RecoverableSignature`: `0x` and 130 lowercase hex, `v` 27 or 28, `s` at most half the curve order. Recovery is simplexmq's `recoverAddress`.
  - `personalDigest :: ByteString -> ByteString` (EIP-191).
  - The messages of the contract, exactly: `orderMessage`, `findMessage`, `retargetMessage` (with `duration`), `takenCodeMessage` (with `codeLength`).
  - The address decoder goes through `strDecode` of the hex (there is no constructor). `parseChecksumAddress` (for order bodies) accepts only text equal to `strEncode` of the decoded address, since `strP` accepts one-case hex unchecked; `parseAnyCaseAddress` (for `find`) lower-cases first.
- **Acceptance:** the digests and recovered signers equal viem's for the webapp's vectors.
- **Tests:** `RegisterName`, `SetText`, `ClearRecords` digests and EIP-191 messages against `webapp/test/crypto.test.ts` and `flow.test.ts` vectors; a high-`s` signature refused though recovery accepts it; `v` 0, 1 and 29 refused; a wrong-case and an all-lowercase checksum refused by `parseChecksumAddress`; `NameRegistrationTests` green with its one-link vector.
- **Depends on:** stage 1.

### Stage 3: the service's names endpoints

#### T4. Wiring: configuration, HTTP helpers, phases, the names router

- **Goal:** what every later task builds on, without an import cycle. Nothing answers differently until later tasks add routes.
- **Files:** `StoreService/Config.hs`; `apps/simplex-store-service/store_service.ini.example`; new `StoreService/Web/Http.hs`; `StoreService/Web/Server.hs`; `StoreService/Service.hs`; new `StoreService/Names/Env.hs`; new `StoreService/Names/Phases.hs`; new `StoreService/Names/Turns.hs`; new `StoreService/Web/Names.hs`; `simplex-chat.cabal` (its own new modules, and the branch's `Names.Chain` and `Names.Registration` in the executable); `tests/Test.hs`; `tests/Bots/StoreService/ConfigTests.hs`.
- **Do:**
  - **The cycle:** move out of `Web/Server.hs` into `Web/Http.hs` everything a handler needs: `Respond`, `Limit`, `takeToken`, `limited`, `clientKey` and the bucket map, `readLimit`, `createLimit`, the new limits (`search` 30, `preview` 20, `code` 10, `find` 5, `records` 10 per minute), `maxBuckets` (now 32768), `jsonResponse`, `jsonError`, the error responses, `readBoundedBody`, `maxBodyBytes`, `nameBodyBytes` (16384), and `only` (today local to `webApp`'s `where`), so a known names path with another method answers `405 method_not_allowed` with `Allow`. The rate-limit state moves with them into a `HttpEnv` that `WebEnv` holds. `Web/Server.hs` and `Web/Names/*` import `Web/Http.hs`; `Web/Server.hs` imports `Web.Names.namesApp`, and no `Web/Names/*` module imports `Web/Server.hs`. Handlers that the invoice side shares (`invoiceView`, `createAtProvider`) move into `Web/Http.hs` or a module of their own when T11 needs them.
  - `[names]`: `tld`, `years` (2; startup refuses below 2), `term_start` (`2027-01-01T00:00:00Z`), `term_slack_seconds` (3600), `reveal_margin_seconds` (900), `max_order_moves` (3). `[chain]`: `rpc_url`, `chain_id`, `controller`, `sponsor`, `base_registrar`, `resolver`, `service_key`, `confirmations` (3), `gas_limit_cap` (8000000). `[listener]`: `names_static_dir`, `names_export_dir`, `names_hosts`. All in `knownSettings`. The names side is off when `[names]` is absent.
  - The RPC URL and the key never appear in a log line or an error.
  - `Names/Phases.hs`: a map keyed by registration id, outside `waitingCount`, holding each registration's `version`; `publish` after a commit, and `awaitChange id seenVersion micros`. Later tasks publish from the start.
  - `Names/Env.hs`: `NamesEnv` and `newNamesEnv cc userId config store`, called once from `Service.hs` with the `ChatController` and the service user's id, so no later task needs `Service.hs` to reach the chat core. Later tasks add their parts (chain client T5, profiles T7b, sender T16).
  - `Web/Names.hs`: `namesApp`, a route list that later tasks extend.
  - `Names/Turns.hs`: the per-name and per-code serialization of the contract's section 1, as its own module so no later task edits another's file to use it: `withLabelTurn :: NamesEnv -> LabelHash -> IO a -> IO a`, `withLabelTurns` (two label hashes, taken in hash order, for `/name`), `withCodeTurn` (a code hash). In-process locks keyed by hash, removed when unused. T9, T11, T12, T14, T15 and T18 import it.
- **Acceptance:** badge endpoints and tests unchanged; the service starts with and without `[names]`; an unknown key in the new sections is warned about and ignored.
- **Tests:** `ConfigTests`: the new sections parsed, defaults, a bad `term_start`, `years` 1 or a bad address refused at startup, the RPC URL absent from the rendered config error; `Phases`: publish wakes a waiter on the published version, not on an older one; a names route with the wrong method answers `405` with `Allow`; `Turns`: two holders of one label hash run one after the other, two of different hashes at once, and `withLabelTurns` A→B with B→A never deadlocks.
- **Depends on:** stage 1.

#### T5. The chain client

- **Goal:** a JSON-RPC client the worker and every read can rely on.
- **Files:** `StoreService/Names/Chain.hs`; `tests/Bots/StoreService/FakeEthNode.hs`; `tests/Bots/StoreService/NameRegistrationTests.hs` (its `ChainError` and `ChainClient` uses); new `tests/Bots/StoreService/NamesChainTests.hs`.
- **Do:**
  - A 10-second timeout per call (`responseTimeout`), and a response size bound.
  - Errors keep `error.code` and `error.data`: a revert (code 3, or "execution reverted") is `ChainReverted (Maybe RevertName)`, decoded against the contracts' error selectors; anything else is `ChainUnavailable` with no URL in its text.
  - `ethCall`, `latestBlock` (number, timestamp, base fee), `transactionReceipt`, `transactionByHash`, `transactionCount`.
  - `sendTransaction` is split: `signTransaction` returns the raw bytes and the hash (keccak256 of the raw bytes); `sendRawTransaction` sends them. `estimateGas` is capped by `gas_limit_cap`.
  - `FakeEthNode` answers `eth_call` from a table keyed by contract and calldata, records raw transactions, mines them on demand with chosen statuses, and can fail, time out or revert with data per method. Later owners (wave table) extend it for dropped transactions, pruned receipts, a nonce used with no receipt and fee-bump replacements.
- **Acceptance:** no call can hang past its timeout; a revert is never read as a node failure; `NameRegistrationTests` green.
- **Tests:** a revert with data decoded by name; `ownerOf`'s revert read as a revert; a timeout and an RPC error read as unavailable without the URL; the signed hash equals the fake node's `keccak256(raw)`.
- **Depends on:** T4.

#### T6. Contract reads

- **Goal:** every chain read the contract needs, typed.
- **Files:** new `StoreService/Names/Contracts.hs`; new `tests/Bots/StoreService/NamesContractsTests.hs`.
- **Do:** on T5 and T3's `Abi`:
  - `nameStatus :: LabelHash -> IO (Either ChainError NameStatus)`: registered while `nameExpires + GRACE_PERIOD ≥ latest block time`, with the registry's `owner(node)` and `resolver(node)` (registry from `baseRegistrar.ens()`, read once) and `GRACE_PERIOD`; else reserved when `reservedNames ≠ 0`; else available.
  - `nameRecords`: on the default resolver only, `text(node, "simplex.contact")`, `text(node, "simplex.channel")`, `nonces(owner)`.
  - `chainPriceCents :: Int -> Int -> IO …`: `rentPriceUSD(replicate length 'a', years × 31536000).base `div` 10^16`.
  - `codeLengthFor :: Int -> Int -> IO (Either ChainError (Maybe Int))`: reads the oracle (`controller.prices()`, then its `prices()`) once, and returns the shortest `L` from 6 such that every length from `L` to 63 costs at most the given cents for the years; `Nothing` when none does.
  - `sponsorNonce`, `resolverNonce`, `ownerOfToken` (a revert is `Nothing`), `nameExpires`, `commitmentTime`, `controllerAvailable label`, `commitmentAges` (read once at startup).
- **Acceptance:** each read's calldata equals viem's `encodeFunctionData` for the same call (vectors computed once and pasted).
- **Tests:** against `FakeEthNode`: the three statuses, grace period, another resolver, prices for 6, 7 and 12 letters at the launch list (40000, 20000, 10000 cents for 2 years), `codeLengthFor` 6, 7 and 8 for those prices, a non-monotone list (6 letters cheaper than 7), no qualifying length, a failed read.
- **Depends on:** T3, T5.

#### T7a. Link kinds

- **Goal:** one link check for the reveal, the records relay and previews, landing early and on its own.
- **Files:** new `StoreService/Names/Links.hs`; new `tests/Bots/StoreService/NamesLinksTests.hs`.
- **Do:**
  - `linkKindOf :: Text -> Maybe LinkKind`: a short contact address (`/a#`) or channel (`/c#`) link, `https://<host>/…` or `simplex:/…`, within 4096 bytes. Its cases are the webapp's `src/names.ts` vectors.
  - `linkHosts :: Text -> [Host]`: the link's server hosts, the authority of an `https://` link or the server hosts carried inside a `simplex:/` link (parsed with the chat core's link parser, so the hosts are those the core will connect to).
- **Tests:** the webapp's link vectors; full links, wrong kinds and a link one byte over the bound refused; hosts from an `https://` link and from a `simplex:/` link with one and with several servers.
- **Depends on:** T4.

#### T7b. Link profiles

- **Goal:** the chats a name opens, as the app's connection-plan alert shows them, with outbound fetches bounded and private addresses refused.
- **Files:** new `StoreService/Names/Profiles.hs`; new `tests/Bots/StoreService/NamesProfilesTests.hs`.
- **Do:**
  - **The host rule** (contract section 3), best effort, before the chat core is asked to fetch: every host of `linkHosts` is checked; an IP literal as it is, a name by resolving it. A private, loopback, link-local, unique-local (`fc00::/7`), CGNAT (`100.64.0.0/10`), unspecified or multicast address, among a literal or any resolved address, or a `.onion` or unresolvable host, means no fetch and `profile: unavailable`; the link stays valid. A public IP literal passes. There is no address pinning: the pinned simplexmq resolves inside `connectTCPClient` with no hook, so DNS rebinding between the two lookups remains possible. The deployment's egress rule (T20) is the real guard.
  - `chatProfile`: runs `APIConnectPlan` (through the `ChatController` that `NamesEnv` holds) with resolve mode off and maps the plan: `CAPOk` with `ContactShortLinkData` → `business` when `business` or `peerType` business, `bot` for `peerType` bot, else `contact`; `GLPOk` (and `GLPNoRelays`, `GLPUpdateRequired`) with `GroupShortLinkData` → `channel`, `subscribers` from `publicGroupData.publicMemberCount`. `displayName`, `fullName`, `image` from the profile. A known chat (`CAPKnown`, `GLPKnown`, own link) uses the stored profile.
  - Bounds: `searchFetchSlots` (4) for search answers and `previewFetchSlots` (2) for previews, each service-wide; `fetchTimeout` (3 seconds) per fetch, which also bounds the wait for a slot; a 10-minute cache per link (bounded in size). No slot in time, a timeout or a failure gives the empty profile with `profile: unread`; the host rule gives `profile: unavailable`.
  - `entity`: the profile with `link` and `deepLink`.
- **Acceptance:** the JSON equals the contract's entity and preview shapes.
- **Tests:** the plan-to-profile mapping as a pure function over constructed plans (contact, business, bot, channel with a count, known chat, error); the cache, both concurrency bounds (previews filling theirs while search still fetches) and the timeout with a fake fetcher, including a fetch giving up when no slot frees in time (`unread`); a public IP literal passing; literals and names resolving to `127.0.0.1`, `10.0.0.1`, `100.64.0.1`, `fe80::1` and `fd00::1` refused (`unavailable`), with a stub resolver; a `.onion` and an unresolvable host `unavailable`; a link with one public and one private host `unavailable`; one integration test in the chat test harness: a test user's address short link and a channel link fetched through the service user's controller.
- **Depends on:** T4, T7a.

#### T8. Storage

- **Goal:** the names tables and every query the endpoints, the sender and the worker use.
- **Files:** `StoreService/Store/SQLite/Migrations.hs`, `StoreService/Store/Postgres/Migrations.hs` (migration `20261010_names`); new `StoreService/Names/Store.hs`; new `tests/Bots/StoreService/NamesStoreTests.hs`.
- **Do:**
  - Tables `sx_badge_service_name_prepaid_codes`, `…_name_registrations` (with `duration`, `price`, `version`, `live`, `order_hash`, the order's `method`, `moves` and `links_refused_at`), `…_name_record_edits`, `…_name_invoices`, and `…_name_transactions` for every signed transaction (its registration or edit, kind, raw bytes, hash, registrar nonce, sent and mined blocks, status), as the plan's "Storage" lists them, without a price table.
  - Constraints: unique `commitment` and unique `(owner, label_hash)` among rows with `live` set; unique `reserved_by` on codes; edits unique on `(owner, nonce)` among pending and mined rows.
  - `order_hash`: SHA-256 of exactly the order's fields in a fixed encoding, tagged by kind: `name-invoice` with `labelHash, length, duration, owner, commitment, sealed, method, price`, or `name-code` with `labelHash, length, duration, owner, commitment, sealed, codeHash`. Never the timestamp or the signature.
  - Queries: create a paid or code registration (the code reserved by a conditional update in the same transaction); the existing-order lookups (exact match by `order_hash` among all stored orders; live by commitment, or by owner and label hash, telling an overdue one apart: its open invoice past its `expires_at`, whatever the poller has marked); `closeOverdue`, which in the caller's transaction marks that invoice `expired` and clears its registration's `live`; registering hashes; every registration write, phase changes and the reveal's fields included, as a compare-and-set on the `version` the caller read, bumping and returning it, and reporting a lost race; `live` kept in step with the phase and the invoice's status; reveal fields; clear-reveal; retarget; issue, reserve, release, spend and revoke codes; edits by body hash and by owner and nonce; relayed edits per name in a window; registrations by owner (find, the 50 newest); unpaid cleanup candidates; failed code orders past 30 days; transactions: insert a signed one before sending, record a receipt or block, list those without a receipt (resume), list by registration or edit.
- **Acceptance:** migration up and down on SQLite and Postgres; badge tables untouched.
- **Tests:** migration up/down; each query's effect; the unique constraints (a second live registration for one owner and hash, or one commitment, refused; a code reserved by one registration refused to another); `version` bumped on every change; a write with a stale `version` changing nothing; an overdue live order reported as overdue, and `closeOverdue` freeing the unique constraint for a new insert in the same transaction; `order_hash` equal for two requests differing only in timestamp and signature, and different for any other field.
- **Depends on:** T4.

#### T9. Prepaid codes: check, issue, revoke

- **Goal:** the code check and the operator's codes.
- **Files:** new `StoreService/Names/Codes.hs`; new `StoreService/Web/Names/Codes.hs`; `StoreService/Service.hs` (`//issue name`, `//revoke` of a prepaid code); `StoreService/Group/Command.hs` (parsing `/issue name`, `/bulk name`); `StoreService/Group.hs` (running them, beside `GCIssue`/`GCBulk`); new `tests/Bots/StoreService/NamesCodesTests.hs`.
- **Do:**
  - `POST /api/code/check` as the contract's section 4, under the `code` limit; `revoked` is a state, an unknown code `400 code_invalid`; no registration named.
  - `//issue name <length> [years <Y>] [paid|unpaid|free]`, `/issue name <length> [years <Y>]`, `/bulk name <length> [years <Y>] count <N>`: length 6–63, years 2–10 (default 2; below 2 refused). `//revoke` takes a badge or a prepaid code.
  - `reserveCode`, `releaseCode`, `spendCode`, each under `withCodeTurn` (T4's `Turns`), for T12, T15, T17 and T18.
- **Acceptance:** the check never changes a code.
- **Tests:** each state; unknown and malformed codes; an owner code sent to the check refused as `code_invalid`; the operator commands in the existing bot and group test harnesses; years 1 refused; two concurrent reservations of one code, one winning.
- **Depends on:** T1, T4, T8.

#### T10. Search and link preview

- **Goal:** `GET /api/name/:hash` and `POST /api/link/preview`.
- **Files:** new `StoreService/Web/Names/Search.hs`; new `tests/Bots/StoreService/NamesSearchTests.hs`.
- **Do:** the contract's sections 2 and 3: available with the chain price, `years` and `termStart`; registered with entities (each with `profile`), raw records, owner, resolver, nonce, expiry and `gracePeriod`; another resolver; being registered here; reserved; `provider_unavailable` for any failed read, records included. Previews through T7b's preview bound and host rule.
- **Acceptance:** the JSON of every answer equals the contract's examples field for field.
- **Tests:** each answer against `FakeEthNode` and a fake profile fetcher; length 5 and 64 `invalid_name`; a bad hash `bad_request`; the search limit; a record with two links joined by `;` giving two entities and a full link giving none; preview of each kind, an invalid and an over-long link (`invalid_link`), and a private-host link accepted with `profile: unavailable`.
- **Depends on:** T6, T7b, T8.

#### T11. Paid orders, the registration view and the invoice's name side

- **Goal:** `POST /api/invoice` for names, the views it answers with, settlement, cancel and cleanup.
- **Files:** new `StoreService/Names/Orders.hs` (the order checks shared with T12 and T15: formats, the order signature, existing orders, the timestamp, the duration, the chain, the price; one at a time per label hash); new `StoreService/Names/View.hs` (the registration view of the contract's section 6, with `version`, `moves`, `linksRefused` from `links_refused_at`, `codeLength` from T6 and `unread`); new `StoreService/Web/Names/Orders.hs`; `StoreService/Web/Server.hs` (`createInvoiceHandler` dispatches on `kind`; the name invoice view); `StoreService/Web/Http.hs` (the shared invoice view helpers); `StoreService/Store/Invoices.hs` (every invoice query gains the name side; provider-ref lookups across both tables, refusing a ref found in both); `StoreService/Orders.hs` (settling a name invoice queues its registration in the same transaction, only while open, then publishes; a late payment of a closed one alerts and leaves it `unpaid`); `StoreService/Poller.hs` (the 30-day cleanup with a provider read first; the overdue sweep for names); `StoreService/Providers/BTCPay.hs` (`InvoiceReceivedPayment`, `InvoicePaymentSettled` as read hints); `StoreService/Names/Store.hs` if a query is missing; new `tests/Bots/StoreService/NamesOrdersTests.hs`.
- **Do:** the contract's section 5 for `/api/invoice` and cancel, in its order of checks; publish after every commit.
  - Orders run under `withLabelTurn` (T4's `Turns`).
  - **`live`:** every transition that changes it is in this task, in the same transaction as the change: settlement (past `unpaid`, live), cancel (`cancelOpenInvoice`), the poller's overdue sweep (`expireOverdue`) and its expiry of an invoice it reads, a late payment of a closed invoice (stays not live), the 30-day cleanup (deleted), and phase changes to `taken` or `code_issued` (not live).
  - **An overdue order is closed by the next one:** a new order from the same wallet and name that meets a live order whose invoice is past its expiry time with no money received calls `closeOverdue` in its own transaction, then inserts; after the commit it asks the provider to cancel the old invoice, best effort.
  - **Settlement queues only a live registration.** A settled signal for a closed one (cancelled, expired, or closed by a newer order) is a late payment: the invoice is marked paid, the registration stays `unpaid`, the operator is alerted, support refunds it.
- **Acceptance:** badge invoices unchanged (their tests green); a name invoice's view, and the registration view inside it, as the contract's.
- **Tests** (the plan's Verification, orders): a paid order; an exact resend answered with the same invoice, even after the timestamp window, with a new timestamp and signature, and after the registration went past `unpaid` (answered before the chain read); an exact resend of an expired order answered with its expired invoice; a new order of the same wallet and name accepted after a cancel, and after the old invoice's expiry time before the poller marks it (closing the old one); a payment of the old invoice arriving after a newer order replaced it, recorded as a late payment with an alert and the new order unaffected; a stale timestamp on a new order refused; a resend differing in any one field (including `sealed`) refused `409 order_exists` with no other field; an order for the same owner and hash with a new commitment refused the same way; an order carrying someone else's public commitment refused without disclosing anything; a bad or other signer's signature refused; two concurrent orders for one owner and hash, one winning; the duration slack before and after `termStart`; `catalog_changed` with the chain price; `name_taken` for registered, reserved and being registered here; `name_check_unavailable`; `sealed` missing or over 11095 and a body over 16384 refused, a longest order accepted; two orders for one name from two wallets both accepted (the later one ends `taken` in T17); cancel answers (`funded`, `not_open`, `200 paid`, `200 expired` with marks); a late payment of a closed invoice leaving `unpaid` and alerting; the cleanup deleting an `unpaid` registration after 30 days, sparing one that received a payment, recording a payment found only at cleanup, keeping it when the provider does not answer; the view's `unread` on a failed `intentNonce` read, and on a failed `codeLength` read in `taken`.
- **Depends on:** T3, T6, T8.

#### T12. Orders paid with a prepaid code

- **Goal:** `POST /api/name/register`.
- **Files:** `StoreService/Names/Orders.hs`, `StoreService/Web/Names/Orders.hs` (owned in wave 5); `tests/Bots/StoreService/NamesOrdersTests.hs`.
- **Do:** the contract's section 5, in its order of checks, under `withLabelTurn` and `withCodeTurn`; an overdue order of the same wallet and name closed first, as in T11; the code reserved by the conditional update in the transaction that creates the `queued` registration, then a publish.
- **Tests:** a code order; an exact resend in any phase, the code spent or not, answered with its id, also with a new timestamp and signature; a resend differing in one field refused `order_exists`; a bad or another signer's signature refused; a stale timestamp on a new order refused; an open invoice for the name refused `order_exists`; each code error, `code_not_covering`; the duration for the code's years; `sealed` over 11095 refused; two concurrent orders spending one code for two names, one winning; the shared `code` limit with the check.
- **Depends on:** T9, T11.

#### T13. The long polls

- **Goal:** pages follow a registration by its `version`.
- **Files:** new `StoreService/Web/Names/Registration.hs` (`GET /api/registration/:id`); `StoreService/Web/Server.hs` (`readInvoiceHandler` takes `seenVersion`; a paid name invoice holds on the version alone; `paid` with `unpaid` answers at once); new `tests/Bots/StoreService/NamesPhasesTests.hs`.
- **Do:** the holds of the contract's section 6 on T4's `Phases` and T11's view; a lapsed `committed` read as `failed`.
- **Tests:** each change (phase, label set and cleared, reveal times after a re-commit, error) waking a held invoice poll and a held registration poll; an unpaid invoice held on its status waking on expiry; one paid after it closed answered at once; `waitingCount` unchanged by name polls.
- **Depends on:** T11.

#### T14. The reveal

- **Goal:** `POST /api/registration/:id/reveal`.
- **Files:** new `StoreService/Names/Reveal.hs`; `StoreService/Web/Names/Registration.hs` (owned in wave 6); new `tests/Bots/StoreService/NamesRevealTests.hs`.
- **Do:** the contract's section 7, in its order of checks, under `withLabelTurn`: the commitment rebuilt locally with `makeCommitment` from the sent links and the stored duration (check 5); `verifyRegisterIntent` plus T3's low-`s` parse and the sponsor nonce from the chain (check 6); only then the links through T7a's `linkKindOf` (check 7), recording `links_refused_at` on `invalid_link` as a compare-and-set write that bumps `version` and publishes; the accepted reveal's write as a compare-and-set on the `version` read, a lost race answering `409 registration_changed` with the current view; bump `version` and publish after the commit. Every request takes one read token, whatever its outcome.
- **Tests:** with and without links; an identical repeat in a later phase; a different repeat refused; a high-`s` intent refused; a wrong signer, a stale nonce, a past deadline, a label not matching the hash, a second link for one key (`invalid_link`), a link one byte over the bound (`invalid_link`), a wrong duration (commitment mismatch); a reveal with bad links that do not rebuild the commitment, or with a wrong signer, refused `bad_request` without recording a refusal; a refusal recorded only for the committed links with the owner's signature, waking a held poll, and the view read by another browser showing `linksRefused: true`; a reveal with two records at the bound passing the body cap; `provider_unavailable` on the nonce read; `invalid_link` recording the refusal; a reveal racing a `/name` (with a stubbed check pause), exactly one taking effect and the other answered `registration_changed` or `order_exists`/`bad_request`; a held poll woken by the label being set.
- **Depends on:** T2, T3, T6, T7a, T13.

#### T15. Owner-signed requests: find, another name, a code instead

- **Goal:** `find`, `/name` and `/code`.
- **Files:** new `StoreService/Web/Names/Owner.hs`; `StoreService/Names/Orders.hs` (the retarget and the taken code; owned in wave 6); new `tests/Bots/StoreService/NamesOwnerTests.hs`.
- **Do:** the contract's section 8, each write a compare-and-set on `version`, then publish:
  - Every lost compare-and-set answers `409 registration_changed` with the current view.
  - `find` with `labelHash` and `sealed`, the 50 newest.
  - `/name`: under `withLabelTurns` (the old and the new label hash, in hash order); allowed from `taken` (paid), `failed`, and `committed` only with `links_refused_at` set; a body equal to the current fields answers the view unchanged; at most `max_order_moves` successful moves, counted on success, then `409 move_limit` with the current view (the page shows a support line and the reference); the duration in its message; the order checks for the new fields except this registration (`order_exists`); `price_too_high`; for a code order the code checked and reserved again through T9 under its per-code turn (`code_used`, `code_reserved`, `code_invalid`, `code_expired`, `code_not_covering`); the reveal, transactions and `links_refused_at` cleared (`linksRefused` false); a code still reserved by this order kept even if revoked or expired since, as `/retry` does.
  - `/code`: an identical repeat first (the issued code's hash) answers `200 {}` whatever `codeLength` the request carries; otherwise `codeLength` in the body and the signed message, `minLength` from `codeLengthFor`, `409 code_length_changed` with the current value when they differ; `code_unavailable` when none; the order's years; `code_conflict`; `503` on a failed read.
- **Tests:** each with a wrong signer and a stale timestamp refused; `find` newest first with retargeted and `unpaid` entries, capped at 50, `not_found`; `/name` from `registered` and from a `committed` order with no refused reveal refused, from `failed` and from `committed` after a refused reveal accepted (the same name with other links too), a repeat of the current fields answered unchanged with no move counted, the fourth move `move_limit` with the current view, meeting another live order `order_exists`, a dearer name `price_too_high` with its price, a cheaper one accepted with the new `price`, a code order to a shorter name `code_not_covering`, a code order after the 30-day sweep re-reserving an unused code and answering `code_used` for one spent elsewhere, a code order whose held code was revoked or expired since accepted, `linksRefused` false after a move, `sealed` over 11095 refused; `/name` A→B running concurrently with another order's B→A without deadlock; `/code` repeat answered 200 even after a price change that would move the length, its `minLength` for 6, 7 and 12 letters at the launch prices (6, 7 and 8), `code_length_changed` after a price change, `code_unavailable`, `503` on a failed read, the view's `codeLength` kept in `code_issued`; a held poll woken by `/name` and by `/code`.
- **Depends on:** T3, T6, T9, T12, T13.

### Stage 4: registration on chain

#### T16. The registrar key's one sender

- **Goal:** every transaction of the registrar key goes out once, in nonce order, and survives a restart.
- **Files:** new `StoreService/Names/Sender.hs`; new `tests/Bots/StoreService/NamesSenderTests.hs`.
- **Do:**
  - One queue for commits, registers and record edits. Each transaction: dry run (`eth_estimateGas`, capped), signed, saved through T8's transaction queries with its raw bytes, hash and registrar nonce, then sent.
  - Receipts followed; a transaction the node does not know is rebroadcast while its registrar nonce is unused; after a timeout a fee-bumped replacement (same nonce, every hash kept).
  - Once the nonce is used with no receipt for any hash, the owner of the transaction (worker or relay) is asked to judge it from chain state; nothing is signed again before that.
  - On startup, every saved transaction without a receipt resumes this way.
- **Tests:** two senders' work never sharing a nonce; a fee-bumped replacement mined under its new hash; a restart rebroadcasting a saved raw transaction; a dropped one whose nonce was used, judged from chain state before any new signature.
- **Depends on:** T5, T8.

#### T17. The worker

- **Goal:** each registration from `queued` to `registered`, `taken` or `failed`, resumable.
- **Files:** new `StoreService/Names/Worker.hs`; `StoreService/Service.hs` (start and resume the workers and the daily sweeps); new `tests/Bots/StoreService/NamesWorkerTests.hs`.
- **Do:** the plan's stage 4 worker and phases: commit (reading `commitments` first; `UnexpiredCommitmentExists` → `committed`; a lapsed commitment cleared and committed again a second after `revealBefore`); `committed` until `revealAfter` and the reveal; register via `registerWithCredit`; `InvalidNonce` → `committed` with the reveal cleared; `registered` at a status-1 receipt with `ownerOf` the owner, `confirmations` blocks later, with `expires_at`, spending the code (T9); `taken` on `NameNotAvailable`/`NameReserved` or any revert while `available(label)` is false, unless `ownerOf` is the owner, releasing a code; `failed` otherwise, alerting on `InsufficientAllowance`, keeping a code reserved; a commitment not revealed by `revealBefore` → `failed`. Every chain read retried with the phase kept. Every change bumps `version` and publishes. The daily sweep releases codes of registrations `failed` for 30 days.
- **Tests:** commit, wait, register; two wallets' paid orders for one name, the later ending `taken` with its value kept; a revert to taken (code released); a restart mid-phase; a commit reverting `UnexpiredCommitmentExists`; a commit out of gas sent again; a commit reverting at the lapse second, sent a second later; a pending commit waited for; `InvalidNonce` clearing the reveal; a lapse to `failed` keeping the code; the code sweep.
- **Depends on:** T9, T13, T14, T16.

#### T18. Try again

- **Goal:** `POST /api/registration/:id/retry`.
- **Files:** `StoreService/Web/Names/Registration.hs`, `StoreService/Names/Worker.hs` (owned in wave 8); `tests/Bots/StoreService/NamesWorkerTests.hs`.
- **Do:** the contract's section 9 and the plan's retry steps under `withLabelTurn`, a lost compare-and-set answering `409 registration_changed`, `revealMargin` from `reveal_margin_seconds`, re-reserving a released code through T9; each write a compare-and-set on `version`, then publish.
- **Tests** (the plan's `/retry` list): outside `failed` refused; a consumed commitment after a redeem → `registered`; a released code now used elsewhere (the error, still `failed`); a moved sponsor nonce clearing the reveal; a live commitment straight to `committed`; a third party's live re-commit clearing the reveal and `commitTx`; our own live commit after a moved nonce keeping its hashes; less than `revealMargin` left, committed again after `revealBefore`; a requeue clearing `failed_at`, `error` and the register's hashes; a held poll woken by the requeue.
- **Depends on:** T9, T17.

#### T19. The records relay

- **Goal:** `POST /api/name/records`.
- **Files:** new `StoreService/Names/Records.hs`; new `StoreService/Web/Names/Records.hs`; new `tests/Bots/StoreService/NamesRecordsTests.hs`.
- **Do:** the contract's section 10 and the plan's relay, in the contract's order of checks: links through T7a's `linkKindOf`; repeats only of sent edits; the deadline, owner and nonce before the per-name cap; the cap counting relayed edits only; the pending row inserted before the edit joins the sender; `{tx}` once mined, `record_reverted`, `504 edit_pending` after 90 seconds or a chain that stops answering after the send, `503 provider_unavailable` before it with nothing stored; "applied" from the edit's own sent hashes first (a status-1 receipt answers that hash), and only without a known receipt from the nonce passed and the record holding the value, else `record_reverted` with `InvalidNonce`; expiry once a block `confirmations` deep is past the deadline.
- **Tests** (the plan's `name_record_edits` list): a wrong signer, a high `s` or a passed deadline refused before any row or gas; ten edits signed by a non-owner leaving the owner's cap untouched; a refused edit's repeat checked anew; a pending edit resumed after a restart; one past its deadline freed while its transaction is still rebroadcast; `InvalidNonce` and `SignatureExpired` reverts of an edit another relayer applied answered `{tx: null}`; a nonce consumed by another body with a different value answered `record_reverted` `InvalidNonce`; a restart-expired edit found applied becoming `mined`; an identical repeat after a restart; a concurrent twin losing the unique insert before signing; a reverted edit's repeat answered `record_reverted` while a new edit with its nonce is pending; a revert freeing the nonce; a failed re-read retried with the edit pending; the cap; `edit_pending` and its repeat answered `{tx}` once mined; the link bound.
- **Depends on:** T3, T6, T7a, T8, T16.

### Stages 6 and 7: the service side

#### T20. Both sites from one service

- **Goal:** the export and serving of stage 6.
- **Files:** `StoreService/Web/Server.hs` (`exportWebapp` writes the names site with the Stripe key; all-in-one mode picks the site by `Host` against `names_hosts`; `contentTypeFor` gains `font/ttf` and `font/woff2` if missing); `scripts/store-service/compose.yml`; new `scripts/store-service/egress.sh`; `apps/simplex-store-service/README.md` (Caddy host blocks; the names CSP with `img-src 'self' data: https://*.stripe.com`; Stripe.js on the page's origin, accepted 2026-10-20); `tests/Bots/StoreService/WebTests.hs`.
- **The egress rule** (the real guard behind T7b's best-effort host check): the service's outbound traffic to private, loopback, link-local, unique-local, CGNAT, unspecified and multicast ranges is denied, except the addresses it is configured to use (the RPC node, the database, BTCPay), which the rule lists from the ini. With the service on host networking, `egress.sh` installs it as `iptables`/`ip6tables` rules matching the service's user (`-m owner --uid-owner`); on a Docker bridge network, as rules on that network. The README says how to install and check it.
- **Tests:** both sites exported with the key; `Host` picking the site; a names asset served with its type. The egress rule is checked by hand on the deployment: a connection to a private address refused, to the RPC node and the database allowed (README steps).
- **Depends on:** T4.

#### T21. The service against the contract

- **Goal:** the Haskell service answers the webapp as the mock does.
- **Files:** new `scripts/store-service/names-contract-run.sh`; `apps/simplex-store-service/README.md` (how to run it).
- **Do:** start anvil and deploy with the webapp's `dev/deploy.ts`; write an ini for that deployment with no payment provider configured (the cases below need no paid order, and the fake providers are in-process test fixtures); seed prepaid codes with `//issue name`; serve the names build all-in-one; run the webapp's e2e cases that need no `/control` endpoint and no payment (search answers, Check your code, a code order to `registered` with links, the reveal from another browser with the name code, Edit links) with `BASE_URL` at the service.
- **Acceptance:** those cases pass in Chromium against the service, unchanged from their run against the mock.
- **Depends on:** T10–T20 merged (it runs in wave 9, after T18's merge), and the mock's Appendix A changes in the webapp.

## Waves, owners and merges

Each wave starts from `sh/store-service` as merged at the end of the last one. "Owns" lists the shared files of the section "Shared files" that the task alone edits in that wave.

| Wave | Tasks | Owns |
|---|---|---|
| 1 | T1, T2, T3, T4 | T3: `NameRegistrationTests.hs`. T4 (the wave's integration task): `Web/Server.hs`, `Web/Http.hs`, `Service.hs`, `Names/Turns.hs` |
| 2 | T5, T7a, T7b, T8 | T5: `NameRegistrationTests.hs`, `FakeEthNode.hs`. T8: `Names/Store.hs`, the migrations |
| 3 | T6, T9, T16 | T9: `Service.hs`. T16: `FakeEthNode.hs` (T6 uses T5's call table as it is) |
| 4 | T10, T11, T19 | T11: `Web/Server.hs`, `Web/Http.hs`, `Names/Orders.hs`, `Names/Store.hs`, `Store/Invoices.hs`, `Orders.hs`, `Poller.hs`. T19: `FakeEthNode.hs` |
| 5 | T12, T13 | T12: `Names/Orders.hs`, `Names/Store.hs`. T13: `Web/Server.hs`, `Web/Names/Registration.hs` |
| 6 | T14, T15, T20 | T14: `Web/Names/Registration.hs`. T15: `Names/Orders.hs`. T20: `Web/Server.hs`, `README.md` |
| 7 | T17 | `Service.hs`, `Names/Worker.hs`, `FakeEthNode.hs` |
| 8 | T18 | `Web/Names/Registration.hs`, `Names/Worker.hs` |
| 9 | T21 | `README.md` |

Each task is one or more atomic commits (`<scope>: <subject>`, no body) on its own branch. It ends with the build and its `-m "StoreService names: <part>"` tests green in its worktree, and the badge specs green. The manager merges the wave's branches one at a time and runs the build and all `StoreService` specs after each merge.

## Owner answers (2026-10-21, binding)

1. **Orders opened from the app** are dropped: the app will buy names itself (#7530 §13.6). Every web order has an owner code and `sealed`; there are no `?name=&owner=` links and no app-signed intents.
2. **No resolver service** for the new deployment: the SMP relays and the app read the chain from their own node (reth with nimbus), as the service does. Sequence gap 31 is closed.
3. **Launch prices:** $100, $200 and $400 for 2 years, set on chain with `setPrices`: base 5000 cents a year, 7 letters 10000, 6 letters 20000. No per-name prices.
4. **365-day years**, showing the real end date: for an order before 1 January 2027, 2 years end on 31 December 2028, 5 years on 31 December 2031.

Manager rulings on the first review round: orders disclosed only to who made them (contract section 1); per-name and per-code serialization; the dApp line on Check your order, the page before payment (ep, 2026-10-21 08:43, recorded in the owner decisions); prepaid codes and the paid years at least 2; one worktree per task with one global build lock.

Manager rulings on the third round: an overdue order closed by the next order's transaction, settlement only of a live order, a payment for a closed order a late payment; no address pinning (the pinned simplexmq cannot), a best-effort host check before the core fetches and an egress rule in the deployment, every host of a link checked; T7 split so link kinds land alone in wave 2; a links refusal recorded only after the commitment and intent checks; a `/code` repeat answering the issued code; `move_limit` answering the order's state with a support line; `registration_changed` on every lost compare-and-set; the turns in their own module from the wave-1 integration task.

Manager rulings on the second round: `/name` only from `failed`, a reveal refused for its links, or `taken`, at most 3 moves per order; `/name` re-checks and re-reserves a code order's code; the reveal joins the per-name serialization with a version compare-and-set; the link host rule (public IP literals allowed, `.onion` and unresolvable hosts `unavailable`; its pinning replaced in the third round); separate search and preview fetch bounds that never wait past their timeout; T21 in its own wave; `live` transitions in one task (the expiry-time rule replaced in the third round by closing the overdue order); worktrees start from a copy of the wave's base build.

Still open and not backend: the dApp host, and the app's menu path in "Use in the app".

## Decided here

- **Label rule:** simplexmq's `SimplexDomain` parser on `label.tld`, not #7390's `validLabel`, which is unmerged and refuses `--` only at positions 3–4. One rule with the relays.
- **Prices without the label:** `rentPriceUSD` with a placeholder label of the same length. The deployed oracle reads only the length, and the RPC node never sees the label before the reveal.
- **Paid years:** 2, as a setting (`[names] years`, at least 2). No years picker: the mockup shows a fixed term.
- **The term check:** `duration` in every order, register and retarget body, checked within 1 hour of `max(0, termStart − now)` and stored; returned in the view so the name code alone rebuilds the commitment; in the order and retarget signatures.
- **A new commitment per new order**, with one live order per wallet and name (by commitment, or owner and label hash), enforced by the order lock and unique constraints.
- **Signed orders:** the owner signs every order, which proves the owner (so nobody fills another wallet's `find` list) and lets the service refuse every non-identical order with a bare `order_exists`.
- **A taken order's code** pays for any name no dearer than the order, for its years: the shortest length from which every length is no dearer, given as `codeLength`, kept after `code_issued`.
- **A revoked code** answers `state: revoked`; a `reserved` code names no registration.
- **`find` returns `labelHash` and `sealed`**, the 50 newest, so a name code's verdict names its orders in one request.
- **The intent is signed at the reveal** (gap 34): the mock and the mockup's copy ("keep this page open, or reopen its link within 24 hours") assume it, and the service learns no name before the commit.
- **`/name` from `failed`, `taken`, or a `committed` order whose reveal was refused for its links** (manager's ruling), at most 3 moves, so refused links can be corrected without support and no order can drive endless commits.
- **Alain's prototype differences** (gaps 32, 33): the derived secret and `revealBefore` as the deadline stay; agreeing them with Alain is coordination, not an owner question.
- **One short link per key** (gap 35), as the mock, the redesign and the app's `firstNameLink` already do.
- **Limits** (gap 29): the values in the contract's section 1, every request counted, per-minute windows so `Limit` needs no change, `maxBuckets` 32768, an edit cap of 10 relayed edits an hour per name, `revealMargin` 900 seconds.
- **An edit judged applied by nonce and record** (gap 30): kept, with "applied" defined as the nonce passed and the record holding the value.
- **`edit_pending` after 90 seconds**, so a records request never outlives a proxy's idle timeout while the edit goes on.
- **Long polls on `version`**, so changes inside a phase reach the page at once.
- **A name paid for here but not yet `registering`** still searches as available: blocking it earlier would tell searchers it is being bought before the reveal. The later of two buyers ends `taken` with its value kept.
