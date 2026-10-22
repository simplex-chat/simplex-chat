# StoreService names backend: tasks for stages 2–4

This is the implementation plan for the names backend in StoreService: stages 2, 3 and 4 of [`2026-10-09-names-web-store-plan.md`](2026-10-09-names-web-store-plan.md), and the service side of stages 6 and 7. Every endpoint must answer as [`2026-10-21-store-api.md`](2026-10-21-store-api.md) says; the plan says how. Tasks are numbered T1–T27 (T7 split into T7a and T7b), each with its goal, files, acceptance, tests and dependencies; T22–T26 follow the owner's decisions of 2026-10-22 (balances), T27 documents the gas key, and the webapp's tasks for them, W1f-1 to W1f-3, come after the waves. The owner's answers and the decisions taken here close the document.

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
  - *Owned files*: within a wave, each of these has one owner task, given in the wave table; no other task of the wave edits it: `StoreService/Web/Server.hs`, `StoreService/Web/Http.hs`, `StoreService/Service.hs`, `StoreService/Names/Store.hs`, `StoreService/Names/Turns.hs`, the two migration modules, `StoreService/Names/Orders.hs`, `StoreService/Store/Invoices.hs`, `StoreService/Orders.hs`, `StoreService/Poller.hs`, `StoreService/Web/Names/Registration.hs`, `StoreService/Names/Worker.hs`, `StoreService/Names/View.hs`, `StoreService/Names/Signatures.hs`, `StoreService/Names/Balances.hs`, `StoreService/Web/Names/Orders.hs`, `StoreService/Web/Names/Owner.hs`, `tests/Bots/StoreService/NameRegistrationTests.hs`, `tests/Bots/StoreService/FakeEthNode.hs`, `apps/simplex-store-service/README.md`.
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
- **Tests** (`describe "StoreService names: codes"` in `BadgeTests.hs`): the branch's owner-code vectors; prepaid vectors copied from `simplex.domains/webapp/test/crypto.test.ts` (parse, canonical, display, hash); the taken-order code derivation vector (`takenOrderCode`) as a parse vector (removed by T22); random codes round-trip; a letter in the length refused.
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
  - The messages of the contract, exactly: `orderMessage`, `findMessage`, `retargetMessage` (with `duration`), `takenCodeMessage` (with `codeLength`; removed by T22, which T25's `spendMessage` follows).
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
  - `codeLengthFor :: Int -> Int -> IO (Either ChainError (Maybe Int))`: reads the oracle (`controller.prices()`, then its `prices()`) once, and returns the shortest `L` from 6 such that every length from `L` to 63 costs at most the given cents for the years; `Nothing` when none does. Removed by T22 with `/code`.
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
  - **`live`:** every transition that changes it is in this task, in the same transaction as the change: settlement (past `unpaid`, live), cancel (`cancelOpenInvoice`), the poller's overdue sweep (`expireOverdue`) and its expiry of an invoice it reads, a late payment of a closed invoice (stays not live), the 30-day cleanup (deleted), and phase changes to `taken` or `code_issued` (not live; `code_issued` removed by T22).
  - **An overdue order is closed by the next one:** a new order from the same wallet and name that meets a live order whose invoice is past its expiry time with no money received calls `closeOverdue` in its own transaction, then inserts; after the commit it asks the provider to cancel the old invoice, best effort.
  - **Settlement queues only a live registration.** A settled signal for a closed one (cancelled, expired, or closed by a newer order) is a late payment: the invoice is marked paid, the registration stays `unpaid`, the operator is alerted, support refunds it. (From T24: what arrived is credited to that order's balance instead, owner 2026-10-22.)
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

- **Goal:** `find`, `/name` and `/code`. Its `/code`, and `/name` from `taken`, are removed by T22 and T24 (owner, 2026-10-22).
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

### Balances (owner, 2026-10-22)

The owner's decisions of 2026-10-22 remove the code for a taken order and keep a taken paid order's payment as a balance, which buys another name paying only the difference (contract section 8, Balances and `/spend`). T22 removes what T15 built for `/code`; T23 to T26 build the balance.

#### T22. Remove the taken-order code

- **Goal:** nothing of `/code` is left in the service (contract Appendix A, item 36).
- **Files:** `StoreService/Web/Names/Owner.hs`; `StoreService/Web/Names.hs` (its route line); `StoreService/Names/Orders.hs`; `StoreService/Web/Names/Orders.hs` (the refusals' responses); `StoreService/Names/Signatures.hs`; `StoreService/Names/Contracts.hs`; `StoreService/Names/Abi.hs`; `StoreService/Names/Store.hs`; `StoreService/Names/View.hs`; the two migration modules (migration `20261022_drop_taken_codes`); `tests/BadgeTests.hs`; `tests/Bots/StoreService/` `NamesOwnerTests.hs`, `NamesOrdersTests.hs`, `NamesStoreTests.hs`, `NamesContractsTests.hs`, `NamesSignatureTests.hs`, `NamesWiringTests.hs`.
- **Do:**
  - Delete `takenCodeHandler`, `TakenCodeRequest`, `parseTakenCode` and the route; `issuable`, `issuedCodeHash`, `checkTakenCodeLength`, `issueTakenCode`, `TakenCodeConflict`; the refusals `ORCodeLengthChanged`, `ORCodeUnavailable`, `ORCodeConflict`, and `ORProviderUnavailable` once nothing else uses it, with their lines in `orderRefusalResponse`; `takenCodeMessage`; `codeLengthFor`, and `decodePrices`, its only caller's decoder; `PCodeIssued`, `nrIssuedCodeId`, `setIssuedCode`; the view's `codeLength`, `vrCodeLength` and `readViewChain`'s `taken` and `code_issued` branches, so `unread` names only `intentNonce`.
  - Migration `20261022_drop_taken_codes`, SQLite and Postgres: first `UPDATE … SET phase = 'taken', price = 0, live = 0 WHERE phase = 'code_issued'` (its worth went into the issued code, which stays a valid prepaid code, so it gets no balance in T23 and every row still decodes); then `DROP INDEX` of `idx_name_registrations_issued_code`, then `ALTER TABLE … DROP COLUMN issued_code_id`. The down migration adds the column and index back; converted rows stay `taken`. No database holds names rows yet; the step keeps the migration correct anyway. SQLite has `DROP COLUMN` from 3.35: checked with the `sqlite3` CLI (3.45.1, `foreign_keys` on) for a column with a `REFERENCES` clause once its index is gone; the bundled SQLCipher is SQLite 3.39.2, and the migration test runs it there.
  - `codeConflict` in `Web/Http.hs` stays: badge invoices answer it. `/name` from `taken` stays until T24.
- **Acceptance:** `rg -n 'code_issued|PCodeIssued|takenCode|TakenCode|issued_code_id|IssuedCode|codeLengthFor|decodePrices|"codeLength"|code_length_changed|code_unavailable' apps/simplex-store-service/src src tests` finds nothing; `POST /api/registration/<id>/code` answers `404 not_found`; the badge specs green, their `code_conflict` included.
- **Tests:** deleted with the code: the "a code instead" block, the `/code` half of the polls test, the `codeLengthFor` and `decodePrices` vectors, `takenCodeMessage`'s vector, the taken-order code vectors in `BadgeTests`. Changed: the live table without `code_issued`; the view's field list and `readViewChain`'s cases without `codeLength`; the store's column list; migration up and down with `20261022_drop_taken_codes`, on SQLite and Postgres, with a `code_issued` row before it read back as `taken`, price 0, not live; the wiring test's `404` for `/code`.
- **Depends on:** T15 (merged). It runs in wave 8 beside T18, which owns none of its files; both add lines to `Web/Names.hs` and `NamesOwnerTests.hs`, which the integrator merges.

#### T23. Balances: storage, turns and the view

- **Goal:** the balance of contract section 8 stored, with every query the later tasks use, its turn, its fields in the view, every close of an order releasing its hold, and `placeOrder` able to close a balance's overdue holder. Nothing credits, holds or spends yet.
- **Files:** the two migration modules (migration `20261022_name_balances`, which sorts after `20261022_drop_taken_codes`); `StoreService/Names/Store.hs`; `StoreService/Orders.hs` (`closeOrder`'s release); `StoreService/Names/Turns.hs`; new `StoreService/Names/Balances.hs`; `StoreService/Names/View.hs`; `StoreService/Names/Orders.hs`; the modules matching on `RegistrationPayment` (`rg -n 'RPInvoice|RPCode' apps/simplex-store-service/src`), for the new constructor only; `StoreService/Web/Names/Orders.hs` also for `/api/invoice`'s call of `createPaidRegistration` (line 129 at e6fb909d6), which passes its `OTInvoice` terms; `simplex-chat.cabal`; `tests/Test.hs`; `tests/Bots/StoreService/NamesStoreTests.hs`; new `tests/Bots/StoreService/NamesBalancesTests.hs`; `tests/Bots/StoreService/NamesOrdersTests.hs` (the view).
- **Do:**
  - **Schema:** table `name_balances`: `registration_id` (the primary key: the order the balance belongs to), `amount` (cents left, `CHECK (amount >= 0)`), `credited`, `refunded` (default 0), `reserved_by` (the holder; unique, nullable), `refunded_at`, `created_at`, `updated_at`. Registrations gain `balance_source` (the order whose balance paid, indexed) and `balance_used` (cents, default 0). Name invoices gain `late_credited` (cents of a late payment already credited from this invoice, default 0). Payments gain `confirmed_amount` (cents of confirmed money, default 0; T24 writes it, only rising). `reserved_by` and `balance_source` reference registrations with the default action (restrict): the 30-day cleanup skips an unpaid order that a balance still names as its holder, and logs it, instead of deleting it. Up and down, SQLite and Postgres.
  - **Data step**, in the same migration: `UPDATE … SET live = 0 WHERE phase = 'taken'`, and a balance of `price` for every `taken` order without a code and with `price` above 0 (credited = amount = price). Until T24 changes `orderLive`, `/name` from such a row is refused by `updateRegistration` (`NotLive`, `bad_request`). No database holds names rows yet.
  - **Types:** `RPBalance`, the payment of an order with neither invoice nor code; `nrBalanceSource`, `nrBalanceUsed`; `Balance`; `OTBalance` in `orderHash`, tagged `name-balance`, over the order fields, the owner, the source's id, `price`, the balance seen and the method or ""; `ORBalanceChanged` in `OrderRefusal`, answered `409 balance_changed` with the current balance (`Web/Names/Orders.hs`).
  - **Queries**, each one conditional statement reporting whether it applied: `getBalance` (`held` and `heldUntil` from the holder's invoice; a hold by an order whose invoice is past its expiry with no money received reads as none, by the sweep's `unfundedQuery`); `overdueHolder`; `balanceByReference` (a registration id, or a name invoice id); `balanceChain` (from an order back along `balance_source` to the first, following `balance_source` only from orders past `unpaid`, since a closed difference order released its source's balance; each with its invoice, method, the cents it paid and its invoice's `late_credited`); `creditLatePayment` (in the caller's transaction, after the invoice's lock: the payment row's `confirmed_amount` less the invoice's `late_credited`, added to the order's balance by `creditBalance`, and `late_credited` raised to it, conditional on its old value; nothing when the increase is not positive; it returns the cents credited, and whether money is seen but not confirmed: received money with `confirmed_amount` at or below `late_credited`); `uncreditedLateInvoices` (closed unpaid name invoices past a given time whose payment row holds money and whose `late_credited` is 0); `creditBalance` (an upsert adding to `amount` and `credited`); `createBalanceRegistration` (a `queued` order, and its price spent where `amount` is the amount seen and nothing holds it); `createPaidRegistration` taking the order's `OrderTerms` (so a difference spend is stored under its `name-balance` hash, which its exact resend looks up) and an optional held part (the hold written after the registration's insert, on the same condition); `spendHeldBalance` (where this holder holds it and `amount ≥ used`; the hold cleared); `releaseHeldBalance` (by holder); `refundBalance` (where `amount` equals the cents given, above 0, and nothing holds it: `amount` 0, `refunded = refunded + cents`, `refunded_at` set).
  - **Turns:** `withBalanceTurn :: Turns -> RegistrationId -> IO a -> IO a`, taken after any label turn and never with a code turn; the module's header says so.
  - **Releases:** every close of an unpaid order releases its hold in the transaction that clears `live`: `closeOverdue` (`Names/Store.hs`: a next order, `/spend`) and `closeOrder` (`Orders.hs`: settlement's expiry, cancel, the poller's sweep). A late payment leaves the released balance alone (T24 credits it to the closed order's own balance).
  - **`Names/Balances.hs`:** `recordRefund :: Turns -> DBStore -> Text -> Int -> UTCTime -> IO (Either RefundRefusal RefundRecord)` (in the balance's turn and one transaction: the reference resolved, then `refundBalance`). It refuses any held balance, overdue holder or not, naming the holder and its invoice's expiry, so the console never closes an order it cannot announce or cancel at the provider; the poller's sweep closes it. The record names the order, the cents, and the whole `balanceChain`.
  - **`placeOrder`** closes, besides the overdue orders of the same wallet and name, the extra orders its caller names (a balance's holder) in its transaction, and returns them all for `announceClosed`. A named holder no longer live is skipped (the caller's conditional hold or spend then decides); one still live that `closeOverdue` cannot close refuses with `ORBalanceChanged`.
  - **The view:** `balance` (`amount`, `held`, `heldUntil`, `refunded`; no holder id) and `fromBalance` (contract section 6), read with `getBalance` in `registrationView`; `find`'s `invoiceId` null for `RPBalance`, and `find` returning the 50 newest and then every older order holding a balance above 0.
  - **`RPBalance` everywhere** behaves as a paid order: no code to reserve, release or spend; `/name`'s price cap; the worker keeps it through `taken` as it keeps an invoice order.
- **Acceptance:** migration up and down on SQLite and Postgres, with the data step; for orders without a balance every answer is as before, except the view's new `balance` (null) and `fromBalance` (0).
- **Tests:** each query's effect, and its refusal leaving the row unchanged; two holds of one balance, one winning; a refund refused while held and accepted once released; `spendHeldBalance` refused for another holder; `amount = credited − spent − refunded` after a sequence of credits, a spend at once, a held spend, a released hold and a refund, where spent is the sum of `balance_used` over the orders with this `balance_source` that are past `unpaid` (a closed unpaid holder recorded `balance_used` but spent nothing); the hold released by each close: `cancelOpenInvoice`, the sweep's `expireOverdue`, settlement's expiry, and `closeOverdue`; a hold by an overdue holder read as none and closed by `placeOrder` (its invoice `expired`, its registration not live, the hold gone, all in the caller's transaction); a named holder no longer live skipped, and one funded meanwhile refusing `ORBalanceChanged`; a hold and a spend read back through a new store connection (a restart); the cleanup skipping and logging an unpaid order still named as a holder; `balanceByReference` by either id; `balanceChain` over three orders; the `name-balance` `order_hash` changing with each field and not with the timestamp or the signature, and stored by `createPaidRegistration` for a difference spend; `recordRefund`'s refusals (no balance, another amount, held by an open holder, held by an overdue holder) and its record with the chain; credit, refund, credit, refund, with `refunded` the sum and the invariant holding; `balanceChain` of a closed difference order stopping at itself; `creditLatePayment` with nothing confirmed, with a confirmed rise, and repeated; `find` with 51 orders returning an older one with a balance; the data step on a `taken` paid row (balance of its price, not live) and on a `taken` code row (none); `withBalanceTurn` running two holders of one balance one after the other, and nested in `withLabelTurn` without deadlock; the view's `balance` and `fromBalance`.
- **Postgres:** one spec on two connections, in the wave's Postgres build: a spend at once against `refundBalance`, `spendHeldBalance` against `refundBalance`, and `creditLatePayment` against `refundBalance` (the refund refusing once the credit committed, as `amount = cents` is re-checked), each pair started together many times, exactly one conditional write applying each time and `amount` never negative. The safety argument rests on READ COMMITTED re-reading the row's `WHERE` after the lock.
- **Depends on:** T22.

#### T24. The balance's life: taken, settlement, closes and moves

- **Goal:** a paid order that ends `taken` keeps its worth as a balance; a late payment is credited to its order's balance (owner, 2026-10-22); a difference invoice spends its hold when paid; `/name` no longer moves a taken order, and a move to a cheaper name keeps the difference. (T23 releases holds at every close.)
- **Files:** `StoreService/Names/Worker.hs`; `StoreService/Orders.hs` (settlement's spend, late credits); `StoreService/Poller.hs` (the cleanup's lines); `StoreService/Providers.hs` (`Received` gains `rcvConfirmed`); `StoreService/Providers/BTCPay.hs` (each payment's `value` decoded, the settled sum); `StoreService/Providers/Stripe.hs` (the succeeded amount as confirmed); `StoreService/Store/Invoices.hs` (`confirmed_amount` in the payment upsert, only rising); the fake providers in the tests; `StoreService/Names/Store.hs` (`orderLive`); `StoreService/Names/Orders.hs`; `StoreService/Web/Names/Owner.hs`; `tests/Bots/StoreService/` `NamesWorkerTests.hs`, `NamesOrdersTests.hs`, `NamesOwnerTests.hs`, `NamesStoreTests.hs`, `NamesPhasesTests.hs`.
- **Do:**
  - The worker's `taken` for a paid order (invoice, balance or both): the `taken` write and `creditBalance` of its `price` in one transaction (registration before balance), in the name's turn and then the balance's turn. A code order releases its code as now.
  - `orderLive`: `taken` is not live. `movable` drops `taken`, so `/name` from `taken` answers `bad_request`.
  - Settlement (`nameSettled`): the queue write and `spendHeldBalance` in one transaction. A held spend that does not apply cannot happen while every close releases its hold; if it does, the order is queued all the same, as the buyer paid the difference, and the operator is alerted with both ids.
  - **Confirmed money** (contract section 8): `Received` gains `rcvConfirmed`. BTCPay: the sum of the chosen method's payments with status `Settled`, each `value` times the method's rate, in cents. Stripe: the succeeded amount (`SigSettled` only), else 0. The payment upsert writes `confirmed_amount` with the larger of the old and new values, and `newPayment` counts a rise in it as a new payment even when the totals are unchanged, so an `Expired` invoice whose late payment confirms later reaches the late outcome again. Badge settlement ignores it.
  - **Late payments:** every outcome that was `NOLate` (paid after the close, `NotLive`, a provider close holding money, a new figure on a closed invoice) and `cancelOpenInvoice`'s late case run `creditLatePayment` inside the same transaction, beside `upsertPayment`, never after its commit (a crash between two commits would lose the credit: the same figure is not a new payment again). `NameOutcome`'s late case carries the cents credited out of the transaction for the log. A credit is an info log naming the invoice, the order and the cents: the money is the buyer's balance, and support acts only when the buyer asks. Money seen but not confirmed (a mempool payment, Monero confirming with a zero figure) credits nothing and is an info log saying it is pending.
  - **`LiveOrderExists` at settlement** (another live order holds the commitment, or the owner and the name; impossible while the unique indexes hold): the order is closed there as `closeOrder` closes it (not live, its hold released), its payment credited as a late payment, and the operator alerted, since an invariant broke. No sweep closes a paid invoice, so leaving it live would keep its hold for ever.
  - The poller: the cleanup's "received a payment; refund it" line becomes an info line saying the order is kept with its balance; its comment on settlement's alert follows. Its daily pass alerts (`logError`, once a day per invoice) for each of `uncreditedLateInvoices` past `settleWindow`: money seen that never confirmed within the poller's read window, for support to check at the provider. The badge poller's refund alert is unchanged.
  - `/name` of a paid order to a cheaper name: `price` lowered and the difference credited to the order's balance in the move's transaction (registration before balance), in the label turns and then the balance's turn.
- **Tests:** two wallets' paid orders for one name, the later `taken` with a balance of its price and not live; a taken order paid wholly from a balance and one paid partly, each credited its whole `price` as a balance of its own, the source's balance untouched; a code order taken: its code released, no balance; a refused credit write leaving the order as it was (one transaction); a held order settled: `queued`, the hold spent, the source's `amount` down by `fromBalance`; a difference invoice funded before its expiry time and settled after it, still `open` in the database: `queued`, the hold spent; a late payment after the close credited to the closed order's balance, the released source balance untouched, logged at info; a full late payment, a crypto payment confirming over two reports (credited twice, the increase only), the same report twice (credited once), a provider close holding a confirmed part payment, a cancel meeting confirmed money; an unconfirmed late payment crediting nothing (logged as pending), then credited once when its payment reads `Settled` with the totals unchanged; an unconfirmed payment replaced (RBF) or dropped never crediting, and alerted by the daily pass once `settleWindow` has passed; Monero's zero figure while confirming logged as pending, not alerted; the BTCPay settled sum decoded from a recorded payment-methods body with one `Processing` and one `Settled` payment; Stripe's succeeded amount confirmed; the credit and the payment written in one transaction (a failure after the credit leaves neither); `LiveOrderExists` at settlement closing the order, crediting and alerting; a credit while the closed order's balance is held by a spend (the held spend settles, the credit stays); a covered `/spend` that read the amount before a credit answering `balance_changed`; credit, refund, late credit, refund, with `refunded` the sum; the cleanup keeping such an order with its balance; the credited balance spent by `/spend` from the closed order and refunded by `recordRefund`; settlement racing `recordRefund` (the refund refused while held, the balance spent after); the alert when the held spend does not apply (a hold removed by hand); `/name` from `taken` refused; a cheaper move crediting the difference, a cheaper move whose compare-and-set loses leaving no credit, and a dearer one still `price_too_high`; `fromBalance` unchanged by a move; a held poll woken by the `taken` write and not by a balance change.
- **Depends on:** T23, T18.

#### T25. Spending a balance

- **Goal:** `POST /api/registration/:id/spend`.
- **Files:** `StoreService/Web/Names/Orders.hs` (the handler; `createAtProvider` takes the invoice's amount and the insert, shared with `/api/invoice`; the resend answer for `RPBalance`); `StoreService/Names/Balances.hs` (the balance check and the order's placement); `StoreService/Names/Signatures.hs` (`spendMessage`); `StoreService/Web/Names.hs` (its route); new `tests/Bots/StoreService/NamesSpendTests.hs`; `tests/Bots/StoreService/NamesWiringTests.hs` (the route's `405` with `Allow`); `tests/Test.hs`; `simplex-chat.cabal`.
- **Do:** the contract's `/spend` in its order of checks, under the new name's label turn and then the balance's turn: the order fields by `parseOrderFields`; existing orders by `existingOrder` with the `name-balance` hash; the balance's overdue holder closed with the wallet's overdue orders in `placeOrder`'s transaction; a covered price by `createBalanceRegistration`, a difference by the provider and then `createPaidRegistration` with its hold; a write refused at the balance answering `balance_changed`, with the provider's invoice cancelled as for an order that lost; answers after the turn (`Answer`), publishing closed orders and cancelling their invoices at the provider.
- **Tests:** a covered spend (`queued`, `fromBalance` the price, the source's `amount` less the price) and an exact one (`amount` 0); a difference spend (the invoice view with `amount` the difference, the registration `unpaid` with `fromBalance` the balance, the source's balance `held` with `heldUntil` the invoice's expiry); an exact resend of a difference spend in `unpaid` (found by its `name-balance` hash, not met as `order_exists`) and in later phases (moved there by T23's store writes), with a new timestamp and signature, after the balance changed; an `/api/invoice` body with the same order fields not matching it; `balance_changed` for a stale `balance`, a held balance, and an order with none (`balance: null`); a `method` with a covered price, and none with a difference, `bad_request`; another signer, a stale timestamp, an unknown id `404`; `order_exists`; `name_taken`; `catalog_changed`; `name_check_unavailable`; `provider_unavailable`; a balance held by an overdue order spent (that order closed, its invoice `expired`, published, cancelled at the provider); the holder closed meanwhile by a cancel (the spend goes through) and funded meanwhile (`balance_changed`, the provider invoice cancelled); two concurrent spends of one balance for two names, one winning and the other `balance_changed`; the write refused at the balance after the provider invoice was made, by a store write between check 6 and the transaction through a second `DBStore` (`balance_changed`, the provider invoice cancelled); a spend racing `recordRefund`, one winning; the create limit; the `sealed` and body caps.
- **Depends on:** T23. Its paid path is end to end only with T24's settlement: the wave-10 integrator adds that spec (below).

#### T26. The operator's refund

- **Goal:** support records a refund, which zeroes a balance (contract section 8, Balances).
- **Files:** `StoreService/Service.hs` (the `//refund` parser, its run and replies, the usage line); `apps/simplex-store-service/README.md` (the operator commands, `//refund` with support's steps); `tests/Bots/StoreService/GroupIntegrationTests.hs` (the operator command specs); `tests/Bots/StoreService/BotTests.hs` (`badgeCmdUsage`).
- **Do:**
  - `//refund <reference>` answers the balance and the command that records it: "Order <id>: balance $100.00 (credited $100.00, refunded $0.00). Paid by: <the chain>. To record its refund: //refund <reference> 10000". The chain lists, from this order back along `balance_source` to the first, each order's invoice, method and cents, a late payment's cents marked as such. A held balance names its holder and its invoice's expiry; an order without a balance says so.
  - `//refund <reference> <cents>` runs `recordRefund` and answers "Recorded a refund of $100.00 for order <id>. Refund it now, by: <the chain>", or why nothing was recorded: no balance; the balance is $X now, not the cents given; "held by order <id>, its invoice expired at <time>; the poller closes it within <grace>; run the command again then" (or "open until <time>" for a holder not yet overdue).
  - The reference is an invoice id or a registration id, as the page shows it. The command is the service console's `//` only, as `//revoke` of a prepaid code is: it takes the balance's turn, which the group lane does not hold. A second process on the same database (the CLI mode) is kept safe by the conditional write, not the turn.
  - Each recorded refund is logged with its reference and cents.
- **Tests:** a refund shown, recorded, then shown as refunded; each refusal; a held balance refused, then recorded once its holder's invoice is cancelled (T23's release in `closeOrder`); an overdue holder refused with its expiry, then recorded once the sweep closed it; a chain of three orders in the reply; a late-paid difference order whose chain stops at itself, its cents marked late; a reference by invoice id and by registration id; the usage line.
- **Depends on:** T23.

#### T27. The gas key in the README

- **Goal:** the operator can create, fund, allow and rotate the registrar key from the README alone (owner, 2026-10-22: README steps only; no startup log, no alert, no external signer).
- **Files:** `apps/simplex-store-service/README.md` (a section "The gas key", linked from the `[chain]` settings); `apps/simplex-store-service/store_service.ini.example` (the `service_key` comment points to it).
- **Do:** steps with exact commands (Foundry's `cast`, as the local chain uses, and the JSON-RPC `curl` form for each read), against the configured chain and the reth node at `[chain] rpc_url`:
  - **Create** the key on the service host (`cast wallet new`), keep it only in the ini as `[chain] service_key` (`0x` and 64 hex digits), with the file readable by the service user alone; never in a log, a shell history or the repository.
  - **Find its address:** `cast wallet address --private-key …`, and the same address in the ini check (`cast wallet address` on the value read from the file).
  - **Grant the allowance:** the guardian (the controller's `beneficiary`) sends `SimplexSponsor.setRegistrarAllowance(address registrar, uint256 allowanceUSD)`, in attoUSD (cents × 10^16); it replaces, not adds; zero is the kill switch. Check it with `registrarAllowance(address)`. The worker alerts `InsufficientAllowance` when it runs out.
  - **Fund and top up** with ETH on `[chain] chain_id`: the gas of one commit and one register, measured on the local chain (the webapp's `dev/start.sh` and `dev/deploy.ts`) and written in the section, times the expected orders, at `[chain] max_fee_per_gas`; a record edit's gas too.
  - **Check the balance** through the reth RPC: `eth_getBalance` of the address at `latest` (`cast balance --rpc-url …`), and the pending nonce against the latest (`eth_getTransactionCount`) to see transactions in flight.
  - **Rotate:** create and fund the new key and grant its allowance; stop the service when `sx_badge_service_name_transactions` has no `pending` row (if it has, start it again on the old key until they settle, since saved transactions are signed by the old key and the sender orders them by its nonce); put the new key in the ini and start; then the guardian zeroes the old key's allowance and the old key's ETH is sent back. The task reads `Names/Sender.hs` to confirm what a restart does with transactions of another key and writes the steps to match.
- **Acceptance:** each command run once against the local anvil chain from the webapp's deploy, its output shape quoted; no code change.
- **Depends on:** T16, T17 (merged). It runs in wave 9, where nobody else edits the README.

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
- **Depends on:** T10–T20 and T22–T26 merged (it runs in wave 11), and the mock's Appendix A changes in the webapp, W1f-1 to W1f-3 included. The balance needs a paid order, which this run has no provider for; the service's own specs (T23–T25 and the wave-10 integrator's) and the webapp's e2e against the mock cover it.

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
| 8 | T18, T22 | T18: `Web/Names/Registration.hs`, `Names/Worker.hs`. T22: `Names/Orders.hs`, `Names/Store.hs`, the migrations, `Names/View.hs`, `Names/Signatures.hs`, `Web/Names/Orders.hs`, `Web/Names/Owner.hs` |
| 9 | T23, T27 | `Names/Store.hs`, the migrations, `Orders.hs`, `Names/Turns.hs`, `Names/Balances.hs`, `Names/View.hs`, `Names/Orders.hs`, and, for `RPBalance`'s case, `balance_changed`'s response and `createPaidRegistration`'s call only, `Names/Worker.hs`, `Web/Names/Orders.hs`, `Web/Names/Owner.hs`. T27: `README.md` |
| 10 | T24, T25, T26 | T24: `Names/Worker.hs`, `Orders.hs`, `Poller.hs`, `Providers.hs`, `Providers/BTCPay.hs`, `Providers/Stripe.hs`, `Store/Invoices.hs`, `Names/Store.hs`, `Names/Orders.hs`, `Web/Names/Owner.hs`. T25: `Web/Names/Orders.hs`, `Names/Balances.hs`, `Names/Signatures.hs`. T26: `Service.hs`, `README.md` |
| 11 | T21 | `README.md` |

The wave-10 integrator, after merging T24, T25 and T26, adds one end-to-end spec to `NamesSpendTests.hs`: a difference spend paid at `FakeBTCPay` and settled (the order `queued`, the hold spent); the same cancelled (the hold released); a covered spend's order ending `taken` (a balance of its own, its whole `price`); a `//refund` of the rest; and a difference invoice paid after it closed, credited to its order's balance. Wave 10 has no other dependency between its tasks.

Each task is one or more atomic commits (`<scope>: <subject>`, no body) on its own branch. It ends with the build and its `-m "StoreService names: <part>"` tests green in its worktree, and the badge specs green. The manager merges the wave's branches one at a time and runs the build and all `StoreService` specs after each merge.

## Webapp tasks for the balance

In simplex.domains, branch `sh/names-webapp`, in the webapp's workstream beside waves 8 to 10. They follow the contract alone, and land before T21, which runs the webapp's e2e cases against the service. The mockup (ep's, v15) still shows Take a code; its balance screens are drawn with W1f-3 and go to ep with its review.

#### W1f-1. Remove the taken-order code from the mock and the page

- **Goal:** contract Appendix A, item 36, in the mock and the page.
- **Files:** `dev/mock.ts`; `dev/mock/service.ts`; `src/api.ts`; `src/crypto/code.ts`; `src/flow.ts`; `src/store.ts`; `src/screens/order.ts`, `search.ts`, `names.ts`; `src/views/order.ts`, `search.ts`, `names.ts`; the tests that use them (`rg -l 'code_issued|takenCode|takenOrderCode|takeCode|codeLength|code-issued|codeOffer' src dev test e2e`).
- **Do:** delete the mock's `takenCode`, its route, `codeLengthFor`, `issuedCodeLength` and the `code_issued` phase; `api.takeCode`, the view's `codeLength`, and `code_issued` in `Phase` and `PHASES`; `takenOrderCode`; `flow.takeCodeFor` and `flow.takenCodeOf`; the record's `takenCode`; the `code-issued` kind, `buildCodeIssued`, `takeCode` and Take a code on the taken page; `codeOffer` and "Take a code instead"; the "Code taken" chip. The taken page keeps Choose another name until W1f-3.
- **Acceptance:** the `rg` above finds nothing; vitest, and the e2e in Chromium, Firefox and WebKit, green.
- **Tests:** deleted with the code: the `takenOrderCode` vectors, the code-issued screens, the code cases of `e2e/taken.spec.ts`; the rest adjusted.
- **Depends on:** the contract (commit "plans: names taken orders keep a balance").

#### W1f-2. Balances in the mock and the API client

- **Goal:** the mock serves the contract's balances and `/spend`; `src/api.ts` and `src/flow.ts` use them.
- **Files:** `dev/mock/service.ts`; `dev/mock.ts`; `src/api.ts`; `src/flow.ts`; `src/store.ts`; `test/api.test.ts`; `test/flow.test.ts`; the mock's tests.
- **Do (mock):** a balance per order (`amount`, `credited`, `refunded`, the holder); credited with `price` when a paid order is taken (`/control/take`), and with the difference on a cheaper `retarget`; `taken` not live and not movable; `spend` in the contract's order of checks, with `balance_changed`; settlement (`/control/settle`) queues the order and spends its hold; a cancel, an expiry (`/control/expire`, `/control/overdue`) and an overdue order closed by a next order or a spend release it; a hold by an overdue order reads free; `POST /control/refund/:reference`, with optional `cents`, answering as `recordRefund`; the view's `balance` (`amount`, `held`, `heldUntil`, `refunded`) and `fromBalance`; a refund of a held balance refused; a late payment (`/control/settle` on a closed invoice, and on an expired one with a part payment) credited to that order's balance, each confirmed figure once, and a `/control/partial` or `/control/confirming` on a closed invoice crediting nothing until settled; `find`'s `invoiceId` null for an order paid wholly from a balance, and older orders with a balance after the 50 newest.
- **Do (client):** `RegistrationView` gains `balance` and `fromBalance`; `api.spend`; `flow.spendBalance(record, label, links, quote, method)` builds the order with the source record's owner code (the same owner, `sealed` under it), signs `simplex.domains spend …`, and keeps the new order's record with that owner code, `method` (`"balance"` when the balance covers it) and `balanceSource` (the paying order's id, so the page can name a balance's holder); `OrderRecord.method` gains `"balance"`; `paid` is the registration's `price`, not the invoice `amount`, for every order (`paidWith` today takes `amount`, which for a difference invoice is the difference only); `balance_changed` gives the current balance to the caller.
- **Tests:** the mock's `spend` in each answer and refusal of T25's list; settlement and every close of a hold; a refund; `spendBalance`'s message and body against a vector from a fixed key, the same vector T25's test pastes.
- **Depends on:** W1f-1.

#### W1f-3. The balance on the page

- **Goal:** the buyer sees and uses a balance in Your names, the search, Check your order, the taken page and the payment page.
- **Files:** `src/views/names.ts`, `search.ts`, `checkout.ts`, `order.ts`, `shell.ts` (the support popup); `src/screens/names.ts`, `search.ts`, `checkout.ts`, `order.ts`; their unit tests; `e2e/taken.spec.ts`; new `e2e/balance.spec.ts`.
- **Do:**
  - **Your names:** an order whose view has a `balance` with an `amount` shows "Balance $X" with **Use** (the search, `?balance=<registrationId>`) and **Refund** (a popup: "Write to chat@simplex.chat with the reference <id>; support refunds it by hand", the reference being the order's invoice id, else its registration id); a balance and a refund show together ("Balance $X" and "Refunded $Y", `refunded` being the sum of refunds); a held balance says it is in use by an open invoice until `heldUntil`, and links the holder when this browser's records have it (`balanceSource`); a refunded one "Refunded $X by support". An order paid from a balance says so in its line.
  - **The search with a balance:** "Your balance: $X" in place of the retarget alert; an available name says "Covered by your balance; $Y stays" or "Pay $Z more".
  - **Check your order** with a balance: the price, "Balance −$X", then either "Nothing to pay" with Register, or "Pay $Z more" with the payment methods; the order goes out by `flow.spendBalance`; on `balance_changed` the page reads the balance again and says what changed.
  - **The taken page:** "Someone registered it first. Your payment of $X is kept as a balance: use it for another name, paying only the difference, or ask support for a refund." with Use and Refund; Choose another name leaves it.
  - **The payment page** of a difference invoice: "$X from your balance; this invoice is the rest, $Z."
  - **A late payment:** Your names' line "Paid after the invoice closed; support refunds it" and "Part of the payment arrived; support sorts it out" become "Paid after the invoice closed; kept as your balance" (with Use and Refund); the order page's paid-late text and the rate-expired line ("what you sent is refunded") say the money is kept as the order's balance.
  - A failed order keeps Choose another name; its cheaper-name line says the rest stays as balance, from the view's `price` and `balance`, not the record's `paid`.
  - The support address is `CONTACT_EMAIL`, chat@simplex.chat (`src/views/shell.ts`), as on every support line (manager's ruling, 2026-10-22).
- **Acceptance:** the flows against the mock in Chromium, Firefox (158 included) and WebKit, with motion and no flashes; Lighthouse as before.
- **Tests:** unit tests of each state above; e2e: a paid order taken, then its balance spent on a cheaper name (covered, the rest kept) and on a dearer one (the difference paid by card in the mock, the name registered, the balance 0); a difference invoice cancelled, the balance free again; a refund by `/control/refund`, shown as refunded; the name code restored in another browser showing the balance.
- **Depends on:** W1f-2.

## Owner answers (2026-10-21, binding)

1. **Orders opened from the app** are dropped: the app will buy names itself (#7530 §13.6). Every web order has an owner code and `sealed`; there are no `?name=&owner=` links and no app-signed intents.
2. **No resolver service** for the new deployment: the SMP relays and the app read the chain from their own node (reth with nimbus), as the service does. Sequence gap 31 is closed.
3. **Launch prices:** $100, $200 and $400 for 2 years, set on chain with `setPrices`: base 5000 cents a year, 7 letters 10000, 6 letters 20000. No per-name prices.
4. **365-day years**, showing the real end date: for an order before 1 January 2027, 2 years end on 31 December 2028, 5 years on 31 December 2031.

Manager rulings on the first review round: orders disclosed only to who made them (contract section 1); per-name and per-code serialization; the dApp line on Check your order, the page before payment (ep, 2026-10-21 08:43, recorded in the owner decisions); prepaid codes and the paid years at least 2; one worktree per task with one global build lock.

Manager rulings on the third round: an overdue order closed by the next order's transaction, settlement only of a live order, a payment for a closed order a late payment; no address pinning (the pinned simplexmq cannot), a best-effort host check before the core fetches and an egress rule in the deployment, every host of a link checked; T7 split so link kinds land alone in wave 2; a links refusal recorded only after the commitment and intent checks; a `/code` repeat answering the issued code; `move_limit` answering the order's state with a support line; `registration_changed` on every lost compare-and-set; the turns in their own module from the wave-1 integration task.

Manager rulings on the second round: `/name` only from `failed`, a reveal refused for its links, or `taken`, at most 3 moves per order; `/name` re-checks and re-reserves a code order's code; the reveal joins the per-name serialization with a version compare-and-set; the link host rule (public IP literals allowed, `.onion` and unresolvable hosts `unavailable`; its pinning replaced in the third round); separate search and preview fetch bounds that never wait past their timeout; T21 in its own wave; `live` transitions in one task (the expiry-time rule replaced in the third round by closing the overdue order); worktrees start from a copy of the wave's base build.

Still open and not backend: the dApp host, and the app's menu path in "Use in the app".

## Owner decisions (2026-10-22, binding)

1. **No code when a paid name is taken.** `/code`, `code_issued`, the view's `codeLength` and the page's Take a code go, though T15 built them (ep had asked to keep them; the owner overrules). T22 removes them.
2. **A balance per order.** A taken paid order's payment becomes a balance owned by that order's name code, never merged across orders or wallets. Your names shows it; buying another name with it pays only the difference by card, BTC or XMR, or nothing when it covers the price.
3. **Leftover** stays as balance, usable again, or the buyer asks support for a refund by hand (a popup: write to chat@simplex.chat with the reference).
4. **Refunds** by support on request only: no automatic refund, no refund address screen. The operator records one with `//refund` (T26).
5. **A balance never expires.**
6. **Prepaid codes are unchanged:** a code order that ends taken releases its code and gets no balance.
7. **Late payments** (owner's answer, 2026-10-22): a payment for any name invoice closed before its money arrived, a difference invoice included, is credited to that order's balance; Your names shows it, and it buys a name or is refunded by support like any balance (T24).

Manager rulings (2026-10-22): the refund popup writes to chat@simplex.chat (`CONTACT_EMAIL`); a move to a cheaper name credits the difference to the order's balance.

## Decided here

- **Label rule:** simplexmq's `SimplexDomain` parser on `label.tld`, not #7390's `validLabel`, which is unmerged and refuses `--` only at positions 3–4. One rule with the relays.
- **Prices without the label:** `rentPriceUSD` with a placeholder label of the same length. The deployed oracle reads only the length, and the RPC node never sees the label before the reveal.
- **Paid years:** 2, as a setting (`[names] years`, at least 2). No years picker: the mockup shows a fixed term.
- **The term check:** `duration` in every order, register and retarget body, checked within 1 hour of `max(0, termStart − now)` and stored; returned in the view so the name code alone rebuilds the commitment; in the order and retarget signatures.
- **A new commitment per new order**, with one live order per wallet and name (by commitment, or owner and label hash), enforced by the order lock and unique constraints.
- **Signed orders:** the owner signs every order, which proves the owner (so nobody fills another wallet's `find` list) and lets the service refuse every non-identical order with a bare `order_exists`.
- **A balance is its own table, keyed by the order it belongs to**, not columns of the registration: other orders' spends, settlement and the operator write it, and none of them may bump that registration's `version` or lock its row out of order. Rows lock invoice, registration, then balance.
- **US cents**, as every price and invoice amount; refunds are of that amount, paid back as support decides.
- **A spend is a new order**, `POST /api/registration/:id/spend`, signed by the balance's owner, for a new order of that owner: the same name code owns the new name, as `/name` keeps the owner. It is not an `/api/invoice` field, since a covered spend has no invoice, and not `/name`, which moves an order instead of paying for one.
- **A hold, not a deduction, while the difference invoice is open**: only one order's open invoice can hold a balance (a unique `reserved_by`), settlement spends it, and every close releases it in the transaction that closes the order. A hold by an overdue order reads free and is closed by the next spend, as section 1 closes an overdue order; a refund refuses any held balance and waits for the poller, since the console cannot announce a close or cancel an invoice at the provider.
- **The page states the balance it saw** (`balance` in the body), so the buyer never pays a difference other than the one shown; `balance_changed` carries the current balance.
- **`/name` no longer moves a taken order**: the balance spend replaces it, so one path buys another name with a taken order's money. `/name` from `failed` and after refused links stays.
- **A move to a cheaper name keeps the difference as balance** instead of refunding nothing, as the owner's leftover rule does for a spend (manager's ruling, 2026-10-22).
- **A taken order credits its whole `price`**, including any part from another balance, as a balance of its own: two balances, never merged.
- **A late payment credits only confirmed money** (BTCPay payments `Settled`, Stripe succeeded), in cents at the invoice's rate, each rise once (`confirmed_amount` per payment, `late_credited` per invoice), inside settlement's own transaction. Reason: a 0-conf payment can be replaced or dropped, and a credit spent by a covered `/spend` registers a name at once. Money seen but unconfirmed is logged as pending; only an invoice still holding money with nothing credited after `settleWindow` alerts.
- **Balance changes leave `version` alone**: the order is final once `taken`, and the page reads the view for its balance.
- **The view says a balance is held, not by whom** (`held`, `heldUntil`): the holder's id would give whoever holds the source's link the holder's capabilities. The page names the holder from its own records.
- **`fromBalance` is fixed when the order is made**; a move does not change it.
- **`find` returns older orders with a balance** after the 50 newest, since a balance never expires.
- **Refunds by the `//refund` console command only**, as `//revoke` of a prepaid code: it takes the balance's turn, which the group lane does not hold. It names the reference the page shows (invoice id, else registration id), shows the balance and every invoice that funded it back along the chain of balances, and records only the whole amount given.
- **A revoked code** answers `state: revoked`; a `reserved` code names no registration.
- **`find` returns `labelHash` and `sealed`**, the 50 newest, so a name code's verdict names its orders in one request.
- **The intent is signed at the reveal** (gap 34): the mock and the mockup's copy ("keep this page open, or reopen its link within 24 hours") assume it, and the service learns no name before the commit.
- **`/name` from `failed`, or a `committed` order whose reveal was refused for its links** (manager's ruling; `taken` removed 2026-10-22), at most 3 moves, so refused links can be corrected without support and no order can drive endless commits.
- **Alain's prototype differences** (gaps 32, 33): the derived secret and `revealBefore` as the deadline stay; agreeing them with Alain is coordination, not an owner question.
- **One short link per key** (gap 35), as the mock, the redesign and the app's `firstNameLink` already do.
- **Limits** (gap 29): the values in the contract's section 1, every request counted, per-minute windows so `Limit` needs no change, `maxBuckets` 32768, an edit cap of 10 relayed edits an hour per name, `revealMargin` 900 seconds.
- **An edit judged applied by nonce and record** (gap 30): kept, with "applied" defined as the nonce passed and the record holding the value.
- **`edit_pending` after 90 seconds**, so a records request never outlives a proxy's idle timeout while the edit goes on.
- **Long polls on `version`**, so changes inside a phase reach the page at once.
- **A name paid for here but not yet `registering`** still searches as available: blocking it earlier would tell searchers it is being bought before the reveal. The later of two buyers ends `taken` with its value kept.
