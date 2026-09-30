# Per-server roles for self-hosted servers — Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers-extended-cc:subagent-driven-development (if subagents available) or superpowers-extended-cc:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give each individual self-hosted SMP server its own receive / private-routing / name-resolution toggles, stored per server and plumbed to the agent.

**Architecture:** Add `roles :: Maybe ServerRoles` to simplex-chat's `UserServer'`; store it in three nullable `protocol_servers` columns; resolve `Maybe → ServerRoles` in `agentServerCfgs` (default `storage+proxy` on, `names` off). No simplexmq change. Add the three toggles to custom SMP server screens on Kotlin and Swift.

**Tech Stack:** Haskell (simplex-chat), SQLite + PostgreSQL migrations, Kotlin (multiplatform), Swift (iOS). Tests via `cabal test` (HSpec).

**Design doc:** `plans/2026-07-14-self-hosted-server-roles-product.md`

**Companion checkout:** simplex-chat builds against simplexmq pinned in `cabal.project:24`; the working `../simplexmq` already contains `ServerRoles{storage,proxy,names}`. To build/test locally against it, uncomment `packages: . ../simplexmq` (`cabal.project:2`). No simplexmq edits are made by this plan.

---

## Chunk 1: Haskell backend (types, resolution, DB, store, validation, tests)

### Task 1: Add per-server `roles` to `UserServer'` and resolve it in `agentServerCfgs`

**Files:**
- Modify: `src/Simplex/Chat/Operators.hs:242-250` (type), `:331-333` (constructor), `:438-448` (resolution), add constant near `operatorRoles` (`:174`)

- [ ] **Step 1: Add the field to `UserServer'`** (after `enabled`, before `deleted`)

```haskell
data UserServer' s (p :: ProtocolType) = UserServer
  { serverId :: DBEntityId' s,
    server :: ProtoServerWithAuth p,
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool,
    roles :: Maybe ServerRoles,
    deleted :: Bool
  }
```

- [ ] **Step 2: Add the default constant** (next to `operatorRoles`, `:174`)

```haskell
-- Default roles for a self-hosted server without stored roles: receive + private
-- routing on, name resolution off. Also the default for newly added servers.
defaultUserServerRoles :: ServerRoles
defaultUserServerRoles = ServerRoles {storage = True, proxy = True, names = False}
```

- [ ] **Step 3: Set `roles = Nothing` in the sole constructor** `newUserServer_` (`:333`)

```haskell
newUserServer_ preset enabled server =
  UserServer {serverId = DBNewEntity, server, preset, tested = Nothing, enabled, roles = Nothing, deleted = False}
```

- [ ] **Step 4: Resolve per-server roles in `agentServerCfgs`** (`:442-448`). Bind the field via record pattern (no `OverloadedRecordDot`; bare `roles` selector is ambiguous with `ServerCfg.roles`).

```haskell
    agentServer srv@UserServer {server, enabled, roles = srvRoles} =
      case find (\(d, _) -> any (matchingHost d) (srvHost srv)) opDomains of
        Just (_, op@ServerOperator {operatorId = DBEntityId opId, enabled = opEnabled})
          | opEnabled -> Just ServerCfg {server, enabled, operator = Just opId, roles = operatorRoles p op}
          | otherwise -> Nothing
        Nothing ->
          Just ServerCfg {server, enabled, operator = Nothing, roles = fromMaybe defaultUserServerRoles srvRoles}
```

- [ ] **Step 5: Drop the now-unused `allRoles` import.** `allRoles` (`Operators.hs:52`) had its only use at `:448`, which Step 4 replaces. Remove it from the import list to avoid an unused-import warning:

```haskell
import Simplex.Messaging.Agent.Env.SQLite (ServerCfg (..), ServerRoles (..))
```

- [ ] **Step 6: Build.** `fromMaybe` is already imported in `Operators.hs`. `ServerRoles(..)` is imported (`:52`).

Run: `cabal build lib:simplex-chat --ghc-options -O0`
Expected: compile errors only at other `UserServer{...}` construction sites — none exist beyond `newUserServer_` and `getProtocolServers` (fixed in Task 3). Record *patterns* elsewhere (`Operators.hs:410,416,532,537,538,568`, `Profiles.hs:981`) are unaffected.

- [ ] **Step 7: Commit** `feat(servers): add per-server roles field to UserServer`

### Task 2: Database migrations (SQLite + Postgres) + schema

**Files:**
- Create: `src/Simplex/Chat/Store/SQLite/Migrations/M20260714_server_roles.hs`
- Create: `src/Simplex/Chat/Store/Postgres/Migrations/M20260714_server_roles.hs`
- Modify: `src/Simplex/Chat/Store/SQLite/Migrations.hs` (import + list), `src/Simplex/Chat/Store/Postgres/Migrations.hs` (import + list)
- Modify: `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql`, `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql` (regenerated, see Step 5)

- [ ] **Step 1: SQLite migration** (template: `M20260707_file_digest.hs`, `M20260603_simplex_name.hs`)

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260714_server_roles where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260714_server_roles :: Query
m20260714_server_roles =
  [sql|
ALTER TABLE protocol_servers ADD COLUMN role_storage INTEGER;
ALTER TABLE protocol_servers ADD COLUMN role_proxy INTEGER;
ALTER TABLE protocol_servers ADD COLUMN role_names INTEGER;
|]

down_m20260714_server_roles :: Query
down_m20260714_server_roles =
  [sql|
ALTER TABLE protocol_servers DROP COLUMN role_storage;
ALTER TABLE protocol_servers DROP COLUMN role_proxy;
ALTER TABLE protocol_servers DROP COLUMN role_names;
|]
```

- [ ] **Step 2: Postgres migration** (template: Postgres `M20260707_file_digest.hs`; `[r|...|]` and `Text`)

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260714_server_roles where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260714_server_roles :: Text
m20260714_server_roles =
  [r|
ALTER TABLE protocol_servers ADD COLUMN role_storage SMALLINT;
ALTER TABLE protocol_servers ADD COLUMN role_proxy SMALLINT;
ALTER TABLE protocol_servers ADD COLUMN role_names SMALLINT;
|]

down_m20260714_server_roles :: Text
down_m20260714_server_roles =
  [r|
ALTER TABLE protocol_servers DROP COLUMN role_storage;
ALTER TABLE protocol_servers DROP COLUMN role_proxy;
ALTER TABLE protocol_servers DROP COLUMN role_names;
|]
```

- [ ] **Step 3: Register SQLite migration** (`SQLite/Migrations.hs`): add `import ...M20260714_server_roles` after the `M20260707_file_digest` import (`:166`), and append to the list after the `20260707_file_digest` entry (`:330`) — add a comma to that line:

```haskell
    ("20260707_file_digest", m20260707_file_digest, Just down_m20260707_file_digest),
    ("20260714_server_roles", m20260714_server_roles, Just down_m20260714_server_roles)
```

- [ ] **Step 4: Register Postgres migration** (`Postgres/Migrations.hs`): mirror Step 3 at the import (`:43`) and list (`:84`).

- [ ] **Step 5: Regenerate reference schema.** The repo keeps `chat_schema.sql` in sync with migrations (see the "ci: update query plans" commits). Regenerate both SQLite and Postgres `chat_schema.sql` using the repo's schema-dump script rather than hand-editing. If no script is found, hand-add the three columns to the `protocol_servers` block in both `chat_schema.sql` files (SQLite `INTEGER`, Postgres `smallint`).

Run: `cabal build lib:simplex-chat --ghc-options -O0`
Expected: PASS.

- [ ] **Step 6: Commit** `feat(servers): add nullable role columns to protocol_servers`

### Task 3: Store read/write of `roles` (`Profiles.hs`)

**Files:**
- Modify: `src/Simplex/Chat/Store/Profiles.hs:639-654` (select/read), `:656-667` (insert), `:669-679` (update)

- [ ] **Step 1: Read roles in `getProtocolServers`.** Extend the SELECT and `toUserServer`. Store roles as three nullable `BoolInt` columns; reconstruct `Maybe ServerRoles` (all present → `Just`, else `Nothing`).

**MUST split with `:.`.** The existing select is 8 columns → an 8-tuple. Adding 3 gives 11, but the SQLite/Postgres `FromRow` instances cap flat tuples at **10** — an 11-element flat tuple has no instance and will not compile. Parse the 3 role columns as a trailing group via `:.`.

```haskell
      [sql|
        SELECT smp_server_id, host, port, key_hash, basic_auth, preset, tested, enabled,
               role_storage, role_proxy, role_names
        FROM protocol_servers
        WHERE user_id = ? AND protocol = ?
      |]
```

```haskell
    toUserServer :: ((DBEntityId, NonEmpty TransportHost, String, C.KeyHash, Maybe Text, BoolInt, Maybe BoolInt, BoolInt) :. (Maybe BoolInt, Maybe BoolInt, Maybe BoolInt)) -> UserServer p
    toUserServer ((serverId, host, port, keyHash, auth_, BI preset, tested, BI enabled) :. (rStorage, rProxy, rNames)) =
      let server = ProtoServerWithAuth (ProtocolServer p host port keyHash) (BasicAuth . encodeUtf8 <$> auth_)
          roles = ServerRoles <$> (unBI <$> rStorage) <*> (unBI <$> rProxy) <*> (unBI <$> rNames)
       in UserServer {serverId, server, preset, tested = unBI <$> tested, enabled, roles, deleted = False}
```

`ServerRoles <$> mA <*> mB <*> mC :: Maybe ServerRoles` (Applicative on `Maybe`) yields `Just` only when all three are present, else `Nothing` — matching the all-or-none storage. Constructor arg order (`storage, proxy, names`) matches the column order. Ensure `:.` (from the DB simple package) and `ServerRoles(..)` are imported in `Profiles.hs`.

- [ ] **Step 2: Write roles in `insertProtocolServer`.** Add the columns and values (extract `roles` via record pattern):

```haskell
insertProtocolServer db p User {userId} ts srv@UserServer {server, preset, tested, enabled, roles} = do
  DB.execute
    db
    [sql|
      INSERT INTO protocol_servers
        (protocol, host, port, key_hash, basic_auth, preset, tested, enabled,
         role_storage, role_proxy, role_names, user_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
    |]
    (serverColumns p server :. (BI preset, BI <$> tested, BI enabled) :. roleColumns roles :. (userId, ts, ts))
```

- [ ] **Step 3: Write roles in `updateProtocolServer`** similarly:

```haskell
updateProtocolServer db p ts UserServer {serverId, server, preset, tested, enabled, roles} =
  DB.execute
    db
    [sql|
      UPDATE protocol_servers
      SET protocol = ?, host = ?, port = ?, key_hash = ?, basic_auth = ?,
          preset = ?, tested = ?, enabled = ?,
          role_storage = ?, role_proxy = ?, role_names = ?, updated_at = ?
      WHERE smp_server_id = ?
    |]
    (serverColumns p server :. (BI preset, BI <$> tested, BI enabled) :. roleColumns roles :. (ts, serverId))
```

- [ ] **Step 4: Add the `roleColumns` helper** (private, in `Profiles.hs`). Map each selector over the `Maybe` — no two-branch `case`, so the all-or-none property is structural (a `Nothing` maps to three `Nothing`s automatically):

```haskell
roleColumns :: Maybe ServerRoles -> (Maybe BoolInt, Maybe BoolInt, Maybe BoolInt)
roleColumns mr = (BI . storage <$> mr, BI . proxy <$> mr, BI . names <$> mr)
```

(`storage`/`proxy`/`names` are unambiguous selectors — unique to `ServerRoles`. If a future duplicate makes them ambiguous under `DuplicateRecordFields`, fall back to a punned `\case`.)

- [ ] **Step 5: Build.** `cabal build lib:simplex-chat --ghc-options -O0` → PASS.
- [ ] **Step 6: Commit** `feat(servers): persist per-server roles`

### Task 4: Update validation for the custom (self-hosted) bucket

**Files:**
- Modify: `src/Simplex/Chat/Operators.hs:524-532` (`noServersErrs`, `hasRole`), `:564-569` (`noNamesServersWarns`, `namesEnabled`)

This task **fuses two parallel role-coverage helpers into one.** `hasRole` (`:531`, storage/proxy) and `namesEnabled` (`:569`, names) are the same group-level check specialised to different role selectors, both returning `True` for the custom bucket. They cannot express per-server roles, so both are replaced by a single role-parameterised `hasRoleCoverage`.

**Preserve** (do not touch): `noServers` + `srvEnabled` (still used for the `USENoServers` empty-check at `:526`) and `opEnabled` (also used by `noChatRelaysWarns`, `:560`). **Remove:** `hasRole` and `namesEnabled`.

- [ ] **Step 1: Make coverage checks per-server-role-aware for the no-operator bucket.** Today `hasRole`/`namesEnabled` return `True` for the custom bucket (`operator' = Nothing`) and are group-level `u -> Bool` filters combined with `noServers cond = not $ any srvEnabled $ userServers p $ filter cond uss`. Group-level filtering **cannot** express per-server roles for the custom bucket — the evaluation must move to the per-server level.

**Effective-role helper** — takes the protocol singleton (`operatorRoles` needs it; `noServersErrs` runs for BOTH SMP and XFTP at `:522-523`, so hardcoding `SPSMP` would read `smpRoles` for XFTP operator servers — a bug):

```haskell
-- effective role for coverage: operator servers use operator roles (per protocol),
-- self-hosted servers use per-server roles (default when unset).
serverHasRole :: UserProtocol p => SProtocolType p -> (ServerRoles -> Bool) -> Maybe ServerOperator -> Maybe ServerRoles -> Bool
serverHasRole p roleSel op srvRoles = case op of
  Just o@ServerOperator {enabled} -> enabled && roleSel (operatorRoles p o)
  Nothing -> roleSel (fromMaybe defaultUserServerRoles srvRoles)
```

**Restructure the checks to per-server evaluation.** For a role selector, "coverage exists" = any enabled, non-deleted server in any bucket whose effective role is on. Per bucket `u` the operator is `operator' u` and each server's roles come from `AUS _ UserServer{enabled, deleted, roles}` (the `roles` field is reachable — same pattern already used at `:532,:568`):

```haskell
    hasRoleCoverage :: (UserServersClass u, ProtocolTypeI p, UserProtocol p) => SProtocolType p -> (ServerRoles -> Bool) -> [u] -> Bool
    hasRoleCoverage p roleSel =
      any (\u -> any (srvOk (operator' u)) (map aUserServer' (servers' p u)))
      where
        srvOk op (AUS _ UserServer {enabled, deleted, roles}) =
          enabled && not deleted && serverHasRole p roleSel op roles
```

Then rewrite `noServersErrs` (`:527`) storage/proxy branches as `[USEStorageMissing p' user | not (hasRoleCoverage p storage uss)] <> [USEProxyMissing p' user | not (hasRoleCoverage p proxy uss)]` (keep the `noServers opEnabled` empty-check at `:526` as-is), and `noNamesServersWarns` (`:565`) as `[USWNoNamesServers user | not (hasRoleCoverage SPSMP names uss)]`. Keep the exact constructors (`USEStorageMissing`, `USEProxyMissing`, `USWNoNamesServers`). XFTP `names` is never checked (names coverage is SMP-only).

**Deliberate duplication (do not try to unify with `agentServerCfgs`).** Both this helper and `agentServerCfgs` (Task 1) resolve "operator-vs-self-hosted → effective roles", but their semantics for a *disabled operator* differ: `agentServerCfgs` drops the server entirely (`opEnabled … | otherwise -> Nothing`), whereas coverage treats it as contributing no roles while `USENoServers` handles existence. They also run over different shapes (`[ServerCfg]` per user vs. `[UserOperatorServers]` across users). Unifying them would grow the blast radius for no clarity gain; the shared logic is a 2-line `case` — leave it in both. (Flag for future: if a third consumer appears, extract `effectiveServerRoles :: SProtocolType p -> Maybe ServerOperator -> Maybe ServerRoles -> Maybe ServerRoles`.)

- [ ] **Step 2: Build + reason through each call site.** Ensure `noServers`/`noNamesServers` now consult per-server roles for the custom bucket. `cabal build lib:simplex-chat --ghc-options -O0` → PASS.
- [ ] **Step 3: Commit** `fix(servers): validate coverage using per-server roles`

### Task 5: Haskell tests

**Files:**
- Modify/Create: server-config test module (locate the existing `agentServerCfgs` / operators test; e.g. under `tests/` — search `agentServerCfgs`, `validateUserServers`, `Operators`). If none, add `tests/ChatTests/ServerRolesTest.hs` and register it in the test runner.

- [ ] **Step 1: Write failing tests.**
  - `agentServerCfgs` for a self-hosted SMP server with `roles = Just (ServerRoles True False True)` → `ServerCfg.roles == ServerRoles True False True`, `operator == Nothing`.
  - self-hosted with `roles = Nothing` → `ServerCfg.roles == defaultUserServerRoles` (`names == False`).
  - operator-matched server ignores per-server `roles` → uses `operatorRoles`.
  - store round-trip: insert a self-hosted server with `Just` roles, read back equal; insert with `Nothing`, read back `Nothing`.
  - `validateUserServers`: custom-bucket-only user with all servers `names = False` → `USWNoNamesServers` present; enabling `names` on one → absent.
- [ ] **Step 2: Run, expect FAIL.** `cabal test --test-option=--match="/ServerRoles/"`
- [ ] **Step 3: (Implementation already done in Tasks 1–4.)** Run, expect PASS.
- [ ] **Step 4: Commit** `test(servers): cover per-server roles resolution and storage`

---

## Chunk 2: UI (Kotlin + Swift)

### Task 6: Kotlin (Desktop/Android) model + toggles

**Files:**
- Modify: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt:4787` (`UserServer`), `:4808,4818-4845` (sample/empty), and `.../networkAndServers/ScanProtocolServer.kt:22`
- Modify: `.../networkAndServers/ProtocolServerView.kt` (`UseServerSection` / `CustomServer`, ~`:161-234`) and `.../networkAndServers/NewServerView.kt`
- Reference: `.../networkAndServers/OperatorView.kt:260-320` (toggle pattern), `ServerRoles` (`SimpleXAPI.kt:4636`)

- [ ] **Step 1: Add `roles` to `UserServer`** with default to avoid breaking constructors:

```kotlin
val roles: ServerRoles? = null,
```
Place it before `deleted`; update the primary constructor and the named-arg call sites (`empty` `:4808`, samples `:4818-4845`, `ScanProtocolServer.kt:22`). Since it has a default, unchanged call sites still compile.

- [ ] **Step 2: Add the toggles to custom SMP servers.** In `ProtocolServerView.kt`, inside `CustomServer` / `UseServerSection`, when `server.protocol == smp && !server.preset`, add a `SectionView(MR.strings.operator_use_for_messages)` with three `PreferenceToggle` rows bound to the server's roles (receive → `storage`, private routing → `proxy`, names → `names`), mirroring `OperatorView.kt:261-320`. Editing writes back a new `ServerRoles` onto the edited `UserServer` state. Default the displayed roles to `ServerRoles(storage = true, proxy = true, names = false)` when `roles == null`.
  - Strings: `operator_use_for_messages`, `operator_use_for_messages_receiving`, `operator_use_for_messages_private_routing`, `operator_use_for_names` (already exist, `strings.xml:2195-2198`).
  - Do NOT show for XFTP or preset/operator servers.

- [ ] **Step 3: New-server default.** In `NewServerView.kt`, construct the new SMP `UserServer` with `roles = ServerRoles(storage = true, proxy = true, names = false)` so the toggles show the default and persist on save.

- [ ] **Step 4: Build.** Compile the multiplatform common module (repo's gradle/build command).
- [ ] **Step 5: Commit** `feat(servers): per-server role toggles on Android/desktop`

### Task 7: Swift (iOS) model + toggles

**Files:**
- Modify: `apps/ios/Shared/Model/AppAPITypes.swift:1953` (`UserServer` struct + `CodingKeys` `:2017`)
- Modify: `apps/ios/Shared/Views/UserSettings/NetworkAndServers/ProtocolServerView.swift` (`customServer()` / `useServerSection` `:88,123`) and `NewServerView.swift`
- Reference: `.../NetworkAndServers/OperatorView.swift:106-114` (toggle pattern), `ServerRoles` (`AppAPITypes.swift:1807`)

- [ ] **Step 1: Add `roles` to `UserServer`.**

```swift
var roles: ServerRoles?
```
Add `case roles` to `CodingKeys` (`:2017`). Update initializers / sample data accordingly.

- [ ] **Step 2: Add the toggles to custom SMP servers.** In `ProtocolServerView.swift`, inside `customServer()` / `useServerSection`, when the server is SMP and `!preset`, add a `Section("Use for messages")` with three `Toggle`s bound to the server's roles (`"To receive"` → `storage`, `"For private routing"` → `proxy`, `"To resolve names"` → `names`), mirroring `OperatorView.swift:106-114`. Bind to the edited server's `roles`, defaulting to `ServerRoles(storage: true, proxy: true, names: false)` when `nil`.
  - iOS string keys are the literal English text (already used by `OperatorView.swift`); `"To resolve names"` may still need adding to `Localizable.strings` translations (English == key).
  - Do NOT show for XFTP or preset/operator servers.

- [ ] **Step 3: New-server default.** In `NewServerView.swift`, default the new SMP server's `roles` to `ServerRoles(storage: true, proxy: true, names: false)`.

- [ ] **Step 4: Build** the iOS target (or type-check the changed files).
- [ ] **Step 5: Commit** `feat(servers): per-server role toggles on iOS`

---

## Verification (whole feature)

- [ ] `cabal test --test-option=--match="/ServerRoles/"` passes.
- [ ] Full backend build: `cabal build --ghc-options -O0`.
- [ ] Migration up/down: apply and roll back `20260714_server_roles` on a copy DB (SQLite + Postgres); confirm existing rows read as `roles = Nothing` → resolved to `defaultUserServerRoles`.
- [ ] Manual UI: add two self-hosted SMP servers, set different role toggles on each, save, reopen — toggles are independent and persisted; name resolution defaults off; toggles absent on XFTP and operator servers.
- [ ] Round-trip via API: `APIGetUserServers` returns `roles` for edited servers and omits it (Nothing) for untouched ones.
