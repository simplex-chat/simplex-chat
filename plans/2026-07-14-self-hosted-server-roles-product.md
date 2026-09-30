# Per-server roles for self-hosted servers — Product / Design

**Date:** 2026-07-14
**Status:** Design (approved for planning)
**Scope:** simplex-chat backend + DB + Desktop/Android (Kotlin) + iOS (Swift). **No simplexmq changes.**

## Problem

Self-hosted SMP servers cannot be configured individually for what they are used
for. The three capabilities — **receiving** messages (`storage` role), **private
routing** (`proxy` role), and **name resolution** (`names` role) — are controlled
only per *operator* (SimpleX / Flux), via the operator toggles in `OperatorView`.
Servers the user adds themselves fall into the "custom" / no-operator bucket and
are hard-coded to `allRoles` (all three on) in `agentServerCfgs`
(`src/Simplex/Chat/Operators.hs:448`). There is no UI, storage, or type to give a
single self-hosted server a subset of roles.

This is a problem because a self-hosted server is usually not a name resolver
(name resolution needs an Ethereum/SNRC endpoint via `[NAMES] enable: on` on the
smp-server). Today such a server is still offered to the agent as names-capable,
so a name lookup can be routed to it and fail. Users also may want a self-hosted
server used only for receiving, or only for private routing.

## Goal

Give **each individual added self-hosted SMP server** its own independent toggles:
**To receive** / **For private routing** / **To resolve names**, stored per server
and plumbed to the agent as that server's `ServerRoles`.

## Current state (verified)

- **Agent (simplexmq) — already complete.** `ServerCfg { server, operator :: Maybe
  OperatorId, enabled, roles :: ServerRoles }` and `ServerRoles { storage, proxy,
  names }` exist (`Simplex/Messaging/Agent/Env/SQLite.hs:98,106`). The agent
  partitions servers into `storageSrvs / proxySrvs / nameSrvs` in `mkUserServers`
  and consumes them (`getNextServer`, `getSMPProxyClient`, `getNextNameServer` /
  `resolveName` / RSLV). The agent always receives **concrete** `ServerRoles` and
  never applies defaults.
- **simplex-chat backend.** `UserServer'` (`Operators.hs:242`) has **no roles
  field**. Roles are resolved only in `agentServerCfgs` (`Operators.hs:438-448`):
  operator servers → `operatorRoles p op` (read from `server_operators` columns);
  self-hosted servers → hard-coded `allRoles`.
- **DB.** `protocol_servers` (`chat_schema.sql:548`) stores all user servers with
  only `enabled / preset / tested` — **no operator link and no role columns**.
  Roles live only per-operator in `server_operators`
  (`smp_role_storage/proxy/names`, `xftp_role_storage/proxy`).
- **UI.** Operator screens (`OperatorView.kt:260-320`, `OperatorView.swift:106-114`)
  have the three toggles. The individual server screens
  (`ProtocolServerView`, `NewServerView` on both platforms) show only
  address / test / enabled / delete. Client `UserServer`
  (`SimpleXAPI.kt:4787`, `AppAPITypes.swift:1953`) has no roles field.

## Decisions

1. **Where the logic lives — no simplexmq change.** The accurate, consistent
   approach follows the existing layering: the agent always gets a concrete
   `ServerRoles`; the only place roles are resolved/defaulted is simplex-chat's
   `agentServerCfgs`. We keep the `Maybe ServerRoles → ServerRoles` resolution
   there, alongside the existing `allRoles` / `operatorRoles` handling. No core
   version bump, no wire/protocol change.
2. **SMP self-hosted only.** SMP has all three roles; XFTP has no `names` role.
   The toggles are shown only for custom (non-preset) SMP servers. XFTP
   self-hosted servers are functionally unchanged.
3. **Legacy default `NULL → names OFF`.** For rows without stored roles (existing
   servers after upgrade, or servers not yet edited) the resolved value is
   `defaultUserServerRoles = ServerRoles { storage = True, proxy = True, names =
   False }`. This is also the default for newly added servers, so migrated and new
   servers behave identically: receive + private routing on, name resolution off.

## Design

### Types (`Operators.hs`)

- Add `roles :: Maybe ServerRoles` to `UserServer'` (per-server; `Nothing` = use
  default). `DuplicateRecordFields` is already enabled (both `ServerCfg` and
  `UserServer'` will have a `roles` field).
- Add `defaultUserServerRoles :: ServerRoles = ServerRoles True True False`.
- `agentServerCfgs`, self-hosted branch (`:448`):
  `roles = fromMaybe defaultUserServerRoles (roles srv)`. Operator branch
  unchanged — the per-server field is ignored for operator-matched servers, so
  operator roles still win.

Per-server, not global: `roles` is one value per `UserServer`; each server
resolves to its own `ServerCfg`.

### Storage (`protocol_servers`)

Three **nullable** columns (mirrors `server_operators`): `role_storage`,
`role_proxy`, `role_names` (INTEGER, no default → existing rows NULL). Read into
`Maybe ServerRoles` (all three present → `Just`, else `Nothing`); write all three
from `roles`.

### chat↔UI API

`UserServer'` JSON is derived with `defaultJSON` (`omitNothingFields = True`), so
the new `Maybe` `roles` field is omitted when `Nothing` — exactly like the
existing `tested :: Maybe Bool`. Older clients/servers remain compatible.

### Validation (`Operators.hs:519-577`)

`noNamesServersWarns` (`:564-569`) and `noServersErrs`'s `hasRole` (`:531`)
currently derive coverage from the *operator* and treat the no-operator (custom)
bucket as `True` for every role. With per-server roles and names-off default this
would be wrong (a self-hosted-only user would never see "no name servers"). These
must evaluate the custom bucket from each server's resolved per-server `roles`.

### UI (Kotlin + Swift)

- Add `roles: ServerRoles?` to the client `UserServer`.
- On custom (non-preset) **SMP** server screens only — `ProtocolServerView` and
  `NewServerView`, both platforms — add a "Use for messages" section with the
  three toggles, mirroring `OperatorView`. Reuse existing string keys:
  `operator_use_for_messages`, `operator_use_for_messages_receiving`,
  `operator_use_for_messages_private_routing`, `operator_use_for_names`.
- New server default: `storage = on, proxy = on, names = off`.

## Behavior / backward compatibility

- Existing self-hosted servers keep receive + private routing; name resolution is
  off after upgrade (intentional — avoids routing lookups to non-resolver
  servers). A user who wants a self-hosted resolver enables the toggle.
- No protocol / wire change; no simplexmq change; operator behavior unchanged.

## Out of scope

- Per-server roles for operator (SimpleX/Flux) servers — they keep operator-level
  roles.
- XFTP per-server roles.
- Any change to the name-resolution protocol or the agent.

## Testing strategy

- Haskell: unit tests for `agentServerCfgs` (self-hosted `Just`/`Nothing` →
  correct `ServerCfg.roles`; operator servers ignore per-server roles), store
  round-trip of `roles` through `protocol_servers`, and `validateUserServers`
  names/storage/proxy coverage for the custom bucket.
- UI: manual verification that toggles render only for custom SMP servers, persist
  per server, default names-off, and survive save/reload.
