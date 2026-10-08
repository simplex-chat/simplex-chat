# SimpleX name lookup — the core API

The core half of #7525. Model: `plans/sketches/2026-09-18-names-lookup-flows.excalidraw`, states 1a–4d. Registry types: simplexmq `src/Simplex/Messaging/Names/Record.hs`.

The types, scenarios, plan logic, CLI, app behaviour and decisions are in `plans/2026-09-28-name-warnings.md`.

## Table of contents

1. API
2. Local lookups
3. Resolve modes
4. Tests

## 1. API

```haskell
data ChatResponse
  = ...
  | CRConnectionPlan
      { user :: User,
        connLink :: ACreatedConnLink,
        planSimplexName :: Maybe SimplexNameInfo,
        otherSimplexName :: Maybe SimplexNameInfo,
        connectionPlan :: ConnectionPlan
      }

data ChatEvent
  = ...
  | CEvtNameMoved {user :: User, contactIds :: [ContactId], groupIds :: [GroupId]}
```

`ConnectionPlan`, `NameChange`, `NameWarning`, `SimplexDomainError` and `DomainVerification` are in name-warnings §4.

```haskell
connectPlan :: User -> AConnectTarget -> PlanResolveMode -> Maybe LinkOwnerSig
            -> Maybe (Either ChatError NameRecord)
            -> CM (ACreatedConnLink, Maybe SimplexNameInfo, Maybe SimplexNameInfo, ConnectionPlan)
```

A bare name's resolution is passed to each kind's plan, so the name is resolved once.

Registration does not go through `ConnectionPlan`.

## 2. Local lookups

By name, before the comparison with the name's link:
- `getContactToConnect` (`Direct.hs`): `cp.contact_domain`, with `contact_domain_verified = 1`;
- `getGroupToConnect` (`Groups.hs`): `gp.group_domain`, with `group_domain_verified = 1`;
- `getUserContactLinkViaTarget` (`Profiles.hs`): the user's profile claim;
- `getGroupInfoViaUserTarget` (`Groups.hs`): `gp.group_domain`, with `group_domain_verified` other than 2 (moved).

A prepared chat is set verified only when the link's profile claims the name (`APIPrepareContact`, `APIPrepareGroup`).

## 3. Resolve modes

| target | mode | answer |
|---|---|---|
| name | `PRMUnknown`, `PRMAllGroups` | resolved on every lookup, and compared with the local chat |
| name | `PRMNever` | the local chat, or `CENotResolvedLocally` |
| contact or group short link, local | `PRMUnknown`, `PRMNever` | the local plan |
| contact short link, local | `PRMAllGroups` | the local plan |
| group short link of a joined group | `PRMAllGroups` | the group, refreshed from its link data |
| group short link of the own group | `PRMAllGroups` | the local plan |
| contact or group short link, nothing local | `PRMUnknown`, `PRMAllGroups` | the link's plan |
| contact or group short link, nothing local | `PRMNever` | `CENotResolvedLocally` |

The mode is ignored for invitation links. `allGroups` and `on` parse as `PRMAllGroups`.

- `PRMNever`: the apps' name search.
- `PRMUnknown`: a tap on "Connect to …".
- `PRMAllGroups`: the directory service, with links.

`CENotResolvedLocally` is discarded by both apps.

## 4. Tests

A name is re-pointed with `registerName`, or registered as expired with `registerExpiredName`. A registry query is detected by the next plan's output. The other tests are in name-warnings §10.
- the name moved: the new chat after it is prepared (`testPlanKnownNameNewChatOpened`);
- a chat prepared with a name its profile does not claim is left unverified (`testPrepareNameNotClaimed`);
- with `resolve=never`, the answer is taken from the store (`testPlanNameResolveNever`).
