# Name lookup review fixes

Repository: simplex-chat/simplex-chat.
Branch: `ep/name-lookup-review-fixes`.
Base: `ep/name-alert-fixtures-2`, on master `88767bbc3` (#8220).

## Findings

| Finding | Change |
|---|---|
| A5-2 | mcp: verified names only, name warning texts, a lapsed name refused, a moved name reported |
| A5-4 | `ToField DomainVerification`; SQL compares with parameters; `NameVerified` |
| A5-5 | name warnings as values in the connection plan |
| A5-7 | `NCLapsed` includes the name's domain |
| A5-8 | one name lookup; an expired name refused everywhere |
| A5-9, A5-14, A5-15, A5-16 | the store writes of a verified chat mark the other chats moved, under a lock per user and name |
| A5-10 | expiry compared with the registry's block time |
| A5-18 | one function for the name claimed by a group profile |
| A5-19 | the plan's name domain and the moved known chat bound once |
| A5-22 | a known business chat's name written only when changed |
| A5-24 | user guide |
| A5-26 | one check of the badge service address |
| 326cff68c | cherry-picked: a name warning matched in the same case |

Not changed: A5-1, A5-3, A5-6, A5-20, A5-21, A5-23, A5-25 (master has `BadgeServiceErrorCode` in all languages).
Not in this pull request: A5-13.

## Name lookup

`resolveName user nm domain :: CM (Either NameWarning NameRecord)`
1. `resolveSimplexName` of the agent: `NameResponse`.
2. `nameRecordOrWarning now domain response`.

`nameRecordOrWarning`
- time: `fromMaybe now lastBlockTs`
- expired: `expires` before the time
- grace date: shown when not before the time

`resolveNameRecord user nm domain :: CM NameRecord`
- `resolveName`, a warning thrown as `CESimplexDomainNotReady domain (SDENameWarning w)`
- callers: setting the user's name, setting a channel's name, a service request to a name

The core creates no `NAME NOT_FOUND` error.

Verification of a contact's or channel's name:

| Answer | State | Reason |
|---|---|---|
| record, the chat's link, for a contact its proof | verified | — |
| record, another link | failed | the name does not resolve to ... |
| expired | failed | the name expired |
| available | failed | the name is not registered |
| reserved | failed | the name is not registered |
| not registered | failed | the name is not registered |
| `NAME NOT_FOUND` of a router before v22, channel | failed | the name is not registered |

## Connection plan

`connectPlan`, the name resolved before for a bare name:
- type: `Maybe (Either ChatError (Either NameWarning NameRecord))`
- value: `tryAllErrors (resolveName user nm d)`

`resolveNameLink ni :: CM (Either NameWarning ShortLinkContact)`
- the pre-resolved name, or `resolveName`
- the first link of the name's kind, or the error `SDENoValidLink`

`resolveSLink`: the warning of `resolveNameLink` thrown as `SDENameWarning`.

`knownNamePlan`, a chat known for the name:

| `resolveNameLink` | Plan |
|---|---|
| the chat's link | the known chat |
| another link | that link's plan, `NCMoved` |
| `NWNotRegistered` | the known chat |
| another warning | the known chat, `NCLapsed nameDomain w` |
| error | as before |

```haskell
data NameChange
  = NCLapsed {nameDomain :: SimplexDomain, nameWarning :: NameWarning}
  | NCMoved {knownChat :: AChatInfo}
```

Read from `NCLapsed`:
- View.hs: the warning line
- iOS `planNameWarningAlert(plan)`
- Kotlin `planNameWarningAlert(plan)`

```mermaid
flowchart LR
  R["resolveName"] --> W["nameRecordOrWarning"]
  W --> RR["resolveNameRecord<br>warning thrown"]
  W --> V["verification<br>warning: failed"]
  W --> NL["resolveNameLink"]
  NL --> S["resolveSLink<br>warning thrown"]
  NL --> K["knownNamePlan<br>warning: NCLapsed"]
```

## Verification state

`DomainVerification` in the database:

| Value | State |
|---|---|
| 0 | failed |
| 1 | verified |
| 2 | moved |

- `ToField DomainVerification` beside `FromField`.
- SQL compares `*_domain_verified` with `DVVerified` and `DVMoved` as parameters.

Store.Direct:
```haskell
type NameVerified = Bool

type MovedChats = ([ContactId], [GroupId])
```

`setOtherNameChatsMoved db user chat :: IO MovedChats`, in Store.Direct, for a verified chat:

| Chat | Name kind | Moved |
|---|---|---|
| contact | contact | other contacts, business chats |
| business chat | contact | contacts, other business chats |
| channel | channel | other non-business groups |
| other or not verified | — | none |

- contacts: one SELECT of the ids, one UPDATE
- groups: one UPDATE ... RETURNING

Store functions that call `setOtherNameChatsMoved`, with `MovedChats` in the result:
- `setContactDomainVerified`
- `setGroupDomainVerified`, and through it `setPreparedGroupDomain`, `createPreparedGroup`
- `createPreparedContact`, through `setContactDomainVerified`
- `updatePreparedContactUser`
- `updatePreparedGroupUser`

`withNameLock user domain_ action :: CM (a, MovedChats)`, in Library.Internal:
- lock: `CLSimplexName userId (fullDomainName domain)` of `ChatLockEntity`, for a domain
- `CEvtNameMoved` for non-empty `MovedChats`

Callers:

| Command or function | User | Domain |
|---|---|---|
| APIPrepareContact | user | the verified domain |
| APIPrepareGroup | user | the verified domain |
| APIChangePreparedContactUser | new user | the contact's claim |
| APIChangePreparedGroupUser | new user | the group's claim |
| APIVerifyContactDomain | user | the claim |
| APIVerifyGroupDomain | user | the claim |
| runUpdateGroupProfile | user | the new profile's claim |
| contactLinkPlan, a known business chat | user | the plan's domain |
| updateGroupFromLinkData | user | the resolved domain |
| updateContactFromLinkData | user | the link profile's claim |

```mermaid
sequenceDiagram
  participant C as caller
  participant L as withNameLock
  participant S as store transaction
  C->>L: user, domain, action
  L->>L: lock CLSimplexName user domain
  L->>S: action
  S->>S: verification state written
  S->>S: setOtherNameChatsMoved
  S-->>L: chat, MovedChats
  L->>L: CEvtNameMoved, when MovedChats is not empty
  L-->>C: chat, MovedChats
```

## Other changes

- `groupClaim :: GroupProfile -> Maybe SimplexDomain`, in Simplex.Chat.Types, in place of:
  - `groupClaim` of Library.Internal
  - the local `groupClaim` of `updateGroupProfile`
  - the body of View `groupSimplexDomain`
  - the domain of a group plan in `groupLinkPlan`
  - the domain of Directory `verifiedGroupDomain`
- `planDomain` and `knownMoved` in the `where` of `CTShortContact`.
- A known business chat of a contact plan: its domain and verification written when they differ from the plan's domain and `DVVerified`.
- `requestBadgeIssue`: `requireBadgeService`.
- docs/guide/register-simplex-name.md: the moved indicator, the name warning alerts.

## MCP server

packages/simplex-chat-mcp, built and tested with the repository's types (scope `mcp` of scripts/ci):

| Function | Change |
|---|---|
| `simplexNameOf` | a name for `DomainVerification.Verified` only |
| `chatErrorTypeText` | `nameWarning`: `nameWarningText` |
| `nameWarningText` | the apps' words of the alerts for a chat not known |
| `classifyPlan` | `lapsed`: a tool error with `nameWarningText` |
| `classifyPlan` | `moved`: `movedFrom`, the reference of the known chat |
| `planFields` | `moved_from` in the result of `connect` |

## Tests

- `testNameRecordOrWarning`: the block time before and after `expires`.
- `testChannelDomainVerify`: an expired name not verified.
- `testSetNameExpired`: an expired name not set.
- Fixtures of a lapsed name: `nameDomain` in `nameChange`.
- Kotlin and iOS alert tests: `planNameWarningAlert(plan)`.
- mcp: name warning texts, a lapsed name refused, `movedFrom`, `simplexName` of a verified chat only.
