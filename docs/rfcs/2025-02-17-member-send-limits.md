# Member sending limits

## Problem

Rate limiting member sending to prevent abuse in groups.

## Solution

Each member record to have `rateLimit :: Maybe MemberRateLimit`, Nothing by default. Nothing means default rate limit for regular members (`MRLWindow`), and no limit for owners/admins/moderators (`MRLNone`). Default rate limit is defined in configuration, e.g. limit of 15 messages in 60 second window, or could be defined in group profile.

Rate limit can be overridden per member by sending `XGrpMemRestrict` with updated `rateLimit` by member of same role or higher, similar to changing roles. `APIRateLimitMember` allows to enable or disable rate limiting for member, we could also provide more granular control in it (pass `WindowLimit`), but it seems unnecessary complex for UI.

```haskell
data MemberRateLimit
  = MRLNone -- default for owners, admins, moderators
  | MRLWindow WindowLimit
  deriving (Eq, Show)

data WindowLimit = WindowLimit
  { window :: Int, -- seconds
    limit :: Int
  }
  deriving (Eq, Show)

-- sent in XGrpMemRestrict
data MemberRestrictions = MemberRestrictions
  { restriction :: MemberRestrictionStatus,
    rateLimit :: Maybe MemberRateLimit -- Nothing means use default
  }
  deriving (Eq, Show)

-- new api in ChatCommand
| APIRateLimitMember GroupId GroupMemberId

-- new response in ChatResponse
| CRMemberRateLimit {user :: User, groupInfo :: GroupInfo, member :: GroupMember}

-- new field in GroupMember
data GroupMember = GroupMember {
  ...
  rateLimit :: Maybe MemberRateLimit,
  ...
}
```

Rate limit overrides to be persisted on group member records.

```sql
ALTER TABLE group_members ADD COLUMN rate_limit TEXT; -- MemberRateLimit JSON encoded
```

Limits can be tracked inside fixed windows both for receiving and sending.

```haskell
data ChatController = ChatController {
  ...
  memberLimits :: TMap (GroupId, MemberId) (TVar MemberRateLimitWindow),
  ownLimits :: TMap GroupId (TVar MemberRateLimitWindow),
  ...
}

data MemberRateLimitWindow = MemberRateLimitWindow {
  startedAt :: UTCTime,
  windowLimit :: WindowLimit,
  messages :: Int
}
```

Client to track limit for each writing member in state - `memberLimits`. If current window's interval has passed, checked against `startedAt` of `MemberRateLimitWindow`, reset `messages` counter.

Track own limits per group - `ownLimits`. When limit in group is reached, `CRGroupSendingLimited blocked = True` event is sent to UI to block sending in group. Unblock group sending in UI by scheduling background process to send `CRGroupSendingLimited blocked = False` after interval?

```haskell
-- new event in ChatResponse
| CRGroupSendingLimited {user :: User, groupInfo :: GroupInfo, blocked :: Bool}
```

### Receiving messages from limited members

Receiving message from member that exceeded their limit would fail it as prohibited. We can limit content messages, updates, reactions, etc. Practically only regular members would be limited, so there's no need for limiting service messages. Should we limit deletes?

Problems:

- Inconsistencies in window tracking on sending and receiving sides -> track based on item_ts?;
- Subscription produces message surges;
- Server downtime (or network outage) leads to accumulation of scheduled messages on sending side -> item_ts tracking wouldn't help. Issue is similar to subscription, where many messages can be received in short span legitimately.
- This approach doesn't reduce load of retrieving message and of all machinery passing from agent to chat.

Subscription issue could be solved by not tracking limits during subscription (client "knows" when it has subscribed) and for some time after it. For how long - 30 seconds / 1 minute? - arbitrary, longer absence periods lead to more not yet retrieved messages, freed quotas resulting in more sent pending messages.

Better solution would be to not drop (prohibit) messages at all, but stop reception per connection for periods of time. Possible approaches:

- Don't send (delay) ACK - bad idea as it would lead to repeated processing on client restart, and other possible failures in delivery.
- ACK with parameter `wait` for server - server would wait before sending next message.
- Unsubscribe (kill client?)/resubscribe after some time - more expensive.
- Signal agent to hold on next message - similar to delayed ACK but at least we don't process message. (for example, also via ACK but parameter to wait is for agent)
- Rework chat-to-agent communication to communicate via per connection queue (currently it's single `subQ`) - complex - but we get less expensive "unsubscribe"? Essentially agent still holds on to message like in previous approach.
