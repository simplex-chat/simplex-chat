# Flexible user records

## Problem

Currently user records work as rigid containers for conversations. New conversations can be created only for an active user. Users want to be able to select a user record for a new conversation right at the connection screen (i.e. when scanning QR code or pasting a link). A similar problem was previously solved regarding Incognito mode - initially it required changing a global setting, then it was moved as a toggle to the connection screen, then it was reworked to be offered after scanning the link.

## Solution

## UI

Connection UI would offer to join connections as other users.

Current options when joining:

```
- "Use current profile"
- "Use new incognito profile"
```

Will change to:

If there're only 2 users:

```
- "Use current profile"
- "Use new incognito profile"
- "Use <user 2 name> profile"
```

If there's more than 2 users:

```
- "Use current profile"
- "Use new incognito profile"
- "Use other profile" (opens sheet with list of users)
```

Things to consider:

- hidden users should be excluded from this selection
- choosing different user should make it active and open chat list for this user, then create pending connection there
- should connection plan api take into account all users?

## Other ideas

### Incognito chats in a separate user "profile"

Having incognito conversations interleaved with "main profile" conversations is another point of confusion, as incognito profile is offered as an alternative to main profile, but conversations are still "attached" to it and inherit some of its settings (e.g. servers). We could unite all incognito chats under a new dummy "incognito" user profile. It would have a special representation in UI, not as a regular user profile, but as an incognito mode. It would allow customizing a specific theme, servers, preferences and other settings for all incognito chats.

``` haskell
-- Types

data User = User
  { ...
    incognitoUser :: Bool,
    ...
  }

-- Controller

APIConnect UserId IncognitoEnabled (Maybe AConnectionRequestUri)
-- -> changed to ->
APIConnect UserId (Maybe AConnectionRequestUri)
-- since conversation being incognito would be defined by user
```

Considerations / problems:

- migration of existing incognito conversations is non trivial, as it requires migrating both agent and chat connection records to a new user record, in addition to migrating other chat entities.
  - some programmatic one-time migration on chat start would be required, e.g.:
    - create new user in agent;
    - create new user in chat with incognito_user set to true;
    - for each (non hidden, see below) user read chat list, update user_id for all chat entities:
      - contacts,
      - groups,
      - group_members,
      - contact_profiles,
      - chat_items, etc.
  - this new user servers would include all servers from users that had incognito conversations (?).
  - this seems quite complex error-prone.
- it may be more pragmatic to not migrate old conversations to new user record, but instead filter them out in their respective user chat lists, and filter them in incognito user profile.
  - in this case "legacy" incognito conversations would be marked by their user record (avatar/name inside chat list; note inside chat view saying that such and such settings are inherited from user x).
  - we could still make a hack to apply same incognito user theme for "legacy" incognito conversations.
- on the other hand the second approach requires loading all chats for the incognito user (this may be related to "All chats view", see below).
- when creating an incognito conversation for a hidden user, it should still be attached to that user.
  - or we could create an "incognito hidden user".
- considering complexities, this all seems quite a rabbit hole and may be not worth it..
- MVP may be to do nothing for legacy incognito contacts and just explain it in app. A-la "New incognito conversations will appear here, previously created incognito conversations will stay attached to user profiles they were created in".

### Forward messages between users

Should be somewhat easy in backend:

``` haskell
APIForwardChatItem {toChatRef :: ChatRef, fromChatRef :: ChatRef, chatItemId :: ChatItemId, ttl :: Maybe Int}
-- -> changed to ->
APIForwardChatItem {toUserId :: UserId, toChatRef :: ChatRef, fromUserId :: UserId, fromChatRef :: ChatRef, chatItemId :: ChatItemId, ttl :: Maybe Int}
-- or include UserId into ChatRef
```

More complex in UI - requires "knowing" conversations for other / all users:
- either have all conversations for all users in model.
- or have other users expand in forward list, and request their chat lists at that point.

### Per user network settings

Requires changes in agent and in backend.

In agent requires storing network settings in UserId to settings maps, similar to servers:

``` haskell
data AgentClient = AgentClient
  { ...
    useNetworkConfig :: TVar (NetworkConfig, NetworkConfig),
    -- -> changed to ->
    useNetworkConfig :: TMap UserId (NetworkConfig, NetworkConfig), -- slow/fast per user
  }
```

Chat APIs:

``` haskell
APISetNetworkConfig NetworkConfig
APIGetNetworkConfig
-- -> changed to ->
APISetNetworkConfig UserId NetworkConfig
APIGetNetworkConfig UserId
```

### All chats in united list

We could add a view where chats for all users could be viewed in a single list / filtered by users.

We could:
- either always load all chats for all users (see Incognito user, Forward between users above) and have a single api, then filter conversations by user in UI
- or modify/duplicate APIGetChats api and queries.
- in any case may require some rework of pagination queries, as indexes might become inefficient.

``` haskell
APIGetChats {userId :: UserId, pendingConnections :: Bool, pagination :: PaginationByTime, query :: ChatListQuery}
-- -> changed to ->
APIGetChats {pendingConnections :: Bool, pagination :: PaginationByTime, query :: ChatListQuery}
-- or
APIGetChats {userId :: Maybe UserId, pendingConnections :: Bool, pagination :: PaginationByTime, query :: ChatListQuery}
-- with Nothing meaning all
-- or
APIGetChats {userIds :: [UserId], pendingConnections :: Bool, pagination :: PaginationByTime, query :: ChatListQuery}
-- to filter for multiple users? or only filter in UI?
```

### Move chats between user profiles

This would further deepen the illusion of user record being a conversation tag rather than a rigid container for conversations.

There are some of the same issues as described in migration of incognito conversation settings.

"Moved" conversation would still be using servers that were configured for the previous user.
Perhaps it makes more sense to implement after automated queue rotation.
