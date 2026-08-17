# Remove left padding on consecutive (no-avatar) received messages in channels

## Problem

In a channel, received messages show the sender avatar on the first message of a
run and hide it on consecutive messages, but those consecutive messages still
reserve the avatar-sized **left padding** so they line up under the first. For a
channel's feed-style layout this indentation wastes horizontal space —
consecutive received messages should sit flush-left where the avatar would be.
This applies to **both** the channel owner's broadcasts and contributors' posts.

Desired behaviour: in channels, any received message that does **not** show an
avatar (a consecutive post from the same sender) drops the avatar-sized left
padding. The first message of a run still shows the avatar and keeps its layout;
when the run is broken (a different sender, or a time gap), the next message
shows the avatar again — this run logic is unchanged, only the no-avatar left
padding is reduced.

## The two received directions in a channel

Received items in a channel arrive as one of two directions
(`Subscriber.hs`, `saveRcvCI`):

- **`ChannelRcv`** (no member) — the **owner's** broadcast, sent "as the channel".
  The backend permits sending-as-group only to the owner, and a channel owner's
  main-scope messages are always sent as group (`ChatInfo.sendAsGroup` is true
  for `useRelays && memberRole >= Owner` in the main scope), so received owner
  posts arrive as `ChannelRcv`. Shows the channel avatar.
- **`GroupRcv(member)`** (attributed) — a **contributor's** post, carrying the
  member. Shows the member avatar.

Both are received messages, and the change now applies to **both** when they hide
the avatar. (An earlier revision scoped this to `ChannelRcv`/owner only; it now
covers contributors too, per request.)

## Change

In channels — gated on `ChatInfo.isChannel` (the `useRelays` flag, which is
reliably present, unlike the optional group-type predicate) — the no-avatar
branches for **both** `ChannelRcv` and `GroupRcv` drop the avatar-sized left
padding down to the base inset where the avatar itself starts. In non-channel
groups the `GroupRcv` no-avatar layout is unchanged (`isChannel` is false). The
avatar-shown layouts, sent messages, and all other chats are unchanged.

The same Row's `end` padding is already gated on `chatInfo.isChannel` (the merged
right-gap change #7106), so gating `start` on `isChannel` keeps each Row
internally consistent and the change precisely "in channels".

### Android / desktop (`apps/multiplatform`, `ChatView.kt`, `ChatItemsList`)

Both the `CIDirection.GroupRcv` and `CIDirection.ChannelRcv` `showAvatar == false`
rows:

```kotlin
// before
.padding(start = 8.dp + (MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier) + 4.dp, end = …)
// after
.padding(start = if (chatInfo.isChannel) 8.dp else 8.dp + (MEMBER_IMAGE_SIZE * fontSizeSqrtMultiplier) + 4.dp, end = …)
```

### iOS (`apps/ios`, `ChatView.swift`, `chatItemListView`)

Both the `.groupRcv` and `.channelRcv` no-avatar branches:

```swift
// before
.padding(.leading, 10 + memberImageSize + 12)
// after
.padding(.leading, chat.chatInfo.isChannel ? 12 : 10 + memberImageSize + 12)
```

## Run behaviour (unchanged)

`shouldShowAvatar(current, older)` shows the avatar on the first message of a
same-sender run and hides it on consecutive ones; a different sender or a gap
resets the run. For `GroupRcv` "same sender" is the same `memberId`; for
`ChannelRcv` consecutive channel broadcasts count as the same sender. Only the
no-avatar left padding is changed.

## Scope

- Affects: all received consecutive (no-avatar) messages **in channels** — owner
  broadcasts (`ChannelRcv`) and contributor posts (`GroupRcv`). This includes a
  channel's member-support sub-scope, which renders through the same
  `ChatItemsList` with the channel's `isChannel`; treating it the same way is
  consistent with the merged right-gap change (#7106), which also gates that
  Row's `end` padding on `isChannel` without a scope filter.
- Unchanged: the first message of each run (avatar shown), sent messages, regular
  groups, business chats and direct chats (`isChannel` false — the `else` branch
  preserves the original avatar-inset value exactly), and any non-channel
  `ChannelRcv` welcome item.

## Verification

- Android/desktop: `:common:compileKotlinDesktop` compiles clean.
- iOS: small, type-safe constant change; build/verify on macOS (Xcode) — not
  compilable on the Linux build host used here.
- Visual (both platforms), in a channel:
  - First message of a run (owner or contributor): avatar shown, layout unchanged.
  - Following messages from the same sender (no avatar): now flush-left.
  - A different sender / time gap resets the run — the next message shows the
    avatar again.
  - Regular groups, business and direct chats keep their existing indentation.
