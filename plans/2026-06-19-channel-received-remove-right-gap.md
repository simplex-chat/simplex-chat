# Remove the right gap on received messages in channels

## Problem

In groups, received messages are laid out as left-aligned chat bubbles whose
maximum width is capped well short of the right edge, leaving a large empty gap
on the right so long content wraps early. In channels this wastes horizontal
space — channel posts are broadcast/feed-style content that reads better using
nearly the full row width.

## Change

For channels only, received messages drop the right-side gap so content can use
nearly the full row width (a small edge margin remains). This only changes the
maximum available width: long text uses more of the row, short messages still
size to content, and media stays within its existing cap. Sent messages keep
their existing layout.

### Android / desktop (`apps/multiplatform`)

The `end` padding becomes `12.dp` (the same edge margin sent messages use)
instead of `adjustTailPaddingOffset(66.dp, …)`, at the four received-message
layout sites in `ChatItemsList` (`ChatView.kt`): the `GroupRcv`
(member-attributed) and `ChannelRcv` (unattributed) branches, each with and
without an avatar.

```kotlin
end = if (voiceWithTransparentBack || chatInfo.isChannel) 12.dp
      else adjustTailPaddingOffset(66.dp, start = false)
```

### iOS (`apps/ios`)

iOS computes one per-message `maxWidth` in `ChatView.swift` and applies it to
every bubble; the `* 0.84` factor is the gap. For a received message in a
channel that factor is dropped (full width minus the avatar inset) — the same
geometry the voice-message case already uses:

```swift
let channelReceived = !ci.chatDir.sent && cInfo.isChannel
let maxWidth = cInfo.chatType == .group
? voiceNoFrame || channelReceived
? (g.size.width - 28) - 42
: (g.size.width - 28) * 0.84 - 42
: ...
```

The received check (`!ci.chatDir.sent`) is explicit here because, unlike the
Kotlin layout (which has a separate received branch), iOS shares one `maxWidth`
between sent and received.

## Why gate on `ChatInfo.isChannel` (`useRelays`)

The change is gated per chat on `ChatInfo.isChannel`, which is
`groupInfo?.useRelays == true` — `chatInfo.isChannel` on both Android/desktop and
iOS (`cInfo.isChannel`).

This is the robust signal. The whole channel feature on both platforms keys on
`useRelays` (channel preferences, member management, info view, broadcast
compose, etc.); `useRelays` is a non-optional `Bool` that is always present on a
group.

- **Not on the group-type `isChannel`** (`publicGroup?.groupType == channel`).
  This was the first attempt and it left the gap in place on iOS. The likely
  mechanism: `publicGroup` is an optional reconstructed from nullable DB columns
  (`src/Simplex/Chat/Store/Groups.hs` `toGroupProfile`, plus a creation path that
  sets `publicGroup = Nothing`), so when it is not populated for a chat the
  optional chain silently evaluates to `false` and the gap is never removed.
  `useRelays` cannot fail this way — it is a required `Bool` set at group
  creation (`useRelays = not direct`, `Commands.hs:2080`). Independent of the
  exact mechanism, `useRelays` is the safer signal. It is also as precise: the
  only group type ever constructed is `GTChannel` (`GTGroup` is defined but never
  instantiated), and `useRelays == true` is set on exactly that same
  public-group/channel path, so `useRelays == true` ⟺ "is a channel" for every
  chat today — regular groups, business chats and direct chats all have
  `useRelays` false/absent (verified: no non-channel path sets it true).
- **Not on the item direction.** The unattributed `ChannelRcv` direction is
  produced for any group message without an attributed member, not only in
  channels, and channels also contain member-attributed (`GroupRcv`) posts.
  Gating on direction would both over- and under-match, so the gate is the
  per-chat `isChannel`.

## Scope

Regular groups, business chats, and direct chats are unchanged (`isChannel` is
false for them). Sent messages are untouched.

## Verification

- Android/desktop: `:common:compileKotlinDesktop` compiles clean.
- iOS: change is a small, type-safe Swift expression; build/verify on macOS
  (Xcode) — not compilable on the Linux build host used here.
- Visual (both platforms): in a channel, long received messages widen toward the
  right edge; in a regular group and in direct chats the right gap is unchanged;
  sent messages are unchanged everywhere.
