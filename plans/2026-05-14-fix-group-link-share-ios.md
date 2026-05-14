# Share Channel Link — Filter Saved Messages (iOS)

Companion to [#6958](https://github.com/simplex-chat/simplex-chat/pull/6958) (`nd/fix-group-link-share`, Android/Desktop).
Branch `nd/fix-group-link-share-ios`, base `master`.

## 1. The bug

On the iOS channel-link "Share via chat" picker, **Saved Messages** is offered as a destination. Tapping it produces `chat commandError Failed reading: empty` from the server.

This is the iOS counterpart of bug #1 from PR #6958. Bug #2 from that PR (the "Share via chat" button rendering on plain groups) does **not** exist on iOS — `GroupLinkView.swift:110` already gates the button with `if groupInfo?.groupProfile.publicGroup != nil`, and plain groups have `publicGroup == nil`.

## 2. Root cause

`APIShareChatMsgContent` is parsed with `sendRefP` in `src/Simplex/Chat/Library/Commands.hs:5426`:

```haskell
sendRefP =
  (A.char '@' $> SRDirect <*> A.decimal)
    <|> (A.char '#' $> SRGroup <*> A.decimal <*> optional gcScopeP <*> asGroupP)
```

The iOS client emits `*<id>` for `ChatType.local` (Saved Messages) via the standard chat-type prefix. `sendRefP` has no `*` branch, attoparsec returns `Failed reading: empty`, the handler never runs.

This is the correct server behaviour — sharing a channel link to one's own note folder is not a meaningful operation. The picker offered the destination by accident: `filterChatsToForwardTo` in `apps/ios/SimpleXChat/ChatUtils.swift:56` unconditionally inserts `ChatInfo.local` at index 0:

```swift
public func filterChatsToForwardTo<C: ChatLike>(chats: [C]) -> [C] {
    var filteredChats = chats.filter { c in
        c.chatInfo.chatType != .local && canForwardToChat(c.chatInfo)
    }
    if let privateNotes = chats.first(where: { $0.chatInfo.chatType == .local }) {
        filteredChats.insert(privateNotes, at: 0)
    }
    return filteredChats
}
```

`shareChannelPicker` (`apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift:1103`) builds a `ChatItemForwardingView`, which calls `filterChatsToForwardTo`. So the channel-link picker inherits the Saved-Messages-at-index-0 behaviour that the forward picker wants.

## 3. Approaches considered

| # | Approach | Note |
|---|----------|------|
| A | **Final** — parameterize the filter: add `includeLocal: Bool = true` to `filterChatsToForwardTo` and to `ChatItemForwardingView`; pass `includeLocal: false` from `shareChannelPicker`. | Default keeps existing call-sites untouched. Mirrors PR #6958's pattern — the filter decides, callers express intent. |
| B | Post-filter `.local` inside `ChatItemForwardingView` after the call to `filterChatsToForwardTo`. | Same line count, but duplicates the `.local` predicate at the consumer instead of expressing it at the producer. |
| C | Pass a closure filter to `ChatItemForwardingView`. | A closure encodes one bit as a function — strictly more machinery for the same outcome. |
| D | Mirror Kotlin literally: read a global `SharedContent.ChatLink` discriminator inside the filter. | iOS's `SharedContent` lives in the Share Extension target, not the main app — the Kotlin-style predicate doesn't translate. |

Approach A wins on minimality (5 lines, three files), preserves all default behaviour, and matches the architectural pattern of PR #6958 (decision lives where the data is produced).

## 4. Final implementation

### 4.1 `apps/ios/SimpleXChat/ChatUtils.swift` — add `includeLocal` parameter

```diff
-public func filterChatsToForwardTo<C: ChatLike>(chats: [C]) -> [C] {
+public func filterChatsToForwardTo<C: ChatLike>(chats: [C], includeLocal: Bool = true) -> [C] {
     var filteredChats = chats.filter { c in
         c.chatInfo.chatType != .local && canForwardToChat(c.chatInfo)
     }
-    if let privateNotes = chats.first(where: { $0.chatInfo.chatType == .local }) {
+    if includeLocal, let privateNotes = chats.first(where: { $0.chatInfo.chatType == .local }) {
         filteredChats.insert(privateNotes, at: 0)
     }
     return filteredChats
 }
```

Default value preserves the contract for every existing caller (`ChatItemForwardingView.swift:26`, `SimpleX SE/ShareModel.swift:71-72`). The `if includeLocal, let ...` form Swift-natively short-circuits — no nested block needed.

### 4.2 `apps/ios/Shared/Views/Chat/ChatItemForwardingView.swift` — thread the flag

```diff
     var isProhibited: ((Chat) -> Bool)? = nil
     var onSelectChat: ((Chat) -> Void)? = nil
+    var includeLocal: Bool = true

     @State private var searchText: String = ""
     @State private var alert: SomeAlert?
-    private let chatsToForwardTo = filterChatsToForwardTo(chats: ChatModel.shared.chats)
+    private var chatsToForwardTo: [Chat] { filterChatsToForwardTo(chats: ChatModel.shared.chats, includeLocal: includeLocal) }
```

`private let → private var` (computed) is required because Swift property initializers cannot read sibling instance properties. The computed form re-evaluates when `body` runs — in this view that is twice per render (lines 49 and 52), against a list of size `chats.count`. No meaningful cost; if a profile ever flagged it, switching to a custom `init(...)` that captures `includeLocal` once is a trivial follow-up.

### 4.3 `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift` — opt out from the channel-link picker

```diff
     let v = ChatItemForwardingView(
         title: "Share channel",
         isProhibited: { $0.prohibitedByPref(hasSimplexLink: true, isMediaOrFileAttachment: false, isVoice: false) },
-        onSelectChat: { chat in shareChatLink(chat, sourceGroupInfo: groupInfo, composeState: composeState) }
+        onSelectChat: { chat in shareChatLink(chat, sourceGroupInfo: groupInfo, composeState: composeState) },
+        includeLocal: false
     )
```

One-line opt-out from the only iOS site that uses the channel-link share flow.

### 4.4 What is *not* changed

- **`GroupLinkView.swift:110`** — already gates "Share via chat" with `groupInfo?.groupProfile.publicGroup != nil`. Bug #2 from PR #6958 has no iOS analog.
- **Forward picker** (`ChatView.swift:279, 282`) — uses `ChatItemForwardingView`'s default `includeLocal: true`. Saved Messages still appears at index 0.
- **Share extension** (`SimpleX SE/ShareModel.swift:71-72`) — calls `filterChatsToForwardTo` directly with the default. Unchanged.
- **Haskell.** `sendRefP` and `APIShareChatMsgContent` stay at master. The client just stops offering destinations the server refuses.
- **Android/Desktop, all other share/forward paths.**

## 5. Why this works

The server is the source of truth for which destinations are valid for `APIShareChatMsgContent`:

- Destinations: `@<id>` (direct), `#<id>` (group / scope). Local (`*<id>`) is rejected as a parse failure, by construction.
- Sources: groups with `publicGroup` and `groupLink`. iOS already gates the source side correctly.

The client's job is to offer choices the server will accept. The picker offered Local in error; this PR narrows the offer to match the server's grammar. The default-`true` parameter means every other caller keeps its current behaviour without modification.

## 6. Behaviour changes — full inventory

1. **Picking Saved Messages in the iOS share-channel-link picker is no longer possible.** This is the bug fix.
2. **Forward picker — unchanged.** Default `includeLocal: true`. Forward-to-Saved-Messages still works.
3. **Share extension picker — unchanged.** Default `includeLocal: true`.
4. **`GroupLinkView` button gate — unchanged.** Already correct on iOS.

Nothing else changes. Verified by reading the diff against master line-by-line.

## 7. Verification

1. **Diff is six insertions, four deletions across three files** (`git diff --stat`):
   - `apps/ios/SimpleXChat/ChatUtils.swift                     | 4 ++--`
   - `apps/ios/Shared/Views/Chat/ChatItemForwardingView.swift  | 3 ++-`
   - `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift | 3 ++-`
2. **iOS build** requires Xcode on macOS — not run in this environment. To run by reviewer.
3. **Manual on iOS once built:**
   - Open a public channel → profile → "Share via chat" → picker shows direct + group destinations only, **no "Saved Messages" row**.
   - Long-press a message → Forward → picker still shows Saved Messages at the top (regression check).
   - Open a plain group → group-link management → no "Share via chat" button (already correct, regression check).

## 8. Trade-offs and follow-ups

1. **Computed `chatsToForwardTo` re-evaluates on body refresh** rather than caching at struct init. In practice, twice per render against a small list, with `ChatModel.shared.chats` already SwiftUI-observed. Switching to a custom `init(...)` that captures `includeLocal` and assigns `chatsToForwardTo` once is a one-step refactor if ever needed.
2. **The flag is binary, not content-typed.** Kotlin discriminates on `SharedContent` variant; iOS uses an explicit caller intent. If a future iOS site needed to skip Local for a non-share-channel reason, the same flag applies — no further changes needed.
