# Share Channel Link — Filter Saved Messages, Gate Plain Groups (Multiplatform)

PR: [#6958](https://github.com/simplex-chat/simplex-chat/pull/6958) · branch `nd/fix-group-link-share` · final commit `312072a5e`

## 1. The bug

Two failures on the Android/Desktop "Share via chat" flow for a channel link, both ending in a server error string the user sees in a red toast:

1. **Picking Saved Messages as the destination.** Server returns `chat commandError Failed reading: empty`. The user cannot share a channel link to their own note folder.
2. **Source group is not a channel.** The "Share via chat" button on the link-management screen (`GroupLinkView.kt`) renders for plain groups too. Tapping it produces `chat commandError not a public group`.

Both are reachable from the multiplatform client only. iOS does not hit either: it uses a different picker filter and gates the button by `publicGroup != nil` (which already implies `useRelays`).

## 2. Root cause

### Bug #1 — Saved Messages is not a valid destination for this command

`APIShareChatMsgContent` is parsed with `sendRefP` (`src/Simplex/Chat/Library/Commands.hs:5426`):

```haskell
sendRefP =
  (A.char '@' $> SRDirect <*> A.decimal)
    <|> (A.char '#' $> SRGroup <*> A.decimal <*> optional gcScopeP <*> asGroupP)
```

The client emits `*<id>` for `ChatType.Local` (Saved Messages) via the standard `chatType.rawValue` prefix. `sendRefP` has no `*` branch, attoparsec returns `Failed reading: empty`, the handler never runs.

This is the correct server behaviour. Sharing a channel link to one's own note folder is not a meaningful operation — the user can save the channel link by other means (copy from the channel-link screen). The client offered the destination by accident: the picker (`ShareListView.kt:199`) included `ChatInfo.Local` for every `SharedContent` flavour, including `SharedContent.ChatLink`.

### Bug #2 — share button rendered for plain groups

`GroupLinkView.kt:261` renders the share-via-chat button whenever `shareGroupInfo` is non-null:

```kotlin
if (shareGroupInfo != null) {
  SettingsActionItem(painterResource(MR.images.ic_forward), stringResource(MR.strings.share_via_chat), …)
}
```

Two callers pass `shareGroupInfo` — `GroupChatInfoView.kt:170` and `ChatView.kt:3207` — both pass it unconditionally as `shareGroupInfo = groupInfo`. So a plain group (`useRelays == false`) ends up with a button whose action calls `APIShareChatMsgContent` against a source the server refuses with `not a public group`.

The sibling button in `GroupChatInfoView.kt:602` is already wrapped in `if (groupInfo.useRelays) { … if (channelLink != null) … }`. `GroupLinkView` was missing the equivalent gate.

## 3. Approaches considered

| # | Approach | Note |
|---|----------|------|
| A | Widen the server: add `SRLocal NoteFolderId` to `SendRef` with a parser branch; replace the `Nothing → throwCmdError "not a public group"` arm with a build-from-short-link path producing an unsigned `MCChat`. | The first commit on this branch (`7d4648b9f`). Widens the protocol surface (a new destination grammar) and the message domain (an unsigned card variant whose recipient story is unspecified), to make reachable a feature the user can achieve another way. Rejected as poor design. |
| B | **Final** — client-side, multiplatform only: filter `ChatInfo.Local` out of the picker for `SharedContent.ChatLink`; add `&& isChannel` to the button gate in `GroupLinkView`. | Two single-line changes. The failing paths become unreachable from the UI. Forward to Saved Messages, all other share flavours, iOS, and Haskell are untouched. |

Approach B is the smaller fix and aligns the UI with the server's grammar — the picker no longer offers a destination the server refuses, and the button no longer appears where there is no channel link to share.

## 4. Final implementation

### 4.1 `ShareListView.kt:199` — exclude Local from the channel-link picker

```kotlin
val sorted = chatModel.chats.value.toList().filter { it.chatInfo.ready && it.chatInfo.sendMsgEnabled && !(chatModel.sharedContent.value is SharedContent.ChatLink && it.chatInfo is ChatInfo.Local) }.sortedByDescending { it.chatInfo is ChatInfo.Local }
```

One clause appended to the existing predicate: `&& !(chatModel.sharedContent.value is SharedContent.ChatLink && it.chatInfo is ChatInfo.Local)`. Reads as "exclude (sharing-link AND local)". Kotlin's `is` binds tighter than `&&`, so the inner parens are only around the AND for `!` to negate.

`chatModel.sharedContent.value` is read inside the filter lambda, once per chat. Inside `derivedStateOf`, each read registers a Compose dependency — same dependency set as a hoisted `val` would produce, and small enough (`chats.value.size`) that there is no observable cost. The hunk is +1/-1.

The trailing `sortedByDescending { it.chatInfo is ChatInfo.Local }` is left untouched. It is a no-op when no Locals are present, and removing it would touch a line that does not need to change.

Other `SharedContent` flavours (`Text`, `Media`, `File`, `Forward`) keep their previous behaviour. Forwarding to Saved Messages still works — the new clause is false when `sharedContent` is not `ChatLink`.

### 4.2 `GroupLinkView.kt:261` — gate the share button by `isChannel`

```kotlin
if (shareGroupInfo != null && isChannel) {
  SettingsActionItem(painterResource(MR.images.ic_forward), stringResource(MR.strings.share_via_chat), …)
}
```

`isChannel` is the existing parameter of this view (declared at line 35 and 175, used for channel-specific rows throughout the file). Both callers already pass `isChannel = groupInfo.useRelays`, so the new clause is equivalent to "render only when `useRelays == true`" — matching the rule for the sibling button in `GroupChatInfoView.kt:602`. The hunk is +1/-1.

### 4.3 What is *not* changed

- **Haskell.** `src/Simplex/Chat/Controller.hs` and `src/Simplex/Chat/Library/Commands.hs` stay at master. `SendRef` has no `SRLocal` constructor; `APIShareChatMsgContent` still refuses non-public sources with `not a public group`; `sendRefP` has no `*` branch. The client just never sends those commands now.
- **iOS.** No file under `apps/ios/` is touched. `filterChatsToForwardTo` (`apps/ios/SimpleXChat/ChatUtils.swift:56`) still inserts `.local` at index 0 — iOS's share-channel picker uses the same function as forward, and changing it would touch the forward picker. `GroupLinkView.swift:110` already gates by `groupInfo?.groupProfile.publicGroup != nil`, which already implies `useRelays` on iOS (only channels carry a `publicGroup` profile in practice). Neither failure has been reported on iOS through this flow.
- **`GroupChatInfoView.kt`** `ShareViaChatButton`. Already wrapped in `if (groupInfo.useRelays) { … if (channelLink != null) … }` (lines 602–614). Nothing to change.
- **`ChatItemForwardingView`** equivalent on iOS, `ComposeView.kt` consumer of `SharedContent.ChatLink`, the `apiShareChatMsgContent` API surface, and every other share/forward path. The new filter clause is false outside `SharedContent.ChatLink`, so all other consumers see the same picker.

## 5. Why this works

The server is the source of truth for which destinations and which sources are valid for `APIShareChatMsgContent`:

- Destinations: `@<id>` (direct), `#<id>` (group / scope) — defined by `sendRefP`. Local (`*<id>`) is rejected as a parse failure, by construction.
- Sources: groups with a `publicGroup` profile and `groupLink` — defined by the `Just PublicGroupProfile {…}` arm of `APIShareChatMsgContent`.

The client's job is to offer choices the server will accept. Bug #1 was an offer mismatch (Local in the destination list); bug #2 was an offer mismatch (button rendered on a source with no `publicGroup`). The fix narrows the client's offers to match the server's grammar — without changing the server, and without adding state that has to be kept in sync.

Two booleans, two single-line changes. The picker filter clause is false for every `SharedContent` flavour that is not `ChatLink`, so no other share path is affected. The button gate reuses `isChannel`, the existing parameter that the rest of the file already uses for channel-vs-group dispatch.

## 6. Behaviour changes — full inventory

1. **Picking Saved Messages in the share-channel-link picker is no longer possible.** This is the bug fix. The destination simply isn't listed.
2. **"Share via chat" in `GroupLinkView` is hidden on plain groups.** Previously rendered but unusable; now correctly hidden.
3. **Forward picker, Media picker, File picker, Text picker — unchanged.** New filter clause is false for every non-`ChatLink` `SharedContent`.
4. **`ShareViaChatButton` in `GroupChatInfoView` — unchanged.** Already gated correctly.

Nothing else changes. Verified by reading the diff against master line-by-line.

## 7. Verification

1. **Linux desktop build** succeeded end-to-end against the current branch tip (`312072a5e`), producing `SimpleX_Chat-x86_64-fix-group-link-share.AppImage` via `bash /home/user/build/linux.sh`.
2. **Diff is exactly two single-line hunks** in two files:
   - `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupLinkView.kt | 2 +-`
   - `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ShareListView.kt | 2 +-`
3. **Manual on desktop:**
   - Open a public channel that is not yours → profile → "Share via chat" → picker shows direct + group destinations only, no "Saved Messages" row → pick a contact → channel-link card appears in compose.
   - Open a plain group → group-link management screen → no "Share via chat" button.
   - Open a channel's group-link management screen → "Share via chat" button still appears.
   - Forward an existing message → picker still shows Saved Messages at the top (regression check).

## 8. Trade-offs and follow-ups

1. **iOS retains the bug-#1 path latent.** `filterChatsToForwardTo` inserts `.local` for the channel-link picker on iOS. The same fix as the Kotlin one — pass `includeLocal: false` from `shareChannelPicker` — is a separate, scoped change for an iOS PR. Out of scope here.
2. **`chatModel.sharedContent.value` read inside the filter lambda** evaluates per chat in the predicate, rather than hoisted to a `val` once. Diff minimality wins: the original line was one line, the new line is one line. If profiling ever showed this on a hot path (it does not — `derivedStateOf` and chat-list sizes), hoisting is a trivial follow-up.
3. **The `sortedByDescending { it.chatInfo is ChatInfo.Local }` call** remains in the channel-link path even though there are no Locals to sort. Removing it for that path only would require splitting the chain. Diff minimality: leave it.
