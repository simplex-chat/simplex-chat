# iOS — Share chat card (MCChat)

Share a public group/channel link as a card in any chat. Backend (`APIShareChatMsgContent`, `SharePublicGroup`) exists. This plan covers: send-side UI (picker, compose, send), receive-side UI (card rendering, tap-to-connect with owner verification), and the plumbing between.

---

## 0. UX flow

1. Channel info screen (`GroupChatInfoView`): two entry points.
   - **Quick-access action button** row (next to search/mute): "share" button — visible when `groupInfo.useRelays` and channel has a public link. Non-owners see it too.
   - **Section button**: "Share via chat" inside the existing channel-link section (after existing system "Share link" button).
2. Tap either → **destination picker sheet** (reused from `ChatItemForwardingView` by parameterization, NOT a new view).
3. Tap a destination → sheet dismisses, navigates to the destination chat, compose shows **plaque** above input: "Sharing #channelName" (reused `ContextItemView` by parameterization, NOT a new view).
4. User may type optional text. Tap Send.
5. Backend builds MCChat: `text = <user-typed text>\n<link>` (link appended for old clients), signed with owner key if applicable. Sent as one message.
6. **Receive side**: card renders as a group-invitation-style tile (reused from `CIGroupInvitationView` by extracting shared component, NOT copy-pasted). Shows profile image + channel name + icon per link type + "from channel owner" if `ownerSig` present. Tap → `planAndConnect` flow with `linkOwnerSig` → alert shows owner verification result alongside standard plan info.

---

## B. Implementation, file by file

### 1. `SimpleXChat/ChatTypes.swift` — `LinkOwnerSig`, `OwnerVerification`, `MsgContent.chat` update

**LinkOwnerSig** (new struct, near `MsgChatLink` ~line 4790):
```swift
public struct LinkOwnerSig: Codable, Equatable, Hashable {
    public let ownerId: String?
    public let chatBinding: String
    public let ownerSig: String
}
```

**OwnerVerification** (new enum, near `GroupLinkPlan` in AppAPITypes.swift ~line 1379):
```swift
enum OwnerVerification: Decodable, Hashable {
    case verified
    case failed(reason: String)
}
```

**MsgContent.chat** — add `ownerSig` field:
- Case: `case chat(text: String, chatLink: MsgChatLink, ownerSig: LinkOwnerSig?)`
- CodingKeys: add `case ownerSig`
- Decoder (4719-4722): add `let ownerSig = try container.decodeIfPresent(LinkOwnerSig.self, forKey: .ownerSig)`
- Encoder (4764-4767): add `try container.encodeIfPresent(ownerSig, forKey: .ownerSig)`
- text getter (4605): `case let .chat(text, _, _)`
- `==` (4679): `case let (.chat(lt, ll, ls), .chat(rt, rl, rs)): return lt == rt && ll == rl && ls == rs`
- ComposeView.swift:1480: `case let .chat(_, chatLink, ownerSig): return .chat(text: msgText, chatLink: chatLink, ownerSig: ownerSig)`

### 2. `Shared/Model/AppAPITypes.swift` — `SendRef`, plan types, command, response

**SendRef** (new enum, near `ref()` helper ~line 580):
```swift
enum SendRef {
    case direct(contactId: Int64)
    case group(groupId: Int64, scope: GroupChatScope?, asGroup: Bool)
}

func sendRef(_ r: SendRef) -> String {
    switch r {
    case let .direct(contactId): "@\(contactId)"
    case let .group(groupId, scope, asGroup):
        "#\(groupId)\(scopeRef(scope))\(asGroup ? "(as_group=on)" : "")"
    }
}
```

**Plan types — add `ownerVerification`** to `.ok` cases:
- `InvitationLinkPlan.ok` (1352): `case ok(contactSLinkData_: ContactShortLinkData?, ownerVerification: OwnerVerification?)`
- `ContactAddressPlan.ok` (1359): `case ok(contactSLinkData_: ContactShortLinkData?, ownerVerification: OwnerVerification?)`
- `GroupLinkPlan.ok` (1374): `case ok(groupSLinkInfo_: GroupShortLinkInfo?, groupSLinkData_: GroupShortLinkData?, ownerVerification: OwnerVerification?)`

**ChatCommand** — new case after `apiForwardChatItems` (line 64):
```swift
case apiShareChatMsgContent(shareChatType: ChatType, shareChatId: Int64, toSendRef: SendRef)
```
cmdString: `"/_share chat content \(ref(shareChatType, shareChatId, scope: nil)) \(sendRef(toSendRef))"`

**APIConnectPlan** — extend with `linkOwnerSig`:
- Current (line ~150ish): `case apiConnectPlan(userId: Int64, connLink: String?)`
- Change to: `case apiConnectPlan(userId: Int64, connLink: String?, linkOwnerSig: LinkOwnerSig?)`
- cmdString: append `linkOwnerSig.map { " sig=" + encodeJSON($0) } ?? ""` (matches Haskell parser `optional (" sig=" *> jsonP)`)

**ChatResponse1** — new case after `newChatItems` (823):
```swift
case chatMsgContent(user: UserRef, msgContent: MsgContent)
```
Plus `responseType` + `details` entries.

### 3. `Shared/Model/SimpleXAPI.swift` — wrappers

**apiShareChatMsgContent** (near apiForwardChatItems, line 506):
```swift
func apiShareChatMsgContent(shareChatType: ChatType, shareChatId: Int64, toSendRef: SendRef) async throws -> MsgContent {
    let r: APIResult<ChatResponse1> = await chatApiSendCmd(
        .apiShareChatMsgContent(shareChatType: shareChatType, shareChatId: shareChatId, toSendRef: toSendRef)
    )
    if case let .result(.chatMsgContent(_, mc)) = r { return mc }
    throw r.unexpected
}
```

**apiConnectPlan** (line 1023-1032): add `linkOwnerSig: LinkOwnerSig? = nil` parameter, pass to `.apiConnectPlan(userId:connLink:linkOwnerSig:)`.

### 4. `SimpleXChat/ChatUtils.swift` — NO new filter function

Reuse `filterChatsToForwardTo` + `canForwardToChat` as-is. Chat cards ARE simplex links, so `prohibitedByPref(hasSimplexLink: true, ...)` applies and correctly gates by the destination's simplex-links preference + user's role. No separate filter.

### 5. `ChatItemForwardingView.swift` — parameterize for dual use

Add parameters to support both forwarding and sharing modes. **No new view file.**

Add to the struct:
```swift
var title: String = "Forward"
var chats: [Chat]  // caller provides filtered list (replaces internal filterChatsToForwardTo)
var isProhibited: ((Chat) -> Bool)? = nil  // default: existing prohibitedByPref check; sharing overrides
var onSelect: (Chat) -> Void  // replaces the inline tap handler
```

Remove `chatItems`, `fromChatInfo`, and `composeState` — the `onSelect` closure captures whatever the caller needs. The caller builds `ComposeState` and does navigation externally.

Existing forwarding call site (`ChatView.swift:278`) adapts:
```swift
ChatItemForwardingView(
    title: "Forward",
    chats: filterChatsToForwardTo(chats: chatModel.chats),
    isProhibited: { chat in forwardedChatItems.map { ci in chat.prohibitedByPref(...) }.contains(true) },
    onSelect: { chat in
        dismiss forwarding sheet
        set composeState to forwarding context
        if different chat: loadOpenChat(chat.id)
    }
)
```

Sharing call site (from GroupChatInfoView):
```swift
ChatItemForwardingView(
    title: "Share channel",
    chats: filterChatsToForwardTo(chats: chatModel.chats),  // same filter
    isProhibited: { chat in chat.prohibitedByPref(hasSimplexLink: true, isMediaOrFileAttachment: false, isVoice: false) },
    onSelect: { chat in
        dismiss info sheet
        set composeState to .sharingChatCard(sourceGroupInfo)
        if different chat: loadOpenChat(chat.id)
    }
)
```

### 6. `ContextItemView.swift` — parameterize for chat-card context

Add an optional `customText: String?` property. When set, render that text instead of the ChatItem preview. Everything else (icon, cancel button, background, layout) stays the same.

```swift
var customText: String? = nil  // e.g., "Sharing #news"
```

When `customText != nil`:
- Display the string in place of the `msgContentView` / multi-message count
- Use `Color(uiColor: .tertiarySystemBackground)` for background (no ChatItem to derive color from)

ComposeView's `contextItemView()` dispatch for the new case:
```swift
case let .sharingChatCard(sourceGroupInfo):
    ContextItemView(
        chat: chat,
        contextItems: [],
        contextIcon: "arrowshape.turn.up.forward",
        cancelContextItem: { composeState = composeState.copy(contextItem: .noContextItem) },
        customText: "Sharing #\(sourceGroupInfo.groupProfile.displayName)"
    )
    Divider()
```

### 7. `ComposeView.swift` — new context case + send dispatch

**New `ComposeContextItem` case** (line 20-26):
```swift
case sharingChatCard(sourceGroupInfo: GroupInfo)
```
Name is `sharingChatCard` (not channel-specific — MCChat is general).

**Convenience init** (after `forwardingItems` init at 90-96):
```swift
init(sharingChatCard sourceGroupInfo: GroupInfo) {
    self.message = ""
    self.parsedMessage = []
    self.preview = .noPreview
    self.contextItem = .sharingChatCard(sourceGroupInfo: sourceGroupInfo)
    self.voiceMessageRecordingState = .noRecording
}
```

**Accessor** (after `forwarding` at 146-150):
```swift
var sharingChatCard: Bool {
    switch contextItem {
    case .sharingChatCard: return true
    default: return false
    }
}
```

**sendEnabled** (176): add `|| sharingChatCard`.

**Draft-restore guard** (`ChatView.swift:758`): extend `!composeState.forwarding` to `!composeState.forwarding && !composeState.sharingChatCard`.

**Send dispatch** in `sendMessageAsync` (before forwarding branch at 1354):
```swift
if case let .sharingChatCard(sourceGroupInfo) = composeState.contextItem {
    sent = await shareChatCard(sourceGroupInfo, ttl)
} else if case let .forwardingItems(...) = ... {
```

**Helper** inside the same scope:
```swift
func shareChatCard(_ sourceGroupInfo: GroupInfo, _ ttl: Int?) async -> ChatItem? {
    let toSendRef: SendRef
    switch chat.chatInfo {
    case let .direct(contact):
        toSendRef = .direct(contactId: contact.contactId)
    case let .group(gInfo, scope):
        toSendRef = .group(groupId: gInfo.groupId, scope: scope, asGroup: gInfo.useRelays)
    default:
        return nil
    }
    do {
        var mc = try await apiShareChatMsgContent(
            shareChatType: .group, shareChatId: sourceGroupInfo.groupId, toSendRef: toSendRef
        )
        // Append user-typed text: backend returns MCChat with text=link; prepend user message if present
        if !composeState.message.isEmpty, case let .chat(text, chatLink, ownerSig) = mc {
            mc = .chat(text: composeState.message + "\n" + text, chatLink: chatLink, ownerSig: ownerSig)
        }
        return await send(mc, quoted: nil, live: false, ttl: ttl, mentions: [:])
    } catch {
        logger.error("shareChatCard failed: \(error.localizedDescription)")
        return nil
    }
}
```

**Post-send draft-restore** (1411-1417): mirror `wasForwarding` with `wasSharing`.

### 8. `GroupChatInfoView.swift` — two entry points + composeState plumbing

**Add to struct**: `@Binding var composeState: ComposeState` and `@State private var showSharePicker = false`.

**Quick-access button** — in `infoActionButtons()` (line 354-370), add after `channelLinkActionButton` / `addMembersActionButton` branch:
```swift
if groupInfo.useRelays && groupInfo.groupProfile.publicGroup?.groupLink != nil {
    InfoViewButton(image: "arrowshape.turn.up.forward", title: "share", width: buttonWidth) {
        showSharePicker = true
    }
    .disabled(!groupInfo.ready)
}
```
Adjust the `buttonWidth` divisor accordingly (4 → 5 if all four buttons can show).

**Section button** — in `if groupInfo.useRelays` Section (line 104-125), after existing "Share link" button (115):
```swift
Button {
    showSharePicker = true
} label: {
    Label("Share via chat", systemImage: "arrowshape.turn.up.forward")
}
```

**Sheet** — on the body:
```swift
.sheet(isPresented: $showSharePicker) {
    let shareChats = filterChatsToForwardTo(chats: ChatModel.shared.chats)
    if #available(iOS 16.0, *) {
        ChatItemForwardingView(
            title: "Share channel",
            chats: shareChats,
            isProhibited: { $0.prohibitedByPref(hasSimplexLink: true, isMediaOrFileAttachment: false, isVoice: false) },
            onSelect: { chat in selectShareDestination(chat) }
        ).presentationDetents([.fraction(0.8)])
    } else {
        ChatItemForwardingView(
            title: "Share channel",
            chats: shareChats,
            isProhibited: { $0.prohibitedByPref(hasSimplexLink: true, isMediaOrFileAttachment: false, isVoice: false) },
            onSelect: { chat in selectShareDestination(chat) }
        )
    }
}
```

**selectShareDestination helper** in the same struct:
```swift
private func selectShareDestination(_ chat: Chat) {
    showSharePicker = false
    composeState = ComposeState(sharingChatCard: groupInfo)
    if chat.id != ChatModel.shared.chatId {
        ItemsModel.shared.loadOpenChat(chat.id)
    }
    dismiss()  // dismiss info sheet too
}
```

**ChatView.swift:505-517**: pass `composeState: $composeState` to `GroupChatInfoView`.

### 9. View rendering — `MsgContent.chat` text handling

**On send**: text = `<user message>\n<link>`. Link is the `strEncode groupLink` that the backend includes. If user typed nothing, text = just the link.

**On display**: when rendering an `MCChat` message, strip the last line from `text` if it equals `chatLink`'s encoded link. This way:
- Old clients (no MCChat support) see text as-is: "hello\nhttps://simplex.chat/g#..." — usable.
- New clients (MCChat support) see "hello" + the rendered card — no redundant link.

Implement in the card view's text rendering (§10 below). The stripping logic:
```swift
func chatCardText(_ text: String, _ chatLink: MsgChatLink) -> String {
    let link = chatLinkStr(chatLink)
    if text.hasSuffix("\n" + link) {
        return String(text.dropLast(link.count + 1))
    }
    return text
}
```
Where `chatLinkStr` extracts the encoded link from the `MsgChatLink` variant.

### 10. Card rendering — shared component from `CIGroupInvitationView`

Extract a reusable **`CICardView`** from `CIGroupInvitationView`. This is a shared component that both views use (not a copy-paste).

**`CICardView`** (new file `Shared/Views/Chat/ChatItem/CICardView.swift`):
Provides the outer frame: background with chat-tail padding, ZStack with bottomTrailing meta, VStack with:
- Header slot (profile image + name)
- Divider
- Body slot (action text, subtitle)

Parameterized by:
```swift
struct CICardView<Header: View, Body: View>: View {
    @ObservedObject var chat: Chat
    var chatItem: ChatItem
    var header: Header
    var body_: Body  // avoid collision with View.body
    var onTap: (() -> Void)?
}
```

**`CIGroupInvitationView`** refactored to use `CICardView`:
- Passes `groupInfoView(action)` as header
- Passes invitation text + "Tap to join" as body
- Passes `joinGroup(groupId)` as onTap
- All existing behaviour preserved (status checks, progress indicator, incognito)

**`CIChatLinkView`** (new file, uses `CICardView`):
- Header: `ProfileImage` from `groupProfile.image` / `profile.image` + display name. Icon = same as chat list (no need to invent):
  - `.group` channel (`publicGroup?.groupType == .channel`): `antenna.radiowaves.left.and.right.circle.fill` (from `GroupInfo.chatIconName` when `useRelays`)
  - `.group` non-channel: `person.2.circle.fill` (from `GroupInfo.chatIconName` default)
  - `.group` business: `briefcase.circle.fill` (from `GroupInfo.chatIconName` business case)
  - `.contact` non-bot: `person.crop.circle.fill` (from `Contact.chatIconName`)
  - `.contact` business address: `briefcase.circle.fill`
  - `.invitation`: `person.crop.circle.fill`
- Body: stripped text (via `chatCardText`) + subtitle line:
  - If `ownerSig != nil`: "signed" (secondary color) — same as CLI, it's a claim, verification happens on tap
  - Action line: "Tap to open" (primary color)
- onTap (both sent and received): calls `planAndConnect(connLink, linkOwnerSig: ownerSig, ...)` — full connect flow with owner verification

**Wire-in** at `ChatItemView.swift:73-90`, before the `isShortEmoji` check:
```swift
if case let .chat(_, chatLink, _) = ci.content.msgContent {
    CIChatLinkView(chat: chat, chatItem: ci, chatLink: chatLink)
} else if let mc = ... {
```

### 11. Connect flow — owner verification in alerts

**`planAndConnect`** (`NewChatView.swift:1218`): add optional `linkOwnerSig: LinkOwnerSig? = nil` parameter. Pass to `apiConnectPlan(connLink:linkOwnerSig:)`.

**Alert text** — in the `.ok` branches of `planAndConnect` where the alert is built (lines 1255-1410), extend the alert body with owner verification info:
- `case .verified`: append "Channel owner signature verified." to alert message.
- `case .failed(let reason)`: append "Owner signature verification failed: \(reason)." to alert message. Consider making this a warning-styled alert.
- `nil` (no sig): no additional text.

This surfaces in the standard connect-confirmation alert before the user taps "Connect" / "Join".

### 12. Haskell — strip ownerSig on forward

When a received MCChat message is forwarded (the existing forward-items path in `Library/Commands.hs`), the `dropSig` function already strips `ownerSig` for cross-chat forwarding (binding mismatch). The existing code at `Commands.hs:1000-1006` handles this. **No additional Haskell work for v1.**

Card forwarding (subscriber shares the card further) naturally produces an unsigned card — the subscriber doesn't have the owner's key. This is correct.

---

## C. Decisions — all resolved

1. **Icon for "share" button**: `arrowshape.turn.up.forward` — easy to change later.
2. **Text stripping**: strip only the last line if it exactly matches the encoded link. User doesn't control the last line (backend appends it). If user also types the link, the typed copy remains — no special handling needed.
3. **"signed" label on card**: shown unconditionally when `ownerSig != nil`. It's a claim (same as CLI "signed"); verification happens on tap via `planAndConnect`.
4. **Card tap for sent items**: yes, same `planAndConnect` flow for both sent and received.
5. **Icons**: reuse existing `chatIconName` icons from chat list — `antenna.radiowaves.left.and.right.circle.fill` (channel), `person.2.circle.fill` (group), `briefcase.circle.fill` (business), `person.crop.circle.fill` (contact/invitation). Contact address sharing accounts for business address.

---

## D. Items to lock during coding (not user decisions)

- Exact `chatApiSendCmd` decode shape vs `chatSendCmd` / `processSendMessageCmd` for `apiShareChatMsgContent` response.
- `CICardView` exact slot API: whether to use `@ViewBuilder` closures or generic type params — decide during extraction from `CIGroupInvitationView` based on what minimises the diff.
- `planAndConnect` alert builder structure — may need a helper to format the owner-verification line, to be added inline during the `.ok` branch modifications.
- `chatLinkStr` extraction — how to get the encoded link string from `MsgChatLink` for text-stripping. Likely just `strEncode` equivalent on the `connLink` / `invLink` / `groupLink` field.
