# Kotlin/Desktop — Share chat card (MCChat) — Implementation Plan

Port of iOS commit `f49d98511` to Kotlin multiplatform codebase. Every section maps an iOS change to its Kotlin equivalent with file:line anchors.

---

## 1. Types — `ChatModel.kt`

### 1.1 Add `LinkOwnerSig` (new type, near line 4551 after `MsgChatLink`)

```kotlin
@Serializable
data class LinkOwnerSig(
    val ownerId: String? = null,
    val chatBinding: String,
    val ownerSig: String
)
```

iOS equivalent: `ChatTypes.swift` `LinkOwnerSig` struct.

### 1.2 Add `ownerSig` to `MCChat` (line ~4310)

Current: `class MCChat(override val text: String, val chatLink: MsgChatLink): MsgContent()`
Change to: `class MCChat(override val text: String, val chatLink: MsgChatLink, val ownerSig: LinkOwnerSig? = null): MsgContent()`

### 1.3 Add `chatLinkStr` property to `MsgContent` (near `text` property)

```kotlin
val chatLinkStr: String?
    get() = (this as? MCChat)?.chatLink?.connLinkStr
```

### 1.4 Update `MsgContentSerializer` (lines 4366-4496)

In the `"chat"` case of the deserializer, add `ownerSig` field:
```kotlin
"chat" -> {
    val text = json["text"]?.jsonPrimitive?.content ?: ""
    val chatLink = Json.decodeFromJsonElement<MsgChatLink>(json["chatLink"]!!)
    val ownerSig = json["ownerSig"]?.let { Json.decodeFromJsonElement<LinkOwnerSig>(it) }
    MCChat(text, chatLink, ownerSig)
}
```

In the serializer, add `ownerSig` to the `MCChat` case:
```kotlin
is MCChat -> buildJsonObject {
    put("type", "chat")
    put("text", mc.text)
    put("chatLink", Json.encodeToJsonElement(mc.chatLink))
    mc.ownerSig?.let { put("ownerSig", Json.encodeToJsonElement(it)) }
}
```

### 1.5 Add computed properties to `MsgChatLink` (line ~4547)

The existing `MsgChatLink` sealed class uses `@SerialName` annotations for JSON. The Haskell side uses `taggedObjectJSON` format (`{"type": "group", ...}`). Need to verify the existing `@SerialName` produces the right format — it should, since kotlinx.serialization with `classDiscriminator = "type"` matches.

Add after the sealed class definition:
```kotlin
sealed class MsgChatLink {
    // ... existing cases ...

    val isPublicGroup: Boolean
        get() = (this as? Group)?.groupProfile?.publicGroup != null

    val connLinkStr: String
        get() = when (this) {
            is Group -> connLink
            is Contact -> connLink
            is Invitation -> invLink
        }

    val image: String?
        get() = when (this) {
            is Group -> groupProfile.image
            is Contact -> profile.image
            is Invitation -> profile.image
        }

    val displayName: String
        get() = when (this) {
            is Group -> groupProfile.displayName
            is Contact -> profile.displayName
            is Invitation -> profile.displayName
        }

    val fullName: String
        get() = when (this) {
            is Group -> groupProfile.fullName
            is Contact -> profile.fullName
            is Invitation -> profile.fullName
        }

    val shortDescription: String?
        get() {
            val s = when (this) {
                is Group -> groupProfile.shortDescr
                is Contact -> profile.shortDescr
                is Invitation -> profile.shortDescr
            }
            return s?.trim()?.ifEmpty { null }
        }

    val iconRes: ImageResource  // for ProfileImage icon parameter
        get() = when (this) {
            is Group -> when (groupProfile.publicGroup?.groupType) {
                GroupType.Channel -> MR.images.ic_bigtop_updates_padded
                else -> MR.images.ic_supervised_user_circle_filled
            }
            is Contact -> if (business) MR.images.ic_work_filled_padded else MR.images.ic_account_circle_filled
            is Invitation -> MR.images.ic_account_circle_filled
        }

    val smallIconRes: ImageResource  // for inline icon in context/quote views
        get() = when (this) {
            is Group -> when (groupProfile.publicGroup?.groupType) {
                GroupType.Channel -> MR.images.ic_bigtop_updates
                else -> MR.images.ic_group
            }
            is Contact -> if (business) MR.images.ic_work else MR.images.ic_person
            is Invitation -> MR.images.ic_person
        }

    fun infoLine(signed: Boolean): String {
        var s = when (this) {
            is Group -> when (groupProfile.publicGroup?.groupType) {
                GroupType.Channel -> generalGetString(MR.strings.channel_link)
                else -> generalGetString(MR.strings.group_link)
            }
            is Contact -> if (business) generalGetString(MR.strings.business_address) else generalGetString(MR.strings.contact_address)
            is Invitation -> generalGetString(MR.strings.one_time_link)
        }
        if (signed) {
            s += " " + if (isPublicGroup) generalGetString(MR.strings.from_owner) else generalGetString(MR.strings.signed_parentheses)
        }
        return s
    }
}
```

Icons resolved — see "Resolved decisions" section 1.

### 1.6 Add `OwnerVerification` type (near ConnectionPlan, line ~6844 of SimpleXAPI.kt)

```kotlin
@Serializable
sealed class OwnerVerification {
    @Serializable @SerialName("verified") object Verified : OwnerVerification()
    @Serializable @SerialName("failed") class Failed(val reason: String) : OwnerVerification()
}
```

### 1.7 Update plan types with `ownerVerification` (SimpleXAPI.kt lines 6852-6877)

- `InvitationLinkPlan.Ok`: add `val ownerVerification: OwnerVerification? = null`
- `ContactAddressPlan.Ok`: add `val ownerVerification: OwnerVerification? = null`
- `GroupLinkPlan.Ok`: add `val ownerVerification: OwnerVerification? = null`

---

## 2. API commands — `SimpleXAPI.kt`

### 2.1 Add `ApiShareChatMsgContent` command class (near line 3626)

```kotlin
class ApiShareChatMsgContent(
    val shareChatType: ChatType, val shareChatId: Long,
    val toChatType: ChatType, val toChatId: Long,
    val toScope: GroupChatScope?, val sendAsGroup: Boolean
): CC()
```

Add `cmdString`:
```kotlin
is ApiShareChatMsgContent -> {
    val asGroup = if (sendAsGroup) "(as_group=on)" else ""
    "/_share chat content ${chatRef(shareChatType, shareChatId)} ${chatRef(toChatType, toChatId, toScope)}$asGroup"
}
```

### 2.2 Add `CR.ChatMsgContent` response (near line 6320)

```kotlin
@Serializable @SerialName("chatMsgContent")
class ChatMsgContent(val user: UserRef, val msgContent: MsgContent): CR()
```

### 2.3 Add `apiShareChatMsgContent` wrapper function (near line 1133)

```kotlin
suspend fun apiShareChatMsgContent(
    rh: Long?, shareChatType: ChatType, shareChatId: Long,
    toChatType: ChatType, toChatId: Long,
    toScope: GroupChatScope?, sendAsGroup: Boolean
): MsgContent? {
    val r = sendCmd(rh, CC.ApiShareChatMsgContent(shareChatType, shareChatId, toChatType, toChatId, toScope, sendAsGroup))
    if (r is CR.ChatMsgContent) return r.msgContent
    apiErrorAlert("apiShareChatMsgContent", r)
    return null
}
```

### 2.4 Update `apiConnectPlan` (line 1488)

Add `linkOwnerSig: LinkOwnerSig? = null` parameter. Update the `CC.APIConnectPlan` class to include it. Update cmdString to append `sig=<json>` when present.

---

## 3. Compose state — `ComposeView.kt` + `Enums.kt`

### 3.1 Add `SharedContent.ChatLink` to `Enums.kt` (line 13-18)

```kotlin
data class ChatLink(val groupInfo: GroupInfo): SharedContent()
```

This triggers the share flow: sets `chatModel.sharedContent.value = SharedContent.ChatLink(groupInfo)` → navigates to chat list → user picks destination.

### 3.2 Add `ChatLinkPreview` to `ComposePreview` (`ComposeView.kt` line 57-63)

```kotlin
@Serializable class ChatLinkPreview(val chatLink: MsgChatLink, val ownerSig: LinkOwnerSig?): ComposePreview()
```

### 3.3 Update `ComposeState` (`ComposeView.kt` line 103-240)

- `sendEnabled`: add `is ComposePreview.ChatLinkPreview -> true` case
- `linkPreviewAllowed`: add `is ComposePreview.ChatLinkPreview -> false`
- `attachmentPreview`: add `is ComposePreview.ChatLinkPreview -> false`

### 3.4 Add compose preview rendering

In the compose area where previews are rendered, add a case for `ChatLinkPreview` that shows `ComposeChatLinkView` (new composable).

### 3.5 Add send handling

In the send function, add case for `ChatLinkPreview`:
```kotlin
is ComposePreview.ChatLinkPreview -> {
    val linkStr = preview.chatLink.connLinkStr
    val text = if (msgText.isEmpty()) linkStr else "$msgText\n$linkStr"
    send(MsgContent.MCChat(text, preview.chatLink, preview.ownerSig), ...)
}
```

### 3.6 Handle `SharedContent.ChatLink` in `ComposeView.kt` (line 1431-1446, `LaunchedEffect(chatModel.sharedContent.value)`)

When the destination chat opens with `SharedContent.ChatLink`, the `LaunchedEffect` fires. At this point:
- `chatModel.chatId.value` = destination chat ID
- `shared.groupInfo` = source group (what we're sharing)
- The current chat's `ChatInfo` provides destination type/id/scope for the API call

```kotlin
is SharedContent.ChatLink -> {
    // chat variable is available in ComposeView scope — it's the destination chat
    val cInfo = chat.chatInfo
    val sendAsGroup = cInfo.groupInfo?.let { it.useRelays && it.membership.memberRole >= GroupMemberRole.Owner } ?: false
    withBGApi {
        val mc = chatModel.controller.apiShareChatMsgContent(
            chat.remoteHostId, ChatType.Group, shared.groupInfo.groupId,
            cInfo.chatType, cInfo.apiId,
            cInfo.groupChatScope(), sendAsGroup
        )
        if (mc is MsgContent.MCChat) {
            composeState.value = composeState.value.copy(
                preview = ComposePreview.ChatLinkPreview(mc.chatLink, mc.ownerSig)
            )
        } else if (mc != null) {
            AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.error_sharing_channel),
                mc.toString()
            )
        }
    }
}
```

Note: `chat` is available as a parameter in the ComposeView composable scope. `withBGApi` is needed because `apiShareChatMsgContent` is a suspend function and `LaunchedEffect` already runs in a coroutine but the API call should use the standard error handling pattern.

### 3.7 Handle `SharedContent.ChatLink` in `ShareListView.kt` (line 33-54)

Add filtering case in the `when (sharedContent)` block:
```kotlin
is SharedContent.ChatLink -> {
    hasSimplexLink = true  // chat cards ARE simplex links, prohibited by SimplexLinks group pref
}
```

This means in `ShareListNavLinkView` (line 44): `simplexLinkProhibited = hasSimplexLink && !chat.groupFeatureEnabled(GroupFeature.SimplexLinks)` — groups where simplex links are disabled will show as prohibited (disabled row + alert on tap). Direct chats and local notes are unaffected (line 30-31 don't check simplex links for direct).

### 3.8 Handle `SharedContent.ChatLink` in `ShareListNavLinkView.kt` (line 28-67)

The existing `when (chat.chatInfo)` dispatch handles click actions per chat type. For `SharedContent.ChatLink`, the click action (line 37 `directChatAction`, line 54 `groupChatAction`) opens the destination chat. `ComposeView`'s `LaunchedEffect` (§3.6) then picks up the `SharedContent.ChatLink` and sets up the compose preview.

No changes needed to `ShareListNavLinkView` click handlers — they already open the correct chat. The `SharedContent.ChatLink` is consumed by `ComposeView`.

### 3.9 Handle `SharedContent.ChatLink` in `ShareListToolbar` (line 142-147)

Add title for the share list toolbar:
```kotlin
is SharedContent.ChatLink -> stringResource(MR.strings.share_channel)
```

### 3.10 Handle back navigation from share list with `SharedContent.ChatLink` (line 126-133)

When user taps back on the share list with `SharedContent.ChatLink`, should navigate back to the source chat (like Forward navigates back to `fromChatInfo.id`):
```kotlin
if (sharedContent is SharedContent.ChatLink) {
    chatModel.chatId.value = sharedContent.groupInfo.id
}
```

---

## 4. New composables

### 4.1 `ComposeChatLinkView.kt` (new file)

Near `ComposeView.kt`. Shows ProfileImage + displayName + optional shortDescription. Cancel button. Mirrors iOS `ComposeChatLinkView`.

### 4.2 `CIChatLinkHeader.kt` (new file)

Near `FramedItemView.kt`. Shows profile header (image + name + fullName), shortDescription, info line, "Tap to open" + meta. Mirrors iOS `CIChatLinkHeader`.

---

## 5. Message rendering — `FramedItemView.kt`

### 5.1 Add `MCChat` case in content dispatch (line ~296-341)

After the `MCLink` case:
```kotlin
is MsgContent.MCChat -> {
    val hasText = mc.text != mc.chatLink.connLinkStr
    CIChatLinkHeader(chatItem = ci, chatLink = mc.chatLink, ownerSig = mc.ownerSig, hasText = hasText)
    // tap gesture → planAndConnect(mc.chatLink.connLinkStr, linkOwnerSig = mc.ownerSig)
    if (hasText) {
        CIMarkdownText(..., stripLink = mc.chatLink.connLinkStr)
    }
}
```

### 5.2 Add `MCChat` case in quote dispatch (line ~142-183)

```kotlin
is MsgContent.MCChat -> {
    val prefix = buildAnnotatedString {
        append(mc.chatLink.displayName)
        append(if (mc.text != mc.chatLink.connLinkStr) " - " else "")
    }
    CIQuotedMsgView(qi, stripLink = mc.chatLink.connLinkStr, prefix = prefix)
    // + small icon
}
```

### 5.3 Add `stripLink` parameter to text rendering

`CIMarkdownText` / `MarkdownText` (TextItemView.kt) needs `stripLink: String? = null` parameter. Inside, strip the text and formattedText before rendering.

Add `stripTextLink` and `stripFormattedTextLink` functions near `MarkdownText`:

```kotlin
fun stripTextLink(text: String, link: String): String =
    if (text == link) ""
    else if (text.endsWith("\n$link")) text.dropLast(link.length + 1)
    else text

fun stripFormattedTextLink(ft: List<FormattedText>?, link: String): List<FormattedText>? {
    if (ft == null || ft.isEmpty() || ft.last().text != link) return ft
    val result = ft.toMutableList()
    result.removeLast()
    val i = result.lastIndex
    if (i >= 0 && result[i].format == null && result[i].text.endsWith("\n")) {
        result[i] = result[i].copy(text = result[i].text.dropLast(1))
        if (result[i].text.isEmpty()) result.removeLast()
    }
    return result.ifEmpty { null }
}
```

---

## 6. Chat list preview — `ChatPreviewView.kt`

### 6.1 Add content preview for `MCChat` (line ~293-337)

```kotlin
is MsgContent.MCChat -> {
    SmallContentPreview(borderColor = if (mc.chatLink.image != null) ...) {
        ProfileImage(mc.chatLink.image, mc.chatLink.iconName, size)
        // onClick → planAndConnect
    }
}
```

### 6.2 Update text preview (line ~217-290)

For `MCChat`, show `displayName + description` instead of raw text:
```kotlin
is MsgContent.MCChat -> {
    val descr = mc.chatLink.shortDescription?.let { "\n$it" } ?: ""
    itemText = mc.chatLink.displayName + descr
    formattedText = null
}
```

---

## 7. Context/forwarding view — `ContextItemView.kt`

### 7.1 Add `MCChat` attachment icon (line 75-84, `fun attachment()`)

```kotlin
is MsgContent.MCChat -> mc.chatLink.smallIconRes
```

This returns the small icon (e.g., `MR.images.ic_bigtop_updates` for channels). The icon is rendered inline via the existing `inlineContent` mechanism in `MessageText` (line 42-58).

### 7.2 Add `MCChat` case in `ContextMsgPreview` or `MessageText` (line 87-89)

For MCChat, `MessageText` needs:
1. The attachment icon (from §7.1) — rendered inline by existing mechanism
2. `prefix` with `chatLink.displayName + " - "` (or just displayName if no text) — `MarkdownText` already has `prefix: AnnotatedString?`
3. `stripLink = chatLink.connLinkStr` — strips the link from text

Modify `ContextMsgPreview` (line 87-89) or add a special case:
```kotlin
fun ContextMsgPreview(contextItem: ChatItem, lines: Int) {
    val mc = contextItem.content.msgContent
    if (mc is MsgContent.MCChat) {
        val hasText = contextItem.text != mc.chatLink.connLinkStr
        val prefix = buildAnnotatedString { append(mc.chatLink.displayName + if (hasText) " - " else "") }
        MessageText(contextItem, mc.chatLink.smallIconRes, lines, prefix = prefix, stripLink = mc.chatLink.connLinkStr)
    } else {
        MessageText(contextItem, remember(contextItem.id) { attachment(contextItem) }, lines)
    }
}
```

This requires `MessageText` to accept optional `prefix` and `stripLink` parameters and pass them to `MarkdownText`.

---

## 8. Group link / info views

### 8.1 `GroupLinkView.kt` (line ~27)

Add parameter: `groupInfo: GroupInfo? = null`.
Add "Share via chat" button when `groupInfo?.groupProfile?.publicGroup != null`.
Button action: `chatModel.sharedContent.value = SharedContent.ChatLink(groupInfo)` + close modals + navigate to chat list.

### 8.2 `GroupChatInfoView.kt`

Add "Share via chat" button in the channel link section (next to existing "Share link" button).
Button action: same as 8.1 — sets `SharedContent.ChatLink` and navigates.

No `composeState` parameter needed (unlike iOS) — the `SharedContent` pattern handles state transfer without bindings.

### 8.3 Channel creation (equivalent of `AddChannelView`)

Find the Kotlin channel creation flow and pass `groupInfo` to `GroupLinkView` so "Share via chat" is available during creation.

### 8.4 Share flow summary (no separate `shareChatLink` function needed)

Unlike iOS which has a separate `shareChatLink` free function (due to sheet-based navigation), Kotlin's flow is:

1. User taps "Share via chat" → `chatModel.sharedContent.value = SharedContent.ChatLink(groupInfo)` + `chatModel.chatId.value = null` (navigates to chat list showing `ShareListView`)
2. `ShareListView` shows filtered chats with `hasSimplexLink = true` prohibition
3. User picks destination → `directChatAction`/`groupChatAction` opens the chat
4. `ComposeView`'s `LaunchedEffect` fires (§3.6) → calls `apiShareChatMsgContent` → sets `ComposePreview.ChatLinkPreview`
5. User types optional text, taps Send
6. Send dispatch (§3.5) constructs `MCChat(text + link, chatLink, ownerSig)` and sends

The API call happens in `ComposeView`'s `LaunchedEffect`, not in a separate function. Error handling: if the API fails, show alert and clear `sharedContent`.

For the **channel creation flow** (no chat open yet): when `SharedContent.ChatLink` is consumed in `ComposeView` and the API call succeeds, the preview is set directly. No draft fallback needed — the chat IS already open at that point (the user picked it from the share list).

---

## 9. Connect flow

### 9.1 Update `planAndConnect` equivalent

Add `linkOwnerSig: LinkOwnerSig? = null` parameter. Pass to `apiConnectPlan`. Thread `ownerVerification` from plan result to connect alerts.

### 9.2 Add `ownerVerificationMessage` function

```kotlin
fun ownerVerificationMessage(ov: OwnerVerification?): String? = when (ov) {
    is OwnerVerification.Verified -> generalGetString(MR.strings.link_signature_verified)
    is OwnerVerification.Failed -> "⚠️ " + String.format(generalGetString(MR.strings.signature_verification_failed), ov.reason)
    null -> null
}
```

### 9.3 Update connect alerts

Add `information: String? = null` parameter to `AlertManager.showOpenChatAlert` (`AlertManager.kt` line 271). Render as a separate `Text` below subtitle with `MaterialTheme.colors.onSurface` color (not secondary — more prominent).

Update `showPrepareContactAlert` (ConnectPlan.kt line 572) and `showPrepareGroupAlert` (line 612) to accept and pass `ownerVerification`. Thread from `planAndConnectTask` `.Ok` cases.

---

## 10. String resources

Add to `strings.xml` (all platforms). No collisions found with existing keys:
- `chat_link_channel` = "Channel link"
- `chat_link_group` = "Group link"
- `chat_link_business_address` = "Business address"
- `chat_link_contact_address` = "Contact address"
- `chat_link_one_time` = "One-time link"
- `chat_link_from_owner` = "(from owner)"
- `chat_link_signed` = "(signed)"
- `owner_verification_passed` = "Link signature verified."
- `owner_verification_failed` = "⚠️ Signature verification failed: %s."
- `error_sharing_channel` = "Error sharing channel"
- `share_via_chat` = "Share via chat"
- `share_channel` = "Share channel"
- `tap_to_open` = "Tap to open"

---

## Resolved decisions (from investigation)

### 1. Icon resource names (verified from `ChatModel.kt` and MR/images/)
- **Channel**: `MR.images.ic_bigtop_updates_padded` (used in `GroupInfo.chatIconName` when `useRelays`)
- **Group**: `MR.images.ic_supervised_user_circle_filled` (used in `GroupInfo.chatIconName` default)
- **Business**: `MR.images.ic_work_filled_padded` (used in `GroupInfo.chatIconName` business case)
- **Contact**: `MR.images.ic_account_circle_filled` (used in `Contact.chatIconName`)
- **Small (inline text) icons**: `MR.images.ic_bigtop_updates` (channel), `MR.images.ic_group` (group), `MR.images.ic_work` (business), `MR.images.ic_person` (contact/invitation)

### 2. MsgChatLink JSON serialization
Existing `@Serializable` sealed class with `@SerialName` annotations already produces `{"type": "group", ...}` format. No custom serializer needed (confirmed by user). Keep existing pattern.

### 3. Forwarding/sharing picker pattern
Kotlin uses `SharedContent` + navigate to chat list, NOT a sheet picker:
- Forward: sets `chatModel.sharedContent.value = SharedContent.Forward(items, fromInfo)` + `chatModel.chatId.value = null` (returns to chat list)
- Share: add `SharedContent.ChatLink(groupInfo: GroupInfo)` case → sets `sharedContent` → user picks chat from `ShareListView` → opens chat with `ChatLinkPreview` in compose

`ShareListView.kt` (line 44) dispatches on `SharedContent` type for filtering. Add `SharedContent.ChatLink` case there with `hasSimplexLink = true` filtering.

### 4. Navigation after share
- `ModalManager.closeAllModalsEverywhere()` dismisses all modals
- Setting `chatModel.chatId.value = chatId` navigates to a chat
- For the share flow: `shareChatLink` calls API → on success → `ModalManager.closeAllModalsEverywhere()` → sets `composeState` preview → sets `chatModel.chatId.value = destChat.id`

### 5. Draft mechanism (verified at `ChatModel.kt:203-204`)
Same as iOS: `chatModel.draft: MutableState<ComposeState?>` and `chatModel.draftChatId: MutableState<String?>`. Used in `ComposeView.kt:435-444` for save/restore. Same fallback pattern as iOS for the channel creation flow.

### 6. `planAndConnect` (verified at `ConnectPlan.kt:24-48`)
Single function `suspend fun planAndConnect(rhId, shortOrFullLink, close, cleanup, filterKnownContact, filterKnownGroup)` in `ConnectPlan.kt`. Add `linkOwnerSig: LinkOwnerSig? = null` parameter. Thread to `apiConnectPlan`. Thread `ownerVerification` to alert functions.

Alert functions: `showPrepareContactAlert` (line 572) and `showPrepareGroupAlert` (line 612) use `AlertManager.privacySensitive.showOpenChatAlert(...)` which has `subtitle: String?`. Add `information: String? = null` parameter.

### 7. Forwarding view parameterization
No `ChatItemForwardingView` to parameterize — Kotlin uses `ShareListView` which dispatches on `SharedContent` type. Add a new `SharedContent.ChatLink` case. `ShareListView` filters chats and shows the list. When user picks a chat, `ComposeView` reads `sharedContent` and sets compose state accordingly (line 1439: `is SharedContent.Forward -> composeState.value = ...`). Add handling for `SharedContent.ChatLink`.

### 8. String resource naming
Need to check existing strings to avoid collisions. Use `chat_link_channel`, `chat_link_group`, etc. prefix pattern to avoid collision with existing `group_link` string.
