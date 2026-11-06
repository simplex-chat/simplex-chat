# Chat API Reference

## Table of Contents

1. [Overview](#1-overview)
2. [Command Categories](#2-command-categories)
   - 2.1 [User Management](#21-user-management)
   - 2.2 [Chat Lifecycle](#22-chat-lifecycle)
   - 2.3 [Message Operations](#23-message-operations)
   - 2.4 [Group Operations](#24-group-operations)
   - 2.5 [Contact Operations](#25-contact-operations)
   - 2.6 [File Operations](#26-file-operations)
   - 2.7 [Call Operations](#27-call-operations)
   - 2.8 [Settings & Network](#28-settings--network)
   - 2.9 [Chat Tags](#29-chat-tags)
   - 2.10 [Server Operators](#210-server-operators)
   - 2.11 [Archive](#211-archive)
   - 2.12 [Service Requests](#212-service-requests)
3. [Response Types](#3-response-types)
4. [Event Types](#4-event-types)
5. [Error Types](#5-error-types)
6. [Source Files](#6-source-files)
7. [Directory Search and Connection Plans](#7-directory-search-and-connection-plans)

---

## 1. Overview

The SimpleX Chat API bridge connects Kotlin/Compose UI code to the Haskell core via JNI. All communication follows a **command/response JSON protocol**:

```
Kotlin suspend fun api*()
  -> ChatController.sendCmd(rhId, CC.*, ctrl)
       -> serialize CC to cmdString (JSON)
       -> chatSendCmdRetry(ctrl, cmdString, retryNum)   [JNI / external fun]
            -> Haskell core processes command
            -> returns JSON response string
       -> json.decodeFromString<API>(responseString)
            -> API.Result(rhId, CR.*) or API.Error(rhId, ChatError)
  -> pattern-match on CR subclass -> update ChatModel / return data to UI
```

**Key types in the pipeline:**

| Type | Role | Location |
|------|------|----------|
| `CC` (sealed class) | Command definitions (~165 subclasses) | [SimpleXAPI.kt#L4021](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4021) |
| `API` (sealed class) | Top-level response wrapper (`Result` / `Error`) | [SimpleXAPI.kt#L6715](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6715) |
| `CR` (sealed class) | Chat response variants (~180 subclasses) | [SimpleXAPI.kt#L6854](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6854) |
| `ChatError` (sealed class) | Error hierarchy | [SimpleXAPI.kt#L7943](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7943) |
| `ChatController` (object) | Singleton hosting all `api*` functions | [SimpleXAPI.kt#L547](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L547) |

**JNI bridge functions** (declared in [Core.kt#L25](../common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt#L25)):

```kotlin
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
external fun chatCloseStore(ctrl: ChatCtrl): String
external fun chatSendCmdRetry(ctrl: ChatCtrl, msg: String, retryNum: Int): String
external fun chatSendRemoteCmdRetry(ctrl: ChatCtrl, rhId: Int, msg: String, retryNum: Int): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
```

<a id="sendCmd"></a>

**`sendCmd` flow** ([SimpleXAPI.kt#L990](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L990)):

1. Obtains the `ChatCtrl` handle (or uses the provided `otherCtrl`).
2. Serializes the `CC` command to its `cmdString`.
3. Dispatches to `Dispatchers.IO`; calls `chatSendCmdRetry` (local) or `chatSendRemoteCmdRetry` (remote host).
4. Decodes the returned JSON string into `API`.
5. Logs the result to the terminal item list.

<a id="startReceiver"></a>
<a id="recvMsg"></a>
<a id="processReceivedMsg"></a>

**Asynchronous event receiver** (`startReceiver`, [SimpleXAPI.kt#L836](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L836)):

A long-running coroutine on `Dispatchers.IO` repeatedly calls `chatRecvMsgWait` (blocking JNI). Each received `API` message is dispatched to `processReceivedMsg` ([SimpleXAPI.kt#L2969](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2969)), which pattern-matches on `CR` subclasses to update `ChatModel` state and trigger notifications.

---

<a id="CC"></a>

## 2. Command Categories

All functions below are `suspend fun` members of `ChatController` ([SimpleXAPI.kt#L547](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L547)). The `rh` / `rhId` parameter is `Long?` identifying a remote host (`null` = local device).

### 2.1 User Management

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiGetActiveUser` | `rh: Long?, ctrl: ChatCtrl?` | Fetch the currently active user profile | [L1027](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1027) |
| `apiCreateActiveUser` | `rh: Long?, p: Profile?, pastTimestamp: Boolean, ctrl: ChatCtrl?` | Create a new user profile and set it as active | [L1037](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1037) |
| `listUsers` | `rh: Long?` | List all user profiles sorted by display name | [L1057](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1057) |
| `apiSetActiveUser` | `rh: Long?, userId: Long, viewPwd: String?` | Switch the active user to a different profile | [L1067](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1067) |
| `apiSetAllContactReceipts` | `rh: Long?, enable: Boolean` | Enable/disable delivery receipts for all contacts globally | [L1074](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1074) |
| `apiSetUserContactReceipts` | `u: User, userMsgReceiptSettings: UserMsgReceiptSettings` | Set delivery receipt settings for user contacts | [L1080](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1080) |
| `apiSetUserGroupReceipts` | `u: User, userMsgReceiptSettings: UserMsgReceiptSettings` | Set delivery receipt settings for user groups | [L1086](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1086) |
| `apiSetUserAutoAcceptMemberContacts` | `u: User, enable: Boolean` | Toggle auto-accept for member contact requests | [L1092](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1092) |
| `apiHideUser` | `u: User, viewPwd: String` | Hide a user profile behind a password | [L1104](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1104) |
| `apiUnhideUser` | `u: User, viewPwd: String` | Unhide a previously hidden user profile | [L1107](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1107) |
| `apiMuteUser` | `u: User` | Mute all notifications for a user profile | [L1110](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1110) |
| `apiUnmuteUser` | `u: User` | Unmute notifications for a user profile | [L1113](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1113) |
| `apiDeleteUser` | `u: User, delSMPQueues: Boolean, viewPwd: String?` | Delete a user profile and optionally its SMP queues | [L1122](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1122) |
| `apiUpdateProfile` | `rh: Long?, profile: Profile` | Update the active user's display profile | [L1993](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1993) |
| `apiSetProfileAddress` | `rh: Long?, on: Boolean` | Enable/disable including address in user profile | [L2007](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2007) |
| `apiSetUserUIThemes` | `rh: Long?, userId: Long, themes: ThemeModeOverrides?` | Set UI theme overrides for a user | [L2082](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2082) |

### 2.2 Chat Lifecycle

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiStartChat` | `ctrl: ChatCtrl?` | Start the chat engine (returns `true` if newly started) | [L1129](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1129) |
| `apiStopChat` | _(none)_ | Stop the chat engine | [L1147](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1147) |
| `apiSetAppFilePaths` | `filesFolder, tempFolder, assetsFolder, remoteHostsFolder: String, ctrl: ChatCtrl?` | Configure file-system paths for the Haskell core | [L1153](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1153) |
| `apiSetEncryptLocalFiles` | `enable: Boolean` | Enable/disable encryption of locally stored files | [L1159](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1159) |
| `apiSaveAppSettings` | `settings: AppSettings` | Persist application settings to the core | [L1161](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1161) |
| `apiGetAppSettings` | `settings: AppSettings` | Retrieve application settings from the core | [L1167](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1167) |
| `apiGetChats` | `rh: Long?` | Fetch the list of all chats for the active user | [L1205](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1205) |
| `apiGetChat` | `rh, type, id, scope, contentTag, pagination, search` | Fetch a single chat with paginated messages | [L1223](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1223) |
| `apiGetChatContentTypes` | `rh: Long?, type: ChatType, id: Long, scope: GroupChatScope?` | Get available content type filters for a chat | [L1236](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1236) |
| `apiClearChat` | `rh: Long?, type: ChatType, id: Long` | Delete all messages in a chat | [L1986](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1986) |
| `apiDeleteChat` | `rh: Long?, type: ChatType, id: Long, chatDeleteMode: ChatDeleteMode` | Delete a chat (contact, group, connection, etc.) | [L1931](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1931) |
| `apiChatRead` | `rh: Long?, type: ChatType, id: Long` | Mark a chat as read | [L2238](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2238) |
| `apiChatItemsRead` | `rh, type, id, scope, itemIds` | Mark specific chat items as read | [L2252](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2252) |
| `apiChatUnread` | `rh: Long?, type: ChatType, id: Long, unreadChat: Boolean` | Toggle a chat's unread flag | [L2259](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2259) |
| `getChatItemTTL` | `rh: Long?` | Get the auto-delete TTL for chat items | [L1500](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1500) |
| `setChatItemTTL` | `rh: Long?, chatItemTTL: ChatItemTTL` | Set the auto-delete TTL for chat items | [L1513](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1513) |
| `setChatTTL` | `rh: Long?, chatType, id, chatItemTTL` | Set TTL for a specific chat | [L1520](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1520) |

### 2.3 Message Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSendMessages` | `rh, type, id, scope, live, ttl, composedMessages` | Send one or more messages to a chat | [L1266](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1266) |
| `apiCreateChatItems` | `rh: Long?, noteFolderId: Long, composedMessages: List<ComposedMessage>` | Create items in a private notes folder | [L1303](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1303) |
| `apiReportMessage` | `rh, groupId, chatItemId, reportReason, reportText` | Report a message in a group | [L1311](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1311) |
| `apiGetChatItemInfo` | `rh, type, id, scope, itemId` | Get delivery info for a specific chat item | [L1318](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1318) |
| `apiForwardChatItems` | `rh, toChatType, toChatId, toScope, fromChatType, fromChatId, fromScope, itemIds, ttl` | Forward messages between chats | [L1325](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1325) |
| `apiPlanForwardChatItems` | `rh, fromChatType, fromChatId, fromScope, chatItemIds` | Check forward feasibility before forwarding | [L1344](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1344) |
| `apiUpdateChatItem` | `rh, type, id, scope, itemId, updatedMessage, live` | Edit an existing message | [L1351](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1351) |
| `apiChatItemReaction` | `rh, type, id, scope, itemId, add, reaction` | Add or remove a reaction to a message | [L1374](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1374) |
| `apiGetReactionMembers` | `rh: Long?, groupId: Long, itemId: Long, reaction: MsgReaction` | List members who reacted with a specific emoji | [L1381](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1381) |
| `apiDeleteChatItems` | `rh, type, id, scope, itemIds, mode` | Delete messages (for self or for everyone) | [L1389](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1389) |
| `apiDeleteMemberChatItems` | `rh: Long?, groupId: Long, itemIds: List<Long>` | Moderate: delete another member's messages | [L1396](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1396) |
| `apiArchiveReceivedReports` | `rh: Long?, groupId: Long` | Archive all received reports in a group | [L1403](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1403) |
| `apiDeleteReceivedReports` | `rh: Long?, groupId: Long, itemIds: List<Long>, mode: CIDeleteMode` | Delete specific received reports | [L1410](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1410) |

### 2.4 Group Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiNewGroup` | `rh: Long?, incognito: Boolean, groupProfile: GroupProfile` | Create a new group | [L2442](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2442) |
| `apiAddMember` | `rh: Long?, groupId: Long, contactId: Long, memberRole: GroupMemberRole` | Invite a contact to a group | [L2483](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2483) |
| `apiJoinGroup` | `rh: Long?, groupId: Long` | Accept a group invitation | [L2492](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2492) |
| `apiAcceptMember` | `rh: Long?, groupId: Long, groupMemberId: Long, memberRole: GroupMemberRole` | Accept a member joining via group link | [L2518](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2518) |
| `apiDeleteMemberSupportChat` | `rh: Long?, groupId: Long, groupMemberId: Long` | Delete a member's support chat | [L2527](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2527) |
| `apiRemoveMembers` | `rh: Long?, groupId: Long, memberIds: List<Long>, withMessages: Boolean` | Remove members from a group | [L2534](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2534) |
| `apiMembersRole` | `rh: Long?, groupId: Long, memberIds: List<Long>, memberRole: GroupMemberRole` | Change the role of group members | [L2543](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2543) |
| `apiBlockMembersForAll` | `rh: Long?, groupId: Long, memberIds: List<Long>, blocked: Boolean` | Block/unblock members for all group participants | [L2552](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2552) |
| `apiLeaveGroup` | `rh: Long?, groupId: Long` | Leave a group | [L2561](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2561) |
| `apiListMembers` | `rh: Long?, groupId: Long` | List all members of a group | [L2568](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2568) |
| `apiUpdateGroup` | `rh: Long?, groupId: Long, groupProfile: GroupProfile` | Update group profile (name, image, etc.) | [L2575](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2575) |
| `apiCreateGroupLink` | `rh: Long?, groupId: Long, memberRole: GroupMemberRole` | Create a group invitation link | [L2612](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2612) |
| `apiGroupLinkMemberRole` | `rh: Long?, groupId: Long, memberRole: GroupMemberRole` | Update the default role for group link joins | [L2627](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2627) |
| `apiDeleteGroupLink` | `rh: Long?, groupId: Long` | Delete the group invitation link | [L2636](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2636) |
| `apiGetGroupLink` | `rh: Long?, groupId: Long` | Retrieve the current group invitation link | [L2646](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2646) |
| `apiAddGroupShortLink` | `rh: Long?, groupId: Long` | Create a short link for the group | [L2653](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2653) |
| `apiCreateMemberContact` | `rh: Long?, groupId: Long, groupMemberId: Long` | Create a direct contact from a group member | [L2663](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2663) |
| `apiSendMemberContactInvitation` | `rh: Long?, contactId: Long, mc: MsgContent` | Send a direct message invitation to a group member | [L2672](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2672) |
| `apiAcceptMemberContact` | `rh: Long?, contactId: Long` | Accept a member's direct contact invitation | [L2681](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2681) |
| `apiSetMemberSettings` | `rh: Long?, groupId: Long, groupMemberId: Long, memberSettings: GroupMemberSettings` | Configure per-member settings (e.g., mentions) | [L1557](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1557) |
| `apiGroupMemberInfo` | `rh: Long?, groupId: Long, groupMemberId: Long` | Get a group member's info and connection stats | [L1573](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1573) |
| `apiSetGroupAlias` | `rh: Long?, groupId: Long, localAlias: String` | Set a local alias for a group | [L2068](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2068) |

### 2.5 Contact Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiAddContact` | `rh: Long?, incognito: Boolean` | Create a one-time invitation link for a new contact | [L1664](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1664) |
| `apiSetConnectionIncognito` | `rh: Long?, connId: Long, incognito: Boolean` | Toggle incognito on a pending connection | [L1675](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1675) |
| `apiChangeConnectionUser` | `rh: Long?, connId: Long, userId: Long` | Change the user profile on a pending connection | [L1684](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1684) |
| `apiConnectPlan` | `rh: Long?, connLink: String, resolveMode: PlanResolveMode = PRMUnknown, linkOwnerSig: LinkOwnerSig? = null, inProgress: MutableState<Boolean>` | Analyze a connection link (or resolve a SimpleX name) before connecting; returns `ConnectionPlanResult?`. Failure handling: see [section 7](#7-directory-search-and-connection-plans) | [L1708](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1708-L1716) |
| `apiConnect` | `rh: Long?, incognito: Boolean, connLink: CreatedConnLink` | Connect via an invitation or address link | [L1718](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1718) |
| `apiPrepareContact` | `rh, connLink, contactShortLinkData` | Prepare a contact chat from a short link (before connecting) | [L1857](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1857) |
| `apiPrepareGroup` | `rh, connLink, groupShortLinkData` | Prepare a group chat from a short link (before connecting) | [L1866](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1866) |
| `apiConnectPreparedContact` | `rh, contactId, incognito, msg` | Connect to a previously prepared contact | [L1891](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1891) |
| `apiConnectPreparedGroup` | `rh, groupId, incognito, msg` | Join a previously prepared group | [L1901](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1901) |
| `apiConnectContactViaAddress` | `rh: Long?, incognito: Boolean, contactId: Long` | Connect to a contact using their public address | [L1911](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1911) |
| `apiDeleteContact` | `rh: Long?, id: Long, chatDeleteMode: ChatDeleteMode` | Delete a contact and return the deleted Contact | [L1955](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1955) |
| `apiContactInfo` | `rh: Long?, contactId: Long` | Get a contact's connection stats and custom profile | [L1566](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1566) |
| `apiSetContactAlias` | `rh: Long?, contactId: Long, localAlias: String` | Set a local display alias for a contact | [L2061](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2061) |
| `apiSetConnectionAlias` | `rh: Long?, connId: Long, localAlias: String` | Set a local display alias for a pending connection | [L2075](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2075) |
| `apiSetContactPrefs` | `rh: Long?, contactId: Long, prefs: ChatPreferences` | Update feature preferences for a contact | [L2054](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2054) |
| `apiCreateUserAddress` | `rh: Long?` | Create a long-term public contact address | [L2096](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2096) |
| `apiDeleteUserAddress` | `rh: Long?` | Delete the user's public contact address | [L2112](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2112) |
| `apiAddMyAddressShortLink` | `rh: Long?` | Create a short link for the user's address | [L2134](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2134) |
| `apiSetUserAddressSettings` | `rh: Long?, settings: AddressSettings` | Configure auto-accept for incoming contact requests | [L2145](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2145) |
| `apiAcceptContactRequest` | `rh: Long?, incognito: Boolean, contactReqId: Long` | Accept an incoming contact request | [L2159](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2159) |
| `apiRejectContactRequest` | `rh: Long?, contactReqId: Long` | Reject an incoming contact request | [L2182](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2182) |
| `apiSwitchContact` | `rh: Long?, contactId: Long` | Initiate SMP server switch for a contact | [L1594](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1594) |
| `apiAbortSwitchContact` | `rh: Long?, contactId: Long` | Abort an in-progress server switch | [L1608](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1608) |
| `apiSyncContactRatchet` | `rh: Long?, contactId: Long, force: Boolean` | Force ratchet synchronization with a contact | [L1622](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1622) |
| `apiGetContactCode` | `rh: Long?, contactId: Long` | Get the security verification code for a contact | [L1636](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1636) |
| `apiVerifyContact` | `rh: Long?, contactId: Long, connectionCode: String?` | Verify a contact's security code | [L1650](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1650) |

### 2.6 File Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `receiveFiles` | `rhId, user, fileIds, userApprovedRelays, auto` | Accept and download one or more files (handles relay approval) | [L2296](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2296) |
| `receiveFile` | `rhId, user, fileId, userApprovedRelays, auto` | Accept and download a single file (convenience wrapper) | [L2412](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2412) |
| `cancelFile` | `rh: Long?, user: User, fileId: Long` | Cancel an in-progress file transfer and clean up | [L2422](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2422) |
| `apiCancelFile` | `rh: Long?, fileId: Long, ctrl: ChatCtrl?` | Cancel a file transfer (low-level, returns updated chat item) | [L2430](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2430) |
| `uploadStandaloneFile` | `user: UserLike, file: CryptoFile, ctrl: ChatCtrl?` | Upload a standalone file (used for migration) | [L2266](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2266) |
| `downloadStandaloneFile` | `user: UserLike, url: String, file: CryptoFile, ctrl: ChatCtrl?` | Download a standalone file by URL | [L2276](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2276) |
| `standaloneFileInfo` | `url: String, ctrl: ChatCtrl?` | Retrieve metadata for a standalone file link | [L2286](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2286) |

### 2.7 Call Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiGetCallInvitations` | `rh: Long?` | Retrieve pending call invitations | [L2192](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2192) |
| `apiSendCallInvitation` | `rh: Long?, contact: Contact, callType: CallType` | Initiate a call by sending an invitation | [L2199](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2199) |
| `apiRejectCall` | `rh: Long?, contact: Contact` | Reject an incoming call | [L2204](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2204) |
| `apiSendCallOffer` | `rh, contact, rtcSession, rtcIceCandidates, media, capabilities` | Send a WebRTC call offer | [L2209](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2209) |
| `apiSendCallAnswer` | `rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String` | Send a WebRTC call answer | [L2216](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2216) |
| `apiSendCallExtraInfo` | `rh: Long?, contact: Contact, rtcSession: String?, rtcIceCandidates: String` | Send additional ICE candidates or ICE restart offer/answer during a call | [L2222](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2222) |
| `apiEndCall` | `rh: Long?, contact: Contact` | End an active call | [L2228](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2228) |
| `apiCallStatus` | `rh: Long?, contact: Contact, status: WebRTCCallStatus` | Report call status updates to the core | [L2233](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2233) |

### 2.8 Settings & Network

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSetNetworkConfig` | `cfg: NetCfg, showAlertOnError: Boolean, ctrl: ChatCtrl?` | Apply network configuration (SOCKS proxy, timeouts, etc.) | [L1527](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1527) |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Update network reachability information | [L1554](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1554) |
| `apiSetSettings` | `rh: Long?, type: ChatType, id: Long, settings: ChatSettings` | Update per-chat settings (notifications, favorites) | [L1547](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1547) |
| `apiStorageEncryption` | `currentKey: String, newKey: String` | Change the database encryption passphrase | [L1191](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1191) |
| `testStorageEncryption` | `key: String, ctrl: ChatCtrl?` | Verify a database encryption key is correct | [L1198](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1198) |
| `testProtoServer` | `rh: Long?, server: String` | Test connectivity to a protocol server | [L1417](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1417) |
| `reconnectServer` | `rh: Long?, server: String` | Reconnect to a specific server | [L1540](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1540) |
| `reconnectAllServers` | `rh: Long?` | Reconnect to all servers | [L1545](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1545) |
| `apiSetChatUIThemes` | `rh: Long?, chatId: ChatId, themes: ThemeModeOverrides?` | Set per-chat UI theme overrides | [L2089](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2089) |
| `apiContactQueueInfo` | `rh: Long?, contactId: Long` | Get server queue diagnostics for a contact | [L1580](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1580) |
| `apiGroupMemberQueueInfo` | `rh: Long?, groupId: Long, groupMemberId: Long` | Get server queue diagnostics for a group member | [L1587](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1587) |

### 2.9 Chat Tags

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiCreateChatTag` | `rh: Long?, tag: ChatTagData` | Create a new chat tag (folder/label) | [L1244](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1244) |
| `apiSetChatTags` | `rh: Long?, type: ChatType, id: Long, tagIds: List<Long>` | Assign tags to a chat | [L1252](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1252) |
| `apiDeleteChatTag` | `rh: Long?, tagId: Long` | Delete a chat tag | [L1260](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1260) |
| `apiUpdateChatTag` | `rh: Long?, tagId: Long, tag: ChatTagData` | Update a chat tag's name or emoji | [L1262](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1262) |
| `apiReorderChatTags` | `rh: Long?, tagIds: List<Long>` | Set the display order of chat tags | [L1264](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1264) |

### 2.10 Server Operators

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `getServerOperators` | `rh: Long?` | Get server operator conditions detail | [L1433](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1433) |
| `setServerOperators` | `rh: Long?, operators: List<ServerOperator>` | Update the list of server operators | [L1440](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1440) |
| `getUserServers` | `rh: Long?` | Get the user's configured servers per operator | [L1447](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1447) |
| `setUserServers` | `rh: Long?, userServers: List<UserOperatorServers>` | Save user's configured servers per operator | [L1455](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1455) |
| `validateServers` | `rh: Long?, userServers: List<UserOperatorServers>` | Validate server configuration for errors | [L1467](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1467) |
| `getUsageConditions` | `rh: Long?` | Get current and accepted usage conditions | [L1475](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1475) |
| `setConditionsNotified` | `rh: Long?, conditionsId: Long` | Mark conditions as shown to user | [L1482](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1482) |
| `acceptConditions` | `rh: Long?, conditionsId: Long, operatorIds: List<Long>` | Accept usage conditions for operators | [L1489](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1489) |

### 2.11 Archive

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiExportArchive` | `config: ArchiveConfig` | Export chat database to a ZIP archive | [L1173](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1173) |
| `apiImportArchive` | `config: ArchiveConfig` | Import chat database from a ZIP archive | [L1179](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1179) |
| `apiDeleteStorage` | _(none)_ | Delete all chat database storage | [L1185](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1185) |

<a id="ArchiveConfig"></a>

`ArchiveConfig` ([SimpleXAPI.kt#L4765](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4765)):

```kotlin
class ArchiveConfig(
  val archivePath: String,
  val disableCompression: Boolean? = null,
  val parentTempDirectory: String? = null
)
```

### 2.12 Service Requests

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSearchDirectory` | `rh: Long?, text: String, cursor: JsonObject?` | Sends [`CC.APISendServiceRequest`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4136) (`userId, target, timeoutSec: Double?, request: JsonObject`; `/_service_request`) to SimpleX Directory and waits for its response, `CRServiceResponse`, up to the timeout from sending; the lookup of the address before it has its own network timeout; see [section 7](#7-directory-search-and-connection-plans) | [L1695](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1695-L1706) |

---

<a id="API"></a>

## 3. Response Types

All command responses are deserialized into the `API` sealed class ([SimpleXAPI.kt#L6715](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6715)):

```kotlin
sealed class API {
  class Result(val remoteHostId: Long?, val res: CR) : API()
  class Error(val remoteHostId: Long?, val err: ChatError) : API()
}
```

<a id="CR"></a>

The `CR` sealed class ([SimpleXAPI.kt#L6854](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6854)) contains approximately 180 response variants. Key categories:

| Category | Examples | Lines |
|----------|---------|-------|
| User | `ActiveUser`, `UsersList`, `UserPrivacy`, `UserProfileUpdated` | L6751-L6807 |
| Chat state | `ChatStarted`, `ChatRunning`, `ChatStopped`, `ApiChats`, `ApiChat` | L6753-L6757 |
| Tags | `ChatTags`, `TagsUpdated` | L6759, L6786 |
| Contacts | `Invitation`, `SentConfirmation`, `SentInvitation`, `ContactConnected`, `ContactDeleted` | L6787-L6816 |
| Messages | `NewChatItems`, `ChatItemUpdated`, `ChatItemsDeleted`, `ChatItemReaction`, `ForwardPlan` | L6826-L6835 |
| Groups | `GroupCreated`, `SentGroupInvitation`, `UserAcceptedGroupSent`, `GroupUpdated`, `GroupMembers` | L6837-L6869 |
| Files (receive) | `RcvFileAccepted`, `RcvFileStart`, `RcvFileComplete`, `RcvFileCancelled`, `RcvFileError` | L6882-L6892 |
| Files (send) | `SndFileStart`, `SndFileComplete`, `SndFileCancelled`, `SndFileCompleteXFTP` | L6895-L6902 |
| Calls | `CallInvitation`, `CallOffer`, `CallAnswer`, `CallExtraInfo`, `CallEnded` | L6907-L6912 |
| Remote host | `RemoteHostList`, `RemoteHostStarted`, `RemoteHostConnected`, `RemoteHostStopped` | L6916-L6922 |
| Remote ctrl | `RemoteCtrlList`, `RemoteCtrlFound`, `RemoteCtrlConnected`, `RemoteCtrlStopped` | L6925-L6930 |
| Encryption | `ContactPQAllowed`, `ContactPQEnabled` | L6932-L6933 |
| Misc | `CmdOk`, `ArchiveExported`, `ArchiveImported`, `AppSettingsR`, `VersionInfo` | L6935-L6939 |
| Service | `CRServiceResponse` (`user, responseData: JsonObject`): a service's response to `APISendServiceRequest`, left undecoded for the caller | [L6895](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6895) |
| Fallback | `Response` (unknown type + raw JSON), `Invalid` (unparseable) | L6950-L6951 |

Each `CR` subclass is annotated with `@Serializable @SerialName("jsonTag")` for polymorphic JSON deserialization.

---

## 4. Event Types

The chat core pushes asynchronous events through the same `CR` type hierarchy. The `startReceiver` coroutine ([SimpleXAPI.kt#L836](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L836)) continuously calls `chatRecvMsgWait` (blocking JNI), then dispatches each message to `processReceivedMsg` ([SimpleXAPI.kt#L2969](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2969)).

Events handled in `processReceivedMsg` include:

| Event | Description |
|-------|-------------|
| `ContactConnected` | A contact has completed the connection handshake |
| `ContactConnecting` | A contact connection is in progress |
| `ContactSndReady` | Contact's sending channel is ready |
| `ContactDeletedByContact` | A contact deleted their side of the conversation |
| `ReceivedContactRequest` | An incoming contact request arrived |
| `NewChatItems` | New messages received |
| `ChatItemUpdated` | A message was edited |
| `ChatItemsDeleted` | Messages were deleted |
| `ChatItemReaction` | A reaction was added/removed |
| `ChatItemsStatusesUpdated` | Delivery statuses updated |
| `GroupCreated` | A new group was created |
| `ReceivedGroupInvitation` | An invitation to join a group |
| `JoinedGroupMember` | A new member joined |
| `DeletedMember` / `DeletedMemberUser` | A member was removed |
| `LeftMember` | A member left voluntarily |
| `GroupUpdated` | Group profile changed |
| `GroupRelayUpdated` | Owner-side: a relay's `relayStatus` and/or the member's status changed. Fires on `XGrpRelayReject` with `relayStatus = RsRejected` and `GroupMember.memberStatus = MemLeft` — final on owner side until cleared by the relay operator's `/group allow #<channel>` (no event emitted to the owner for that clear). |
| `MemberRole` | A member's role changed |
| `MemberBlockedForAll` | A member was blocked for all |
| `RcvFileStart` / `RcvFileComplete` / `RcvFileError` | File receive progress |
| `SndFileStart` / `SndFileComplete` / `SndFileError` | File send progress |
| `CallInvitation` / `CallOffer` / `CallAnswer` / `CallEnded` | Call signaling events |
| `ContactPQEnabled` | Post-quantum encryption status changed |
| `RemoteHostStopped` / `RemoteCtrlStopped` | Remote access session ended |
| `SubscriptionStatusEvt` | Connection subscription status changed |

Each event triggers updates to `ChatModel` (reactive Compose state) and optionally fires platform notifications via `ntfManager`.

---

<a id="ChatError"></a>

## 5. Error Types

### ChatError ([SimpleXAPI.kt#L7943](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7943))

```kotlin
sealed class ChatError {
  class ChatErrorChat(val errorType: ChatErrorType)       // Application-level errors
  class ChatErrorAgent(val agentError: AgentErrorType)     // SMP/XFTP agent errors
  class ChatErrorStore(val storeError: StoreError)         // Database store errors
  class ChatErrorDatabase(val databaseError: DatabaseError)// Database engine errors
  class ChatErrorRemoteHost(val remoteHostError: ...)      // Remote host errors
  class ChatErrorRemoteCtrl(val remoteCtrlError: ...)      // Remote controller errors
  class ChatErrorInvalidJSON(val json: String)             // JSON parsing failure
}
```

### ChatErrorType ([SimpleXAPI.kt#L7973](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7973))

Common application error codes (~70 variants):

| Error | Meaning |
|-------|---------|
| `NoActiveUser` | No user profile is set as active |
| `UserExists` | Attempted to create a duplicate user |
| `InvalidDisplayName` | Display name contains invalid characters |
| `ChatNotStarted` / `ChatNotStopped` | Chat engine in wrong state |
| `InvalidConnReq` / `UnsupportedConnReq` | Bad or incompatible connection link |
| `ContactNotReady` / `ContactDisabled` | Contact in unusable state |
| `GroupUserRole` | Insufficient group permissions |
| `GroupNotJoined` | User has not joined the group |
| `FileNotFound` / `FileCancelled` / `FileAlreadyReceiving` | File transfer errors |
| `FileNotApproved` | File from unapproved relay server |
| `HasCurrentCall` / `NoCurrentCall` | Call state conflicts |
| `CommandError` / `InternalError` / `CEException` | Generic/internal errors |

### StoreError ([SimpleXAPI.kt#L8147](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L8147))

Database-level errors: `DuplicateName`, `UserNotFound`, `GroupNotFound`, `ChatItemNotFound`, `LargeMsg`, `UserContactLinkNotFound`, etc.

### ArchiveError ([SimpleXAPI.kt#L8683](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L8683))

```kotlin
sealed class ArchiveError {
  class ArchiveErrorImport(val importError: String)
  class ArchiveErrorFile(val file: String, val fileError: String)
}
```

---

## 6. Source Files

| File | Purpose | Path |
|------|---------|------|
| SimpleXAPI.kt | API bridge: all `api*` functions, `CC`, `CR`, `ChatError` | `common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` |
| Core.kt | JNI externals, `initChatController`, `chatMigrateInit` | `common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt` |
| ChatModel.kt | Reactive UI state (`ChatModel` object) | `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt` |
| DatabaseUtils.kt | `DBMigrationResult`, `MigrationError`, DB password helpers | `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/DatabaseUtils.kt` |
| Files.kt | Platform-expect file path declarations | `common/src/commonMain/kotlin/chat/simplex/common/platform/Files.kt` |
| Files.android.kt | Android actual file paths | `common/src/androidMain/kotlin/chat/simplex/common/platform/Files.android.kt` |
| Files.desktop.kt | Desktop actual file paths | `common/src/desktopMain/kotlin/chat/simplex/common/platform/Files.desktop.kt` |
| Cryptor.kt | Platform-expect encryption interface | `common/src/commonMain/kotlin/chat/simplex/common/platform/Cryptor.kt` |
| Cryptor.android.kt | Android: AndroidKeyStore AES-GCM encryption | `common/src/androidMain/kotlin/chat/simplex/common/platform/Cryptor.android.kt` |
| Cryptor.desktop.kt | Desktop: placeholder (no-op) encryption | `common/src/desktopMain/kotlin/chat/simplex/common/platform/Cryptor.desktop.kt` |
| DirectorySearch.kt | Directory request and response, online search result states | `common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt` |

---

## 7. Directory Search and Connection Plans

`ChatController` functions in [`SimpleXAPI.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) used by the online search (see [Chat List, Online Search](client/chat-list.md#11-online-search)):

| Function | Description | Line |
|----------|-------------|------|
| `apiSearchDirectory(rh, text, cursor)` | Sends `CC.APISendServiceRequest` to [`DIRECTORY_SERVICE_LINK`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L13) with [`DIRECTORY_SEARCH_TIMEOUT_SEC`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L15) and the request built by [`directorySearchRequest`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L76-L80); returns [`parseDirectorySearchResponse`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L92-L102)`(responseData)` (the entries, the cursor and `moreCount`), or null on any error (logged: a failed command with `responseType` and `details`, an unusable response with a fixed line and nothing from it). Blocks for the address lookup, then for up to the timeout; uses plain `sendCmd`, so a timeout gives a retry row rather than the alert `sendCmdWithRetry` would show | [L1695](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1695-L1706) |
| `apiConnectPlan(rh, connLink, resolveMode, linkOwnerSig, inProgress)` | Plans a connection; returns `ConnectionPlanResult?`. `NotResolvedLocally` (a `PRMNever` lookup that matched nothing) returns null silently; any other failure shows `apiConnectResponseAlert` only while `inProgress.value` is true, so the background lookups, which pass false, and a cancelled `planAndConnect` show none. The online search treats null from a `PRMNever` plan of a directory result as "not one of your chats" | [L1708](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1708-L1716) |

A service response is represented as a kotlinx `JsonObject` (any JSON), so the API layer does not depend on a service's own types; `parseDirectorySearchResponse` reads its fields and decodes each entry with `json.decodeFromJsonElement`.

All paths are relative to `apps/multiplatform/`.
