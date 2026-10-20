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
| `CC` (sealed class) | Command definitions (~165 subclasses) | [SimpleXAPI.kt#L3954](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L3954) |
| `API` (sealed class) | Top-level response wrapper (`Result` / `Error`) | [SimpleXAPI.kt#L6611](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6611) |
| `CR` (sealed class) | Chat response variants (~180 subclasses) | [SimpleXAPI.kt#L6750](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6750) |
| `ChatError` (sealed class) | Error hierarchy | [SimpleXAPI.kt#L7807](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7807) |
| `ChatController` (object) | Singleton hosting all `api*` functions | [SimpleXAPI.kt#L537](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L537) |

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

**`sendCmd` flow** ([SimpleXAPI.kt#L939](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L939)):

1. Obtains the `ChatCtrl` handle (or uses the provided `otherCtrl`).
2. Serializes the `CC` command to its `cmdString`.
3. Dispatches to `Dispatchers.IO`; calls `chatSendCmdRetry` (local) or `chatSendRemoteCmdRetry` (remote host).
4. Decodes the returned JSON string into `API`.
5. Logs the result to the terminal item list.

<a id="startReceiver"></a>
<a id="recvMsg"></a>
<a id="processReceivedMsg"></a>

**Asynchronous event receiver** (`startReceiver`, [SimpleXAPI.kt#L785](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L785)):

A long-running coroutine on `Dispatchers.IO` repeatedly calls `chatRecvMsgWait` (blocking JNI). Each received `API` message is dispatched to `processReceivedMsg` ([SimpleXAPI.kt#L2920](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2920)), which pattern-matches on `CR` subclasses to update `ChatModel` state and trigger notifications.

---

<a id="CC"></a>

## 2. Command Categories

All functions below are `suspend fun` members of `ChatController` ([SimpleXAPI.kt#L537](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L537)). The `rh` / `rhId` parameter is `Long?` identifying a remote host (`null` = local device).

### 2.1 User Management

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiGetActiveUser` | `rh: Long?, ctrl: ChatCtrl?` | Fetch the currently active user profile | [L976](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L976) |
| `apiCreateActiveUser` | `rh: Long?, p: Profile?, pastTimestamp: Boolean, ctrl: ChatCtrl?` | Create a new user profile and set it as active | [L986](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L986) |
| `listUsers` | `rh: Long?` | List all user profiles sorted by display name | [L1006](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1006) |
| `apiSetActiveUser` | `rh: Long?, userId: Long, viewPwd: String?` | Switch the active user to a different profile | [L1016](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1016) |
| `apiSetAllContactReceipts` | `rh: Long?, enable: Boolean` | Enable/disable delivery receipts for all contacts globally | [L1023](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1023) |
| `apiSetUserContactReceipts` | `u: User, userMsgReceiptSettings: UserMsgReceiptSettings` | Set delivery receipt settings for user contacts | [L1029](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1029) |
| `apiSetUserGroupReceipts` | `u: User, userMsgReceiptSettings: UserMsgReceiptSettings` | Set delivery receipt settings for user groups | [L1035](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1035) |
| `apiSetUserAutoAcceptMemberContacts` | `u: User, enable: Boolean` | Toggle auto-accept for member contact requests | [L1041](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1041) |
| `apiHideUser` | `u: User, viewPwd: String` | Hide a user profile behind a password | [L1053](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1053) |
| `apiUnhideUser` | `u: User, viewPwd: String` | Unhide a previously hidden user profile | [L1056](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1056) |
| `apiMuteUser` | `u: User` | Mute all notifications for a user profile | [L1059](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1059) |
| `apiUnmuteUser` | `u: User` | Unmute notifications for a user profile | [L1062](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1062) |
| `apiDeleteUser` | `u: User, delSMPQueues: Boolean, viewPwd: String?` | Delete a user profile and optionally its SMP queues | [L1071](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1071) |
| `apiUpdateProfile` | `rh: Long?, profile: Profile` | Update the active user's display profile | [L1944](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1944) |
| `apiSetProfileAddress` | `rh: Long?, on: Boolean` | Enable/disable including address in user profile | [L1958](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1958) |
| `apiSetUserUIThemes` | `rh: Long?, userId: Long, themes: ThemeModeOverrides?` | Set UI theme overrides for a user | [L2033](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2033) |

### 2.2 Chat Lifecycle

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiStartChat` | `ctrl: ChatCtrl?` | Start the chat engine (returns `true` if newly started) | [L1078](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1078) |
| `apiStopChat` | _(none)_ | Stop the chat engine | [L1096](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1096) |
| `apiSetAppFilePaths` | `filesFolder, tempFolder, assetsFolder, remoteHostsFolder: String, ctrl: ChatCtrl?` | Configure file-system paths for the Haskell core | [L1102](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1102) |
| `apiSetEncryptLocalFiles` | `enable: Boolean` | Enable/disable encryption of locally stored files | [L1108](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1108) |
| `apiSaveAppSettings` | `settings: AppSettings` | Persist application settings to the core | [L1110](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1110) |
| `apiGetAppSettings` | `settings: AppSettings` | Retrieve application settings from the core | [L1116](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1116) |
| `apiGetChats` | `rh: Long?` | Fetch the list of all chats for the active user | [L1154](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1154) |
| `apiGetChat` | `rh, type, id, scope, contentTag, pagination, search` | Fetch a single chat with paginated messages | [L1172](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1172) |
| `apiGetChatContentTypes` | `rh: Long?, type: ChatType, id: Long, scope: GroupChatScope?` | Get available content type filters for a chat | [L1185](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1185) |
| `apiClearChat` | `rh: Long?, type: ChatType, id: Long` | Delete all messages in a chat | [L1937](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1937) |
| `apiDeleteChat` | `rh: Long?, type: ChatType, id: Long, chatDeleteMode: ChatDeleteMode` | Delete a chat (contact, group, connection, etc.) | [L1882](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1882) |
| `apiChatRead` | `rh: Long?, type: ChatType, id: Long` | Mark a chat as read | [L2189](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2189) |
| `apiChatItemsRead` | `rh, type, id, scope, itemIds` | Mark specific chat items as read | [L2203](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2203) |
| `apiChatUnread` | `rh: Long?, type: ChatType, id: Long, unreadChat: Boolean` | Toggle a chat's unread flag | [L2210](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2210) |
| `getChatItemTTL` | `rh: Long?` | Get the auto-delete TTL for chat items | [L1449](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1449) |
| `setChatItemTTL` | `rh: Long?, chatItemTTL: ChatItemTTL` | Set the auto-delete TTL for chat items | [L1462](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1462) |
| `setChatTTL` | `rh: Long?, chatType, id, chatItemTTL` | Set TTL for a specific chat | [L1469](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1469) |

### 2.3 Message Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSendMessages` | `rh, type, id, scope, live, ttl, composedMessages` | Send one or more messages to a chat | [L1215](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1215) |
| `apiCreateChatItems` | `rh: Long?, noteFolderId: Long, composedMessages: List<ComposedMessage>` | Create items in a private notes folder | [L1252](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1252) |
| `apiReportMessage` | `rh, groupId, chatItemId, reportReason, reportText` | Report a message in a group | [L1260](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1260) |
| `apiGetChatItemInfo` | `rh, type, id, scope, itemId` | Get delivery info for a specific chat item | [L1267](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1267) |
| `apiForwardChatItems` | `rh, toChatType, toChatId, toScope, fromChatType, fromChatId, fromScope, itemIds, ttl` | Forward messages between chats | [L1274](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1274) |
| `apiPlanForwardChatItems` | `rh, fromChatType, fromChatId, fromScope, chatItemIds` | Check forward feasibility before forwarding | [L1293](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1293) |
| `apiUpdateChatItem` | `rh, type, id, scope, itemId, updatedMessage, live` | Edit an existing message | [L1300](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1300) |
| `apiChatItemReaction` | `rh, type, id, scope, itemId, add, reaction` | Add or remove a reaction to a message | [L1323](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1323) |
| `apiGetReactionMembers` | `rh: Long?, groupId: Long, itemId: Long, reaction: MsgReaction` | List members who reacted with a specific emoji | [L1330](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1330) |
| `apiDeleteChatItems` | `rh, type, id, scope, itemIds, mode` | Delete messages (for self or for everyone) | [L1338](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1338) |
| `apiDeleteMemberChatItems` | `rh: Long?, groupId: Long, itemIds: List<Long>` | Moderate: delete another member's messages | [L1345](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1345) |
| `apiArchiveReceivedReports` | `rh: Long?, groupId: Long` | Archive all received reports in a group | [L1352](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1352) |
| `apiDeleteReceivedReports` | `rh: Long?, groupId: Long, itemIds: List<Long>, mode: CIDeleteMode` | Delete specific received reports | [L1359](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1359) |

### 2.4 Group Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiNewGroup` | `rh: Long?, incognito: Boolean, groupProfile: GroupProfile` | Create a new group | [L2393](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2393) |
| `apiAddMember` | `rh: Long?, groupId: Long, contactId: Long, memberRole: GroupMemberRole` | Invite a contact to a group | [L2434](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2434) |
| `apiJoinGroup` | `rh: Long?, groupId: Long` | Accept a group invitation | [L2443](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2443) |
| `apiAcceptMember` | `rh: Long?, groupId: Long, groupMemberId: Long, memberRole: GroupMemberRole` | Accept a member joining via group link | [L2469](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2469) |
| `apiDeleteMemberSupportChat` | `rh: Long?, groupId: Long, groupMemberId: Long` | Delete a member's support chat | [L2478](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2478) |
| `apiRemoveMembers` | `rh: Long?, groupId: Long, memberIds: List<Long>, withMessages: Boolean` | Remove members from a group | [L2485](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2485) |
| `apiMembersRole` | `rh: Long?, groupId: Long, memberIds: List<Long>, memberRole: GroupMemberRole` | Change the role of group members | [L2494](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2494) |
| `apiBlockMembersForAll` | `rh: Long?, groupId: Long, memberIds: List<Long>, blocked: Boolean` | Block/unblock members for all group participants | [L2503](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2503) |
| `apiLeaveGroup` | `rh: Long?, groupId: Long` | Leave a group | [L2512](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2512) |
| `apiListMembers` | `rh: Long?, groupId: Long` | List all members of a group | [L2519](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2519) |
| `apiUpdateGroup` | `rh: Long?, groupId: Long, groupProfile: GroupProfile` | Update group profile (name, image, etc.) | [L2526](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2526) |
| `apiCreateGroupLink` | `rh: Long?, groupId: Long, memberRole: GroupMemberRole` | Create a group invitation link | [L2563](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2563) |
| `apiGroupLinkMemberRole` | `rh: Long?, groupId: Long, memberRole: GroupMemberRole` | Update the default role for group link joins | [L2578](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2578) |
| `apiDeleteGroupLink` | `rh: Long?, groupId: Long` | Delete the group invitation link | [L2587](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2587) |
| `apiGetGroupLink` | `rh: Long?, groupId: Long` | Retrieve the current group invitation link | [L2597](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2597) |
| `apiAddGroupShortLink` | `rh: Long?, groupId: Long` | Create a short link for the group | [L2604](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2604) |
| `apiCreateMemberContact` | `rh: Long?, groupId: Long, groupMemberId: Long` | Create a direct contact from a group member | [L2614](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2614) |
| `apiSendMemberContactInvitation` | `rh: Long?, contactId: Long, mc: MsgContent` | Send a direct message invitation to a group member | [L2623](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2623) |
| `apiAcceptMemberContact` | `rh: Long?, contactId: Long` | Accept a member's direct contact invitation | [L2632](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2632) |
| `apiSetMemberSettings` | `rh: Long?, groupId: Long, groupMemberId: Long, memberSettings: GroupMemberSettings` | Configure per-member settings (e.g., mentions) | [L1506](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1506) |
| `apiGroupMemberInfo` | `rh: Long?, groupId: Long, groupMemberId: Long` | Get a group member's info and connection stats | [L1522](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1522) |
| `apiSetGroupAlias` | `rh: Long?, groupId: Long, localAlias: String` | Set a local alias for a group | [L2019](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2019) |

### 2.5 Contact Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiAddContact` | `rh: Long?, incognito: Boolean` | Create a one-time invitation link for a new contact | [L1613](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1613) |
| `apiSetConnectionIncognito` | `rh: Long?, connId: Long, incognito: Boolean` | Toggle incognito on a pending connection | [L1624](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1624) |
| `apiChangeConnectionUser` | `rh: Long?, connId: Long, userId: Long` | Change the user profile on a pending connection | [L1633](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1633) |
| `apiConnectPlan` | `rh: Long?, connLink: String, resolveMode: PlanResolveMode = PRMUnknown, linkOwnerSig: LinkOwnerSig? = null, inProgress: MutableState<Boolean>, onFailure: ConnectPlanFailure = Alert` | Analyze a connection link (or resolve a SimpleX name) before connecting; returns `ConnectionPlanResult?`. Failure handling: see [section 7](#7-directory-search-and-connection-plans) | [L1654](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1654-L1668) |
| `apiConnect` | `rh: Long?, incognito: Boolean, connLink: CreatedConnLink` | Connect via an invitation or address link | [L1669](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1669) |
| `apiPrepareContact` | `rh, connLink, contactShortLinkData` | Prepare a contact chat from a short link (before connecting) | [L1808](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1808) |
| `apiPrepareGroup` | `rh, connLink, groupShortLinkData` | Prepare a group chat from a short link (before connecting) | [L1817](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1817) |
| `apiConnectPreparedContact` | `rh, contactId, incognito, msg` | Connect to a previously prepared contact | [L1842](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1842) |
| `apiConnectPreparedGroup` | `rh, groupId, incognito, msg` | Join a previously prepared group | [L1852](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1852) |
| `apiConnectContactViaAddress` | `rh: Long?, incognito: Boolean, contactId: Long` | Connect to a contact using their public address | [L1862](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1862) |
| `apiDeleteContact` | `rh: Long?, id: Long, chatDeleteMode: ChatDeleteMode` | Delete a contact and return the deleted Contact | [L1906](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1906) |
| `apiContactInfo` | `rh: Long?, contactId: Long` | Get a contact's connection stats and custom profile | [L1515](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1515) |
| `apiSetContactAlias` | `rh: Long?, contactId: Long, localAlias: String` | Set a local display alias for a contact | [L2012](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2012) |
| `apiSetConnectionAlias` | `rh: Long?, connId: Long, localAlias: String` | Set a local display alias for a pending connection | [L2026](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2026) |
| `apiSetContactPrefs` | `rh: Long?, contactId: Long, prefs: ChatPreferences` | Update feature preferences for a contact | [L2005](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2005) |
| `apiCreateUserAddress` | `rh: Long?` | Create a long-term public contact address | [L2047](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2047) |
| `apiDeleteUserAddress` | `rh: Long?` | Delete the user's public contact address | [L2063](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2063) |
| `apiAddMyAddressShortLink` | `rh: Long?` | Create a short link for the user's address | [L2085](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2085) |
| `apiSetUserAddressSettings` | `rh: Long?, settings: AddressSettings` | Configure auto-accept for incoming contact requests | [L2096](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2096) |
| `apiAcceptContactRequest` | `rh: Long?, incognito: Boolean, contactReqId: Long` | Accept an incoming contact request | [L2110](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2110) |
| `apiRejectContactRequest` | `rh: Long?, contactReqId: Long` | Reject an incoming contact request | [L2133](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2133) |
| `apiSwitchContact` | `rh: Long?, contactId: Long` | Initiate SMP server switch for a contact | [L1543](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1543) |
| `apiAbortSwitchContact` | `rh: Long?, contactId: Long` | Abort an in-progress server switch | [L1557](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1557) |
| `apiSyncContactRatchet` | `rh: Long?, contactId: Long, force: Boolean` | Force ratchet synchronization with a contact | [L1571](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1571) |
| `apiGetContactCode` | `rh: Long?, contactId: Long` | Get the security verification code for a contact | [L1585](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1585) |
| `apiVerifyContact` | `rh: Long?, contactId: Long, connectionCode: String?` | Verify a contact's security code | [L1599](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1599) |

### 2.6 File Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `receiveFiles` | `rhId, user, fileIds, userApprovedRelays, auto` | Accept and download one or more files (handles relay approval) | [L2247](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2247) |
| `receiveFile` | `rhId, user, fileId, userApprovedRelays, auto` | Accept and download a single file (convenience wrapper) | [L2363](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2363) |
| `cancelFile` | `rh: Long?, user: User, fileId: Long` | Cancel an in-progress file transfer and clean up | [L2373](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2373) |
| `apiCancelFile` | `rh: Long?, fileId: Long, ctrl: ChatCtrl?` | Cancel a file transfer (low-level, returns updated chat item) | [L2381](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2381) |
| `uploadStandaloneFile` | `user: UserLike, file: CryptoFile, ctrl: ChatCtrl?` | Upload a standalone file (used for migration) | [L2217](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2217) |
| `downloadStandaloneFile` | `user: UserLike, url: String, file: CryptoFile, ctrl: ChatCtrl?` | Download a standalone file by URL | [L2227](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2227) |
| `standaloneFileInfo` | `url: String, ctrl: ChatCtrl?` | Retrieve metadata for a standalone file link | [L2237](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2237) |

### 2.7 Call Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiGetCallInvitations` | `rh: Long?` | Retrieve pending call invitations | [L2177](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2177) |
| `apiSendCallInvitation` | `rh: Long?, contact: Contact, callType: CallType` | Initiate a call by sending an invitation | [L2184](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2184) |
| `apiRejectCall` | `rh: Long?, contact: Contact` | Reject an incoming call | [L2189](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2189) |
| `apiSendCallOffer` | `rh, contact, rtcSession, rtcIceCandidates, media, capabilities` | Send a WebRTC call offer | [L2194](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2194) |
| `apiSendCallAnswer` | `rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String` | Send a WebRTC call answer | [L2201](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2201) |
| `apiSendCallExtraInfo` | `rh: Long?, contact: Contact, rtcSession: String?, rtcIceCandidates: String` | Send additional ICE candidates or ICE restart offer/answer during a call | [L2207](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2207) |
| `apiEndCall` | `rh: Long?, contact: Contact` | End an active call | [L2213](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2213) |
| `apiCallStatus` | `rh: Long?, contact: Contact, status: WebRTCCallStatus` | Report call status updates to the core | [L2218](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2218) |

### 2.8 Settings & Network

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSetNetworkConfig` | `cfg: NetCfg, showAlertOnError: Boolean, ctrl: ChatCtrl?` | Apply network configuration (SOCKS proxy, timeouts, etc.) | [L1476](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1476) |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Update network reachability information | [L1503](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1503) |
| `apiSetSettings` | `rh: Long?, type: ChatType, id: Long, settings: ChatSettings` | Update per-chat settings (notifications, favorites) | [L1496](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1496) |
| `apiStorageEncryption` | `currentKey: String, newKey: String` | Change the database encryption passphrase | [L1140](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1140) |
| `testStorageEncryption` | `key: String, ctrl: ChatCtrl?` | Verify a database encryption key is correct | [L1147](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1147) |
| `testProtoServer` | `rh: Long?, server: String` | Test connectivity to a protocol server | [L1366](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1366) |
| `reconnectServer` | `rh: Long?, server: String` | Reconnect to a specific server | [L1489](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1489) |
| `reconnectAllServers` | `rh: Long?` | Reconnect to all servers | [L1494](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1494) |
| `apiSetChatUIThemes` | `rh: Long?, chatId: ChatId, themes: ThemeModeOverrides?` | Set per-chat UI theme overrides | [L2040](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2040) |
| `apiContactQueueInfo` | `rh: Long?, contactId: Long` | Get server queue diagnostics for a contact | [L1529](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1529) |
| `apiGroupMemberQueueInfo` | `rh: Long?, groupId: Long, groupMemberId: Long` | Get server queue diagnostics for a group member | [L1536](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1536) |

### 2.9 Chat Tags

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiCreateChatTag` | `rh: Long?, tag: ChatTagData` | Create a new chat tag (folder/label) | [L1193](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1193) |
| `apiSetChatTags` | `rh: Long?, type: ChatType, id: Long, tagIds: List<Long>` | Assign tags to a chat | [L1201](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1201) |
| `apiDeleteChatTag` | `rh: Long?, tagId: Long` | Delete a chat tag | [L1209](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1209) |
| `apiUpdateChatTag` | `rh: Long?, tagId: Long, tag: ChatTagData` | Update a chat tag's name or emoji | [L1211](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1211) |
| `apiReorderChatTags` | `rh: Long?, tagIds: List<Long>` | Set the display order of chat tags | [L1213](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1213) |

### 2.10 Server Operators

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `getServerOperators` | `rh: Long?` | Get server operator conditions detail | [L1382](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1382) |
| `setServerOperators` | `rh: Long?, operators: List<ServerOperator>` | Update the list of server operators | [L1389](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1389) |
| `getUserServers` | `rh: Long?` | Get the user's configured servers per operator | [L1396](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1396) |
| `setUserServers` | `rh: Long?, userServers: List<UserOperatorServers>` | Save user's configured servers per operator | [L1404](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1404) |
| `validateServers` | `rh: Long?, userServers: List<UserOperatorServers>` | Validate server configuration for errors | [L1416](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1416) |
| `getUsageConditions` | `rh: Long?` | Get current and accepted usage conditions | [L1424](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1424) |
| `setConditionsNotified` | `rh: Long?, conditionsId: Long` | Mark conditions as shown to user | [L1431](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1431) |
| `acceptConditions` | `rh: Long?, conditionsId: Long, operatorIds: List<Long>` | Accept usage conditions for operators | [L1438](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1438) |

### 2.11 Archive

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiExportArchive` | `config: ArchiveConfig` | Export chat database to a ZIP archive | [L1122](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1122) |
| `apiImportArchive` | `config: ArchiveConfig` | Import chat database from a ZIP archive | [L1128](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1128) |
| `apiDeleteStorage` | _(none)_ | Delete all chat database storage | [L1134](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1134) |

<a id="ArchiveConfig"></a>

`ArchiveConfig` ([SimpleXAPI.kt#L4661](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4661)):

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
| `apiSearchDirectory` | `rh: Long?, text: String, cursor: JsonObject?` | Sends [`CC.APISendServiceRequest`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4069) (`userId, target, timeoutSec: Double?, request: JsonObject`; `/_service_request`) to SimpleX Directory and waits up to the timeout for its response, `CRServiceResponse`; see [section 7](#7-directory-search-and-connection-plans) | [L1645](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1645-L1652) |

---

<a id="API"></a>

## 3. Response Types

All command responses are deserialized into the `API` sealed class ([SimpleXAPI.kt#L6611](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6611)):

```kotlin
sealed class API {
  class Result(val remoteHostId: Long?, val res: CR) : API()
  class Error(val remoteHostId: Long?, val err: ChatError) : API()
}
```

<a id="CR"></a>

The `CR` sealed class ([SimpleXAPI.kt#L6750](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6750)) contains approximately 180 response variants. Key categories:

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
| Service | `CRServiceResponse` (`user, responseData: JsonObject`): a service's response to `APISendServiceRequest`, left undecoded for the caller | [L6791](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6791) |
| Fallback | `Response` (unknown type + raw JSON), `Invalid` (unparseable) | L6950-L6951 |

Each `CR` subclass is annotated with `@Serializable @SerialName("jsonTag")` for polymorphic JSON deserialization.

---

## 4. Event Types

The chat core pushes asynchronous events through the same `CR` type hierarchy. The `startReceiver` coroutine ([SimpleXAPI.kt#L785](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L785)) continuously calls `chatRecvMsgWait` (blocking JNI), then dispatches each message to `processReceivedMsg` ([SimpleXAPI.kt#L2920](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2920)).

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

### ChatError ([SimpleXAPI.kt#L7807](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7807))

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

### ChatErrorType ([SimpleXAPI.kt#L7837](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7837))

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

### StoreError ([SimpleXAPI.kt#L8011](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L8011))

Database-level errors: `DuplicateName`, `UserNotFound`, `GroupNotFound`, `ChatItemNotFound`, `LargeMsg`, `UserContactLinkNotFound`, etc.

### ArchiveError ([SimpleXAPI.kt#L8547](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L8547))

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
| DirectorySearch.kt | Directory request and response, online search result types | `common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt` |

---

## 7. Directory Search and Connection Plans

`ChatController` functions in [`SimpleXAPI.kt`](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) used by the online search (see [Chat List, Online Search](client/chat-list.md#11-online-search)):

| Function | Description | Line |
|----------|-------------|------|
| `apiSearchDirectory(rh, text, cursor)` | Sends `CC.APISendServiceRequest` to [`DIRECTORY_SERVICE_LINK`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L16) with [`DIRECTORY_SEARCH_TIMEOUT_SEC`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L19) and the request built by [`directorySearchRequest`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L71-L75); returns [`parseDirectorySearchResponse`](../common/src/commonMain/kotlin/chat/simplex/common/model/DirectorySearch.kt#L79-L88)`(responseData)`, or null on any error (logged with `responseType` and `details`). Blocks for up to the timeout; uses plain `sendCmd`, so a timeout gives a retry row rather than the alert `sendCmdWithRetry` would show | [L1645](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1645-L1652) |
| `apiConnectPlan(rh, connLink, resolveMode, linkOwnerSig, inProgress, onFailure)` | Plans a connection; returns `ConnectionPlanResult?`. `NotResolvedLocally` (a `PRMNever` lookup that matched nothing) returns null silently; any other failure is handled by `onFailure` | [L1654](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1654-L1668) |
| `ConnectPlanFailure` | `Alert` (default, every existing caller): `apiConnectResponseAlert`, only while `inProgress.value` is true, as before; `Log`: a debug log only, used by the online search so name resolution and per-entry lookups never raise alerts | [L7522](../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7522) |

A service response is carried as a kotlinx `JsonObject` (any JSON), so the API layer does not depend on a service's own types; `parseDirectorySearchResponse` reads its fields and decodes each entry with `json.decodeFromJsonElement`.

All paths are relative to `apps/multiplatform/`.
