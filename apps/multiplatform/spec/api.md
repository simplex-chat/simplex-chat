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
3. [Response Types](#3-response-types)
4. [Event Types](#4-event-types)
5. [Error Types](#5-error-types)
6. [Source Files](#6-source-files)

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
| `CC` (sealed class) | Command definitions (~130 subclasses) | [SimpleXAPI.kt#L3529](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L3529) |
| `API` (sealed class) | Top-level response wrapper (`Result` / `Error`) | [SimpleXAPI.kt#L5975](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L5975) |
| `CR` (sealed class) | Chat response variants (~180 subclasses) | [SimpleXAPI.kt#L6114](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6114) |
| `ChatError` (sealed class) | Error hierarchy | [SimpleXAPI.kt#L6974](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6974) |
| `ChatController` (object) | Singleton hosting all `api*` functions | [SimpleXAPI.kt#L493](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L493) |

**JNI bridge functions** (declared in [Core.kt#L25](common/src/commonMain/kotlin/chat/simplex/common/platform/Core.kt#L25)):

```kotlin
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
external fun chatCloseStore(ctrl: ChatCtrl): String
external fun chatSendCmdRetry(ctrl: ChatCtrl, msg: String, retryNum: Int): String
external fun chatSendRemoteCmdRetry(ctrl: ChatCtrl, rhId: Int, msg: String, retryNum: Int): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
```

<a id="sendCmd"></a>

**`sendCmd` flow** ([SimpleXAPI.kt#L804](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L804)):

1. Obtains the `ChatCtrl` handle (or uses the provided `otherCtrl`).
2. Serializes the `CC` command to its `cmdString`.
3. Dispatches to `Dispatchers.IO`; calls `chatSendCmdRetry` (local) or `chatSendRemoteCmdRetry` (remote host).
4. Decodes the returned JSON string into `API`.
5. Logs the result to the terminal item list.

<a id="startReceiver"></a>
<a id="recvMsg"></a>
<a id="processReceivedMsg"></a>

**Asynchronous event receiver** (`startReceiver`, [SimpleXAPI.kt#L660](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L660)):

A long-running coroutine on `Dispatchers.IO` repeatedly calls `chatRecvMsgWait` (blocking JNI). Each received `API` message is dispatched to `processReceivedMsg` ([SimpleXAPI.kt#L2568](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2568)), which pattern-matches on `CR` subclasses to update `ChatModel` state and trigger notifications.

---

<a id="CC"></a>

## 2. Command Categories

All functions below are `suspend fun` members of `ChatController` ([SimpleXAPI.kt#L493](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L493)). The `rh` / `rhId` parameter is `Long?` identifying a remote host (`null` = local device).

### 2.1 User Management

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiGetActiveUser` | `rh: Long?, ctrl: ChatCtrl?` | Fetch the currently active user profile | [L841](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L841) |
| `apiCreateActiveUser` | `rh: Long?, p: Profile?, pastTimestamp: Boolean, ctrl: ChatCtrl?` | Create a new user profile and set it as active | [L851](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L851) |
| `listUsers` | `rh: Long?` | List all user profiles sorted by display name | [L871](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L871) |
| `apiSetActiveUser` | `rh: Long?, userId: Long, viewPwd: String?` | Switch the active user to a different profile | [L881](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L881) |
| `apiSetAllContactReceipts` | `rh: Long?, enable: Boolean` | Enable/disable delivery receipts for all contacts globally | [L888](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L888) |
| `apiSetUserContactReceipts` | `u: User, userMsgReceiptSettings: UserMsgReceiptSettings` | Set delivery receipt settings for user contacts | [L894](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L894) |
| `apiSetUserGroupReceipts` | `u: User, userMsgReceiptSettings: UserMsgReceiptSettings` | Set delivery receipt settings for user groups | [L900](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L900) |
| `apiSetUserAutoAcceptMemberContacts` | `u: User, enable: Boolean` | Toggle auto-accept for member contact requests | [L906](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L906) |
| `apiHideUser` | `u: User, viewPwd: String` | Hide a user profile behind a password | [L912](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L912) |
| `apiUnhideUser` | `u: User, viewPwd: String` | Unhide a previously hidden user profile | [L915](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L915) |
| `apiMuteUser` | `u: User` | Mute all notifications for a user profile | [L918](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L918) |
| `apiUnmuteUser` | `u: User` | Unmute notifications for a user profile | [L921](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L921) |
| `apiDeleteUser` | `u: User, delSMPQueues: Boolean, viewPwd: String?` | Delete a user profile and optionally its SMP queues | [L930](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L930) |
| `apiUpdateProfile` | `rh: Long?, profile: Profile` | Update the active user's display profile | [L1682](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1682) |
| `apiSetProfileAddress` | `rh: Long?, on: Boolean` | Enable/disable including address in user profile | [L1694](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1694) |
| `apiSetUserUIThemes` | `rh: Long?, userId: Long, themes: ThemeModeOverrides?` | Set UI theme overrides for a user | [L1732](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1732) |

### 2.2 Chat Lifecycle

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiStartChat` | `ctrl: ChatCtrl?` | Start the chat engine (returns `true` if newly started) | [L937](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L937) |
| `apiStopChat` | _(none)_ | Stop the chat engine | [L955](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L955) |
| `apiSetAppFilePaths` | `filesFolder, tempFolder, assetsFolder, remoteHostsFolder: String, ctrl: ChatCtrl?` | Configure file-system paths for the Haskell core | [L961](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L961) |
| `apiSetEncryptLocalFiles` | `enable: Boolean` | Enable/disable encryption of locally stored files | [L967](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L967) |
| `apiSaveAppSettings` | `settings: AppSettings` | Persist application settings to the core | [L969](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L969) |
| `apiGetAppSettings` | `settings: AppSettings` | Retrieve application settings from the core | [L975](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L975) |
| `apiGetChats` | `rh: Long?` | Fetch the list of all chats for the active user | [L1013](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1013) |
| `apiGetChat` | `rh, type, id, scope, contentTag, pagination, search` | Fetch a single chat with paginated messages | [L1031](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1031) |
| `apiGetChatContentTypes` | `rh: Long?, type: ChatType, id: Long, scope: GroupChatScope?` | Get available content type filters for a chat | [L1044](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1044) |
| `apiClearChat` | `rh: Long?, type: ChatType, id: Long` | Delete all messages in a chat | [L1675](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1675) |
| `apiDeleteChat` | `rh: Long?, type: ChatType, id: Long, chatDeleteMode: ChatDeleteMode` | Delete a chat (contact, group, connection, etc.) | [L1620](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1620) |
| `apiChatRead` | `rh: Long?, type: ChatType, id: Long` | Mark a chat as read | [L1888](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1888) |
| `apiChatItemsRead` | `rh, type, id, scope, itemIds` | Mark specific chat items as read | [L1902](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1902) |
| `apiChatUnread` | `rh: Long?, type: ChatType, id: Long, unreadChat: Boolean` | Toggle a chat's unread flag | [L1909](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1909) |
| `getChatItemTTL` | `rh: Long?` | Get the auto-delete TTL for chat items | [L1286](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1286) |
| `setChatItemTTL` | `rh: Long?, chatItemTTL: ChatItemTTL` | Set the auto-delete TTL for chat items | [L1299](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1299) |
| `setChatTTL` | `rh: Long?, chatType, id, chatItemTTL` | Set TTL for a specific chat | [L1306](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1306) |

### 2.3 Message Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSendMessages` | `rh, type, id, scope, live, ttl, composedMessages` | Send one or more messages to a chat | [L1074](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1074) |
| `apiCreateChatItems` | `rh: Long?, noteFolderId: Long, composedMessages: List<ComposedMessage>` | Create items in a private notes folder | [L1111](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1111) |
| `apiReportMessage` | `rh, groupId, chatItemId, reportReason, reportText` | Report a message in a group | [L1119](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1119) |
| `apiGetChatItemInfo` | `rh, type, id, scope, itemId` | Get delivery info for a specific chat item | [L1126](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1126) |
| `apiForwardChatItems` | `rh, toChatType, toChatId, toScope, fromChatType, fromChatId, fromScope, itemIds, ttl` | Forward messages between chats | [L1133](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1133) |
| `apiPlanForwardChatItems` | `rh, fromChatType, fromChatId, fromScope, chatItemIds` | Check forward feasibility before forwarding | [L1138](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1138) |
| `apiUpdateChatItem` | `rh, type, id, scope, itemId, updatedMessage, live` | Edit an existing message | [L1145](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1145) |
| `apiChatItemReaction` | `rh, type, id, scope, itemId, add, reaction` | Add or remove a reaction to a message | [L1168](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1168) |
| `apiGetReactionMembers` | `rh: Long?, groupId: Long, itemId: Long, reaction: MsgReaction` | List members who reacted with a specific emoji | [L1175](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1175) |
| `apiDeleteChatItems` | `rh, type, id, scope, itemIds, mode` | Delete messages (for self or for everyone) | [L1183](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1183) |
| `apiDeleteMemberChatItems` | `rh: Long?, groupId: Long, itemIds: List<Long>` | Moderate: delete another member's messages | [L1190](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1190) |
| `apiArchiveReceivedReports` | `rh: Long?, groupId: Long` | Archive all received reports in a group | [L1197](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1197) |
| `apiDeleteReceivedReports` | `rh: Long?, groupId: Long, itemIds: List<Long>, mode: CIDeleteMode` | Delete specific received reports | [L1204](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1204) |

### 2.4 Group Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiNewGroup` | `rh: Long?, incognito: Boolean, groupProfile: GroupProfile` | Create a new group | [L2092](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2092) |
| `apiAddMember` | `rh: Long?, groupId: Long, contactId: Long, memberRole: GroupMemberRole` | Invite a contact to a group | [L2100](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2100) |
| `apiJoinGroup` | `rh: Long?, groupId: Long` | Accept a group invitation | [L2109](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2109) |
| `apiAcceptMember` | `rh: Long?, groupId: Long, groupMemberId: Long, memberRole: GroupMemberRole` | Accept a member joining via group link | [L2135](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2135) |
| `apiDeleteMemberSupportChat` | `rh: Long?, groupId: Long, groupMemberId: Long` | Delete a member's support chat | [L2144](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2144) |
| `apiRemoveMembers` | `rh: Long?, groupId: Long, memberIds: List<Long>, withMessages: Boolean` | Remove members from a group | [L2151](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2151) |
| `apiMembersRole` | `rh: Long?, groupId: Long, memberIds: List<Long>, memberRole: GroupMemberRole` | Change the role of group members | [L2160](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2160) |
| `apiBlockMembersForAll` | `rh: Long?, groupId: Long, memberIds: List<Long>, blocked: Boolean` | Block/unblock members for all group participants | [L2169](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2169) |
| `apiLeaveGroup` | `rh: Long?, groupId: Long` | Leave a group | [L2178](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2178) |
| `apiListMembers` | `rh: Long?, groupId: Long` | List all members of a group | [L2185](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2185) |
| `apiUpdateGroup` | `rh: Long?, groupId: Long, groupProfile: GroupProfile` | Update group profile (name, image, etc.) | [L2192](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2192) |
| `apiCreateGroupLink` | `rh: Long?, groupId: Long, memberRole: GroupMemberRole` | Create a group invitation link | [L2211](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2211) |
| `apiGroupLinkMemberRole` | `rh: Long?, groupId: Long, memberRole: GroupMemberRole` | Update the default role for group link joins | [L2226](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2226) |
| `apiDeleteGroupLink` | `rh: Long?, groupId: Long` | Delete the group invitation link | [L2235](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2235) |
| `apiGetGroupLink` | `rh: Long?, groupId: Long` | Retrieve the current group invitation link | [L2245](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2245) |
| `apiAddGroupShortLink` | `rh: Long?, groupId: Long` | Create a short link for the group | [L2252](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2252) |
| `apiCreateMemberContact` | `rh: Long?, groupId: Long, groupMemberId: Long` | Create a direct contact from a group member | [L2262](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2262) |
| `apiSendMemberContactInvitation` | `rh: Long?, contactId: Long, mc: MsgContent` | Send a direct message invitation to a group member | [L2271](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2271) |
| `apiAcceptMemberContact` | `rh: Long?, contactId: Long` | Accept a member's direct contact invitation | [L2280](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2280) |
| `apiSetMemberSettings` | `rh: Long?, groupId: Long, groupMemberId: Long, memberSettings: GroupMemberSettings` | Configure per-member settings (e.g., mentions) | [L1343](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1343) |
| `apiGroupMemberInfo` | `rh: Long?, groupId: Long, groupMemberId: Long` | Get a group member's info and connection stats | [L1353](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1353) |
| `apiSetGroupAlias` | `rh: Long?, groupId: Long, localAlias: String` | Set a local alias for a group | [L1718](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1718) |

### 2.5 Contact Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiAddContact` | `rh: Long?, incognito: Boolean` | Create a one-time invitation link for a new contact | [L1444](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1444) |
| `apiSetConnectionIncognito` | `rh: Long?, connId: Long, incognito: Boolean` | Toggle incognito on a pending connection | [L1455](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1455) |
| `apiChangeConnectionUser` | `rh: Long?, connId: Long, userId: Long` | Change the user profile on a pending connection | [L1464](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1464) |
| `apiConnectPlan` | `rh: Long?, connLink: String, inProgress: MutableState<Boolean>` | Analyze a connection link before connecting | [L1474](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1474) |
| `apiConnect` | `rh: Long?, incognito: Boolean, connLink: CreatedConnLink` | Connect via an invitation or address link | [L1482](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1482) |
| `apiPrepareContact` | `rh, connLink, contactShortLinkData` | Prepare a contact chat from a short link (before connecting) | [L1546](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1546) |
| `apiPrepareGroup` | `rh, connLink, groupShortLinkData` | Prepare a group chat from a short link (before connecting) | [L1555](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1555) |
| `apiConnectPreparedContact` | `rh, contactId, incognito, msg` | Connect to a previously prepared contact | [L1580](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1580) |
| `apiConnectPreparedGroup` | `rh, groupId, incognito, msg` | Join a previously prepared group | [L1590](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1590) |
| `apiConnectContactViaAddress` | `rh: Long?, incognito: Boolean, contactId: Long` | Connect to a contact using their public address | [L1600](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1600) |
| `apiDeleteContact` | `rh: Long?, id: Long, chatDeleteMode: ChatDeleteMode` | Delete a contact and return the deleted Contact | [L1644](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1644) |
| `apiContactInfo` | `rh: Long?, contactId: Long` | Get a contact's connection stats and custom profile | [L1346](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1346) |
| `apiSetContactAlias` | `rh: Long?, contactId: Long, localAlias: String` | Set a local display alias for a contact | [L1711](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1711) |
| `apiSetConnectionAlias` | `rh: Long?, connId: Long, localAlias: String` | Set a local display alias for a pending connection | [L1725](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1725) |
| `apiSetContactPrefs` | `rh: Long?, contactId: Long, prefs: ChatPreferences` | Update feature preferences for a contact | [L1704](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1704) |
| `apiCreateUserAddress` | `rh: Long?` | Create a long-term public contact address | [L1746](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1746) |
| `apiDeleteUserAddress` | `rh: Long?` | Delete the user's public contact address | [L1762](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1762) |
| `apiAddMyAddressShortLink` | `rh: Long?` | Create a short link for the user's address | [L1784](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1784) |
| `apiSetUserAddressSettings` | `rh: Long?, settings: AddressSettings` | Configure auto-accept for incoming contact requests | [L1795](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1795) |
| `apiAcceptContactRequest` | `rh: Long?, incognito: Boolean, contactReqId: Long` | Accept an incoming contact request | [L1809](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1809) |
| `apiRejectContactRequest` | `rh: Long?, contactReqId: Long` | Reject an incoming contact request | [L1832](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1832) |
| `apiSwitchContact` | `rh: Long?, contactId: Long` | Initiate SMP server switch for a contact | [L1374](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1374) |
| `apiAbortSwitchContact` | `rh: Long?, contactId: Long` | Abort an in-progress server switch | [L1388](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1388) |
| `apiSyncContactRatchet` | `rh: Long?, contactId: Long, force: Boolean` | Force ratchet synchronization with a contact | [L1402](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1402) |
| `apiGetContactCode` | `rh: Long?, contactId: Long` | Get the security verification code for a contact | [L1416](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1416) |
| `apiVerifyContact` | `rh: Long?, contactId: Long, connectionCode: String?` | Verify a contact's security code | [L1430](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1430) |

### 2.6 File Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `receiveFiles` | `rhId, user, fileIds, userApprovedRelays, auto` | Accept and download one or more files (handles relay approval) | [L1946](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1946) |
| `receiveFile` | `rhId, user, fileId, userApprovedRelays, auto` | Accept and download a single file (convenience wrapper) | [L2062](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2062) |
| `cancelFile` | `rh: Long?, user: User, fileId: Long` | Cancel an in-progress file transfer and clean up | [L2072](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2072) |
| `apiCancelFile` | `rh: Long?, fileId: Long, ctrl: ChatCtrl?` | Cancel a file transfer (low-level, returns updated chat item) | [L2080](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2080) |
| `uploadStandaloneFile` | `user: UserLike, file: CryptoFile, ctrl: ChatCtrl?` | Upload a standalone file (used for migration) | [L1916](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1916) |
| `downloadStandaloneFile` | `user: UserLike, url: String, file: CryptoFile, ctrl: ChatCtrl?` | Download a standalone file by URL | [L1926](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1926) |
| `standaloneFileInfo` | `url: String, ctrl: ChatCtrl?` | Retrieve metadata for a standalone file link | [L1936](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1936) |

### 2.7 Call Operations

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiGetCallInvitations` | `rh: Long?` | Retrieve pending call invitations | [L1842](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1842) |
| `apiSendCallInvitation` | `rh: Long?, contact: Contact, callType: CallType` | Initiate a call by sending an invitation | [L1849](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1849) |
| `apiRejectCall` | `rh: Long?, contact: Contact` | Reject an incoming call | [L1854](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1854) |
| `apiSendCallOffer` | `rh, contact, rtcSession, rtcIceCandidates, media, capabilities` | Send a WebRTC call offer | [L1859](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1859) |
| `apiSendCallAnswer` | `rh: Long?, contact: Contact, rtcSession: String, rtcIceCandidates: String` | Send a WebRTC call answer | [L1866](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1866) |
| `apiSendCallExtraInfo` | `rh: Long?, contact: Contact, rtcIceCandidates: String` | Send additional ICE candidates during a call | [L1872](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1872) |
| `apiEndCall` | `rh: Long?, contact: Contact` | End an active call | [L1878](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1878) |
| `apiCallStatus` | `rh: Long?, contact: Contact, status: WebRTCCallStatus` | Report call status updates to the core | [L1883](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1883) |

### 2.8 Settings & Network

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiSetNetworkConfig` | `cfg: NetCfg, showAlertOnError: Boolean, ctrl: ChatCtrl?` | Apply network configuration (SOCKS proxy, timeouts, etc.) | [L1313](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1313) |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Update network reachability information | [L1340](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1340) |
| `apiSetSettings` | `rh: Long?, type: ChatType, id: Long, settings: ChatSettings` | Update per-chat settings (notifications, favorites) | [L1333](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1333) |
| `apiStorageEncryption` | `currentKey: String, newKey: String` | Change the database encryption passphrase | [L999](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L999) |
| `testStorageEncryption` | `key: String, ctrl: ChatCtrl?` | Verify a database encryption key is correct | [L1006](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1006) |
| `testProtoServer` | `rh: Long?, server: String` | Test connectivity to a protocol server | [L1211](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1211) |
| `reconnectServer` | `rh: Long?, server: String` | Reconnect to a specific server | [L1326](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1326) |
| `reconnectAllServers` | `rh: Long?` | Reconnect to all servers | [L1331](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1331) |
| `apiSetChatUIThemes` | `rh: Long?, chatId: ChatId, themes: ThemeModeOverrides?` | Set per-chat UI theme overrides | [L1739](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1739) |
| `apiContactQueueInfo` | `rh: Long?, contactId: Long` | Get server queue diagnostics for a contact | [L1360](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1360) |
| `apiGroupMemberQueueInfo` | `rh: Long?, groupId: Long, groupMemberId: Long` | Get server queue diagnostics for a group member | [L1367](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1367) |

### 2.9 Chat Tags

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiCreateChatTag` | `rh: Long?, tag: ChatTagData` | Create a new chat tag (folder/label) | [L1052](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1052) |
| `apiSetChatTags` | `rh: Long?, type: ChatType, id: Long, tagIds: List<Long>` | Assign tags to a chat | [L1060](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1060) |
| `apiDeleteChatTag` | `rh: Long?, tagId: Long` | Delete a chat tag | [L1068](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1068) |
| `apiUpdateChatTag` | `rh: Long?, tagId: Long, tag: ChatTagData` | Update a chat tag's name or emoji | [L1070](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1070) |
| `apiReorderChatTags` | `rh: Long?, tagIds: List<Long>` | Set the display order of chat tags | [L1072](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1072) |

### 2.10 Server Operators

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `getServerOperators` | `rh: Long?` | Get server operator conditions detail | [L1219](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1219) |
| `setServerOperators` | `rh: Long?, operators: List<ServerOperator>` | Update the list of server operators | [L1226](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1226) |
| `getUserServers` | `rh: Long?` | Get the user's configured servers per operator | [L1233](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1233) |
| `setUserServers` | `rh: Long?, userServers: List<UserOperatorServers>` | Save user's configured servers per operator | [L1241](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1241) |
| `validateServers` | `rh: Long?, userServers: List<UserOperatorServers>` | Validate server configuration for errors | [L1253](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1253) |
| `getUsageConditions` | `rh: Long?` | Get current and accepted usage conditions | [L1261](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1261) |
| `setConditionsNotified` | `rh: Long?, conditionsId: Long` | Mark conditions as shown to user | [L1268](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1268) |
| `acceptConditions` | `rh: Long?, conditionsId: Long, operatorIds: List<Long>` | Accept usage conditions for operators | [L1275](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1275) |

### 2.11 Archive

| Command | Parameters | Description | Line |
|---------|-----------|-------------|------|
| `apiExportArchive` | `config: ArchiveConfig` | Export chat database to a ZIP archive | [L981](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L981) |
| `apiImportArchive` | `config: ArchiveConfig` | Import chat database from a ZIP archive | [L987](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L987) |
| `apiDeleteStorage` | _(none)_ | Delete all chat database storage | [L993](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L993) |

<a id="ArchiveConfig"></a>

`ArchiveConfig` ([SimpleXAPI.kt#L4162](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L4162)):

```kotlin
class ArchiveConfig(
  val archivePath: String,
  val disableCompression: Boolean? = null,
  val parentTempDirectory: String? = null
)
```

---

<a id="API"></a>

## 3. Response Types

All command responses are deserialized into the `API` sealed class ([SimpleXAPI.kt#L5975](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L5975)):

```kotlin
sealed class API {
  class Result(val remoteHostId: Long?, val res: CR) : API()
  class Error(val remoteHostId: Long?, val err: ChatError) : API()
}
```

<a id="CR"></a>

The `CR` sealed class ([SimpleXAPI.kt#L6114](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6114)) contains approximately 180 response variants. Key categories:

| Category | Examples | Lines |
|----------|---------|-------|
| User | `ActiveUser`, `UsersList`, `UserPrivacy`, `UserProfileUpdated` | L6104-L6157 |
| Chat state | `ChatStarted`, `ChatRunning`, `ChatStopped`, `ApiChats`, `ApiChat` | L6106-L6110 |
| Tags | `ChatTags`, `TagsUpdated` | L6112, L6137 |
| Contacts | `Invitation`, `SentConfirmation`, `SentInvitation`, `ContactConnected`, `ContactDeleted` | L6138-L6165 |
| Messages | `NewChatItems`, `ChatItemUpdated`, `ChatItemsDeleted`, `ChatItemReaction`, `ForwardPlan` | L6176-L6184 |
| Groups | `GroupCreated`, `SentGroupInvitation`, `UserAcceptedGroupSent`, `GroupUpdated`, `GroupMembers` | L6186-L6219 |
| Files (receive) | `RcvFileAccepted`, `RcvFileStart`, `RcvFileComplete`, `RcvFileCancelled`, `RcvFileError` | L6221-L6232 |
| Files (send) | `SndFileStart`, `SndFileComplete`, `SndFileCancelled`, `SndFileCompleteXFTP` | L6234-L6244 |
| Calls | `CallInvitation`, `CallOffer`, `CallAnswer`, `CallExtraInfo`, `CallEnded` | L6246-L6251 |
| Remote host | `RemoteHostList`, `RemoteHostStarted`, `RemoteHostConnected`, `RemoteHostStopped` | L6255-L6262 |
| Remote ctrl | `RemoteCtrlList`, `RemoteCtrlFound`, `RemoteCtrlConnected`, `RemoteCtrlStopped` | L6264-L6269 |
| Encryption | `ContactPQAllowed`, `ContactPQEnabled` | L6271-L6272 |
| Misc | `CmdOk`, `ArchiveExported`, `ArchiveImported`, `AppSettingsR`, `VersionInfo` | L6274-L6283 |
| Fallback | `Response` (unknown type + raw JSON), `Invalid` (unparseable) | L6282-L6283 |

Each `CR` subclass is annotated with `@Serializable @SerialName("jsonTag")` for polymorphic JSON deserialization.

---

## 4. Event Types

The chat core pushes asynchronous events through the same `CR` type hierarchy. The `startReceiver` coroutine ([SimpleXAPI.kt#L660](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L660)) continuously calls `chatRecvMsgWait` (blocking JNI), then dispatches each message to `processReceivedMsg` ([SimpleXAPI.kt#L2568](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2568)).

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

### ChatError ([SimpleXAPI.kt#L6974](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L6974))

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

### ChatErrorType ([SimpleXAPI.kt#L7004](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7004))

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

### StoreError ([SimpleXAPI.kt#L7168](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7168))

Database-level errors: `DuplicateName`, `UserNotFound`, `GroupNotFound`, `ChatItemNotFound`, `LargeMsg`, `UserContactLinkNotFound`, etc.

### ArchiveError ([SimpleXAPI.kt#L7658](common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7658))

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

All paths are relative to `apps/multiplatform/`.
