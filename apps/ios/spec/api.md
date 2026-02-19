# SimpleX Chat iOS -- Chat API Reference

> Complete specification of the ChatCommand, ChatResponse, ChatEvent, and ChatError types that form the API between the Swift UI layer and the Haskell core.
>
> Related specs: [Architecture](architecture.md) | [State Management](state.md) | [README](README.md)
> Related product: [Concept Index](../product/concepts.md)

**Source:** [`AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift) | [`SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) | [`APITypes.swift`](../SimpleXChat/APITypes.swift) | [`API.swift`](../SimpleXChat/API.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Command Categories (ChatCommand)](#2-command-categories)
3. [Response Types (ChatResponse)](#3-response-types)
4. [Event Types (ChatEvent)](#4-event-types)
5. [Error Types (ChatError)](#5-error-types)
6. [FFI Bridge Functions](#6-ffi-bridge-functions)
7. [Result Type (APIResult)](#7-result-type)

---

## 1. Overview

The iOS app communicates with the Haskell core exclusively through a command/response protocol:

1. Swift constructs a `ChatCommand` enum value
2. The command's `cmdString` property serializes it to a text command
3. The FFI bridge sends the string to Haskell via `chat_send_cmd_retry`
4. Haskell returns a JSON response, decoded as `APIResult<ChatResponse>`
5. Async events arrive separately via `chat_recv_msg_wait`, decoded as `ChatEvent`

**Source files**:
- [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift) -- `ChatCommand` ([L15](../Shared/Model/AppAPITypes.swift#L15)), `ChatResponse0` ([L654](../Shared/Model/AppAPITypes.swift#L654)), `ChatResponse1` ([L776](../Shared/Model/AppAPITypes.swift#L776)), `ChatResponse2` ([L916](../Shared/Model/AppAPITypes.swift#L916)), `ChatEvent` ([L1063](../Shared/Model/AppAPITypes.swift#L1063)) enums
- [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift) -- `APIResult<R>` ([L27](../SimpleXChat/APITypes.swift#L27)), `ChatAPIResult` ([L65](../SimpleXChat/APITypes.swift#L65)), `ChatError` ([L699](../SimpleXChat/APITypes.swift#L699))
- [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) -- FFI bridge functions (`chatSendCmd` [L121](../Shared/Model/SimpleXAPI.swift#L121), `chatRecvMsg` [L237](../Shared/Model/SimpleXAPI.swift#L237))
- [`SimpleXChat/API.swift`](../SimpleXChat/API.swift) -- Low-level FFI (`sendSimpleXCmd` [L114](../SimpleXChat/API.swift#L115), `recvSimpleXMsg` [L136](../SimpleXChat/API.swift#L137))
- `SimpleXChat/ChatTypes.swift` -- Data types used in commands/responses (User, Contact, GroupInfo, ChatItem, etc.)
- `../../src/Simplex/Chat/Controller.hs` -- Haskell controller (function `chat_send_cmd_retry`, `chat_recv_msg_wait`)

---

## 2. Command Categories

The `ChatCommand` enum ([`AppAPITypes.swift` L15](../Shared/Model/AppAPITypes.swift#L15)) contains all commands the iOS app can send to the Haskell core. Commands are organized below by functional area.

### 2.1 User Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `showActiveUser` | -- | Get current active user | [L15](../Shared/Model/AppAPITypes.swift#L16) |
| `createActiveUser` | `profile: Profile?, pastTimestamp: Bool` | Create new user profile | [L16](../Shared/Model/AppAPITypes.swift#L17) |
| `listUsers` | -- | List all user profiles | [L17](../Shared/Model/AppAPITypes.swift#L18) |
| `apiSetActiveUser` | `userId: Int64, viewPwd: String?` | Switch active user | [L18](../Shared/Model/AppAPITypes.swift#L19) |
| `apiHideUser` | `userId: Int64, viewPwd: String` | Hide user behind password | [L23](../Shared/Model/AppAPITypes.swift#L24) |
| `apiUnhideUser` | `userId: Int64, viewPwd: String` | Unhide hidden user | [L24](../Shared/Model/AppAPITypes.swift#L25) |
| `apiMuteUser` | `userId: Int64` | Mute notifications for user | [L25](../Shared/Model/AppAPITypes.swift#L26) |
| `apiUnmuteUser` | `userId: Int64` | Unmute notifications for user | [L26](../Shared/Model/AppAPITypes.swift#L27) |
| `apiDeleteUser` | `userId: Int64, delSMPQueues: Bool, viewPwd: String?` | Delete user profile | [L27](../Shared/Model/AppAPITypes.swift#L28) |
| `apiUpdateProfile` | `userId: Int64, profile: Profile` | Update user display name/image | [L140](../Shared/Model/AppAPITypes.swift#L140) |
| `setAllContactReceipts` | `enable: Bool` | Set delivery receipts for all contacts | [L19](../Shared/Model/AppAPITypes.swift#L20) |
| `apiSetUserContactReceipts` | `userId: Int64, userMsgReceiptSettings` | Per-user contact receipt settings | [L20](../Shared/Model/AppAPITypes.swift#L21) |
| `apiSetUserGroupReceipts` | `userId: Int64, userMsgReceiptSettings` | Per-user group receipt settings | [L21](../Shared/Model/AppAPITypes.swift#L22) |
| `apiSetUserAutoAcceptMemberContacts` | `userId: Int64, enable: Bool` | Auto-accept group member contacts | [L22](../Shared/Model/AppAPITypes.swift#L23) |

### 2.2 Chat Lifecycle Control

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `startChat` | `mainApp: Bool, enableSndFiles: Bool` | Start chat engine | [L28](../Shared/Model/AppAPITypes.swift#L29) |
| `checkChatRunning` | -- | Check if chat is running | [L29](../Shared/Model/AppAPITypes.swift#L30) |
| `apiStopChat` | -- | Stop chat engine | [L30](../Shared/Model/AppAPITypes.swift#L31) |
| `apiActivateChat` | `restoreChat: Bool` | Resume from background | [L31](../Shared/Model/AppAPITypes.swift#L32) |
| `apiSuspendChat` | `timeoutMicroseconds: Int` | Suspend for background | [L32](../Shared/Model/AppAPITypes.swift#L33) |
| `apiSetAppFilePaths` | `filesFolder, tempFolder, assetsFolder` | Set file storage paths | [L33](../Shared/Model/AppAPITypes.swift#L34) |
| `apiSetEncryptLocalFiles` | `enable: Bool` | Toggle local file encryption | [L34](../Shared/Model/AppAPITypes.swift#L35) |

### 2.3 Chat & Message Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetChats` | `userId: Int64` | Get all chat previews for user | [L43](../Shared/Model/AppAPITypes.swift#L44) |
| `apiGetChat` | `chatId, scope, contentTag, pagination, search` | Get messages for a chat | [L44](../Shared/Model/AppAPITypes.swift#L45) |
| `apiGetChatContentTypes` | `chatId, scope` | Get content type counts for a chat | [L45](../Shared/Model/AppAPITypes.swift#L46) |
| `apiGetChatItemInfo` | `type, id, scope, itemId` | Get detailed info for a message | [L46](../Shared/Model/AppAPITypes.swift#L47) |
| `apiSendMessages` | `type, id, scope, sendAsGroup, live, ttl, composedMessages` | Send one or more messages; `sendAsGroup` sends as channel owner | [L48](../Shared/Model/AppAPITypes.swift#L48) |
| `apiCreateChatItems` | `noteFolderId, composedMessages` | Create items in notes folder | [L53](../Shared/Model/AppAPITypes.swift#L54) |
| `apiUpdateChatItem` | `type, id, scope, itemId, updatedMessage, live` | Edit a sent message | [L55](../Shared/Model/AppAPITypes.swift#L56) |
| `apiDeleteChatItem` | `type, id, scope, itemIds, mode` | Delete messages | [L56](../Shared/Model/AppAPITypes.swift#L57) |
| `apiDeleteMemberChatItem` | `groupId, itemIds` | Moderate group messages | [L57](../Shared/Model/AppAPITypes.swift#L58) |
| `apiChatItemReaction` | `type, id, scope, itemId, add, reaction` | Add/remove emoji reaction | [L60](../Shared/Model/AppAPITypes.swift#L61) |
| `apiGetReactionMembers` | `userId, groupId, itemId, reaction` | Get who reacted | [L61](../Shared/Model/AppAPITypes.swift#L62) |
| `apiPlanForwardChatItems` | `fromChatType, fromChatId, fromScope, itemIds` | Plan message forwarding | [L62](../Shared/Model/AppAPITypes.swift#L63) |
| `apiForwardChatItems` | `toChatType, toChatId, toScope, sendAsGroup, from..., itemIds, ttl` | Forward messages; `sendAsGroup` forwards as channel owner | [L64](../Shared/Model/AppAPITypes.swift#L64) |
| `apiReportMessage` | `groupId, chatItemId, reportReason, reportText` | Report group message | [L55](../Shared/Model/AppAPITypes.swift#L55) |
| `apiChatRead` | `type, id, scope` | Mark entire chat as read | [L165](../Shared/Model/AppAPITypes.swift#L165) |
| `apiChatItemsRead` | `type, id, scope, itemIds` | Mark specific items as read | [L166](../Shared/Model/AppAPITypes.swift#L166) |
| `apiChatUnread` | `type, id, unreadChat` | Toggle unread badge | [L167](../Shared/Model/AppAPITypes.swift#L167) |

### 2.4 Contact Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiAddContact` | `userId, incognito` | Create invitation link | [L125](../Shared/Model/AppAPITypes.swift#L125) |
| `apiConnect` | `userId, incognito, connLink` | Connect via link | [L135](../Shared/Model/AppAPITypes.swift#L135) |
| `apiConnectPlan` | `userId, connLink` | Plan connection (preview) | [L128](../Shared/Model/AppAPITypes.swift#L128) |
| `apiPrepareContact` | `userId, connLink, contactShortLinkData` | Prepare contact from link | [L129](../Shared/Model/AppAPITypes.swift#L129) |
| `apiPrepareGroup` | `userId, connLink, directLink, groupShortLinkData` | Prepare group from link; `directLink` indicates whether link is a direct (non-relay) group link | [L130](../Shared/Model/AppAPITypes.swift#L130) |
| `apiConnectPreparedContact` | `contactId, incognito, msg` | Connect prepared contact | [L133](../Shared/Model/AppAPITypes.swift#L133) |
| `apiConnectContactViaAddress` | `userId, incognito, contactId` | Connect via address | [L136](../Shared/Model/AppAPITypes.swift#L136) |
| `apiAcceptContact` | `incognito, contactReqId` | Accept contact request | [L153](../Shared/Model/AppAPITypes.swift#L153) |
| `apiRejectContact` | `contactReqId` | Reject contact request | [L154](../Shared/Model/AppAPITypes.swift#L154) |
| `apiDeleteChat` | `type, id, chatDeleteMode` | Delete conversation | [L137](../Shared/Model/AppAPITypes.swift#L137) |
| `apiClearChat` | `type, id` | Clear conversation history | [L138](../Shared/Model/AppAPITypes.swift#L138) |
| `apiListContacts` | `userId` | List all contacts | [L139](../Shared/Model/AppAPITypes.swift#L139) |
| `apiSetContactPrefs` | `contactId, preferences` | Set contact preferences | [L141](../Shared/Model/AppAPITypes.swift#L141) |
| `apiSetContactAlias` | `contactId, localAlias` | Set local alias | [L142](../Shared/Model/AppAPITypes.swift#L142) |
| `apiSetConnectionAlias` | `connId, localAlias` | Set pending connection alias | [L144](../Shared/Model/AppAPITypes.swift#L144) |
| `apiContactInfo` | `contactId` | Get contact info + connection stats | [L111](../Shared/Model/AppAPITypes.swift#L111) |
| `apiSetConnectionIncognito` | `connId, incognito` | Toggle incognito on pending connection | [L126](../Shared/Model/AppAPITypes.swift#L126) |

### 2.5 Group Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiNewGroup` | `userId, incognito, groupProfile` | Create new group | [L72](../Shared/Model/AppAPITypes.swift#L72) |
| `apiNewPublicGroup` | `userId, incognito, relayIds, groupProfile` | Create new public group (channel) with chat relays | [L73](../Shared/Model/AppAPITypes.swift#L73) |
| `apiAddMember` | `groupId, contactId, memberRole` | Invite contact to group | [L74](../Shared/Model/AppAPITypes.swift#L74) |
| `apiJoinGroup` | `groupId` | Accept group invitation | [L75](../Shared/Model/AppAPITypes.swift#L75) |
| `apiAcceptMember` | `groupId, groupMemberId, memberRole` | Accept member (knocking) | [L76](../Shared/Model/AppAPITypes.swift#L76) |
| `apiRemoveMembers` | `groupId, memberIds, withMessages` | Remove members | [L80](../Shared/Model/AppAPITypes.swift#L80) |
| `apiLeaveGroup` | `groupId` | Leave group | [L81](../Shared/Model/AppAPITypes.swift#L81) |
| `apiListMembers` | `groupId` | List group members | [L82](../Shared/Model/AppAPITypes.swift#L82) |
| `apiUpdateGroupProfile` | `groupId, groupProfile` | Update group name/image/description | [L83](../Shared/Model/AppAPITypes.swift#L83) |
| `apiMembersRole` | `groupId, memberIds, memberRole` | Change member roles | [L78](../Shared/Model/AppAPITypes.swift#L78) |
| `apiBlockMembersForAll` | `groupId, memberIds, blocked` | Block members for all | [L79](../Shared/Model/AppAPITypes.swift#L79) |
| `apiCreateGroupLink` | `groupId, memberRole` | Create shareable group link | [L84](../Shared/Model/AppAPITypes.swift#L84) |
| `apiGroupLinkMemberRole` | `groupId, memberRole` | Change group link default role | [L85](../Shared/Model/AppAPITypes.swift#L85) |
| `apiDeleteGroupLink` | `groupId` | Delete group link | [L86](../Shared/Model/AppAPITypes.swift#L86) |
| `apiGetGroupLink` | `groupId` | Get existing group link | [L87](../Shared/Model/AppAPITypes.swift#L87) |
| `apiAddGroupShortLink` | `groupId` | Add short link to group | [L88](../Shared/Model/AppAPITypes.swift#L88) |
| `apiCreateMemberContact` | `groupId, groupMemberId` | Create direct contact from group member | [L89](../Shared/Model/AppAPITypes.swift#L89) |
| `apiSendMemberContactInvitation` | `contactId, msg` | Send contact invitation to member | [L90](../Shared/Model/AppAPITypes.swift#L90) |
| `apiGroupMemberInfo` | `groupId, groupMemberId` | Get member info + connection stats | [L112](../Shared/Model/AppAPITypes.swift#L112) |
| `apiDeleteMemberSupportChat` | `groupId, groupMemberId` | Delete member support chat | [L77](../Shared/Model/AppAPITypes.swift#L77) |
| `apiSetMemberSettings` | `groupId, groupMemberId, memberSettings` | Set per-member settings | [L110](../Shared/Model/AppAPITypes.swift#L110) |
| `apiSetGroupAlias` | `groupId, localAlias` | Set local group alias | [L143](../Shared/Model/AppAPITypes.swift#L143) |

### 2.6 Chat Tags

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetChatTags` | `userId` | Get all user tags | [L43](../Shared/Model/AppAPITypes.swift#L43) |
| `apiCreateChatTag` | `tag: ChatTagData` | Create a new tag | [L49](../Shared/Model/AppAPITypes.swift#L49) |
| `apiSetChatTags` | `type, id, tagIds` | Assign tags to a chat | [L50](../Shared/Model/AppAPITypes.swift#L50) |
| `apiDeleteChatTag` | `tagId` | Delete a tag | [L51](../Shared/Model/AppAPITypes.swift#L51) |
| `apiUpdateChatTag` | `tagId, tagData` | Update tag name/emoji | [L52](../Shared/Model/AppAPITypes.swift#L52) |
| `apiReorderChatTags` | `tagIds` | Reorder tags | [L53](../Shared/Model/AppAPITypes.swift#L53) |

### 2.7 File Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `receiveFile` | `fileId, userApprovedRelays, encrypted, inline` | Accept and download file | [L168](../Shared/Model/AppAPITypes.swift#L168) |
| `setFileToReceive` | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive | [L169](../Shared/Model/AppAPITypes.swift#L169) |
| `cancelFile` | `fileId` | Cancel file transfer | [L170](../Shared/Model/AppAPITypes.swift#L170) |
| `apiUploadStandaloneFile` | `userId, file: CryptoFile` | Upload file to XFTP (no chat) | [L180](../Shared/Model/AppAPITypes.swift#L180) |
| `apiDownloadStandaloneFile` | `userId, url, file: CryptoFile` | Download from XFTP URL | [L181](../Shared/Model/AppAPITypes.swift#L181) |
| `apiStandaloneFileInfo` | `url` | Get file metadata from XFTP URL | [L182](../Shared/Model/AppAPITypes.swift#L182) |

### 2.8 WebRTC Call Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSendCallInvitation` | `contact, callType` | Initiate call | [L156](../Shared/Model/AppAPITypes.swift#L156) |
| `apiRejectCall` | `contact` | Reject incoming call | [L157](../Shared/Model/AppAPITypes.swift#L157) |
| `apiSendCallOffer` | `contact, callOffer: WebRTCCallOffer` | Send SDP offer | [L158](../Shared/Model/AppAPITypes.swift#L158) |
| `apiSendCallAnswer` | `contact, answer: WebRTCSession` | Send SDP answer | [L159](../Shared/Model/AppAPITypes.swift#L159) |
| `apiSendCallExtraInfo` | `contact, extraInfo: WebRTCExtraInfo` | Send ICE candidates | [L160](../Shared/Model/AppAPITypes.swift#L160) |
| `apiEndCall` | `contact` | End active call | [L161](../Shared/Model/AppAPITypes.swift#L161) |
| `apiGetCallInvitations` | -- | Get pending call invitations | [L162](../Shared/Model/AppAPITypes.swift#L162) |
| `apiCallStatus` | `contact, callStatus` | Report call status change | [L163](../Shared/Model/AppAPITypes.swift#L163) |

### 2.9 Push Notifications

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetNtfToken` | -- | Get current notification token | [L65](../Shared/Model/AppAPITypes.swift#L65) |
| `apiRegisterToken` | `token, notificationMode` | Register device token with server | [L66](../Shared/Model/AppAPITypes.swift#L66) |
| `apiVerifyToken` | `token, nonce, code` | Verify token registration | [L67](../Shared/Model/AppAPITypes.swift#L67) |
| `apiCheckToken` | `token` | Check token status | [L68](../Shared/Model/AppAPITypes.swift#L68) |
| `apiDeleteToken` | `token` | Unregister token | [L69](../Shared/Model/AppAPITypes.swift#L69) |
| `apiGetNtfConns` | `nonce, encNtfInfo` | Get notification connections (NSE) | [L70](../Shared/Model/AppAPITypes.swift#L70) |
| `apiGetConnNtfMessages` | `connMsgReqs` | Get notification messages (NSE) | [L71](../Shared/Model/AppAPITypes.swift#L71) |

### 2.10 Settings & Configuration

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSaveSettings` | `settings: AppSettings` | Save app settings to core | [L41](../Shared/Model/AppAPITypes.swift#L41) |
| `apiGetSettings` | `settings: AppSettings` | Get settings from core | [L42](../Shared/Model/AppAPITypes.swift#L42) |
| `apiSetChatSettings` | `type, id, chatSettings` | Per-chat notification settings | [L109](../Shared/Model/AppAPITypes.swift#L109) |
| `apiSetChatItemTTL` | `userId, seconds` | Set global message TTL | [L101](../Shared/Model/AppAPITypes.swift#L101) |
| `apiGetChatItemTTL` | `userId` | Get global message TTL | [L102](../Shared/Model/AppAPITypes.swift#L102) |
| `apiSetChatTTL` | `userId, type, id, seconds` | Per-chat message TTL | [L103](../Shared/Model/AppAPITypes.swift#L103) |
| `apiSetNetworkConfig` | `networkConfig: NetCfg` | Set network configuration | [L104](../Shared/Model/AppAPITypes.swift#L104) |
| `apiGetNetworkConfig` | -- | Get network configuration | [L105](../Shared/Model/AppAPITypes.swift#L105) |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Set network type/status | [L106](../Shared/Model/AppAPITypes.swift#L106) |
| `reconnectAllServers` | -- | Force reconnect all servers | [L107](../Shared/Model/AppAPITypes.swift#L107) |
| `reconnectServer` | `userId, smpServer` | Reconnect specific server | [L108](../Shared/Model/AppAPITypes.swift#L108) |

### 2.11 Database & Storage

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiStorageEncryption` | `config: DBEncryptionConfig` | Set/change database encryption | [L39](../Shared/Model/AppAPITypes.swift#L39) |
| `testStorageEncryption` | `key: String` | Test encryption key | [L40](../Shared/Model/AppAPITypes.swift#L40) |
| `apiExportArchive` | `config: ArchiveConfig` | Export database archive | [L36](../Shared/Model/AppAPITypes.swift#L36) |
| `apiImportArchive` | `config: ArchiveConfig` | Import database archive | [L37](../Shared/Model/AppAPITypes.swift#L37) |
| `apiDeleteStorage` | -- | Delete all storage | [L38](../Shared/Model/AppAPITypes.swift#L38) |

### 2.12 Server Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetServerOperators` | -- | Get server operators | [L93](../Shared/Model/AppAPITypes.swift#L93) |
| `apiSetServerOperators` | `operators` | Set server operators | [L94](../Shared/Model/AppAPITypes.swift#L94) |
| `apiGetUserServers` | `userId` | Get user's configured servers | [L95](../Shared/Model/AppAPITypes.swift#L95) |
| `apiSetUserServers` | `userId, userServers` | Set user's servers | [L96](../Shared/Model/AppAPITypes.swift#L96) |
| `apiValidateServers` | `userId, userServers` | Validate server configuration; returns errors and warnings | [L97](../Shared/Model/AppAPITypes.swift#L97) |
| `apiGetUsageConditions` | -- | Get usage conditions | [L98](../Shared/Model/AppAPITypes.swift#L98) |
| `apiAcceptConditions` | `conditionsId, operatorIds` | Accept usage conditions | [L100](../Shared/Model/AppAPITypes.swift#L100) |
| `apiTestProtoServer` | `userId, server` | Test server connectivity | [L92](../Shared/Model/AppAPITypes.swift#L92) |

### 2.13 Theme & UI

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSetUserUIThemes` | `userId, themes: ThemeModeOverrides?` | Set per-user theme | [L145](../Shared/Model/AppAPITypes.swift#L145) |
| `apiSetChatUIThemes` | `chatId, themes: ThemeModeOverrides?` | Set per-chat theme | [L146](../Shared/Model/AppAPITypes.swift#L146) |

### 2.14 Remote Desktop

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `setLocalDeviceName` | `displayName` | Set device name for pairing | [L172](../Shared/Model/AppAPITypes.swift#L172) |
| `connectRemoteCtrl` | `xrcpInvitation` | Connect to desktop via QR code | [L173](../Shared/Model/AppAPITypes.swift#L173) |
| `findKnownRemoteCtrl` | -- | Find previously paired desktops | [L174](../Shared/Model/AppAPITypes.swift#L174) |
| `confirmRemoteCtrl` | `remoteCtrlId` | Confirm known remote controller | [L175](../Shared/Model/AppAPITypes.swift#L175) |
| `verifyRemoteCtrlSession` | `sessionCode` | Verify session code | [L176](../Shared/Model/AppAPITypes.swift#L176) |
| `listRemoteCtrls` | -- | List known remote controllers | [L177](../Shared/Model/AppAPITypes.swift#L177) |
| `stopRemoteCtrl` | -- | Stop remote session | [L178](../Shared/Model/AppAPITypes.swift#L178) |
| `deleteRemoteCtrl` | `remoteCtrlId` | Delete known controller | [L179](../Shared/Model/AppAPITypes.swift#L179) |

### 2.15 Diagnostics

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `showVersion` | -- | Get core version info | [L184](../Shared/Model/AppAPITypes.swift#L184) |
| `getAgentSubsTotal` | `userId` | Get total SMP subscriptions | [L185](../Shared/Model/AppAPITypes.swift#L185) |
| `getAgentServersSummary` | `userId` | Get server summary stats | [L186](../Shared/Model/AppAPITypes.swift#L186) |
| `resetAgentServersStats` | -- | Reset server statistics | [L187](../Shared/Model/AppAPITypes.swift#L187) |

### 2.16 Address Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiCreateMyAddress` | `userId` | Create SimpleX address | [L147](../Shared/Model/AppAPITypes.swift#L147) |
| `apiDeleteMyAddress` | `userId` | Delete SimpleX address | [L148](../Shared/Model/AppAPITypes.swift#L148) |
| `apiShowMyAddress` | `userId` | Show current address | [L149](../Shared/Model/AppAPITypes.swift#L149) |
| `apiAddMyAddressShortLink` | `userId` | Add short link to address | [L150](../Shared/Model/AppAPITypes.swift#L150) |
| `apiSetProfileAddress` | `userId, on: Bool` | Toggle address in profile | [L151](../Shared/Model/AppAPITypes.swift#L151) |
| `apiSetAddressSettings` | `userId, addressSettings` | Configure address settings | [L152](../Shared/Model/AppAPITypes.swift#L152) |

### 2.17 Connection Security

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetContactCode` | `contactId` | Get verification code | [L121](../Shared/Model/AppAPITypes.swift#L121) |
| `apiGetGroupMemberCode` | `groupId, groupMemberId` | Get member verification code | [L122](../Shared/Model/AppAPITypes.swift#L122) |
| `apiVerifyContact` | `contactId, connectionCode` | Verify contact identity | [L123](../Shared/Model/AppAPITypes.swift#L123) |
| `apiVerifyGroupMember` | `groupId, groupMemberId, connectionCode` | Verify group member identity | [L124](../Shared/Model/AppAPITypes.swift#L124) |
| `apiSwitchContact` | `contactId` | Switch contact connection (key rotation) | [L115](../Shared/Model/AppAPITypes.swift#L115) |
| `apiSwitchGroupMember` | `groupId, groupMemberId` | Switch group member connection | [L116](../Shared/Model/AppAPITypes.swift#L116) |
| `apiAbortSwitchContact` | `contactId` | Abort contact switch | [L117](../Shared/Model/AppAPITypes.swift#L117) |
| `apiAbortSwitchGroupMember` | `groupId, groupMemberId` | Abort member switch | [L118](../Shared/Model/AppAPITypes.swift#L118) |
| `apiSyncContactRatchet` | `contactId, force` | Sync double-ratchet state | [L119](../Shared/Model/AppAPITypes.swift#L119) |
| `apiSyncGroupMemberRatchet` | `groupId, groupMemberId, force` | Sync member ratchet | [L120](../Shared/Model/AppAPITypes.swift#L120) |

---

## 3. Response Types

Responses are split across three enums due to Swift enum size limitations:

### ChatResponse0

Synchronous query responses ([`AppAPITypes.swift` L654](../Shared/Model/AppAPITypes.swift#L654)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `activeUser` | `user: User` | Current active user | [L655](../Shared/Model/AppAPITypes.swift#L655) |
| `usersList` | `users: [UserInfo]` | All user profiles | [L656](../Shared/Model/AppAPITypes.swift#L656) |
| `chatStarted` | -- | Chat engine started | [L657](../Shared/Model/AppAPITypes.swift#L657) |
| `chatRunning` | -- | Chat is already running | [L658](../Shared/Model/AppAPITypes.swift#L658) |
| `chatStopped` | -- | Chat engine stopped | [L659](../Shared/Model/AppAPITypes.swift#L659) |
| `apiChats` | `user, chats: [ChatData]` | All chat previews | [L660](../Shared/Model/AppAPITypes.swift#L660) |
| `apiChat` | `user, chat: ChatData, navInfo` | Single chat with messages | [L661](../Shared/Model/AppAPITypes.swift#L661) |
| `chatTags` | `user, userTags: [ChatTag]` | User's chat tags | [L663](../Shared/Model/AppAPITypes.swift#L663) |
| `chatItemInfo` | `user, chatItem, chatItemInfo` | Message detail info | [L664](../Shared/Model/AppAPITypes.swift#L664) |
| `serverTestResult` | `user, testServer, testFailure` | Server test result | [L665](../Shared/Model/AppAPITypes.swift#L665) |
| `userServersValidation` | `user, serverErrors: [UserServersError], serverWarnings: [UserServersWarning]` | Server validation result with errors and warnings | [L668](../Shared/Model/AppAPITypes.swift#L668) |
| `networkConfig` | `networkConfig: NetCfg` | Current network config | [L671](../Shared/Model/AppAPITypes.swift#L671) |
| `contactInfo` | `user, contact, connectionStats, customUserProfile` | Contact details | [L672](../Shared/Model/AppAPITypes.swift#L672) |
| `groupMemberInfo` | `user, groupInfo, member, connectionStats` | Member details | [L673](../Shared/Model/AppAPITypes.swift#L673) |
| `connectionVerified` | `verified, expectedCode` | Verification result | [L683](../Shared/Model/AppAPITypes.swift#L683) |
| `tagsUpdated` | `user, userTags, chatTags` | Tags changed | [L684](../Shared/Model/AppAPITypes.swift#L684) |

### ChatResponse1

Contact, message, and profile responses ([`AppAPITypes.swift` L776](../Shared/Model/AppAPITypes.swift#L776)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `invitation` | `user, connLinkInvitation, connection` | Created invitation link | [L777](../Shared/Model/AppAPITypes.swift#L777) |
| `connectionPlan` | `user, connLink, connectionPlan` | Connection plan preview | [L780](../Shared/Model/AppAPITypes.swift#L780) |
| `newPreparedChat` | `user, chat: ChatData` | Prepared contact/group | [L781](../Shared/Model/AppAPITypes.swift#L781) |
| `contactDeleted` | `user, contact` | Contact deleted | [L790](../Shared/Model/AppAPITypes.swift#L790) |
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages sent/received | [L808](../Shared/Model/AppAPITypes.swift#L808) |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited | [L811](../Shared/Model/AppAPITypes.swift#L811) |
| `chatItemReaction` | `user, added, reaction` | Reaction change | [L813](../Shared/Model/AppAPITypes.swift#L813) |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted | [L815](../Shared/Model/AppAPITypes.swift#L815) |
| `contactsList` | `user, contacts: [Contact]` | All contacts list | [L816](../Shared/Model/AppAPITypes.swift#L816) |
| `userProfileUpdated` | `user, fromProfile, toProfile` | Profile changed | [L796](../Shared/Model/AppAPITypes.swift#L796) |
| `userContactLinkCreated` | `user, connLinkContact` | Address created | [L804](../Shared/Model/AppAPITypes.swift#L804) |
| `forwardPlan` | `user, chatItemIds, forwardConfirmation` | Forward plan result | [L810](../Shared/Model/AppAPITypes.swift#L810) |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted | [L809](../Shared/Model/AppAPITypes.swift#L809) |

### ChatResponse2

Group, file, call, notification, and misc responses ([`AppAPITypes.swift` L916](../Shared/Model/AppAPITypes.swift#L916)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `groupCreated` | `user, groupInfo` | New group created | [L918](../Shared/Model/AppAPITypes.swift#L918) |
| `publicGroupCreated` | `user, groupInfo, groupLink, groupRelays: [GroupRelay]` | New public group (channel) created with relay info | [L919](../Shared/Model/AppAPITypes.swift#L919) |
| `sentGroupInvitation` | `user, groupInfo, contact, member` | Group invitation sent | [L920](../Shared/Model/AppAPITypes.swift#L920) |
| `groupMembers` | `user, group: Group` | Group member list | [L924](../Shared/Model/AppAPITypes.swift#L924) |
| `membersRoleUser` | `user, groupInfo, members, toRole` | Role changed | [L928](../Shared/Model/AppAPITypes.swift#L928) |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile updated | [L930](../Shared/Model/AppAPITypes.swift#L930) |
| `groupLinkCreated` | `user, groupInfo, groupLink` | Group link created | [L931](../Shared/Model/AppAPITypes.swift#L931) |
| `rcvFileAccepted` | `user, chatItem` | File download started | [L938](../Shared/Model/AppAPITypes.swift#L938) |
| `callInvitations` | `callInvitations: [RcvCallInvitation]` | Pending calls | [L947](../Shared/Model/AppAPITypes.swift#L947) |
| `ntfToken` | `token, status, ntfMode, ntfServer` | Notification token info | [L950](../Shared/Model/AppAPITypes.swift#L950) |
| `versionInfo` | `versionInfo, chatMigrations, agentMigrations` | Core version | [L958](../Shared/Model/AppAPITypes.swift#L958) |
| `cmdOk` | `user_` | Generic success | [L959](../Shared/Model/AppAPITypes.swift#L959) |
| `archiveExported` | `archiveErrors: [ArchiveError]` | Export result | [L963](../Shared/Model/AppAPITypes.swift#L963) |
| `archiveImported` | `archiveErrors: [ArchiveError]` | Import result | [L964](../Shared/Model/AppAPITypes.swift#L964) |
| `appSettings` | `appSettings: AppSettings` | Retrieved settings | [L965](../Shared/Model/AppAPITypes.swift#L965) |

---

## 4. Event Types

The `ChatEvent` enum ([`AppAPITypes.swift` L1063](../Shared/Model/AppAPITypes.swift#L1063)) represents async events from the Haskell core. These arrive via `chat_recv_msg_wait` polling, not as responses to commands.

Event processing entry point: [`processReceivedMsg`](../Shared/Model/SimpleXAPI.swift#L2275) in `SimpleXAPI.swift`.

### Connection Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `contactConnected` | `user, contact, userCustomProfile` | Contact connection established | [L1070](../Shared/Model/AppAPITypes.swift#L1070) |
| `contactConnecting` | `user, contact` | Contact connecting in progress | [L1071](../Shared/Model/AppAPITypes.swift#L1071) |
| `contactSndReady` | `user, contact` | Ready to send to contact | [L1072](../Shared/Model/AppAPITypes.swift#L1072) |
| `contactDeletedByContact` | `user, contact` | Contact deleted by other party | [L1069](../Shared/Model/AppAPITypes.swift#L1069) |
| `contactUpdated` | `user, toContact` | Contact profile updated | [L1074](../Shared/Model/AppAPITypes.swift#L1074) |
| `receivedContactRequest` | `user, contactRequest, chat_` | Incoming contact request | [L1073](../Shared/Model/AppAPITypes.swift#L1073) |
| `subscriptionStatus` | `subscriptionStatus, connections` | Connection subscription change | [L1076](../Shared/Model/AppAPITypes.swift#L1076) |

### Message Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages received | [L1078](../Shared/Model/AppAPITypes.swift#L1078) |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited remotely | [L1080](../Shared/Model/AppAPITypes.swift#L1080) |
| `chatItemReaction` | `user, added, reaction: ACIReaction` | Reaction added/removed | [L1081](../Shared/Model/AppAPITypes.swift#L1081) |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted | [L1082](../Shared/Model/AppAPITypes.swift#L1082) |
| `chatItemsStatusesUpdated` | `user, chatItems: [AChatItem]` | Delivery status changed | [L1079](../Shared/Model/AppAPITypes.swift#L1079) |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted | [L1084](../Shared/Model/AppAPITypes.swift#L1084) |
| `chatInfoUpdated` | `user, chatInfo` | Chat metadata changed | [L1077](../Shared/Model/AppAPITypes.swift#L1077) |

### Group Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `receivedGroupInvitation` | `user, groupInfo, contact, memberRole` | Group invitation received | [L1085](../Shared/Model/AppAPITypes.swift#L1085) |
| `userAcceptedGroupSent` | `user, groupInfo, hostContact` | Joined group | [L1086](../Shared/Model/AppAPITypes.swift#L1086) |
| `groupLinkConnecting` | `user, groupInfo, hostMember` | Connecting via group link | [L1087](../Shared/Model/AppAPITypes.swift#L1087) |
| `joinedGroupMemberConnecting` | `user, groupInfo, hostMember, member` | Member joining | [L1089](../Shared/Model/AppAPITypes.swift#L1089) |
| `memberRole` | `user, groupInfo, byMember, member, fromRole, toRole` | Role changed | [L1091](../Shared/Model/AppAPITypes.swift#L1091) |
| `memberBlockedForAll` | `user, groupInfo, byMember, member, blocked` | Member blocked | [L1092](../Shared/Model/AppAPITypes.swift#L1092) |
| `deletedMemberUser` | `user, groupInfo, member, withMessages` | Current user removed | [L1093](../Shared/Model/AppAPITypes.swift#L1093) |
| `deletedMember` | `user, groupInfo, byMember, deletedMember` | Member removed | [L1094](../Shared/Model/AppAPITypes.swift#L1094) |
| `leftMember` | `user, groupInfo, member` | Member left | [L1095](../Shared/Model/AppAPITypes.swift#L1095) |
| `groupDeleted` | `user, groupInfo, member` | Group deleted | [L1096](../Shared/Model/AppAPITypes.swift#L1096) |
| `userJoinedGroup` | `user, groupInfo` | Successfully joined | [L1097](../Shared/Model/AppAPITypes.swift#L1097) |
| `joinedGroupMember` | `user, groupInfo, member` | New member joined | [L1098](../Shared/Model/AppAPITypes.swift#L1098) |
| `connectedToGroupMember` | `user, groupInfo, member, memberContact` | E2E session established with member | [L1099](../Shared/Model/AppAPITypes.swift#L1099) |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile changed | [L1100](../Shared/Model/AppAPITypes.swift#L1100) |
| `groupLinkRelaysUpdated` | `user, groupInfo, groupLink, groupRelays: [GroupRelay]` | Channel relay configuration changed | [L1101](../Shared/Model/AppAPITypes.swift#L1101) |
| `groupMemberUpdated` | `user, groupInfo, fromMember, toMember` | Member info updated | [L1075](../Shared/Model/AppAPITypes.swift#L1075) |

### File Transfer Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `rcvFileStart` | `user, chatItem` | Download started | [L1106](../Shared/Model/AppAPITypes.swift#L1106) |
| `rcvFileProgressXFTP` | `user, chatItem_, receivedSize, totalSize` | Download progress | [L1107](../Shared/Model/AppAPITypes.swift#L1107) |
| `rcvFileComplete` | `user, chatItem` | Download complete | [L1108](../Shared/Model/AppAPITypes.swift#L1108) |
| `rcvFileSndCancelled` | `user, chatItem, rcvFileTransfer` | Sender cancelled | [L1110](../Shared/Model/AppAPITypes.swift#L1110) |
| `rcvFileError` | `user, chatItem_, agentError, rcvFileTransfer` | Download error | [L1111](../Shared/Model/AppAPITypes.swift#L1111) |
| `sndFileStart` | `user, chatItem, sndFileTransfer` | Upload started | [L1114](../Shared/Model/AppAPITypes.swift#L1114) |
| `sndFileComplete` | `user, chatItem, sndFileTransfer` | Upload complete (inline) | [L1115](../Shared/Model/AppAPITypes.swift#L1115) |
| `sndFileProgressXFTP` | `user, chatItem_, fileTransferMeta, sentSize, totalSize` | Upload progress | [L1117](../Shared/Model/AppAPITypes.swift#L1117) |
| `sndFileCompleteXFTP` | `user, chatItem, fileTransferMeta` | XFTP upload complete | [L1119](../Shared/Model/AppAPITypes.swift#L1119) |
| `sndFileError` | `user, chatItem_, fileTransferMeta, errorMessage` | Upload error | [L1121](../Shared/Model/AppAPITypes.swift#L1121) |

### Call Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `callInvitation` | `callInvitation: RcvCallInvitation` | Incoming call | [L1124](../Shared/Model/AppAPITypes.swift#L1124) |
| `callOffer` | `user, contact, callType, offer, sharedKey, askConfirmation` | SDP offer received | [L1125](../Shared/Model/AppAPITypes.swift#L1125) |
| `callAnswer` | `user, contact, answer` | SDP answer received | [L1126](../Shared/Model/AppAPITypes.swift#L1126) |
| `callExtraInfo` | `user, contact, extraInfo` | ICE candidates received | [L1127](../Shared/Model/AppAPITypes.swift#L1127) |
| `callEnded` | `user, contact` | Call ended by remote | [L1128](../Shared/Model/AppAPITypes.swift#L1128) |

### Connection Security Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `contactSwitch` | `user, contact, switchProgress` | Key rotation progress | [L1065](../Shared/Model/AppAPITypes.swift#L1065) |
| `groupMemberSwitch` | `user, groupInfo, member, switchProgress` | Member key rotation | [L1066](../Shared/Model/AppAPITypes.swift#L1066) |
| `contactRatchetSync` | `user, contact, ratchetSyncProgress` | Ratchet sync progress | [L1067](../Shared/Model/AppAPITypes.swift#L1067) |
| `groupMemberRatchetSync` | `user, groupInfo, member, ratchetSyncProgress` | Member ratchet sync | [L1068](../Shared/Model/AppAPITypes.swift#L1068) |

### System Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `chatSuspended` | -- | Core suspended | [L1064](../Shared/Model/AppAPITypes.swift#L1064) |

---

## 5. Error Types

Defined in [`SimpleXChat/APITypes.swift` L699](../SimpleXChat/APITypes.swift#L699):

```swift
public enum ChatError: Decodable, Hashable {
    case error(errorType: ChatErrorType)
    case errorAgent(agentError: AgentErrorType)
    case errorStore(storeError: StoreError)
    case errorDatabase(databaseError: DatabaseError)
    case errorRemoteCtrl(remoteCtrlError: RemoteCtrlError)
    case invalidJSON(json: String)
    case unexpectedResult(type: String)
}
```

### Error Categories

| Category | Enum | Description | Source |
|----------|------|-------------|--------|
| Chat logic | `ChatErrorType` | Business logic errors (e.g., invalid state, permission denied, `chatRelayExists`) | [`APITypes.swift` L722](../SimpleXChat/APITypes.swift#L722) |
| SMP Agent | `AgentErrorType` | Protocol/network errors from the SMP agent layer | [`APITypes.swift` L884](../SimpleXChat/APITypes.swift#L884) |
| Database store | `StoreError` | SQLite query/constraint errors (includes relay-related: `relayUserNotFound`, `duplicateMemberId`, `userChatRelayNotFound`, `groupRelayNotFound`, `groupRelayNotFoundByMemberId`) | [`APITypes.swift` L802](../SimpleXChat/APITypes.swift#L802) |
| Database engine | `DatabaseError` | DB open/migration/encryption errors | [`APITypes.swift` L871](../SimpleXChat/APITypes.swift#L871) |
| Remote control | `RemoteCtrlError` | Remote desktop session errors | [`APITypes.swift` L1054](../SimpleXChat/APITypes.swift#L1054) |
| Parse failure | `invalidJSON` | Failed to decode response JSON | [`APITypes.swift` L699](../SimpleXChat/APITypes.swift#L699) |
| Unexpected | `unexpectedResult` | Response type does not match expected | [`APITypes.swift` L699](../SimpleXChat/APITypes.swift#L699) |

---

## 6. FFI Bridge Functions

Defined in [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift):

### Synchronous (blocking current thread)

```swift
// Throws on error, returns typed result
func chatSendCmdSync<R: ChatAPIResult>(           // SimpleXAPI.swift L93
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) throws -> R

// Returns APIResult (caller handles error)
func chatApiSendCmdSync<R: ChatAPIResult>(         // SimpleXAPI.swift L99
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    retryNum: Int32 = 0,
    log: Bool = true
) -> APIResult<R>
```

### Asynchronous (Swift concurrency)

```swift
// Throws on error, returns typed result
func chatSendCmd<R: ChatAPIResult>(                // SimpleXAPI.swift L121
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) async throws -> R

// Returns APIResult with optional retry on network errors
func chatApiSendCmdWithRetry<R: ChatAPIResult>(    // SimpleXAPI.swift L127
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    inProgress: BoxedValue<Bool>? = nil,
    retryNum: Int32 = 0
) async -> APIResult<R>?
```

### Low-Level FFI

```swift
// Direct C FFI call -- serializes cmd.cmdString, calls chat_send_cmd_retry, decodes response
public func sendSimpleXCmd<R: ChatAPIResult>(      // API.swift L115
    _ cmd: ChatCmdProtocol,
    _ ctrl: chat_ctrl?,
    retryNum: Int32 = 0
) -> APIResult<R>
```

### Event Receiver

```swift
// Polls for async events from the Haskell core
func chatRecvMsg(                                  // SimpleXAPI.swift L237
    _ ctrl: chat_ctrl? = nil
) async -> APIResult<ChatEvent>?

// Processes a received event and updates app state
func processReceivedMsg(                           // SimpleXAPI.swift L2275
    _ res: ChatEvent
) async
```

---

## 7. Result Type

Defined in [`SimpleXChat/APITypes.swift` L27](../SimpleXChat/APITypes.swift#L27):

```swift
public enum APIResult<R>: Decodable where R: Decodable, R: ChatAPIResult {
    case result(R)           // Successful response
    case error(ChatError)    // Error response from core
    case invalid(type: String, json: Data)  // Undecodable response

    public var responseType: String { ... }
    public var unexpected: ChatError { ... }
}

public protocol ChatAPIResult: Decodable {         // APITypes.swift L65
    var responseType: String { get }
    var details: String { get }
    static func fallbackResult(_ type: String, _ json: NSDictionary) -> Self?
}
```

The `decodeAPIResult<R>` function ([`APITypes.swift` L86](../SimpleXChat/APITypes.swift#L86)) handles JSON decoding with fallback logic:
1. Try standard `JSONDecoder.decode(APIResult<R>.self, from: data)`
2. If that fails, try manual JSON parsing via `JSONSerialization`
3. Check for `"error"` key -- return `.error`
4. Check for `"result"` key -- try `R.fallbackResult` or return `.invalid`
5. Last resort: return `.invalid(type: "invalid", json: ...)`

---

## Source Files

| File | Path |
|------|------|
| ChatCommand enum | [`Shared/Model/AppAPITypes.swift` L15](../Shared/Model/AppAPITypes.swift#L15) |
| ChatResponse0/1/2 enums | [`Shared/Model/AppAPITypes.swift` L654, L776, L916](../Shared/Model/AppAPITypes.swift#L654) |
| ChatEvent enum | [`Shared/Model/AppAPITypes.swift` L1063](../Shared/Model/AppAPITypes.swift#L1063) |
| APIResult, ChatError | [`SimpleXChat/APITypes.swift` L27, L699](../SimpleXChat/APITypes.swift#L27) |
| FFI bridge functions | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) |
| Low-level FFI | [`SimpleXChat/API.swift`](../SimpleXChat/API.swift) |
| Data types | `SimpleXChat/ChatTypes.swift` |
| C header | `SimpleXChat/SimpleX.h` |
| Haskell controller | `../../src/Simplex/Chat/Controller.hs` |
