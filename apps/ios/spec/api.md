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
- [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift) -- `ChatCommand` ([L14](../Shared/Model/AppAPITypes.swift#L15)), `ChatResponse0` ([L647](../Shared/Model/AppAPITypes.swift#L649)), `ChatResponse1` ([L768](../Shared/Model/AppAPITypes.swift#L771)), `ChatResponse2` ([L907](../Shared/Model/AppAPITypes.swift#L911)), `ChatEvent` ([L1050](../Shared/Model/AppAPITypes.swift#L1055)) enums
- [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift) -- `APIResult<R>` ([L26](../SimpleXChat/APITypes.swift#L27)), `ChatAPIResult` ([L63](../SimpleXChat/APITypes.swift#L65)), `ChatError` ([L695](../SimpleXChat/APITypes.swift#L699))
- [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) -- FFI bridge functions (`chatSendCmd` [L117](../Shared/Model/SimpleXAPI.swift#L121), `chatRecvMsg` [L230](../Shared/Model/SimpleXAPI.swift#L237))
- [`SimpleXChat/API.swift`](../SimpleXChat/API.swift) -- Low-level FFI (`sendSimpleXCmd` [L114](../SimpleXChat/API.swift#L115), `recvSimpleXMsg` [L136](../SimpleXChat/API.swift#L137))
- `SimpleXChat/ChatTypes.swift` -- Data types used in commands/responses (User, Contact, GroupInfo, ChatItem, etc.)
- `../../src/Simplex/Chat/Controller.hs` -- Haskell controller (function `chat_send_cmd_retry`, `chat_recv_msg_wait`)

---

## 2. Command Categories

The `ChatCommand` enum ([`AppAPITypes.swift` L14](../Shared/Model/AppAPITypes.swift#L15)) contains all commands the iOS app can send to the Haskell core. Commands are organized below by functional area.

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
| `apiUpdateProfile` | `userId: Int64, profile: Profile` | Update user display name/image | [L138](../Shared/Model/AppAPITypes.swift#L139) |
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
| `apiSendMessages` | `type, id, scope, live, ttl, composedMessages` | Send one or more messages | [L47](../Shared/Model/AppAPITypes.swift#L48) |
| `apiCreateChatItems` | `noteFolderId, composedMessages` | Create items in notes folder | [L53](../Shared/Model/AppAPITypes.swift#L54) |
| `apiUpdateChatItem` | `type, id, scope, itemId, updatedMessage, live` | Edit a sent message | [L55](../Shared/Model/AppAPITypes.swift#L56) |
| `apiDeleteChatItem` | `type, id, scope, itemIds, mode` | Delete messages | [L56](../Shared/Model/AppAPITypes.swift#L57) |
| `apiDeleteMemberChatItem` | `groupId, itemIds` | Moderate group messages | [L57](../Shared/Model/AppAPITypes.swift#L58) |
| `apiChatItemReaction` | `type, id, scope, itemId, add, reaction` | Add/remove emoji reaction | [L60](../Shared/Model/AppAPITypes.swift#L61) |
| `apiGetReactionMembers` | `userId, groupId, itemId, reaction` | Get who reacted | [L61](../Shared/Model/AppAPITypes.swift#L62) |
| `apiPlanForwardChatItems` | `fromChatType, fromChatId, fromScope, itemIds` | Plan message forwarding | [L62](../Shared/Model/AppAPITypes.swift#L63) |
| `apiForwardChatItems` | `toChatType, toChatId, toScope, from..., itemIds, ttl` | Forward messages | [L63](../Shared/Model/AppAPITypes.swift#L64) |
| `apiReportMessage` | `groupId, chatItemId, reportReason, reportText` | Report group message | [L54](../Shared/Model/AppAPITypes.swift#L55) |
| `apiChatRead` | `type, id, scope` | Mark entire chat as read | [L163](../Shared/Model/AppAPITypes.swift#L164) |
| `apiChatItemsRead` | `type, id, scope, itemIds` | Mark specific items as read | [L164](../Shared/Model/AppAPITypes.swift#L165) |
| `apiChatUnread` | `type, id, unreadChat` | Toggle unread badge | [L165](../Shared/Model/AppAPITypes.swift#L166) |

### 2.4 Contact Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiAddContact` | `userId, incognito` | Create invitation link | [L123](../Shared/Model/AppAPITypes.swift#L124) |
| `apiConnect` | `userId, incognito, connLink` | Connect via link | [L133](../Shared/Model/AppAPITypes.swift#L134) |
| `apiConnectPlan` | `userId, connLink` | Plan connection (preview) | [L126](../Shared/Model/AppAPITypes.swift#L127) |
| `apiPrepareContact` | `userId, connLink, contactShortLinkData` | Prepare contact from link | [L127](../Shared/Model/AppAPITypes.swift#L128) |
| `apiConnectPreparedContact` | `contactId, incognito, msg` | Connect prepared contact | [L131](../Shared/Model/AppAPITypes.swift#L132) |
| `apiConnectContactViaAddress` | `userId, incognito, contactId` | Connect via address | [L134](../Shared/Model/AppAPITypes.swift#L135) |
| `apiAcceptContact` | `incognito, contactReqId` | Accept contact request | [L151](../Shared/Model/AppAPITypes.swift#L152) |
| `apiRejectContact` | `contactReqId` | Reject contact request | [L152](../Shared/Model/AppAPITypes.swift#L153) |
| `apiDeleteChat` | `type, id, chatDeleteMode` | Delete conversation | [L135](../Shared/Model/AppAPITypes.swift#L136) |
| `apiClearChat` | `type, id` | Clear conversation history | [L136](../Shared/Model/AppAPITypes.swift#L137) |
| `apiListContacts` | `userId` | List all contacts | [L137](../Shared/Model/AppAPITypes.swift#L138) |
| `apiSetContactPrefs` | `contactId, preferences` | Set contact preferences | [L139](../Shared/Model/AppAPITypes.swift#L140) |
| `apiSetContactAlias` | `contactId, localAlias` | Set local alias | [L140](../Shared/Model/AppAPITypes.swift#L141) |
| `apiSetConnectionAlias` | `connId, localAlias` | Set pending connection alias | [L142](../Shared/Model/AppAPITypes.swift#L143) |
| `apiContactInfo` | `contactId` | Get contact info + connection stats | [L109](../Shared/Model/AppAPITypes.swift#L110) |
| `apiSetConnectionIncognito` | `connId, incognito` | Toggle incognito on pending connection | [L124](../Shared/Model/AppAPITypes.swift#L125) |

### 2.5 Group Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiNewGroup` | `userId, incognito, groupProfile` | Create new group | [L71](../Shared/Model/AppAPITypes.swift#L72) |
| `apiAddMember` | `groupId, contactId, memberRole` | Invite contact to group | [L72](../Shared/Model/AppAPITypes.swift#L73) |
| `apiJoinGroup` | `groupId` | Accept group invitation | [L73](../Shared/Model/AppAPITypes.swift#L74) |
| `apiAcceptMember` | `groupId, groupMemberId, memberRole` | Accept member (knocking) | [L74](../Shared/Model/AppAPITypes.swift#L75) |
| `apiRemoveMembers` | `groupId, memberIds, withMessages` | Remove members | [L78](../Shared/Model/AppAPITypes.swift#L79) |
| `apiLeaveGroup` | `groupId` | Leave group | [L79](../Shared/Model/AppAPITypes.swift#L80) |
| `apiListMembers` | `groupId` | List group members | [L80](../Shared/Model/AppAPITypes.swift#L81) |
| `apiUpdateGroupProfile` | `groupId, groupProfile` | Update group name/image/description | [L81](../Shared/Model/AppAPITypes.swift#L82) |
| `apiMembersRole` | `groupId, memberIds, memberRole` | Change member roles | [L76](../Shared/Model/AppAPITypes.swift#L77) |
| `apiBlockMembersForAll` | `groupId, memberIds, blocked` | Block members for all | [L77](../Shared/Model/AppAPITypes.swift#L78) |
| `apiCreateGroupLink` | `groupId, memberRole` | Create shareable group link | [L82](../Shared/Model/AppAPITypes.swift#L83) |
| `apiGroupLinkMemberRole` | `groupId, memberRole` | Change group link default role | [L83](../Shared/Model/AppAPITypes.swift#L84) |
| `apiDeleteGroupLink` | `groupId` | Delete group link | [L84](../Shared/Model/AppAPITypes.swift#L85) |
| `apiGetGroupLink` | `groupId` | Get existing group link | [L85](../Shared/Model/AppAPITypes.swift#L86) |
| `apiAddGroupShortLink` | `groupId` | Add short link to group | [L86](../Shared/Model/AppAPITypes.swift#L87) |
| `apiCreateMemberContact` | `groupId, groupMemberId` | Create direct contact from group member | [L87](../Shared/Model/AppAPITypes.swift#L88) |
| `apiSendMemberContactInvitation` | `contactId, msg` | Send contact invitation to member | [L88](../Shared/Model/AppAPITypes.swift#L89) |
| `apiGroupMemberInfo` | `groupId, groupMemberId` | Get member info + connection stats | [L110](../Shared/Model/AppAPITypes.swift#L111) |
| `apiDeleteMemberSupportChat` | `groupId, groupMemberId` | Delete member support chat | [L75](../Shared/Model/AppAPITypes.swift#L76) |
| `apiSetMemberSettings` | `groupId, groupMemberId, memberSettings` | Set per-member settings | [L108](../Shared/Model/AppAPITypes.swift#L109) |
| `apiSetGroupAlias` | `groupId, localAlias` | Set local group alias | [L141](../Shared/Model/AppAPITypes.swift#L142) |

### 2.6 Chat Tags

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetChatTags` | `userId` | Get all user tags | [L42](../Shared/Model/AppAPITypes.swift#L43) |
| `apiCreateChatTag` | `tag: ChatTagData` | Create a new tag | [L48](../Shared/Model/AppAPITypes.swift#L49) |
| `apiSetChatTags` | `type, id, tagIds` | Assign tags to a chat | [L49](../Shared/Model/AppAPITypes.swift#L50) |
| `apiDeleteChatTag` | `tagId` | Delete a tag | [L50](../Shared/Model/AppAPITypes.swift#L51) |
| `apiUpdateChatTag` | `tagId, tagData` | Update tag name/emoji | [L51](../Shared/Model/AppAPITypes.swift#L52) |
| `apiReorderChatTags` | `tagIds` | Reorder tags | [L52](../Shared/Model/AppAPITypes.swift#L53) |

### 2.7 File Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `receiveFile` | `fileId, userApprovedRelays, encrypted, inline` | Accept and download file | [L166](../Shared/Model/AppAPITypes.swift#L167) |
| `setFileToReceive` | `fileId, userApprovedRelays, encrypted` | Mark file for auto-receive | [L167](../Shared/Model/AppAPITypes.swift#L168) |
| `cancelFile` | `fileId` | Cancel file transfer | [L168](../Shared/Model/AppAPITypes.swift#L169) |
| `apiUploadStandaloneFile` | `userId, file: CryptoFile` | Upload file to XFTP (no chat) | [L178](../Shared/Model/AppAPITypes.swift#L179) |
| `apiDownloadStandaloneFile` | `userId, url, file: CryptoFile` | Download from XFTP URL | [L179](../Shared/Model/AppAPITypes.swift#L180) |
| `apiStandaloneFileInfo` | `url` | Get file metadata from XFTP URL | [L180](../Shared/Model/AppAPITypes.swift#L181) |

### 2.8 WebRTC Call Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSendCallInvitation` | `contact, callType` | Initiate call | [L154](../Shared/Model/AppAPITypes.swift#L155) |
| `apiRejectCall` | `contact` | Reject incoming call | [L155](../Shared/Model/AppAPITypes.swift#L156) |
| `apiSendCallOffer` | `contact, callOffer: WebRTCCallOffer` | Send SDP offer | [L156](../Shared/Model/AppAPITypes.swift#L157) |
| `apiSendCallAnswer` | `contact, answer: WebRTCSession` | Send SDP answer | [L157](../Shared/Model/AppAPITypes.swift#L158) |
| `apiSendCallExtraInfo` | `contact, extraInfo: WebRTCExtraInfo` | Send ICE candidates | [L158](../Shared/Model/AppAPITypes.swift#L159) |
| `apiEndCall` | `contact` | End active call | [L159](../Shared/Model/AppAPITypes.swift#L160) |
| `apiGetCallInvitations` | -- | Get pending call invitations | [L160](../Shared/Model/AppAPITypes.swift#L161) |
| `apiCallStatus` | `contact, callStatus` | Report call status change | [L161](../Shared/Model/AppAPITypes.swift#L162) |

### 2.9 Push Notifications

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetNtfToken` | -- | Get current notification token | [L64](../Shared/Model/AppAPITypes.swift#L65) |
| `apiRegisterToken` | `token, notificationMode` | Register device token with server | [L65](../Shared/Model/AppAPITypes.swift#L66) |
| `apiVerifyToken` | `token, nonce, code` | Verify token registration | [L66](../Shared/Model/AppAPITypes.swift#L67) |
| `apiCheckToken` | `token` | Check token status | [L67](../Shared/Model/AppAPITypes.swift#L68) |
| `apiDeleteToken` | `token` | Unregister token | [L68](../Shared/Model/AppAPITypes.swift#L69) |
| `apiGetNtfConns` | `nonce, encNtfInfo` | Get notification connections (NSE) | [L69](../Shared/Model/AppAPITypes.swift#L70) |
| `apiGetConnNtfMessages` | `connMsgReqs` | Get notification messages (NSE) | [L70](../Shared/Model/AppAPITypes.swift#L71) |

### 2.10 Settings & Configuration

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSaveSettings` | `settings: AppSettings` | Save app settings to core | [L40](../Shared/Model/AppAPITypes.swift#L41) |
| `apiGetSettings` | `settings: AppSettings` | Get settings from core | [L41](../Shared/Model/AppAPITypes.swift#L42) |
| `apiSetChatSettings` | `type, id, chatSettings` | Per-chat notification settings | [L107](../Shared/Model/AppAPITypes.swift#L108) |
| `apiSetChatItemTTL` | `userId, seconds` | Set global message TTL | [L99](../Shared/Model/AppAPITypes.swift#L100) |
| `apiGetChatItemTTL` | `userId` | Get global message TTL | [L100](../Shared/Model/AppAPITypes.swift#L101) |
| `apiSetChatTTL` | `userId, type, id, seconds` | Per-chat message TTL | [L101](../Shared/Model/AppAPITypes.swift#L102) |
| `apiSetNetworkConfig` | `networkConfig: NetCfg` | Set network configuration | [L102](../Shared/Model/AppAPITypes.swift#L103) |
| `apiGetNetworkConfig` | -- | Get network configuration | [L103](../Shared/Model/AppAPITypes.swift#L104) |
| `apiSetNetworkInfo` | `networkInfo: UserNetworkInfo` | Set network type/status | [L104](../Shared/Model/AppAPITypes.swift#L105) |
| `reconnectAllServers` | -- | Force reconnect all servers | [L105](../Shared/Model/AppAPITypes.swift#L106) |
| `reconnectServer` | `userId, smpServer` | Reconnect specific server | [L106](../Shared/Model/AppAPITypes.swift#L107) |

### 2.11 Database & Storage

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiStorageEncryption` | `config: DBEncryptionConfig` | Set/change database encryption | [L38](../Shared/Model/AppAPITypes.swift#L39) |
| `testStorageEncryption` | `key: String` | Test encryption key | [L39](../Shared/Model/AppAPITypes.swift#L40) |
| `apiExportArchive` | `config: ArchiveConfig` | Export database archive | [L35](../Shared/Model/AppAPITypes.swift#L36) |
| `apiImportArchive` | `config: ArchiveConfig` | Import database archive | [L36](../Shared/Model/AppAPITypes.swift#L37) |
| `apiDeleteStorage` | -- | Delete all storage | [L37](../Shared/Model/AppAPITypes.swift#L38) |

### 2.12 Server Operations

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetServerOperators` | -- | Get server operators | [L91](../Shared/Model/AppAPITypes.swift#L92) |
| `apiSetServerOperators` | `operators` | Set server operators | [L92](../Shared/Model/AppAPITypes.swift#L93) |
| `apiGetUserServers` | `userId` | Get user's configured servers | [L93](../Shared/Model/AppAPITypes.swift#L94) |
| `apiSetUserServers` | `userId, userServers` | Set user's servers | [L94](../Shared/Model/AppAPITypes.swift#L95) |
| `apiValidateServers` | `userId, userServers` | Validate server configuration | [L95](../Shared/Model/AppAPITypes.swift#L96) |
| `apiGetUsageConditions` | -- | Get usage conditions | [L96](../Shared/Model/AppAPITypes.swift#L97) |
| `apiAcceptConditions` | `conditionsId, operatorIds` | Accept usage conditions | [L98](../Shared/Model/AppAPITypes.swift#L99) |
| `apiTestProtoServer` | `userId, server` | Test server connectivity | [L90](../Shared/Model/AppAPITypes.swift#L91) |

### 2.13 Theme & UI

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiSetUserUIThemes` | `userId, themes: ThemeModeOverrides?` | Set per-user theme | [L143](../Shared/Model/AppAPITypes.swift#L144) |
| `apiSetChatUIThemes` | `chatId, themes: ThemeModeOverrides?` | Set per-chat theme | [L144](../Shared/Model/AppAPITypes.swift#L145) |

### 2.14 Remote Desktop

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `setLocalDeviceName` | `displayName` | Set device name for pairing | [L170](../Shared/Model/AppAPITypes.swift#L171) |
| `connectRemoteCtrl` | `xrcpInvitation` | Connect to desktop via QR code | [L171](../Shared/Model/AppAPITypes.swift#L172) |
| `findKnownRemoteCtrl` | -- | Find previously paired desktops | [L172](../Shared/Model/AppAPITypes.swift#L173) |
| `confirmRemoteCtrl` | `remoteCtrlId` | Confirm known remote controller | [L173](../Shared/Model/AppAPITypes.swift#L174) |
| `verifyRemoteCtrlSession` | `sessionCode` | Verify session code | [L174](../Shared/Model/AppAPITypes.swift#L175) |
| `listRemoteCtrls` | -- | List known remote controllers | [L175](../Shared/Model/AppAPITypes.swift#L176) |
| `stopRemoteCtrl` | -- | Stop remote session | [L176](../Shared/Model/AppAPITypes.swift#L177) |
| `deleteRemoteCtrl` | `remoteCtrlId` | Delete known controller | [L177](../Shared/Model/AppAPITypes.swift#L178) |

### 2.15 Diagnostics

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `showVersion` | -- | Get core version info | [L182](../Shared/Model/AppAPITypes.swift#L183) |
| `getAgentSubsTotal` | `userId` | Get total SMP subscriptions | [L183](../Shared/Model/AppAPITypes.swift#L184) |
| `getAgentServersSummary` | `userId` | Get server summary stats | [L184](../Shared/Model/AppAPITypes.swift#L185) |
| `resetAgentServersStats` | -- | Reset server statistics | [L185](../Shared/Model/AppAPITypes.swift#L186) |

### 2.16 Address Management

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiCreateMyAddress` | `userId` | Create SimpleX address | [L145](../Shared/Model/AppAPITypes.swift#L146) |
| `apiDeleteMyAddress` | `userId` | Delete SimpleX address | [L146](../Shared/Model/AppAPITypes.swift#L147) |
| `apiShowMyAddress` | `userId` | Show current address | [L147](../Shared/Model/AppAPITypes.swift#L148) |
| `apiAddMyAddressShortLink` | `userId` | Add short link to address | [L148](../Shared/Model/AppAPITypes.swift#L149) |
| `apiSetProfileAddress` | `userId, on: Bool` | Toggle address in profile | [L149](../Shared/Model/AppAPITypes.swift#L150) |
| `apiSetAddressSettings` | `userId, addressSettings` | Configure address settings | [L150](../Shared/Model/AppAPITypes.swift#L151) |

### 2.17 Connection Security

| Command | Parameters | Description | Source |
|---------|-----------|-------------|--------|
| `apiGetContactCode` | `contactId` | Get verification code | [L119](../Shared/Model/AppAPITypes.swift#L120) |
| `apiGetGroupMemberCode` | `groupId, groupMemberId` | Get member verification code | [L120](../Shared/Model/AppAPITypes.swift#L121) |
| `apiVerifyContact` | `contactId, connectionCode` | Verify contact identity | [L121](../Shared/Model/AppAPITypes.swift#L122) |
| `apiVerifyGroupMember` | `groupId, groupMemberId, connectionCode` | Verify group member identity | [L122](../Shared/Model/AppAPITypes.swift#L123) |
| `apiSwitchContact` | `contactId` | Switch contact connection (key rotation) | [L113](../Shared/Model/AppAPITypes.swift#L114) |
| `apiSwitchGroupMember` | `groupId, groupMemberId` | Switch group member connection | [L114](../Shared/Model/AppAPITypes.swift#L115) |
| `apiAbortSwitchContact` | `contactId` | Abort contact switch | [L115](../Shared/Model/AppAPITypes.swift#L116) |
| `apiAbortSwitchGroupMember` | `groupId, groupMemberId` | Abort member switch | [L116](../Shared/Model/AppAPITypes.swift#L117) |
| `apiSyncContactRatchet` | `contactId, force` | Sync double-ratchet state | [L117](../Shared/Model/AppAPITypes.swift#L118) |
| `apiSyncGroupMemberRatchet` | `groupId, groupMemberId, force` | Sync member ratchet | [L118](../Shared/Model/AppAPITypes.swift#L119) |

---

## 3. Response Types

Responses are split across three enums due to Swift enum size limitations:

### ChatResponse0

Synchronous query responses ([`AppAPITypes.swift` L647](../Shared/Model/AppAPITypes.swift#L649)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `activeUser` | `user: User` | Current active user | [L648](../Shared/Model/AppAPITypes.swift#L650) |
| `usersList` | `users: [UserInfo]` | All user profiles | [L649](../Shared/Model/AppAPITypes.swift#L651) |
| `chatStarted` | -- | Chat engine started | [L650](../Shared/Model/AppAPITypes.swift#L652) |
| `chatRunning` | -- | Chat is already running | [L651](../Shared/Model/AppAPITypes.swift#L653) |
| `chatStopped` | -- | Chat engine stopped | [L652](../Shared/Model/AppAPITypes.swift#L654) |
| `apiChats` | `user, chats: [ChatData]` | All chat previews | [L653](../Shared/Model/AppAPITypes.swift#L655) |
| `apiChat` | `user, chat: ChatData, navInfo` | Single chat with messages | [L654](../Shared/Model/AppAPITypes.swift#L656) |
| `chatTags` | `user, userTags: [ChatTag]` | User's chat tags | [L656](../Shared/Model/AppAPITypes.swift#L658) |
| `chatItemInfo` | `user, chatItem, chatItemInfo` | Message detail info | [L657](../Shared/Model/AppAPITypes.swift#L659) |
| `serverTestResult` | `user, testServer, testFailure` | Server test result | [L658](../Shared/Model/AppAPITypes.swift#L660) |
| `networkConfig` | `networkConfig: NetCfg` | Current network config | [L664](../Shared/Model/AppAPITypes.swift#L666) |
| `contactInfo` | `user, contact, connectionStats, customUserProfile` | Contact details | [L665](../Shared/Model/AppAPITypes.swift#L667) |
| `groupMemberInfo` | `user, groupInfo, member, connectionStats` | Member details | [L666](../Shared/Model/AppAPITypes.swift#L668) |
| `connectionVerified` | `verified, expectedCode` | Verification result | [L676](../Shared/Model/AppAPITypes.swift#L678) |
| `tagsUpdated` | `user, userTags, chatTags` | Tags changed | [L677](../Shared/Model/AppAPITypes.swift#L679) |

### ChatResponse1

Contact, message, and profile responses ([`AppAPITypes.swift` L768](../Shared/Model/AppAPITypes.swift#L771)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `invitation` | `user, connLinkInvitation, connection` | Created invitation link | [L769](../Shared/Model/AppAPITypes.swift#L772) |
| `connectionPlan` | `user, connLink, connectionPlan` | Connection plan preview | [L772](../Shared/Model/AppAPITypes.swift#L775) |
| `newPreparedChat` | `user, chat: ChatData` | Prepared contact/group | [L773](../Shared/Model/AppAPITypes.swift#L776) |
| `contactDeleted` | `user, contact` | Contact deleted | [L782](../Shared/Model/AppAPITypes.swift#L785) |
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages sent/received | [L800](../Shared/Model/AppAPITypes.swift#L803) |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited | [L803](../Shared/Model/AppAPITypes.swift#L806) |
| `chatItemReaction` | `user, added, reaction` | Reaction change | [L805](../Shared/Model/AppAPITypes.swift#L808) |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted | [L807](../Shared/Model/AppAPITypes.swift#L810) |
| `contactsList` | `user, contacts: [Contact]` | All contacts list | [L808](../Shared/Model/AppAPITypes.swift#L811) |
| `userProfileUpdated` | `user, fromProfile, toProfile` | Profile changed | [L788](../Shared/Model/AppAPITypes.swift#L791) |
| `userContactLinkCreated` | `user, connLinkContact` | Address created | [L796](../Shared/Model/AppAPITypes.swift#L799) |
| `forwardPlan` | `user, chatItemIds, forwardConfirmation` | Forward plan result | [L802](../Shared/Model/AppAPITypes.swift#L805) |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted | [L801](../Shared/Model/AppAPITypes.swift#L804) |

### ChatResponse2

Group, file, call, notification, and misc responses ([`AppAPITypes.swift` L907](../Shared/Model/AppAPITypes.swift#L911)):

| Response | Key Fields | Description | Source |
|----------|-----------|-------------|--------|
| `groupCreated` | `user, groupInfo` | New group created | [L909](../Shared/Model/AppAPITypes.swift#L913) |
| `sentGroupInvitation` | `user, groupInfo, contact, member` | Group invitation sent | [L910](../Shared/Model/AppAPITypes.swift#L914) |
| `groupMembers` | `user, group: Group` | Group member list | [L914](../Shared/Model/AppAPITypes.swift#L918) |
| `membersRoleUser` | `user, groupInfo, members, toRole` | Role changed | [L918](../Shared/Model/AppAPITypes.swift#L922) |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile updated | [L920](../Shared/Model/AppAPITypes.swift#L924) |
| `groupLinkCreated` | `user, groupInfo, groupLink` | Group link created | [L921](../Shared/Model/AppAPITypes.swift#L925) |
| `rcvFileAccepted` | `user, chatItem` | File download started | [L928](../Shared/Model/AppAPITypes.swift#L932) |
| `callInvitations` | `callInvitations: [RcvCallInvitation]` | Pending calls | [L937](../Shared/Model/AppAPITypes.swift#L941) |
| `ntfToken` | `token, status, ntfMode, ntfServer` | Notification token info | [L940](../Shared/Model/AppAPITypes.swift#L944) |
| `versionInfo` | `versionInfo, chatMigrations, agentMigrations` | Core version | [L948](../Shared/Model/AppAPITypes.swift#L952) |
| `cmdOk` | `user_` | Generic success | [L949](../Shared/Model/AppAPITypes.swift#L953) |
| `archiveExported` | `archiveErrors: [ArchiveError]` | Export result | [L953](../Shared/Model/AppAPITypes.swift#L957) |
| `archiveImported` | `archiveErrors: [ArchiveError]` | Import result | [L954](../Shared/Model/AppAPITypes.swift#L958) |
| `appSettings` | `appSettings: AppSettings` | Retrieved settings | [L955](../Shared/Model/AppAPITypes.swift#L959) |

---

## 4. Event Types

The `ChatEvent` enum ([`AppAPITypes.swift` L1050](../Shared/Model/AppAPITypes.swift#L1055)) represents async events from the Haskell core. These arrive via `chat_recv_msg_wait` polling, not as responses to commands.

Event processing entry point: [`processReceivedMsg`](../Shared/Model/SimpleXAPI.swift#L2266) in `SimpleXAPI.swift`.

### Connection Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `contactConnected` | `user, contact, userCustomProfile` | Contact connection established | [L1057](../Shared/Model/AppAPITypes.swift#L1062) |
| `contactConnecting` | `user, contact` | Contact connecting in progress | [L1058](../Shared/Model/AppAPITypes.swift#L1063) |
| `contactSndReady` | `user, contact` | Ready to send to contact | [L1059](../Shared/Model/AppAPITypes.swift#L1064) |
| `contactDeletedByContact` | `user, contact` | Contact deleted by other party | [L1056](../Shared/Model/AppAPITypes.swift#L1061) |
| `contactUpdated` | `user, toContact` | Contact profile updated | [L1061](../Shared/Model/AppAPITypes.swift#L1066) |
| `receivedContactRequest` | `user, contactRequest, chat_` | Incoming contact request | [L1060](../Shared/Model/AppAPITypes.swift#L1065) |
| `subscriptionStatus` | `subscriptionStatus, connections` | Connection subscription change | [L1063](../Shared/Model/AppAPITypes.swift#L1068) |

### Message Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `newChatItems` | `user, chatItems: [AChatItem]` | New messages received | [L1065](../Shared/Model/AppAPITypes.swift#L1070) |
| `chatItemUpdated` | `user, chatItem: AChatItem` | Message edited remotely | [L1067](../Shared/Model/AppAPITypes.swift#L1072) |
| `chatItemReaction` | `user, added, reaction: ACIReaction` | Reaction added/removed | [L1068](../Shared/Model/AppAPITypes.swift#L1073) |
| `chatItemsDeleted` | `user, chatItemDeletions, byUser` | Messages deleted | [L1069](../Shared/Model/AppAPITypes.swift#L1074) |
| `chatItemsStatusesUpdated` | `user, chatItems: [AChatItem]` | Delivery status changed | [L1066](../Shared/Model/AppAPITypes.swift#L1071) |
| `groupChatItemsDeleted` | `user, groupInfo, chatItemIDs, byUser, member_` | Group items deleted | [L1071](../Shared/Model/AppAPITypes.swift#L1076) |
| `chatInfoUpdated` | `user, chatInfo` | Chat metadata changed | [L1064](../Shared/Model/AppAPITypes.swift#L1069) |

### Group Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `receivedGroupInvitation` | `user, groupInfo, contact, memberRole` | Group invitation received | [L1072](../Shared/Model/AppAPITypes.swift#L1077) |
| `userAcceptedGroupSent` | `user, groupInfo, hostContact` | Joined group | [L1073](../Shared/Model/AppAPITypes.swift#L1078) |
| `groupLinkConnecting` | `user, groupInfo, hostMember` | Connecting via group link | [L1074](../Shared/Model/AppAPITypes.swift#L1079) |
| `joinedGroupMemberConnecting` | `user, groupInfo, hostMember, member` | Member joining | [L1076](../Shared/Model/AppAPITypes.swift#L1081) |
| `memberRole` | `user, groupInfo, byMember, member, fromRole, toRole` | Role changed | [L1078](../Shared/Model/AppAPITypes.swift#L1083) |
| `memberBlockedForAll` | `user, groupInfo, byMember, member, blocked` | Member blocked | [L1079](../Shared/Model/AppAPITypes.swift#L1084) |
| `deletedMemberUser` | `user, groupInfo, member, withMessages` | Current user removed | [L1080](../Shared/Model/AppAPITypes.swift#L1085) |
| `deletedMember` | `user, groupInfo, byMember, deletedMember` | Member removed | [L1081](../Shared/Model/AppAPITypes.swift#L1086) |
| `leftMember` | `user, groupInfo, member` | Member left | [L1082](../Shared/Model/AppAPITypes.swift#L1087) |
| `groupDeleted` | `user, groupInfo, member` | Group deleted | [L1083](../Shared/Model/AppAPITypes.swift#L1088) |
| `userJoinedGroup` | `user, groupInfo` | Successfully joined | [L1084](../Shared/Model/AppAPITypes.swift#L1089) |
| `joinedGroupMember` | `user, groupInfo, member` | New member joined | [L1085](../Shared/Model/AppAPITypes.swift#L1090) |
| `connectedToGroupMember` | `user, groupInfo, member, memberContact` | E2E session established with member | [L1086](../Shared/Model/AppAPITypes.swift#L1091) |
| `groupUpdated` | `user, toGroup: GroupInfo` | Group profile changed | [L1087](../Shared/Model/AppAPITypes.swift#L1092) |
| `groupMemberUpdated` | `user, groupInfo, fromMember, toMember` | Member info updated | [L1062](../Shared/Model/AppAPITypes.swift#L1067) |

### File Transfer Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `rcvFileStart` | `user, chatItem` | Download started | [L1092](../Shared/Model/AppAPITypes.swift#L1097) |
| `rcvFileProgressXFTP` | `user, chatItem_, receivedSize, totalSize` | Download progress | [L1093](../Shared/Model/AppAPITypes.swift#L1098) |
| `rcvFileComplete` | `user, chatItem` | Download complete | [L1094](../Shared/Model/AppAPITypes.swift#L1099) |
| `rcvFileSndCancelled` | `user, chatItem, rcvFileTransfer` | Sender cancelled | [L1096](../Shared/Model/AppAPITypes.swift#L1101) |
| `rcvFileError` | `user, chatItem_, agentError, rcvFileTransfer` | Download error | [L1097](../Shared/Model/AppAPITypes.swift#L1102) |
| `sndFileStart` | `user, chatItem, sndFileTransfer` | Upload started | [L1100](../Shared/Model/AppAPITypes.swift#L1105) |
| `sndFileComplete` | `user, chatItem, sndFileTransfer` | Upload complete (inline) | [L1101](../Shared/Model/AppAPITypes.swift#L1106) |
| `sndFileProgressXFTP` | `user, chatItem_, fileTransferMeta, sentSize, totalSize` | Upload progress | [L1103](../Shared/Model/AppAPITypes.swift#L1108) |
| `sndFileCompleteXFTP` | `user, chatItem, fileTransferMeta` | XFTP upload complete | [L1105](../Shared/Model/AppAPITypes.swift#L1110) |
| `sndFileError` | `user, chatItem_, fileTransferMeta, errorMessage` | Upload error | [L1107](../Shared/Model/AppAPITypes.swift#L1112) |

### Call Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `callInvitation` | `callInvitation: RcvCallInvitation` | Incoming call | [L1110](../Shared/Model/AppAPITypes.swift#L1115) |
| `callOffer` | `user, contact, callType, offer, sharedKey, askConfirmation` | SDP offer received | [L1111](../Shared/Model/AppAPITypes.swift#L1116) |
| `callAnswer` | `user, contact, answer` | SDP answer received | [L1112](../Shared/Model/AppAPITypes.swift#L1117) |
| `callExtraInfo` | `user, contact, extraInfo` | ICE candidates received | [L1113](../Shared/Model/AppAPITypes.swift#L1118) |
| `callEnded` | `user, contact` | Call ended by remote | [L1114](../Shared/Model/AppAPITypes.swift#L1119) |

### Connection Security Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `contactSwitch` | `user, contact, switchProgress` | Key rotation progress | [L1052](../Shared/Model/AppAPITypes.swift#L1057) |
| `groupMemberSwitch` | `user, groupInfo, member, switchProgress` | Member key rotation | [L1053](../Shared/Model/AppAPITypes.swift#L1058) |
| `contactRatchetSync` | `user, contact, ratchetSyncProgress` | Ratchet sync progress | [L1054](../Shared/Model/AppAPITypes.swift#L1059) |
| `groupMemberRatchetSync` | `user, groupInfo, member, ratchetSyncProgress` | Member ratchet sync | [L1055](../Shared/Model/AppAPITypes.swift#L1060) |

### System Events

| Event | Key Fields | Description | Source |
|-------|-----------|-------------|--------|
| `chatSuspended` | -- | Core suspended | [L1051](../Shared/Model/AppAPITypes.swift#L1056) |

---

## 5. Error Types

Defined in [`SimpleXChat/APITypes.swift` L695](../SimpleXChat/APITypes.swift#L699):

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
| Chat logic | `ChatErrorType` | Business logic errors (e.g., invalid state, permission denied) | [`APITypes.swift` L717](../SimpleXChat/APITypes.swift#L722) |
| SMP Agent | `AgentErrorType` | Protocol/network errors from the SMP agent layer | [`APITypes.swift` L873](../SimpleXChat/APITypes.swift#L878) |
| Database store | `StoreError` | SQLite query/constraint errors | [`APITypes.swift` L796](../SimpleXChat/APITypes.swift#L801) |
| Database engine | `DatabaseError` | DB open/migration/encryption errors | [`APITypes.swift` L860](../SimpleXChat/APITypes.swift#L865) |
| Remote control | `RemoteCtrlError` | Remote desktop session errors | [`APITypes.swift` L1043](../SimpleXChat/APITypes.swift#L1048) |
| Parse failure | `invalidJSON` | Failed to decode response JSON | [`APITypes.swift` L695](../SimpleXChat/APITypes.swift#L699) |
| Unexpected | `unexpectedResult` | Response type does not match expected | [`APITypes.swift` L695](../SimpleXChat/APITypes.swift#L699) |

---

## 6. FFI Bridge Functions

Defined in [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift):

### Synchronous (blocking current thread)

```swift
// Throws on error, returns typed result
func chatSendCmdSync<R: ChatAPIResult>(           // SimpleXAPI.swift L91
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) throws -> R

// Returns APIResult (caller handles error)
func chatApiSendCmdSync<R: ChatAPIResult>(         // SimpleXAPI.swift L96
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
func chatSendCmd<R: ChatAPIResult>(                // SimpleXAPI.swift L117
    _ cmd: ChatCommand,
    bgTask: Bool = true,
    bgDelay: Double? = nil,
    ctrl: chat_ctrl? = nil,
    log: Bool = true
) async throws -> R

// Returns APIResult with optional retry on network errors
func chatApiSendCmdWithRetry<R: ChatAPIResult>(    // SimpleXAPI.swift L122
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
func chatRecvMsg(                                  // SimpleXAPI.swift L230
    _ ctrl: chat_ctrl? = nil
) async -> APIResult<ChatEvent>?

// Processes a received event and updates app state
func processReceivedMsg(                           // SimpleXAPI.swift L2248
    _ res: ChatEvent
) async
```

---

## 7. Result Type

Defined in [`SimpleXChat/APITypes.swift` L26](../SimpleXChat/APITypes.swift#L27):

```swift
public enum APIResult<R>: Decodable where R: Decodable, R: ChatAPIResult {
    case result(R)           // Successful response
    case error(ChatError)    // Error response from core
    case invalid(type: String, json: Data)  // Undecodable response

    public var responseType: String { ... }
    public var unexpected: ChatError { ... }
}

public protocol ChatAPIResult: Decodable {         // APITypes.swift L63
    var responseType: String { get }
    var details: String { get }
    static func fallbackResult(_ type: String, _ json: NSDictionary) -> Self?
}
```

The `decodeAPIResult<R>` function ([`APITypes.swift` L83](../SimpleXChat/APITypes.swift#L86)) handles JSON decoding with fallback logic:
1. Try standard `JSONDecoder.decode(APIResult<R>.self, from: data)`
2. If that fails, try manual JSON parsing via `JSONSerialization`
3. Check for `"error"` key -- return `.error`
4. Check for `"result"` key -- try `R.fallbackResult` or return `.invalid`
5. Last resort: return `.invalid(type: "invalid", json: ...)`

---

## Source Files

| File | Path |
|------|------|
| ChatCommand enum | [`Shared/Model/AppAPITypes.swift` L14](../Shared/Model/AppAPITypes.swift#L15) |
| ChatResponse0/1/2 enums | [`Shared/Model/AppAPITypes.swift` L647, L768, L907](../Shared/Model/AppAPITypes.swift#L649) |
| ChatEvent enum | [`Shared/Model/AppAPITypes.swift` L1050](../Shared/Model/AppAPITypes.swift#L1055) |
| APIResult, ChatError | [`SimpleXChat/APITypes.swift` L26, L695](../SimpleXChat/APITypes.swift#L27) |
| FFI bridge functions | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift) |
| Low-level FFI | [`SimpleXChat/API.swift`](../SimpleXChat/API.swift) |
| Data types | `SimpleXChat/ChatTypes.swift` |
| C header | `SimpleXChat/SimpleX.h` |
| Haskell controller | `../../src/Simplex/Chat/Controller.hs` |
