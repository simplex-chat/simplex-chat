[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / ChatApi

# Class: ChatApi

Defined in: [src/api.ts:103](../api.ts#L103)

Main API class for interacting with the chat core library.

## Properties

### ctrl\_

> `protected` **ctrl\_**: `bigint` \| `undefined`

Defined in: [src/api.ts:109](../api.ts#L109)

## Accessors

### ctrl

#### Get Signature

> **get** **ctrl**(): `bigint`

Defined in: [src/api.ts:348](../api.ts#L348)

Chat controller reference

##### Returns

`bigint`

***

### initialized

#### Get Signature

> **get** **initialized**(): `boolean`

Defined in: [src/api.ts:334](../api.ts#L334)

Chat controller is initialized

##### Returns

`boolean`

***

### started

#### Get Signature

> **get** **started**(): `boolean`

Defined in: [src/api.ts:341](../api.ts#L341)

Chat controller is started

##### Returns

`boolean`

## Methods

### apiAcceptContactRequest()

> **apiAcceptContactRequest**(`contactReqId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:853](../api.ts#L853)

Accept contact request.
Network usage: interactive.

#### Parameters

##### contactReqId

`number`

#### Returns

`Promise`\<`Contact`\>

***

### apiAcceptMember()

> **apiAcceptMember**(`groupId`, `groupMemberId`, `memberRole`): `Promise`\<`GroupMember`\>

Defined in: [src/api.ts:601](../api.ts#L601)

Accept group member. Requires Admin role.
Network usage: background.

#### Parameters

##### groupId

`number`

##### groupMemberId

`number`

##### memberRole

`GroupMemberRole`

#### Returns

`Promise`\<`GroupMember`\>

***

### apiAddMember()

> **apiAddMember**(`groupId`, `contactId`, `memberRole`): `Promise`\<`GroupMember`\>

Defined in: [src/api.ts:571](../api.ts#L571)

Add contact to group. Requires bot to have Admin role.
Network usage: interactive.

#### Parameters

##### groupId

`number`

##### contactId

`number`

##### memberRole

`GroupMemberRole`

#### Returns

`Promise`\<`GroupMember`\>

***

### apiAddMemberViaGroup()

> **apiAddMemberViaGroup**(`groupId`, `viaGroupId`, `groupMemberId`, `memberRole`): `Promise`\<`GroupMember`\>

Defined in: [src/api.ts:581](../api.ts#L581)

Add member of another group to group. Requires bot to have Admin role in the group, and direct messages allowed for its role in another group.
Network usage: interactive.

#### Parameters

##### groupId

`number`

##### viaGroupId

`number`

##### groupMemberId

`number`

##### memberRole

`GroupMemberRole`

#### Returns

`Promise`\<`GroupMember`\>

***

### apiBlockMembersForAll()

> **apiBlockMembersForAll**(`groupId`, `groupMemberIds`, `blocked`): `Promise`\<`void`\>

Defined in: [src/api.ts:621](../api.ts#L621)

Block members. Requires Moderator role.
Network usage: background.

#### Parameters

##### groupId

`number`

##### groupMemberIds

`number`[]

##### blocked

`boolean`

#### Returns

`Promise`\<`void`\>

***

### apiCancelFile()

> **apiCancelFile**(`fileId`): `Promise`\<`void`\>

Defined in: [src/api.ts:561](../api.ts#L561)

Cancel file.
Network usage: background.

#### Parameters

##### fileId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiChatItemReaction()

> **apiChatItemReaction**(`chatType`, `chatId`, `chatItemId`, `add`, `reaction`): `Promise`\<`ACIReaction`\>

Defined in: [src/api.ts:533](../api.ts#L533)

Add/remove message reaction.
Network usage: background.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### chatItemId

`number`

##### add

`boolean`

##### reaction

`MsgReaction`

#### Returns

`Promise`\<`ACIReaction`\>

***

### apiChatItemsRead()

> **apiChatItemsRead**(`chatType`, `chatId`, `chatItemIds`): `Promise`\<`ChatInfo`\>

Defined in: [src/api.ts:970](../api.ts#L970)

Mark messages as read.
Network usage: no.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### chatItemIds

`number`[]

#### Returns

`Promise`\<`ChatInfo`\>

***

### apiConnect()

> **apiConnect**(`userId`, `incognito`, `preparedLink`): `Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

Defined in: [src/api.ts:750](../api.ts#L750)

Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link
Network usage: interactive.

#### Parameters

##### userId

`number`

##### incognito

`boolean`

##### preparedLink

`CreatedConnLink`

#### Returns

`Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

***

### apiConnectActiveUser()

> **apiConnectActiveUser**(`connLink`): `Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

Defined in: [src/api.ts:831](../api.ts#L831)

Connect via SimpleX link as string in the active user profile.
Network usage: interactive.

#### Parameters

##### connLink

`string`

#### Returns

`Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

***

### apiConnectContactViaAddress()

> **apiConnectContactViaAddress**(`userId`, `incognito`, `contactId`): `Promise`\<`SentInvitationToContact`\>

Defined in: [src/api.ts:821](../api.ts#L821)

Connect to existing contact via address in its profile.
Network usage: interactive.

#### Parameters

##### userId

`number`

##### incognito

`boolean`

##### contactId

`number`

#### Returns

`Promise`\<`SentInvitationToContact`\>

***

### apiConnectPlan()

> **apiConnectPlan**(`userId`, `connectionLink`): `Promise`\<\[`plan`: `ConnectionPlan`, `link`: `CreatedConnLink`, `planSimplexName?`: `SimplexNameInfo`, `otherSimplexName?`: `SimplexNameInfo`\]\>

Defined in: [src/api.ts:740](../api.ts#L740)

Determine SimpleX link type and if the bot is already connected via this link.
Network usage: interactive.

#### Parameters

##### userId

`number`

##### connectionLink

`string`

#### Returns

`Promise`\<\[`plan`: `ConnectionPlan`, `link`: `CreatedConnLink`, `planSimplexName?`: `SimplexNameInfo`, `otherSimplexName?`: `SimplexNameInfo`\]\>

***

### apiConnectPreparedContact()

> **apiConnectPreparedContact**(`contactId`, `incognito`, `msgContent?`): `Promise`\<`ContactAlreadyExists` \| `StartedConnectionToContact`\>

Defined in: [src/api.ts:796](../api.ts#L796)

Connect to prepared contact.
Network usage: interactive.

#### Parameters

##### contactId

`number`

##### incognito

`boolean`

##### msgContent?

`MsgContent`

#### Returns

`Promise`\<`ContactAlreadyExists` \| `StartedConnectionToContact`\>

***

### apiConnectPreparedGroup()

> **apiConnectPreparedGroup**(`groupId`, `incognito`, `msgContent?`, `ownerContact?`): `Promise`\<`StartedConnectionToGroup`\>

Defined in: [src/api.ts:811](../api.ts#L811)

Connect to prepared group.
Network usage: interactive.

#### Parameters

##### groupId

`number`

##### incognito

`boolean`

##### msgContent?

`MsgContent`

##### ownerContact?

`GroupOwnerContact`

#### Returns

`Promise`\<`StartedConnectionToGroup`\>

***

### apiConnectPreparedLink()

> **apiConnectPreparedLink**(`userId`, `incognito`, `preparedLink`): `Promise`\<`ContactAlreadyExists` \| `SentConfirmation` \| `SentInvitation`\>

Defined in: [src/api.ts:760](../api.ts#L760)

Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link.
Returns the response with the pending connection, or with the contact when it already exists.
Network usage: interactive.

#### Parameters

##### userId

`number`

##### incognito

`boolean`

##### preparedLink

`CreatedConnLink`

#### Returns

`Promise`\<`ContactAlreadyExists` \| `SentConfirmation` \| `SentInvitation`\>

***

### apiContactInfo()

> **apiContactInfo**(`contactId`): `Promise`\<`ContactInfo`\>

Defined in: [src/api.ts:990](../api.ts#L990)

Get contact information.
Network usage: no.

#### Parameters

##### contactId

`number`

#### Returns

`Promise`\<`ContactInfo`\>

***

### apiCreateActiveUser()

> **apiCreateActiveUser**(`profile?`): `Promise`\<`User`\>

Defined in: [src/api.ts:1050](../api.ts#L1050)

Create new user profile
Network usage: no.

#### Parameters

##### profile?

`Profile`

#### Returns

`Promise`\<`User`\>

***

### apiCreateGroupLink()

> **apiCreateGroupLink**(`groupId`, `memberRole`): `Promise`\<`string`\>

Defined in: [src/api.ts:681](../api.ts#L681)

Create group link.
Network usage: interactive.

#### Parameters

##### groupId

`number`

##### memberRole

`GroupMemberRole`

#### Returns

`Promise`\<`string`\>

***

### apiCreateLink()

> **apiCreateLink**(`userId`): `Promise`\<`string`\>

Defined in: [src/api.ts:727](../api.ts#L727)

Create 1-time invitation link.
Network usage: interactive.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiCreateMemberContact()

> **apiCreateMemberContact**(`groupId`, `groupMemberId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:1116](../api.ts#L1116)

Create a direct message contact with a group member.
Returns the created contact.
Network usage: interactive.

#### Parameters

##### groupId

`number`

##### groupMemberId

`number`

#### Returns

`Promise`\<`Contact`\>

***

### apiCreateUserAddress()

> **apiCreateUserAddress**(`userId`): `Promise`\<`CreatedConnLink`\>

Defined in: [src/api.ts:365](../api.ts#L365)

Create bot address.
Network usage: interactive.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`CreatedConnLink`\>

***

### apiDeleteChat()

> **apiDeleteChat**(`chatType`, `chatId`, `deleteMode?`): `Promise`\<`void`\>

Defined in: [src/api.ts:912](../api.ts#L912)

Delete chat.
Network usage: background.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### deleteMode?

`ChatDeleteMode` = `...`

#### Returns

`Promise`\<`void`\>

***

### apiDeleteChatItems()

> **apiDeleteChatItems**(`chatType`, `chatId`, `chatItemIds`, `deleteMode`): `Promise`\<`ChatItemDeletion`[]\>

Defined in: [src/api.ts:508](../api.ts#L508)

Delete message.
Network usage: background.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### chatItemIds

`number`[]

##### deleteMode

`CIDeleteMode`

#### Returns

`Promise`\<`ChatItemDeletion`[]\>

***

### apiDeleteGroupLink()

> **apiDeleteGroupLink**(`groupId`): `Promise`\<`void`\>

Defined in: [src/api.ts:703](../api.ts#L703)

Delete group link.
Network usage: background.

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiDeleteMemberChatItem()

> **apiDeleteMemberChatItem**(`groupId`, `chatItemIds`): `Promise`\<`ChatItemDeletion`[]\>

Defined in: [src/api.ts:523](../api.ts#L523)

Moderate message. Requires Moderator role (and higher than message author's).
Network usage: background.

#### Parameters

##### groupId

`number`

##### chatItemIds

`number`[]

#### Returns

`Promise`\<`ChatItemDeletion`[]\>

***

### apiDeleteUser()

> **apiDeleteUser**(`userId`, `delSMPQueues`, `viewPwd?`): `Promise`\<`void`\>

Defined in: [src/api.ts:1080](../api.ts#L1080)

Delete user profile.
Network usage: background.

#### Parameters

##### userId

`number`

##### delSMPQueues

`boolean`

##### viewPwd?

`string`

#### Returns

`Promise`\<`void`\>

***

### apiDeleteUserAddress()

> **apiDeleteUserAddress**(`userId`): `Promise`\<`void`\>

Defined in: [src/api.ts:375](../api.ts#L375)

Deletes a user address.
Network usage: background.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiGetActiveUser()

> **apiGetActiveUser**(): `Promise`\<`User` \| `undefined`\>

Defined in: [src/api.ts:1030](../api.ts#L1030)

Get active user profile
Network usage: no.

#### Returns

`Promise`\<`User` \| `undefined`\>

***

### apiGetChat()

> **apiGetChat**(`chatType`, `chatId`, `pagination`): `Promise`\<`AChat`\>

Defined in: [src/api.ts:959](../api.ts#L959)

Get chat items.
Network usage: no.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### pagination

`number` \| `ChatPagination`

#### Returns

`Promise`\<`AChat`\>

***

### apiGetChatItemInfo()

> **apiGetChatItemInfo**(`chatType`, `chatId`, `chatItemId`): `Promise`\<`ChatItemInfo`\>

Defined in: [src/api.ts:980](../api.ts#L980)

Get message information.
Network usage: no.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### chatItemId

`number`

#### Returns

`Promise`\<`ChatItemInfo`\>

***

### apiGetChats()

> **apiGetChats**(`userId`, `pagination`, `query?`, `pendingConnections?`): `Promise`\<`AChat`[]\>

Defined in: [src/api.ts:897](../api.ts#L897)

Get chat previews (paginated).
Network usage: no.

Prefer this over apiListContacts / apiListGroups for any scan: those
methods load every record into memory in a single response and will fail
on large databases.

#### Parameters

##### userId

`number`

##### pagination

`Last`

##### query?

`ChatListQuery` = `...`

##### pendingConnections?

`boolean` = `false`

#### Returns

`Promise`\<`AChat`[]\>

***

### apiGetGroupLink()

> **apiGetGroupLink**(`groupId`): `Promise`\<`GroupLink`\>

Defined in: [src/api.ts:712](../api.ts#L712)

Get group link.
Network usage: no.

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupLink`\>

***

### apiGetGroupLinkStr()

> **apiGetGroupLinkStr**(`groupId`): `Promise`\<`string`\>

Defined in: [src/api.ts:718](../api.ts#L718)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiGetNetworkConfig()

> **apiGetNetworkConfig**(): `Promise`\<`NetworkConfig`\>

Defined in: [src/api.ts:1000](../api.ts#L1000)

Get network configuration.
Network usage: no.

#### Returns

`Promise`\<`NetworkConfig`\>

***

### apiGetUserAddress()

> **apiGetUserAddress**(`userId`): `Promise`\<`UserContactLink` \| `undefined`\>

Defined in: [src/api.ts:385](../api.ts#L385)

Get bot address and settings.
Network usage: no.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`UserContactLink` \| `undefined`\>

***

### apiGetUserServers()

> **apiGetUserServers**(`userId`): `Promise`\<`UserOperatorServers`[]\>

Defined in: [src/api.ts:1020](../api.ts#L1020)

Get user servers and chat relays, grouped by operator.
Network usage: no.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`UserOperatorServers`[]\>

***

### apiJoinGroup()

> **apiJoinGroup**(`groupId`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:591](../api.ts#L591)

Join group.
Network usage: interactive.

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiLeaveGroup()

> **apiLeaveGroup**(`groupId`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:641](../api.ts#L641)

Leave group.
Network usage: background.

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiListContacts()

> **apiListContacts**(`userId`): `Promise`\<`Contact`[]\>

Defined in: [src/api.ts:873](../api.ts#L873)

Get contacts.
Network usage: no.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`Contact`[]\>

***

### apiListGroups()

> **apiListGroups**(`userId`, `contactId?`, `search?`): `Promise`\<`GroupInfo`[]\>

Defined in: [src/api.ts:883](../api.ts#L883)

Get groups.
Network usage: no.

#### Parameters

##### userId

`number`

##### contactId?

`number`

##### search?

`string`

#### Returns

`Promise`\<`GroupInfo`[]\>

***

### apiListMembers()

> **apiListMembers**(`groupId`): `Promise`\<`GroupMember`[]\>

Defined in: [src/api.ts:651](../api.ts#L651)

Get group members.
Network usage: no.

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupMember`[]\>

***

### apiListUsers()

> **apiListUsers**(): `Promise`\<`UserInfo`[]\>

Defined in: [src/api.ts:1060](../api.ts#L1060)

Get all user profiles
Network usage: no.

#### Returns

`Promise`\<`UserInfo`[]\>

***

### apiNewGroup()

> **apiNewGroup**(`userId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:661](../api.ts#L661)

Create group.
Network usage: no.

#### Parameters

##### userId

`number`

##### groupProfile

`GroupProfile`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiPrepareContact()

> **apiPrepareContact**(`userId`, `connLink`, `contactSLinkData`, `verifiedDomain?`): `Promise`\<`AChat`\>

Defined in: [src/api.ts:776](../api.ts#L776)

Prepare contact chat from link data before connecting.
Network usage: no.

#### Parameters

##### userId

`number`

##### connLink

`CreatedConnLink`

##### contactSLinkData

`ContactShortLinkData`

##### verifiedDomain?

`string`

#### Returns

`Promise`\<`AChat`\>

***

### apiPrepareGroup()

> **apiPrepareGroup**(`userId`, `connLinkContact`, `direct`, `groupSLinkData`, `verifiedDomain?`): `Promise`\<`AChat`\>

Defined in: [src/api.ts:786](../api.ts#L786)

Prepare group chat from link data before connecting.
Network usage: no.

#### Parameters

##### userId

`number`

##### connLinkContact

`CreatedConnLink`

##### direct

`boolean`

##### groupSLinkData

`GroupShortLinkData`

##### verifiedDomain?

`string`

#### Returns

`Promise`\<`AChat`\>

***

### apiReceiveFile()

> **apiReceiveFile**(`fileId`, `filePath?`, `approvedRelays?`): `Promise`\<`AChatItem`\>

Defined in: [src/api.ts:550](../api.ts#L550)

Receive file.
With `approvedRelays = false`, a file on unknown XFTP relays is received only when the IP address is protected; otherwise `fileNotApproved` is thrown.
Network usage: no.

#### Parameters

##### fileId

`number`

##### filePath?

`string`

##### approvedRelays?

`boolean` = `true`

#### Returns

`Promise`\<`AChatItem`\>

***

### apiRejectContactRequest()

> **apiRejectContactRequest**(`contactReqId`): `Promise`\<`void`\>

Defined in: [src/api.ts:863](../api.ts#L863)

Reject contact request. The user who sent the request is **not notified**.
Network usage: no.

#### Parameters

##### contactReqId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiRemoveMembers()

> **apiRemoveMembers**(`groupId`, `memberIds`, `withMessages?`): `Promise`\<`GroupMember`[]\>

Defined in: [src/api.ts:631](../api.ts#L631)

Remove members. Requires Admin role.
Network usage: background.

#### Parameters

##### groupId

`number`

##### memberIds

`number`[]

##### withMessages?

`boolean` = `false`

#### Returns

`Promise`\<`GroupMember`[]\>

***

### apiSendMemberContactInvitation()

> **apiSendMemberContactInvitation**(`contactId`, `message?`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:1127](../api.ts#L1127)

Send a direct message invitation to a group member contact.
The contact must have been created with [apiCreateMemberContact](#apicreatemembercontact).
Network usage: background.

#### Parameters

##### contactId

`number`

##### message?

`string` \| `MsgContent`

#### Returns

`Promise`\<`Contact`\>

***

### apiSendMessages()

> **apiSendMessages**(`chat`, `messages`, `liveMessage?`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:448](../api.ts#L448)

Send messages.
Network usage: background.

#### Parameters

##### chat

`ChatInfo` \| `ChatRef` \| \[`ChatType`, `number`\]

##### messages

`ComposedMessage`[]

##### liveMessage?

`boolean` = `false`

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSendTextMessage()

> **apiSendTextMessage**(`chat`, `text`, `inReplyTo?`, `priority?`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:472](../api.ts#L472)

Send text message.
`priority`: -1 silent, 0 normal, 1 high (groups only, for roles allowed by the group preference).
Network usage: background.

#### Parameters

##### chat

`ChatInfo` \| `ChatRef` \| \[`ChatType`, `number`\]

##### text

`string`

##### inReplyTo?

`number`

##### priority?

`number`

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSendTextReply()

> **apiSendTextReply**(`chatItem`, `text`, `priority?`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:481](../api.ts#L481)

Send text message in reply to received message.
`priority`: -1 silent, 0 normal, 1 high (groups only, for roles allowed by the group preference).
Network usage: background.

#### Parameters

##### chatItem

`AChatItem`

##### text

`string`

##### priority?

`number`

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSetActiveUser()

> **apiSetActiveUser**(`userId`, `viewPwd?`): `Promise`\<`User`\>

Defined in: [src/api.ts:1070](../api.ts#L1070)

Set active user profile
Network usage: no.

#### Parameters

##### userId

`number`

##### viewPwd?

`string`

#### Returns

`Promise`\<`User`\>

***

### apiSetAddressSettings()

> **apiSetAddressSettings**(`userId`, `__namedParameters`): `Promise`\<`void`\>

Defined in: [src/api.ts:419](../api.ts#L419)

Set bot address settings.
Network usage: interactive.

#### Parameters

##### userId

`number`

##### \_\_namedParameters

[`BotAddressSettings`](api.Interface.BotAddressSettings.md)

#### Returns

`Promise`\<`void`\>

***

### apiSetAutoAcceptMemberContacts()

> **apiSetAutoAcceptMemberContacts**(`userId`, `onOff`): `Promise`\<`void`\>

Defined in: [src/api.ts:949](../api.ts#L949)

Set auto-accept member contacts.
Network usage: no.

#### Parameters

##### userId

`number`

##### onOff

`boolean`

#### Returns

`Promise`\<`void`\>

***

### apiSetContactCustomData()

> **apiSetContactCustomData**(`contactId`, `customData?`): `Promise`\<`void`\>

Defined in: [src/api.ts:939](../api.ts#L939)

Set contact custom data.
Network usage: no.

#### Parameters

##### contactId

`number`

##### customData?

`object`

#### Returns

`Promise`\<`void`\>

***

### apiSetContactPrefs()

> **apiSetContactPrefs**(`contactId`, `preferences`): `Promise`\<`void`\>

Defined in: [src/api.ts:1106](../api.ts#L1106)

Configure chat preference overrides for the contact.
Network usage: background.

#### Parameters

##### contactId

`number`

##### preferences

`Preferences`

#### Returns

`Promise`\<`void`\>

***

### apiSetGroupCustomData()

> **apiSetGroupCustomData**(`groupId`, `customData?`): `Promise`\<`void`\>

Defined in: [src/api.ts:929](../api.ts#L929)

Set group custom data.
Network usage: no.

#### Parameters

##### groupId

`number`

##### customData?

`object`

#### Returns

`Promise`\<`void`\>

***

### apiSetGroupLinkMemberRole()

> **apiSetGroupLinkMemberRole**(`groupId`, `memberRole`): `Promise`\<`void`\>

Defined in: [src/api.ts:694](../api.ts#L694)

Set member role for group link.
Network usage: no.

#### Parameters

##### groupId

`number`

##### memberRole

`GroupMemberRole`

#### Returns

`Promise`\<`void`\>

***

### apiSetMembersRole()

> **apiSetMembersRole**(`groupId`, `groupMemberIds`, `memberRole`): `Promise`\<`void`\>

Defined in: [src/api.ts:611](../api.ts#L611)

Set members role. Requires Admin role.
Network usage: background.

#### Parameters

##### groupId

`number`

##### groupMemberIds

`number`[]

##### memberRole

`GroupMemberRole`

#### Returns

`Promise`\<`void`\>

***

### apiSetNetworkConfig()

> **apiSetNetworkConfig**(`networkConfig`): `Promise`\<`void`\>

Defined in: [src/api.ts:1010](../api.ts#L1010)

Set network configuration.
Network usage: background.

#### Parameters

##### networkConfig

`NetworkConfig`

#### Returns

`Promise`\<`void`\>

***

### apiSetProfileAddress()

> **apiSetProfileAddress**(`userId`, `enable`): `Promise`\<`UserProfileUpdateSummary`\>

Defined in: [src/api.ts:403](../api.ts#L403)

Add address to bot profile.
Network usage: interactive.

#### Parameters

##### userId

`number`

##### enable

`boolean`

#### Returns

`Promise`\<`UserProfileUpdateSummary`\>

***

### apiSetUserDomain()

> **apiSetUserDomain**(`userId`, `simplexDomain?`): `Promise`\<`User`\>

Defined in: [src/api.ts:433](../api.ts#L433)

#### Parameters

##### userId

`number`

##### simplexDomain?

`string`

#### Returns

`Promise`\<`User`\>

***

### apiUpdateChatItem()

> **apiUpdateChatItem**(`chatType`, `chatId`, `chatItemId`, `msgContent`, `liveMessage?`, `priority?`): `Promise`\<`ChatItem`\>

Defined in: [src/api.ts:491](../api.ts#L491)

Update message.
A live message stays live with `liveMessage = true` and is finalized with `liveMessage = false`.
`priority = 1` raises the message to high priority (groups only, for roles allowed by the group preference).
Network usage: background.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### chatItemId

`number`

##### msgContent

`MsgContent`

##### liveMessage?

`boolean` = `false`

##### priority?

`number`

#### Returns

`Promise`\<`ChatItem`\>

***

### apiUpdateGroupProfile()

> **apiUpdateGroupProfile**(`groupId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:671](../api.ts#L671)

Update group profile.
Network usage: background.

#### Parameters

##### groupId

`number`

##### groupProfile

`GroupProfile`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiUpdateProfile()

> **apiUpdateProfile**(`userId`, `profile`): `Promise`\<`UserProfileUpdateSummary` \| `undefined`\>

Defined in: [src/api.ts:1090](../api.ts#L1090)

Update user profile.
Network usage: background.

#### Parameters

##### userId

`number`

##### profile

`Profile`

#### Returns

`Promise`\<`UserProfileUpdateSummary` \| `undefined`\>

***

### close()

> **close**(): `Promise`\<`void`\>

Defined in: [src/api.ts:165](../api.ts#L165)

Stop chat controller and close chat database.
The database is not closed if stopping fails.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### off()

> **off**\<`K`\>(`event`, `subscriber?`): `void`

Defined in: [src/api.ts:306](../api.ts#L306)

Unsubscribe all or a specific handler from a specific event.

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

The event type to unsubscribe from.

##### subscriber?

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`K`\> \| `undefined` = `undefined`

An optional subscriber function for the event.

#### Returns

`void`

***

### offAny()

> **offAny**(`receiver?`): `void`

Defined in: [src/api.ts:322](../api.ts#L322)

Unsubscribe all or a specific handler from any events.

#### Parameters

##### receiver?

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`Tag`\> \| `undefined` = `undefined`

An optional subscriber function for the event.

#### Returns

`void`

***

### on()

#### Call Signature

> **on**\<`K`\>(`subscribers`): `void`

Defined in: [src/api.ts:214](../api.ts#L214)

Subscribe multiple event handlers at once.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### subscribers

[`EventSubscribers`](api.TypeAlias.EventSubscribers.md)

An object mapping event types (CEvt.Tag) to their subscriber functions.

##### Returns

`void`

##### Throws

If the same function is subscribed to event.

#### Call Signature

> **on**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:222](../api.ts#L222)

Subscribe a handler to a specific event.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

The event type to subscribe to.

###### subscriber

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`K`\>

The subscriber function for the event.

##### Returns

`void`

##### Throws

If the same function is subscribed to event.

***

### onAny()

> **onAny**(`receiver`): `void`

Defined in: [src/api.ts:245](../api.ts#L245)

Subscribe a handler to any event.

#### Parameters

##### receiver

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`Tag`\>

The receiver function for any event.

#### Returns

`void`

#### Throws

If the same function is subscribed to event.

***

### once()

> **once**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:256](../api.ts#L256)

Subscribe a handler to a specific event to be delivered one time.

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

The event type to subscribe to.

##### subscriber

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`K`\>

The subscriber function for the event.

#### Returns

`void`

#### Throws

If the same function is subscribed to event.

***

### recvChatEvent()

> **recvChatEvent**(`wait?`): `Promise`\<`ChatEvent` \| `undefined`\>

Defined in: [src/api.ts:357](../api.ts#L357)

#### Parameters

##### wait?

`number` = `500_000`

#### Returns

`Promise`\<`ChatEvent` \| `undefined`\>

***

### sendChatCmd()

> **sendChatCmd**(`cmd`): `Promise`\<`ChatResponse`\>

Defined in: [src/api.ts:353](../api.ts#L353)

#### Parameters

##### cmd

`string`

#### Returns

`Promise`\<`ChatResponse`\>

***

### startChat()

> **startChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:131](../api.ts#L131)

Start chat controller. Must be called with the existing user profile.

#### Returns

`Promise`\<`void`\>

***

### stopChat()

> **stopChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:154](../api.ts#L154)

Stop chat controller.
`close` calls it before closing the database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### wait()

#### Call Signature

> **wait**\<`K`\>(`event`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:264](../api.ts#L264)

Waits for specific event, with an optional predicate.
Returns `undefined` on timeout if specified.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

##### Returns

`Promise`\<`ChatEvent` & `object`\>

#### Call Signature

> **wait**\<`K`\>(`event`, `predicate`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:265](../api.ts#L265)

Waits for specific event, with an optional predicate.
Returns `undefined` on timeout if specified.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

###### predicate

((`event`) => `boolean`) \| `undefined`

##### Returns

`Promise`\<`ChatEvent` & `object`\>

#### Call Signature

> **wait**\<`K`\>(`event`, `timeout`): `Promise`\<ChatEvent & \{ type: K; \} \| `undefined`\>

Defined in: [src/api.ts:266](../api.ts#L266)

Waits for specific event, with an optional predicate.
Returns `undefined` on timeout if specified.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

###### timeout

`number`

##### Returns

`Promise`\<ChatEvent & \{ type: K; \} \| `undefined`\>

#### Call Signature

> **wait**\<`K`\>(`event`, `predicate`, `timeout`): `Promise`\<ChatEvent & \{ type: K; \} \| `undefined`\>

Defined in: [src/api.ts:267](../api.ts#L267)

Waits for specific event, with an optional predicate.
Returns `undefined` on timeout if specified.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

###### predicate

((`event`) => `boolean`) \| `undefined`

###### timeout

`number`

##### Returns

`Promise`\<ChatEvent & \{ type: K; \} \| `undefined`\>

***

### init()

> `static` **init**(`db`, `confirm?`, `queueSize?`): `Promise`\<`ChatApi`\>

Defined in: [src/api.ts:117](../api.ts#L117)

Initializes the ChatApi, loading libsimplex for `db.type` (downloaded on first use).

#### Parameters

##### db

[`DbConfig`](api.TypeAlias.DbConfig.md)

Database configuration (sqlite or postgres).

##### confirm?

[`MigrationConfirmation`](core.Enumeration.MigrationConfirmation.md) = `core.MigrationConfirmation.YesUp`

Migration confirmation mode.

##### queueSize?

`number`

Size of internal queues, the core default is used when omitted.

#### Returns

`Promise`\<`ChatApi`\>
