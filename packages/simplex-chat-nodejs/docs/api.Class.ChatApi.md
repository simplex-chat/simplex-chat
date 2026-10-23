[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / ChatApi

# Class: ChatApi

Defined in: [src/api.ts:100](../api.ts#L100)

Main API class for interacting with the chat core library.

## Properties

### ctrl\_

> `protected` **ctrl\_**: `bigint` \| `undefined`

Defined in: [src/api.ts:106](../api.ts#L106)

## Accessors

### ctrl

#### Get Signature

> **get** **ctrl**(): `bigint`

Defined in: [src/api.ts:345](../api.ts#L345)

Chat controller reference

##### Returns

`bigint`

***

### initialized

#### Get Signature

> **get** **initialized**(): `boolean`

Defined in: [src/api.ts:331](../api.ts#L331)

Chat controller is initialized

##### Returns

`boolean`

***

### started

#### Get Signature

> **get** **started**(): `boolean`

Defined in: [src/api.ts:338](../api.ts#L338)

Chat controller is started

##### Returns

`boolean`

## Methods

### apiAcceptContactRequest()

> **apiAcceptContactRequest**(`contactReqId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:819](../api.ts#L819)

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

Defined in: [src/api.ts:593](../api.ts#L593)

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

Defined in: [src/api.ts:563](../api.ts#L563)

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

Defined in: [src/api.ts:573](../api.ts#L573)

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

Defined in: [src/api.ts:613](../api.ts#L613)

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

Defined in: [src/api.ts:553](../api.ts#L553)

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

Defined in: [src/api.ts:526](../api.ts#L526)

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

Defined in: [src/api.ts:932](../api.ts#L932)

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

Defined in: [src/api.ts:742](../api.ts#L742)

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

Defined in: [src/api.ts:797](../api.ts#L797)

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

Defined in: [src/api.ts:787](../api.ts#L787)

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

Defined in: [src/api.ts:732](../api.ts#L732)

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

Defined in: [src/api.ts:770](../api.ts#L770)

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

Defined in: [src/api.ts:781](../api.ts#L781)

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

Defined in: [src/api.ts:746](../api.ts#L746)

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

Defined in: [src/api.ts:938](../api.ts#L938)

#### Parameters

##### contactId

`number`

#### Returns

`Promise`\<`ContactInfo`\>

***

### apiCreateActiveUser()

> **apiCreateActiveUser**(`profile?`): `Promise`\<`User`\>

Defined in: [src/api.ts:980](../api.ts#L980)

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

Defined in: [src/api.ts:673](../api.ts#L673)

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

Defined in: [src/api.ts:719](../api.ts#L719)

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

Defined in: [src/api.ts:1046](../api.ts#L1046)

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

Defined in: [src/api.ts:362](../api.ts#L362)

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

Defined in: [src/api.ts:878](../api.ts#L878)

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

Defined in: [src/api.ts:501](../api.ts#L501)

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

Defined in: [src/api.ts:695](../api.ts#L695)

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

Defined in: [src/api.ts:516](../api.ts#L516)

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

Defined in: [src/api.ts:1010](../api.ts#L1010)

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

Defined in: [src/api.ts:372](../api.ts#L372)

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

Defined in: [src/api.ts:960](../api.ts#L960)

Get active user profile
Network usage: no.

#### Returns

`Promise`\<`User` \| `undefined`\>

***

### apiGetChat()

> **apiGetChat**(`chatType`, `chatId`, `pagination`): `Promise`\<`AChat`\>

Defined in: [src/api.ts:925](../api.ts#L925)

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

### apiGetChats()

> **apiGetChats**(`userId`, `pagination`, `query?`, `pendingConnections?`): `Promise`\<`AChat`[]\>

Defined in: [src/api.ts:863](../api.ts#L863)

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

Defined in: [src/api.ts:704](../api.ts#L704)

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

Defined in: [src/api.ts:710](../api.ts#L710)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiGetNetworkConfig()

> **apiGetNetworkConfig**(): `Promise`\<`NetworkConfig`\>

Defined in: [src/api.ts:944](../api.ts#L944)

#### Returns

`Promise`\<`NetworkConfig`\>

***

### apiGetUserAddress()

> **apiGetUserAddress**(`userId`): `Promise`\<`UserContactLink` \| `undefined`\>

Defined in: [src/api.ts:382](../api.ts#L382)

Get bot address and settings.
Network usage: no.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`UserContactLink` \| `undefined`\>

***

### apiJoinGroup()

> **apiJoinGroup**(`groupId`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:583](../api.ts#L583)

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

Defined in: [src/api.ts:633](../api.ts#L633)

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

Defined in: [src/api.ts:839](../api.ts#L839)

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

Defined in: [src/api.ts:849](../api.ts#L849)

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

Defined in: [src/api.ts:643](../api.ts#L643)

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

Defined in: [src/api.ts:990](../api.ts#L990)

Get all user profiles
Network usage: no.

#### Returns

`Promise`\<`UserInfo`[]\>

***

### apiNewGroup()

> **apiNewGroup**(`userId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:653](../api.ts#L653)

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

Defined in: [src/api.ts:758](../api.ts#L758)

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

Defined in: [src/api.ts:764](../api.ts#L764)

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

> **apiReceiveFile**(`fileId`, `filePath?`): `Promise`\<`AChatItem`\>

Defined in: [src/api.ts:542](../api.ts#L542)

Receive file.
Network usage: no.

#### Parameters

##### fileId

`number`

##### filePath?

`string`

#### Returns

`Promise`\<`AChatItem`\>

***

### apiRejectContactRequest()

> **apiRejectContactRequest**(`contactReqId`): `Promise`\<`void`\>

Defined in: [src/api.ts:829](../api.ts#L829)

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

Defined in: [src/api.ts:623](../api.ts#L623)

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

Defined in: [src/api.ts:1057](../api.ts#L1057)

Send a direct message invitation to a group member contact.
The contact must have been created with [apiCreateMemberContact](#apicreatemembercontact).
Network usage: interactive.

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

Defined in: [src/api.ts:444](../api.ts#L444)

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

> **apiSendTextMessage**(`chat`, `text`, `inReplyTo?`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:467](../api.ts#L467)

Send text message.
Network usage: background.

#### Parameters

##### chat

`ChatInfo` \| `ChatRef` \| \[`ChatType`, `number`\]

##### text

`string`

##### inReplyTo?

`number`

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSendTextReply()

> **apiSendTextReply**(`chatItem`, `text`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:475](../api.ts#L475)

Send text message in reply to received message.
Network usage: background.

#### Parameters

##### chatItem

`AChatItem`

##### text

`string`

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSetActiveUser()

> **apiSetActiveUser**(`userId`, `viewPwd?`): `Promise`\<`User`\>

Defined in: [src/api.ts:1000](../api.ts#L1000)

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

Defined in: [src/api.ts:416](../api.ts#L416)

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

Defined in: [src/api.ts:915](../api.ts#L915)

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

Defined in: [src/api.ts:905](../api.ts#L905)

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

Defined in: [src/api.ts:1036](../api.ts#L1036)

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

Defined in: [src/api.ts:895](../api.ts#L895)

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

Defined in: [src/api.ts:686](../api.ts#L686)

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

Defined in: [src/api.ts:603](../api.ts#L603)

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

Defined in: [src/api.ts:950](../api.ts#L950)

#### Parameters

##### networkConfig

`NetworkConfig`

#### Returns

`Promise`\<`void`\>

***

### apiSetProfileAddress()

> **apiSetProfileAddress**(`userId`, `enable`): `Promise`\<`UserProfileUpdateSummary`\>

Defined in: [src/api.ts:400](../api.ts#L400)

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

Defined in: [src/api.ts:429](../api.ts#L429)

#### Parameters

##### userId

`number`

##### simplexDomain?

`string`

#### Returns

`Promise`\<`User`\>

***

### apiUpdateChatItem()

> **apiUpdateChatItem**(`chatType`, `chatId`, `chatItemId`, `msgContent`, `liveMessage?`): `Promise`\<`ChatItem`\>

Defined in: [src/api.ts:484](../api.ts#L484)

Update message.
A live message stays live with `liveMessage = true` and is finalized with `liveMessage = false`.
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

#### Returns

`Promise`\<`ChatItem`\>

***

### apiUpdateGroupProfile()

> **apiUpdateGroupProfile**(`groupId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:663](../api.ts#L663)

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

Defined in: [src/api.ts:1020](../api.ts#L1020)

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

Defined in: [src/api.ts:162](../api.ts#L162)

Stop chat controller and close chat database.
The database is not closed if stopping fails.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### off()

> **off**\<`K`\>(`event`, `subscriber?`): `void`

Defined in: [src/api.ts:303](../api.ts#L303)

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

Defined in: [src/api.ts:319](../api.ts#L319)

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

Defined in: [src/api.ts:211](../api.ts#L211)

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

Defined in: [src/api.ts:219](../api.ts#L219)

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

Defined in: [src/api.ts:242](../api.ts#L242)

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

Defined in: [src/api.ts:253](../api.ts#L253)

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

Defined in: [src/api.ts:354](../api.ts#L354)

#### Parameters

##### wait?

`number` = `500_000`

#### Returns

`Promise`\<`ChatEvent` \| `undefined`\>

***

### sendChatCmd()

> **sendChatCmd**(`cmd`): `Promise`\<`ChatResponse`\>

Defined in: [src/api.ts:350](../api.ts#L350)

#### Parameters

##### cmd

`string`

#### Returns

`Promise`\<`ChatResponse`\>

***

### startChat()

> **startChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:128](../api.ts#L128)

Start chat controller. Must be called with the existing user profile.

#### Returns

`Promise`\<`void`\>

***

### stopChat()

> **stopChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:151](../api.ts#L151)

Stop chat controller.
`close` calls it before closing the database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### wait()

#### Call Signature

> **wait**\<`K`\>(`event`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:261](../api.ts#L261)

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

Defined in: [src/api.ts:262](../api.ts#L262)

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

Defined in: [src/api.ts:263](../api.ts#L263)

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

Defined in: [src/api.ts:264](../api.ts#L264)

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

Defined in: [src/api.ts:114](../api.ts#L114)

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
