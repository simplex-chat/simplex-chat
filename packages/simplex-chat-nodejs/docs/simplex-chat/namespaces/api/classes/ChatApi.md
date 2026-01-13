[**simplex-chat**](../../../../README.md)

***

[simplex-chat](../../../../globals.md) / [api](../README.md) / ChatApi

# Class: ChatApi

Defined in: [src/api.ts:35](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L35)

## Properties

### ctrl\_

> `protected` **ctrl\_**: `bigint` \| `undefined`

Defined in: [src/api.ts:41](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L41)

## Accessors

### ctrl

#### Get Signature

> **get** **ctrl**(): `bigint`

Defined in: [src/api.ts:207](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L207)

##### Returns

`bigint`

***

### initialized

#### Get Signature

> **get** **initialized**(): `boolean`

Defined in: [src/api.ts:199](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L199)

##### Returns

`boolean`

***

### receiving

#### Get Signature

> **get** **receiving**(): `boolean`

Defined in: [src/api.ts:203](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L203)

##### Returns

`boolean`

## Methods

### apiAcceptContactRequest()

> **apiAcceptContactRequest**(`contactReqId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:554](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L554)

#### Parameters

##### contactReqId

`number`

#### Returns

`Promise`\<`Contact`\>

***

### apiAcceptMember()

> **apiAcceptMember**(`groupId`, `groupMemberId`, `memberRole`): `Promise`\<`GroupMember`\>

Defined in: [src/api.ts:400](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L400)

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

Defined in: [src/api.ts:384](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L384)

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

### apiBlockMembersForAll()

> **apiBlockMembersForAll**(`groupId`, `groupMemberIds`, `blocked`): `Promise`\<`void`\>

Defined in: [src/api.ts:416](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L416)

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

Defined in: [src/api.ts:373](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L373)

#### Parameters

##### fileId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiChatItemReaction()

> **apiChatItemReaction**(`chatType`, `chatId`, `chatItemId`, `add`, `reaction`): `Promise`\<`ChatItemDeletion`[]\>

Defined in: [src/api.ts:348](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L348)

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

`Promise`\<`ChatItemDeletion`[]\>

***

### apiConnect()

> **apiConnect**(`userId`, `incognito`, `preparedLink?`): `Promise`\<[`ConnReqType`](../enumerations/ConnReqType.md)\>

Defined in: [src/api.ts:527](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L527)

#### Parameters

##### userId

`number`

##### incognito

`boolean`

##### preparedLink?

`CreatedConnLink`

#### Returns

`Promise`\<[`ConnReqType`](../enumerations/ConnReqType.md)\>

***

### apiConnectActiveUser()

> **apiConnectActiveUser**(`connLink`): `Promise`\<[`ConnReqType`](../enumerations/ConnReqType.md)\>

Defined in: [src/api.ts:534](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L534)

#### Parameters

##### connLink

`string`

#### Returns

`Promise`\<[`ConnReqType`](../enumerations/ConnReqType.md)\>

***

### apiConnectPlan()

> **apiConnectPlan**(`userId`, `connectionLink`): `Promise`\<`ConnectionPlan`\>

Defined in: [src/api.ts:519](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L519)

#### Parameters

##### userId

`number`

##### connectionLink

`string`

#### Returns

`Promise`\<`ConnectionPlan`\>

***

### apiCreateActiveUser()

> **apiCreateActiveUser**(`profile?`): `Promise`\<`User`\>

Defined in: [src/api.ts:625](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L625)

#### Parameters

##### profile?

`Profile`

#### Returns

`Promise`\<`User`\>

***

### apiCreateGroupLink()

> **apiCreateGroupLink**(`groupId`, `memberRole`): `Promise`\<`string`\>

Defined in: [src/api.ts:467](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L467)

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

Defined in: [src/api.ts:508](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L508)

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiCreateUserAddress()

> **apiCreateUserAddress**(`userId`): `Promise`\<`CreatedConnLink`\>

Defined in: [src/api.ts:225](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L225)

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`CreatedConnLink`\>

***

### apiDeleteChat()

> **apiDeleteChat**(`chatType`, `chatId`, `deleteMode`): `Promise`\<`void`\>

Defined in: [src/api.ts:589](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L589)

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### deleteMode

`ChatDeleteMode` = `...`

#### Returns

`Promise`\<`void`\>

***

### apiDeleteChatItems()

> **apiDeleteChatItems**(`chatType`, `chatId`, `chatItemIds`, `deleteMode`): `Promise`\<`ChatItemDeletion`[]\>

Defined in: [src/api.ts:327](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L327)

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

Defined in: [src/api.ts:485](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L485)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiDeleteMemberChatItem()

> **apiDeleteMemberChatItem**(`groupId`, `chatItemIds`): `Promise`\<`ChatItemDeletion`[]\>

Defined in: [src/api.ts:340](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L340)

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

Defined in: [src/api.ts:649](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L649)

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

Defined in: [src/api.ts:235](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L235)

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

Defined in: [src/api.ts:607](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L607)

#### Returns

`Promise`\<`User` \| `undefined`\>

***

### apiGetGroupLink()

> **apiGetGroupLink**(`groupId`): `Promise`\<`GroupLink`\>

Defined in: [src/api.ts:492](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L492)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupLink`\>

***

### apiGetGroupLinkStr()

> **apiGetGroupLinkStr**(`groupId`): `Promise`\<`string`\>

Defined in: [src/api.ts:498](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L498)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiGetUserAddress()

> **apiGetUserAddress**(`userId`): `Promise`\<`UserContactLink` \| `undefined`\>

Defined in: [src/api.ts:243](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L243)

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`UserContactLink` \| `undefined`\>

***

### apiJoinGroup()

> **apiJoinGroup**(`groupId`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:392](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L392)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiLeaveGroup()

> **apiLeaveGroup**(`groupId`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:432](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L432)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiListContacts()

> **apiListContacts**(`userId`): `Promise`\<`Contact`[]\>

Defined in: [src/api.ts:573](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L573)

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`Contact`[]\>

***

### apiListGroups()

> **apiListGroups**(`userId`, `contactId?`, `search?`): `Promise`\<`GroupInfo`[]\>

Defined in: [src/api.ts:581](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L581)

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

Defined in: [src/api.ts:440](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L440)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`GroupMember`[]\>

***

### apiListUsers()

> **apiListUsers**(): `Promise`\<`UserInfo`[]\>

Defined in: [src/api.ts:633](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L633)

#### Returns

`Promise`\<`UserInfo`[]\>

***

### apiNewGroup()

> **apiNewGroup**(`userId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:448](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L448)

#### Parameters

##### userId

`number`

##### groupProfile

`GroupProfile`

#### Returns

`Promise`\<`GroupInfo`\>

***

### apiReceiveFile()

> **apiReceiveFile**(`fileId`): `Promise`\<`AChatItem`\>

Defined in: [src/api.ts:365](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L365)

#### Parameters

##### fileId

`number`

#### Returns

`Promise`\<`AChatItem`\>

***

### apiRejectContactRequest()

> **apiRejectContactRequest**(`contactReqId`): `Promise`\<`void`\>

Defined in: [src/api.ts:562](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L562)

#### Parameters

##### contactReqId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiRemoveMembers()

> **apiRemoveMembers**(`groupId`, `memberIds`, `withMessages`): `Promise`\<`GroupMember`[]\>

Defined in: [src/api.ts:424](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L424)

#### Parameters

##### groupId

`number`

##### memberIds

`number`[]

##### withMessages

`boolean` = `false`

#### Returns

`Promise`\<`GroupMember`[]\>

***

### apiSendMessages()

> **apiSendMessages**(`chat`, `messages`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:288](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L288)

#### Parameters

##### chat

`ChatInfo` | `ChatRef` | \[`ChatType`, `number`\]

##### messages

`ComposedMessage`[]

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSendTextMessage()

> **apiSendTextMessage**(`chat`, `text`, `inReplyTo?`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:302](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L302)

#### Parameters

##### chat

`ChatInfo` | `ChatRef` | \[`ChatType`, `number`\]

##### text

`string`

##### inReplyTo?

`number`

#### Returns

`Promise`\<`AChatItem`[]\>

***

### apiSendTextReply()

> **apiSendTextReply**(`chatItem`, `text`): `Promise`\<`AChatItem`[]\>

Defined in: [src/api.ts:306](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L306)

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

Defined in: [src/api.ts:641](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L641)

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

Defined in: [src/api.ts:271](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L271)

#### Parameters

##### userId

`number`

##### \_\_namedParameters

[`BotAddressSettings`](../interfaces/BotAddressSettings.md)

#### Returns

`Promise`\<`void`\>

***

### apiSetContactPrefs()

> **apiSetContactPrefs**(`contactId`, `preferences`): `Promise`\<`void`\>

Defined in: [src/api.ts:671](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L671)

#### Parameters

##### contactId

`number`

##### preferences

`Preferences`

#### Returns

`Promise`\<`void`\>

***

### apiSetGroupLinkMemberRole()

> **apiSetGroupLinkMemberRole**(`groupId`, `memberRole`): `Promise`\<`void`\>

Defined in: [src/api.ts:478](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L478)

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

Defined in: [src/api.ts:408](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L408)

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

### apiSetProfileAddress()

> **apiSetProfileAddress**(`userId`, `enable`): `Promise`\<`UserProfileUpdateSummary`\>

Defined in: [src/api.ts:259](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L259)

#### Parameters

##### userId

`number`

##### enable

`boolean`

#### Returns

`Promise`\<`UserProfileUpdateSummary`\>

***

### apiUpdateChatItem()

> **apiUpdateChatItem**(`chatType`, `chatId`, `chatItemId`, `msgContent`): `Promise`\<`ChatItem`\>

Defined in: [src/api.ts:312](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L312)

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### chatItemId

`number`

##### msgContent

`MsgContent`

#### Returns

`Promise`\<`ChatItem`\>

***

### apiUpdateGroupProfile()

> **apiUpdateGroupProfile**(`groupId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:456](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L456)

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

Defined in: [src/api.ts:657](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L657)

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

Defined in: [src/api.ts:69](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L69)

#### Returns

`Promise`\<`void`\>

***

### off()

> **off**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:178](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L178)

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

##### subscriber

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`K`\> | `undefined`

#### Returns

`void`

***

### offAny()

> **offAny**(`receiver`): `void`

Defined in: [src/api.ts:190](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L190)

#### Parameters

##### receiver

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`Tag`\> | `undefined`

#### Returns

`void`

***

### on()

#### Call Signature

> **on**\<`K`\>(`subscribers`): `void`

Defined in: [src/api.ts:113](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L113)

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### subscribers

###### acceptingBusinessRequest?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"acceptingBusinessRequest"`\>

###### acceptingContactRequest?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"acceptingContactRequest"`\>

###### businessLinkConnecting?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"businessLinkConnecting"`\>

###### chatError?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"chatError"`\>

###### chatErrors?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"chatErrors"`\>

###### chatItemReaction?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"chatItemReaction"`\>

###### chatItemsDeleted?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"chatItemsDeleted"`\>

###### chatItemsStatusesUpdated?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"chatItemsStatusesUpdated"`\>

###### chatItemUpdated?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"chatItemUpdated"`\>

###### connectedToGroupMember?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"connectedToGroupMember"`\>

###### contactConnected?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"contactConnected"`\>

###### contactConnecting?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"contactConnecting"`\>

###### contactDeletedByContact?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"contactDeletedByContact"`\>

###### contactSndReady?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"contactSndReady"`\>

###### contactUpdated?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"contactUpdated"`\>

###### deletedMember?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"deletedMember"`\>

###### deletedMemberUser?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"deletedMemberUser"`\>

###### groupChatItemsDeleted?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"groupChatItemsDeleted"`\>

###### groupDeleted?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"groupDeleted"`\>

###### groupLinkConnecting?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"groupLinkConnecting"`\>

###### groupMemberUpdated?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"groupMemberUpdated"`\>

###### groupUpdated?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"groupUpdated"`\>

###### hostConnected?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"hostConnected"`\>

###### hostDisconnected?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"hostDisconnected"`\>

###### joinedGroupMember?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"joinedGroupMember"`\>

###### joinedGroupMemberConnecting?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"joinedGroupMemberConnecting"`\>

###### leftMember?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"leftMember"`\>

###### memberAcceptedByOther?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"memberAcceptedByOther"`\>

###### memberBlockedForAll?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"memberBlockedForAll"`\>

###### memberRole?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"memberRole"`\>

###### messageError?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"messageError"`\>

###### newChatItems?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"newChatItems"`\>

###### newMemberContactReceivedInv?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"newMemberContactReceivedInv"`\>

###### rcvFileAccepted?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileAccepted"`\>

###### rcvFileComplete?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileComplete"`\>

###### rcvFileDescrReady?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileDescrReady"`\>

###### rcvFileError?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileError"`\>

###### rcvFileSndCancelled?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileSndCancelled"`\>

###### rcvFileStart?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileStart"`\>

###### rcvFileWarning?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"rcvFileWarning"`\>

###### receivedContactRequest?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"receivedContactRequest"`\>

###### receivedGroupInvitation?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"receivedGroupInvitation"`\>

###### sentGroupInvitation?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"sentGroupInvitation"`\>

###### sndFileCompleteXFTP?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"sndFileCompleteXFTP"`\>

###### sndFileError?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"sndFileError"`\>

###### sndFileWarning?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"sndFileWarning"`\>

###### subscriptionStatus?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"subscriptionStatus"`\>

###### userJoinedGroup?

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`"userJoinedGroup"`\>

##### Returns

`void`

#### Call Signature

> **on**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:114](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L114)

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

###### subscriber

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`K`\>

##### Returns

`void`

***

### onAny()

> **onAny**(`receiver`): `void`

Defined in: [src/api.ts:132](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L132)

#### Parameters

##### receiver

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`Tag`\>

#### Returns

`void`

***

### once()

> **once**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:137](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L137)

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

##### subscriber

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`K`\>

#### Returns

`void`

***

### recvChatEvent()

> **recvChatEvent**(`wait`): `Promise`\<`ChatEvent` \| `undefined`\>

Defined in: [src/api.ts:216](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L216)

#### Parameters

##### wait

`number` = `5_000_000`

#### Returns

`Promise`\<`ChatEvent` \| `undefined`\>

***

### sendChatCmd()

> **sendChatCmd**(`cmd`): `Promise`\<`ChatResponse`\>

Defined in: [src/api.ts:212](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L212)

#### Parameters

##### cmd

`string`

#### Returns

`Promise`\<`ChatResponse`\>

***

### startChat()

> **startChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:52](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L52)

#### Returns

`Promise`\<`void`\>

***

### stopChat()

> **stopChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:61](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L61)

#### Returns

`Promise`\<`void`\>

***

### wait()

#### Call Signature

> **wait**\<`K`\>(`event`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:143](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L143)

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

Defined in: [src/api.ts:144](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L144)

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

###### predicate

(`event`) => `boolean` | `undefined`

##### Returns

`Promise`\<`ChatEvent` & `object`\>

#### Call Signature

> **wait**\<`K`\>(`event`, `timeout`): `Promise`\<ChatEvent & \{ type: K; \} \| `undefined`\>

Defined in: [src/api.ts:145](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L145)

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

Defined in: [src/api.ts:146](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L146)

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

###### predicate

(`event`) => `boolean` | `undefined`

###### timeout

`number`

##### Returns

`Promise`\<ChatEvent & \{ type: K; \} \| `undefined`\>

***

### init()

> `static` **init**(`dbPath`, `dbKey`, `confirm`): `Promise`\<`ChatApi`\>

Defined in: [src/api.ts:43](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L43)

#### Parameters

##### dbPath

`string`

##### dbKey

`string` = `""`

##### confirm

[`MigrationConfirmation`](../../core/enumerations/MigrationConfirmation.md) = `core.MigrationConfirmation.YesUp`

#### Returns

`Promise`\<`ChatApi`\>
