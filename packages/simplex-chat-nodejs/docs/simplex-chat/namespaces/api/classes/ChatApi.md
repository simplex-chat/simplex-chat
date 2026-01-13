[**simplex-chat**](../../../../README.md)

***

[simplex-chat](../../../../globals.md) / [api](../README.md) / ChatApi

# Class: ChatApi

Defined in: [src/api.ts:62](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L62)

Main API class for interacting with the chat core library.

## Properties

### ctrl\_

> `protected` **ctrl\_**: `bigint` \| `undefined`

Defined in: [src/api.ts:68](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L68)

## Accessors

### ctrl

#### Get Signature

> **get** **ctrl**(): `bigint`

Defined in: [src/api.ts:295](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L295)

Chat controller reference

##### Returns

`bigint`

***

### initialized

#### Get Signature

> **get** **initialized**(): `boolean`

Defined in: [src/api.ts:281](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L281)

Chat controller is initialized

##### Returns

`boolean`

***

### started

#### Get Signature

> **get** **started**(): `boolean`

Defined in: [src/api.ts:288](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L288)

Chat controller is started

##### Returns

`boolean`

## Methods

### apiAcceptContactRequest()

> **apiAcceptContactRequest**(`contactReqId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:692](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L692)

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

Defined in: [src/api.ts:512](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L512)

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

Defined in: [src/api.ts:492](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L492)

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

### apiBlockMembersForAll()

> **apiBlockMembersForAll**(`groupId`, `groupMemberIds`, `blocked`): `Promise`\<`void`\>

Defined in: [src/api.ts:532](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L532)

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

Defined in: [src/api.ts:482](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L482)

Cancel file.
Network usage: background.

#### Parameters

##### fileId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiChatItemReaction()

> **apiChatItemReaction**(`chatType`, `chatId`, `chatItemId`, `add`, `reaction`): `Promise`\<`ChatItemDeletion`[]\>

Defined in: [src/api.ts:456](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L456)

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

`Promise`\<`ChatItemDeletion`[]\>

***

### apiConnect()

> **apiConnect**(`userId`, `incognito`, `preparedLink?`): `Promise`\<[`ConnReqType`](../enumerations/ConnReqType.md)\>

Defined in: [src/api.ts:661](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L661)

Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link
Network usage: interactive.

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

Defined in: [src/api.ts:670](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L670)

Connect via SimpleX link as string in the active user profile.
Network usage: interactive.

#### Parameters

##### connLink

`string`

#### Returns

`Promise`\<[`ConnReqType`](../enumerations/ConnReqType.md)\>

***

### apiConnectPlan()

> **apiConnectPlan**(`userId`, `connectionLink`): `Promise`\<`ConnectionPlan`\>

Defined in: [src/api.ts:651](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L651)

Determine SimpleX link type and if the bot is already connected via this link.
Network usage: interactive.

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

Defined in: [src/api.ts:769](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L769)

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

Defined in: [src/api.ts:592](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L592)

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

Defined in: [src/api.ts:638](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L638)

Create 1-time invitation link.
Network usage: interactive.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiCreateUserAddress()

> **apiCreateUserAddress**(`userId`): `Promise`\<`CreatedConnLink`\>

Defined in: [src/api.ts:312](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L312)

Create bot address.
Network usage: interactive.

#### Parameters

##### userId

`number`

#### Returns

`Promise`\<`CreatedConnLink`\>

***

### apiDeleteChat()

> **apiDeleteChat**(`chatType`, `chatId`, `deleteMode`): `Promise`\<`void`\>

Defined in: [src/api.ts:732](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L732)

Delete chat.
Network usage: background.

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

Defined in: [src/api.ts:431](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L431)

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

Defined in: [src/api.ts:614](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L614)

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

Defined in: [src/api.ts:446](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L446)

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

Defined in: [src/api.ts:799](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L799)

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

Defined in: [src/api.ts:322](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L322)

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

Defined in: [src/api.ts:749](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L749)

Get active user profile
Network usage: no.

#### Returns

`Promise`\<`User` \| `undefined`\>

***

### apiGetGroupLink()

> **apiGetGroupLink**(`groupId`): `Promise`\<`GroupLink`\>

Defined in: [src/api.ts:623](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L623)

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

Defined in: [src/api.ts:629](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L629)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiGetUserAddress()

> **apiGetUserAddress**(`userId`): `Promise`\<`UserContactLink` \| `undefined`\>

Defined in: [src/api.ts:332](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L332)

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

Defined in: [src/api.ts:502](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L502)

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

Defined in: [src/api.ts:552](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L552)

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

Defined in: [src/api.ts:712](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L712)

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

Defined in: [src/api.ts:722](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L722)

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

Defined in: [src/api.ts:562](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L562)

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

Defined in: [src/api.ts:779](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L779)

Get all user profiles
Network usage: no.

#### Returns

`Promise`\<`UserInfo`[]\>

***

### apiNewGroup()

> **apiNewGroup**(`userId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:572](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L572)

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

### apiReceiveFile()

> **apiReceiveFile**(`fileId`): `Promise`\<`AChatItem`\>

Defined in: [src/api.ts:472](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L472)

Receive file.
Network usage: no.

#### Parameters

##### fileId

`number`

#### Returns

`Promise`\<`AChatItem`\>

***

### apiRejectContactRequest()

> **apiRejectContactRequest**(`contactReqId`): `Promise`\<`void`\>

Defined in: [src/api.ts:702](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L702)

Reject contact request. The user who sent the request is **not notified**.
Network usage: no.

#### Parameters

##### contactReqId

`number`

#### Returns

`Promise`\<`void`\>

***

### apiRemoveMembers()

> **apiRemoveMembers**(`groupId`, `memberIds`, `withMessages`): `Promise`\<`GroupMember`[]\>

Defined in: [src/api.ts:542](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L542)

Remove members. Requires Admin role.
Network usage: background.

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

Defined in: [src/api.ts:380](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L380)

Send messages.
Network usage: background.

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

Defined in: [src/api.ts:398](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L398)

Send text message.
Network usage: background.

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

Defined in: [src/api.ts:406](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L406)

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

Defined in: [src/api.ts:789](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L789)

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

Defined in: [src/api.ts:364](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L364)

Set bot address settings.
Network usage: interactive.

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

Defined in: [src/api.ts:825](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L825)

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

### apiSetGroupLinkMemberRole()

> **apiSetGroupLinkMemberRole**(`groupId`, `memberRole`): `Promise`\<`void`\>

Defined in: [src/api.ts:605](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L605)

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

Defined in: [src/api.ts:522](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L522)

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

### apiSetProfileAddress()

> **apiSetProfileAddress**(`userId`, `enable`): `Promise`\<`UserProfileUpdateSummary`\>

Defined in: [src/api.ts:350](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L350)

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

### apiUpdateChatItem()

> **apiUpdateChatItem**(`chatType`, `chatId`, `chatItemId`, `msgContent`): `Promise`\<`ChatItem`\>

Defined in: [src/api.ts:414](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L414)

Update message.
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

#### Returns

`Promise`\<`ChatItem`\>

***

### apiUpdateGroupProfile()

> **apiUpdateGroupProfile**(`groupId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:582](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L582)

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

Defined in: [src/api.ts:809](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L809)

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

Defined in: [src/api.ts:114](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L114)

Close chat database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### off()

> **off**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:253](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L253)

Unsubscribe all or a specific handler from a specific event.

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

The event type to unsubscribe from.

##### subscriber

An optional subscriber function for the event.

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`K`\> | `undefined`

#### Returns

`void`

***

### offAny()

> **offAny**(`receiver`): `void`

Defined in: [src/api.ts:269](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L269)

Unsubscribe all or a specific handler from any events.

#### Parameters

##### receiver

An optional subscriber function for the event.

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`Tag`\> | `undefined`

#### Returns

`void`

***

### on()

#### Call Signature

> **on**\<`K`\>(`subscribers`): `void`

Defined in: [src/api.ts:163](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L163)

Subscribe multiple event handlers at once.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### subscribers

[`EventSubscribers`](../type-aliases/EventSubscribers.md)

An object mapping event types (CEvt.Tag) to their subscriber functions.

##### Returns

`void`

##### Throws

If the same function is subscribed to event.

#### Call Signature

> **on**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:171](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L171)

Subscribe a handler to a specific event.

##### Type Parameters

###### K

`K` *extends* `Tag`

##### Parameters

###### event

`K`

The event type to subscribe to.

###### subscriber

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`K`\>

The subscriber function for the event.

##### Returns

`void`

##### Throws

If the same function is subscribed to event.

***

### onAny()

> **onAny**(`receiver`): `void`

Defined in: [src/api.ts:194](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L194)

Subscribe a handler to any event.

#### Parameters

##### receiver

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`Tag`\>

The receiver function for any event.

#### Returns

`void`

#### Throws

If the same function is subscribed to event.

***

### once()

> **once**\<`K`\>(`event`, `subscriber`): `void`

Defined in: [src/api.ts:205](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L205)

Subscribe a handler to a specific event to be delivered one time.

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

The event type to subscribe to.

##### subscriber

[`EventSubscriberFunc`](../type-aliases/EventSubscriberFunc.md)\<`K`\>

The subscriber function for the event.

#### Returns

`void`

#### Throws

If the same function is subscribed to event.

***

### recvChatEvent()

> **recvChatEvent**(`wait`): `Promise`\<`ChatEvent` \| `undefined`\>

Defined in: [src/api.ts:304](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L304)

#### Parameters

##### wait

`number` = `5_000_000`

#### Returns

`Promise`\<`ChatEvent` \| `undefined`\>

***

### sendChatCmd()

> **sendChatCmd**(`cmd`): `Promise`\<`ChatResponse`\>

Defined in: [src/api.ts:300](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L300)

#### Parameters

##### cmd

`string`

#### Returns

`Promise`\<`ChatResponse`\>

***

### startChat()

> **startChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:88](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L88)

Start chat controller. Must be called with the existing user profile.

#### Returns

`Promise`\<`void`\>

***

### stopChat()

> **stopChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:102](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L102)

Stop chat controller.
Must be called before closing the database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### wait()

#### Call Signature

> **wait**\<`K`\>(`event`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:213](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L213)

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

Defined in: [src/api.ts:214](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L214)

Waits for specific event, with an optional predicate.
Returns `undefined` on timeout if specified.

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

Defined in: [src/api.ts:215](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L215)

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

Defined in: [src/api.ts:216](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L216)

Waits for specific event, with an optional predicate.
Returns `undefined` on timeout if specified.

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

> `static` **init**(`dbFilePrefix`, `dbKey?`, `confirm?`): `Promise`\<`ChatApi`\>

Defined in: [src/api.ts:76](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L76)

Initializes the ChatApi.

#### Parameters

##### dbFilePrefix

`string`

File prefix for the database files.

##### dbKey?

`string` = `""`

Database encryption key.

##### confirm?

[`MigrationConfirmation`](../../core/enumerations/MigrationConfirmation.md) = `core.MigrationConfirmation.YesUp`

Migration confirmation mode.

#### Returns

`Promise`\<`ChatApi`\>
