[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / ChatApi

# Class: ChatApi

Defined in: [src/api.ts:97](../src/api.ts#L97)

Main API class for interacting with the chat core library.

## Properties

### ctrl\_

> `protected` **ctrl\_**: `bigint` \| `undefined`

Defined in: [src/api.ts:103](../src/api.ts#L103)

## Accessors

### ctrl

#### Get Signature

> **get** **ctrl**(): `bigint`

Defined in: [src/api.ts:331](../src/api.ts#L331)

Chat controller reference

##### Returns

`bigint`

***

### initialized

#### Get Signature

> **get** **initialized**(): `boolean`

Defined in: [src/api.ts:317](../src/api.ts#L317)

Chat controller is initialized

##### Returns

`boolean`

***

### started

#### Get Signature

> **get** **started**(): `boolean`

Defined in: [src/api.ts:324](../src/api.ts#L324)

Chat controller is started

##### Returns

`boolean`

## Methods

### apiAcceptContactRequest()

> **apiAcceptContactRequest**(`contactReqId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:734](../src/api.ts#L734)

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

Defined in: [src/api.ts:554](../src/api.ts#L554)

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

Defined in: [src/api.ts:534](../src/api.ts#L534)

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

Defined in: [src/api.ts:574](../src/api.ts#L574)

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

Defined in: [src/api.ts:524](../src/api.ts#L524)

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

Defined in: [src/api.ts:498](../src/api.ts#L498)

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

> **apiConnect**(`userId`, `incognito`, `preparedLink?`): `Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

Defined in: [src/api.ts:703](../src/api.ts#L703)

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

`Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

***

### apiConnectActiveUser()

> **apiConnectActiveUser**(`connLink`): `Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

Defined in: [src/api.ts:712](../src/api.ts#L712)

Connect via SimpleX link as string in the active user profile.
Network usage: interactive.

#### Parameters

##### connLink

`string`

#### Returns

`Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

***

### apiConnectPlan()

> **apiConnectPlan**(`userId`, `connectionLink`): `Promise`\<\[`ConnectionPlan`, `CreatedConnLink`\]\>

Defined in: [src/api.ts:693](../src/api.ts#L693)

Determine SimpleX link type and if the bot is already connected via this link.
Network usage: interactive.

#### Parameters

##### userId

`number`

##### connectionLink

`string`

#### Returns

`Promise`\<\[`ConnectionPlan`, `CreatedConnLink`\]\>

***

### apiCreateActiveUser()

> **apiCreateActiveUser**(`profile?`): `Promise`\<`User`\>

Defined in: [src/api.ts:871](../src/api.ts#L871)

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

Defined in: [src/api.ts:634](../src/api.ts#L634)

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

Defined in: [src/api.ts:680](../src/api.ts#L680)

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

Defined in: [src/api.ts:937](../src/api.ts#L937)

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

Defined in: [src/api.ts:348](../src/api.ts#L348)

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

Defined in: [src/api.ts:793](../src/api.ts#L793)

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

Defined in: [src/api.ts:473](../src/api.ts#L473)

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

Defined in: [src/api.ts:656](../src/api.ts#L656)

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

Defined in: [src/api.ts:488](../src/api.ts#L488)

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

Defined in: [src/api.ts:901](../src/api.ts#L901)

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

Defined in: [src/api.ts:358](../src/api.ts#L358)

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

Defined in: [src/api.ts:851](../src/api.ts#L851)

Get active user profile
Network usage: no.

#### Returns

`Promise`\<`User` \| `undefined`\>

***

### apiGetChat()

> **apiGetChat**(`chatType`, `chatId`, `count`): `Promise`\<`any`\>

Defined in: [src/api.ts:841](../src/api.ts#L841)

Get chat items.
Network usage: no.

#### Parameters

##### chatType

`ChatType`

##### chatId

`number`

##### count

`number`

#### Returns

`Promise`\<`any`\>

***

### apiGetChats()

> **apiGetChats**(`userId`, `pagination`, `query?`, `pendingConnections?`): `Promise`\<`AChat`[]\>

Defined in: [src/api.ts:778](../src/api.ts#L778)

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

Defined in: [src/api.ts:665](../src/api.ts#L665)

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

Defined in: [src/api.ts:671](../src/api.ts#L671)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiGetUserAddress()

> **apiGetUserAddress**(`userId`): `Promise`\<`UserContactLink` \| `undefined`\>

Defined in: [src/api.ts:368](../src/api.ts#L368)

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

Defined in: [src/api.ts:544](../src/api.ts#L544)

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

Defined in: [src/api.ts:594](../src/api.ts#L594)

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

Defined in: [src/api.ts:754](../src/api.ts#L754)

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

Defined in: [src/api.ts:764](../src/api.ts#L764)

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

Defined in: [src/api.ts:604](../src/api.ts#L604)

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

Defined in: [src/api.ts:881](../src/api.ts#L881)

Get all user profiles
Network usage: no.

#### Returns

`Promise`\<`UserInfo`[]\>

***

### apiNewGroup()

> **apiNewGroup**(`userId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:614](../src/api.ts#L614)

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

Defined in: [src/api.ts:514](../src/api.ts#L514)

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

Defined in: [src/api.ts:744](../src/api.ts#L744)

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

Defined in: [src/api.ts:584](../src/api.ts#L584)

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

Defined in: [src/api.ts:948](../src/api.ts#L948)

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

Defined in: [src/api.ts:417](../src/api.ts#L417)

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

Defined in: [src/api.ts:440](../src/api.ts#L440)

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

Defined in: [src/api.ts:448](../src/api.ts#L448)

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

Defined in: [src/api.ts:891](../src/api.ts#L891)

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

Defined in: [src/api.ts:400](../src/api.ts#L400)

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

Defined in: [src/api.ts:830](../src/api.ts#L830)

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

Defined in: [src/api.ts:820](../src/api.ts#L820)

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

Defined in: [src/api.ts:927](../src/api.ts#L927)

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

Defined in: [src/api.ts:810](../src/api.ts#L810)

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

Defined in: [src/api.ts:647](../src/api.ts#L647)

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

Defined in: [src/api.ts:564](../src/api.ts#L564)

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

Defined in: [src/api.ts:386](../src/api.ts#L386)

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

> **apiUpdateChatItem**(`chatType`, `chatId`, `chatItemId`, `msgContent`, `liveMessage`): `Promise`\<`ChatItem`\>

Defined in: [src/api.ts:456](../src/api.ts#L456)

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

##### liveMessage

`false`

#### Returns

`Promise`\<`ChatItem`\>

***

### apiUpdateGroupProfile()

> **apiUpdateGroupProfile**(`groupId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:624](../src/api.ts#L624)

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

Defined in: [src/api.ts:911](../src/api.ts#L911)

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

Defined in: [src/api.ts:150](../src/api.ts#L150)

Close chat database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### off()

> **off**\<`K`\>(`event`, `subscriber?`): `void`

Defined in: [src/api.ts:289](../src/api.ts#L289)

Unsubscribe all or a specific handler from a specific event.

#### Type Parameters

##### K

`K` *extends* `Tag`

#### Parameters

##### event

`K`

The event type to unsubscribe from.

##### subscriber?

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`K`\> \| `undefined`

An optional subscriber function for the event.

#### Returns

`void`

***

### offAny()

> **offAny**(`receiver?`): `void`

Defined in: [src/api.ts:305](../src/api.ts#L305)

Unsubscribe all or a specific handler from any events.

#### Parameters

##### receiver?

[`EventSubscriberFunc`](api.TypeAlias.EventSubscriberFunc.md)\<`Tag`\> \| `undefined`

An optional subscriber function for the event.

#### Returns

`void`

***

### on()

#### Call Signature

> **on**\<`K`\>(`subscribers`): `void`

Defined in: [src/api.ts:199](../src/api.ts#L199)

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

Defined in: [src/api.ts:207](../src/api.ts#L207)

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

Defined in: [src/api.ts:230](../src/api.ts#L230)

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

Defined in: [src/api.ts:241](../src/api.ts#L241)

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

Defined in: [src/api.ts:340](../src/api.ts#L340)

#### Parameters

##### wait?

`number` = `5_000_000`

#### Returns

`Promise`\<`ChatEvent` \| `undefined`\>

***

### sendChatCmd()

> **sendChatCmd**(`cmd`): `Promise`\<`ChatResponse`\>

Defined in: [src/api.ts:336](../src/api.ts#L336)

#### Parameters

##### cmd

`string`

#### Returns

`Promise`\<`ChatResponse`\>

***

### startChat()

> **startChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:124](../src/api.ts#L124)

Start chat controller. Must be called with the existing user profile.

#### Returns

`Promise`\<`void`\>

***

### stopChat()

> **stopChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:138](../src/api.ts#L138)

Stop chat controller.
Must be called before closing the database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### wait()

#### Call Signature

> **wait**\<`K`\>(`event`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:249](../src/api.ts#L249)

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

Defined in: [src/api.ts:250](../src/api.ts#L250)

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

Defined in: [src/api.ts:251](../src/api.ts#L251)

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

Defined in: [src/api.ts:252](../src/api.ts#L252)

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

Defined in: [src/api.ts:111](../src/api.ts#L111)

Initializes the ChatApi.

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
