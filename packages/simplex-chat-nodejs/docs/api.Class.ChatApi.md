[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / ChatApi

# Class: ChatApi

Defined in: [src/api.ts:95](../src/api.ts#L95)

Main API class for interacting with the chat core library.

## Properties

### ctrl\_

> `protected` **ctrl\_**: `bigint` \| `undefined`

Defined in: [src/api.ts:101](../src/api.ts#L101)

## Accessors

### ctrl

#### Get Signature

> **get** **ctrl**(): `bigint`

Defined in: [src/api.ts:343](../src/api.ts#L343)

Chat controller reference

##### Returns

`bigint`

***

### initialized

#### Get Signature

> **get** **initialized**(): `boolean`

Defined in: [src/api.ts:329](../src/api.ts#L329)

Chat controller is initialized

##### Returns

`boolean`

***

### started

#### Get Signature

> **get** **started**(): `boolean`

Defined in: [src/api.ts:336](../src/api.ts#L336)

Chat controller is started

##### Returns

`boolean`

## Methods

### apiAcceptContactRequest()

> **apiAcceptContactRequest**(`contactReqId`): `Promise`\<`Contact`\>

Defined in: [src/api.ts:749](../src/api.ts#L749)

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

Defined in: [src/api.ts:569](../src/api.ts#L569)

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

Defined in: [src/api.ts:549](../src/api.ts#L549)

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

Defined in: [src/api.ts:589](../src/api.ts#L589)

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

Defined in: [src/api.ts:539](../src/api.ts#L539)

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

Defined in: [src/api.ts:512](../src/api.ts#L512)

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

### apiConnect()

> **apiConnect**(`userId`, `incognito`, `preparedLink`): `Promise`\<[`ConnReqType`](api.Enumeration.ConnReqType.md)\>

Defined in: [src/api.ts:718](../src/api.ts#L718)

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

Defined in: [src/api.ts:727](../src/api.ts#L727)

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

Defined in: [src/api.ts:708](../src/api.ts#L708)

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

Defined in: [src/api.ts:886](../src/api.ts#L886)

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

Defined in: [src/api.ts:649](../src/api.ts#L649)

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

Defined in: [src/api.ts:695](../src/api.ts#L695)

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

Defined in: [src/api.ts:952](../src/api.ts#L952)

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

Defined in: [src/api.ts:360](../src/api.ts#L360)

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

Defined in: [src/api.ts:808](../src/api.ts#L808)

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

Defined in: [src/api.ts:487](../src/api.ts#L487)

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

Defined in: [src/api.ts:671](../src/api.ts#L671)

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

Defined in: [src/api.ts:502](../src/api.ts#L502)

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

Defined in: [src/api.ts:916](../src/api.ts#L916)

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

Defined in: [src/api.ts:370](../src/api.ts#L370)

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

Defined in: [src/api.ts:866](../src/api.ts#L866)

Get active user profile
Network usage: no.

#### Returns

`Promise`\<`User` \| `undefined`\>

***

### apiGetChat()

> **apiGetChat**(`chatType`, `chatId`, `count`): `Promise`\<`any`\>

Defined in: [src/api.ts:856](../src/api.ts#L856)

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

Defined in: [src/api.ts:793](../src/api.ts#L793)

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

Defined in: [src/api.ts:680](../src/api.ts#L680)

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

Defined in: [src/api.ts:686](../src/api.ts#L686)

#### Parameters

##### groupId

`number`

#### Returns

`Promise`\<`string`\>

***

### apiGetUserAddress()

> **apiGetUserAddress**(`userId`): `Promise`\<`UserContactLink` \| `undefined`\>

Defined in: [src/api.ts:380](../src/api.ts#L380)

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

Defined in: [src/api.ts:559](../src/api.ts#L559)

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

Defined in: [src/api.ts:609](../src/api.ts#L609)

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

Defined in: [src/api.ts:769](../src/api.ts#L769)

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

Defined in: [src/api.ts:779](../src/api.ts#L779)

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

Defined in: [src/api.ts:619](../src/api.ts#L619)

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

Defined in: [src/api.ts:896](../src/api.ts#L896)

Get all user profiles
Network usage: no.

#### Returns

`Promise`\<`UserInfo`[]\>

***

### apiNewGroup()

> **apiNewGroup**(`userId`, `groupProfile`): `Promise`\<`GroupInfo`\>

Defined in: [src/api.ts:629](../src/api.ts#L629)

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

Defined in: [src/api.ts:528](../src/api.ts#L528)

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

Defined in: [src/api.ts:759](../src/api.ts#L759)

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

Defined in: [src/api.ts:599](../src/api.ts#L599)

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

Defined in: [src/api.ts:963](../src/api.ts#L963)

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

Defined in: [src/api.ts:431](../src/api.ts#L431)

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

Defined in: [src/api.ts:454](../src/api.ts#L454)

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

Defined in: [src/api.ts:462](../src/api.ts#L462)

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

Defined in: [src/api.ts:906](../src/api.ts#L906)

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

Defined in: [src/api.ts:414](../src/api.ts#L414)

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

Defined in: [src/api.ts:845](../src/api.ts#L845)

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

Defined in: [src/api.ts:835](../src/api.ts#L835)

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

Defined in: [src/api.ts:942](../src/api.ts#L942)

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

Defined in: [src/api.ts:825](../src/api.ts#L825)

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

Defined in: [src/api.ts:662](../src/api.ts#L662)

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

Defined in: [src/api.ts:579](../src/api.ts#L579)

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

Defined in: [src/api.ts:398](../src/api.ts#L398)

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

Defined in: [src/api.ts:470](../src/api.ts#L470)

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

Defined in: [src/api.ts:639](../src/api.ts#L639)

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

Defined in: [src/api.ts:926](../src/api.ts#L926)

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

Defined in: [src/api.ts:157](../src/api.ts#L157)

Stop chat controller and close chat database.
The database is not closed if stopping fails.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### off()

> **off**\<`K`\>(`event`, `subscriber?`): `void`

Defined in: [src/api.ts:301](../src/api.ts#L301)

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

Defined in: [src/api.ts:317](../src/api.ts#L317)

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

Defined in: [src/api.ts:211](../src/api.ts#L211)

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

Defined in: [src/api.ts:219](../src/api.ts#L219)

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

Defined in: [src/api.ts:242](../src/api.ts#L242)

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

Defined in: [src/api.ts:253](../src/api.ts#L253)

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

Defined in: [src/api.ts:352](../src/api.ts#L352)

#### Parameters

##### wait?

`number` = `500_000`

#### Returns

`Promise`\<`ChatEvent` \| `undefined`\>

***

### sendChatCmd()

> **sendChatCmd**(`cmd`): `Promise`\<`ChatResponse`\>

Defined in: [src/api.ts:348](../src/api.ts#L348)

#### Parameters

##### cmd

`string`

#### Returns

`Promise`\<`ChatResponse`\>

***

### startChat()

> **startChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:123](../src/api.ts#L123)

Start chat controller. Must be called with the existing user profile.

#### Returns

`Promise`\<`void`\>

***

### stopChat()

> **stopChat**(): `Promise`\<`void`\>

Defined in: [src/api.ts:146](../src/api.ts#L146)

Stop chat controller.
`close` calls it before closing the database.
Usually doesn't need to be called in chat bots.

#### Returns

`Promise`\<`void`\>

***

### wait()

#### Call Signature

> **wait**\<`K`\>(`event`): `Promise`\<`ChatEvent` & `object`\>

Defined in: [src/api.ts:261](../src/api.ts#L261)

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

Defined in: [src/api.ts:262](../src/api.ts#L262)

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

Defined in: [src/api.ts:263](../src/api.ts#L263)

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

Defined in: [src/api.ts:264](../src/api.ts#L264)

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

Defined in: [src/api.ts:109](../src/api.ts#L109)

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
