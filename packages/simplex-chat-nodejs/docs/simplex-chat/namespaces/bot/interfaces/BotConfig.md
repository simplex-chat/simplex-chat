[**simplex-chat**](../../../../README.md)

***

[simplex-chat](../../../../globals.md) / [bot](../README.md) / BotConfig

# Interface: BotConfig

Defined in: [src/bot.ts:37](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L37)

## Properties

### dbOpts

> **dbOpts**: [`BotDbOpts`](BotDbOpts.md)

Defined in: [src/bot.ts:39](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L39)

***

### events?

> `optional` **events**: `object`

Defined in: [src/bot.ts:46](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L46)

#### acceptingBusinessRequest?

> `optional` **acceptingBusinessRequest**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"acceptingBusinessRequest"`\>

#### acceptingContactRequest?

> `optional` **acceptingContactRequest**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"acceptingContactRequest"`\>

#### businessLinkConnecting?

> `optional` **businessLinkConnecting**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"businessLinkConnecting"`\>

#### chatError?

> `optional` **chatError**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"chatError"`\>

#### chatErrors?

> `optional` **chatErrors**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"chatErrors"`\>

#### chatItemReaction?

> `optional` **chatItemReaction**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"chatItemReaction"`\>

#### chatItemsDeleted?

> `optional` **chatItemsDeleted**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"chatItemsDeleted"`\>

#### chatItemsStatusesUpdated?

> `optional` **chatItemsStatusesUpdated**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"chatItemsStatusesUpdated"`\>

#### chatItemUpdated?

> `optional` **chatItemUpdated**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"chatItemUpdated"`\>

#### connectedToGroupMember?

> `optional` **connectedToGroupMember**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"connectedToGroupMember"`\>

#### contactConnected?

> `optional` **contactConnected**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"contactConnected"`\>

#### contactConnecting?

> `optional` **contactConnecting**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"contactConnecting"`\>

#### contactDeletedByContact?

> `optional` **contactDeletedByContact**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"contactDeletedByContact"`\>

#### contactSndReady?

> `optional` **contactSndReady**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"contactSndReady"`\>

#### contactUpdated?

> `optional` **contactUpdated**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"contactUpdated"`\>

#### deletedMember?

> `optional` **deletedMember**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"deletedMember"`\>

#### deletedMemberUser?

> `optional` **deletedMemberUser**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"deletedMemberUser"`\>

#### groupChatItemsDeleted?

> `optional` **groupChatItemsDeleted**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"groupChatItemsDeleted"`\>

#### groupDeleted?

> `optional` **groupDeleted**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"groupDeleted"`\>

#### groupLinkConnecting?

> `optional` **groupLinkConnecting**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"groupLinkConnecting"`\>

#### groupMemberUpdated?

> `optional` **groupMemberUpdated**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"groupMemberUpdated"`\>

#### groupUpdated?

> `optional` **groupUpdated**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"groupUpdated"`\>

#### hostConnected?

> `optional` **hostConnected**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"hostConnected"`\>

#### hostDisconnected?

> `optional` **hostDisconnected**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"hostDisconnected"`\>

#### joinedGroupMember?

> `optional` **joinedGroupMember**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"joinedGroupMember"`\>

#### joinedGroupMemberConnecting?

> `optional` **joinedGroupMemberConnecting**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"joinedGroupMemberConnecting"`\>

#### leftMember?

> `optional` **leftMember**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"leftMember"`\>

#### memberAcceptedByOther?

> `optional` **memberAcceptedByOther**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"memberAcceptedByOther"`\>

#### memberBlockedForAll?

> `optional` **memberBlockedForAll**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"memberBlockedForAll"`\>

#### memberRole?

> `optional` **memberRole**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"memberRole"`\>

#### messageError?

> `optional` **messageError**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"messageError"`\>

#### newChatItems?

> `optional` **newChatItems**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"newChatItems"`\>

#### newMemberContactReceivedInv?

> `optional` **newMemberContactReceivedInv**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"newMemberContactReceivedInv"`\>

#### rcvFileAccepted?

> `optional` **rcvFileAccepted**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileAccepted"`\>

#### rcvFileComplete?

> `optional` **rcvFileComplete**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileComplete"`\>

#### rcvFileDescrReady?

> `optional` **rcvFileDescrReady**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileDescrReady"`\>

#### rcvFileError?

> `optional` **rcvFileError**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileError"`\>

#### rcvFileSndCancelled?

> `optional` **rcvFileSndCancelled**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileSndCancelled"`\>

#### rcvFileStart?

> `optional` **rcvFileStart**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileStart"`\>

#### rcvFileWarning?

> `optional` **rcvFileWarning**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"rcvFileWarning"`\>

#### receivedContactRequest?

> `optional` **receivedContactRequest**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"receivedContactRequest"`\>

#### receivedGroupInvitation?

> `optional` **receivedGroupInvitation**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"receivedGroupInvitation"`\>

#### sentGroupInvitation?

> `optional` **sentGroupInvitation**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"sentGroupInvitation"`\>

#### sndFileCompleteXFTP?

> `optional` **sndFileCompleteXFTP**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"sndFileCompleteXFTP"`\>

#### sndFileError?

> `optional` **sndFileError**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"sndFileError"`\>

#### sndFileWarning?

> `optional` **sndFileWarning**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"sndFileWarning"`\>

#### subscriptionStatus?

> `optional` **subscriptionStatus**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"subscriptionStatus"`\>

#### userJoinedGroup?

> `optional` **userJoinedGroup**: [`EventSubscriberFunc`](../../api/type-aliases/EventSubscriberFunc.md)\<`"userJoinedGroup"`\>

***

### onCommands?

> `optional` **onCommands**: `object`

Defined in: [src/bot.ts:43](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L43)

#### Index Signature

\[`key`: `string`\]: (`chatItem`, `command`) => `void` \| `Promise`\<`void`\> \| `undefined`

***

### onMessage()?

> `optional` **onMessage**: (`chatItem`, `content`) => `void` \| `Promise`\<`void`\>

Defined in: [src/bot.ts:41](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L41)

#### Parameters

##### chatItem

`AChatItem`

##### content

`MsgContent`

#### Returns

`void` \| `Promise`\<`void`\>

***

### options

> **options**: [`BotOptions`](BotOptions.md)

Defined in: [src/bot.ts:40](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L40)

***

### profile

> **profile**: `Profile`

Defined in: [src/bot.ts:38](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L38)
