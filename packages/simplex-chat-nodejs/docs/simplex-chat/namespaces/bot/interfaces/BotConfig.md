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

> `optional` **events**: [`EventSubscribers`](../../api/type-aliases/EventSubscribers.md)

Defined in: [src/bot.ts:46](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/bot.ts#L46)

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
