[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [bot](Namespace.bot.md) / BotConfig

# Interface: BotConfig

Defined in: [src/bot.ts:36](../src/bot.ts#L36)

## Properties

### dbOpts

> **dbOpts**: [`BotDbOpts`](bot.TypeAlias.BotDbOpts.md)

Defined in: [src/bot.ts:38](../src/bot.ts#L38)

***

### events?

> `optional` **events?**: [`EventSubscribers`](api.TypeAlias.EventSubscribers.md)

Defined in: [src/bot.ts:45](../src/bot.ts#L45)

***

### onCommands?

> `optional` **onCommands?**: `object`

Defined in: [src/bot.ts:42](../src/bot.ts#L42)

#### Index Signature

\[`key`: `string`\]: ((`chatItem`, `command`, `chat`) => `void` \| `Promise`\<`void`\>) \| `undefined`

***

### onMessage?

> `optional` **onMessage?**: (`chatItem`, `content`, `chat`) => `void` \| `Promise`\<`void`\>

Defined in: [src/bot.ts:40](../src/bot.ts#L40)

#### Parameters

##### chatItem

`AChatItem`

##### content

`MsgContent`

##### chat

[`ChatApi`](api.Class.ChatApi.md)

#### Returns

`void` \| `Promise`\<`void`\>

***

### options

> **options**: [`BotOptions`](bot.Interface.BotOptions.md)

Defined in: [src/bot.ts:39](../src/bot.ts#L39)

***

### profile

> **profile**: `Profile`

Defined in: [src/bot.ts:37](../src/bot.ts#L37)
