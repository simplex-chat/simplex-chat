[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [bot](Namespace.bot.md) / BotConfig

# Interface: BotConfig

Defined in: [src/bot.ts:35](../src/bot.ts#L35)

## Properties

### dbOpts

> **dbOpts**: [`BotDbOpts`](bot.TypeAlias.BotDbOpts.md)

Defined in: [src/bot.ts:37](../src/bot.ts#L37)

***

### events?

> `optional` **events?**: [`EventSubscribers`](api.TypeAlias.EventSubscribers.md)

Defined in: [src/bot.ts:44](../src/bot.ts#L44)

***

### onCommands?

> `optional` **onCommands?**: `object`

Defined in: [src/bot.ts:41](../src/bot.ts#L41)

#### Index Signature

\[`key`: `string`\]: ((`chatItem`, `command`) => `void` \| `Promise`\<`void`\>) \| `undefined`

***

### onMessage?

> `optional` **onMessage?**: (`chatItem`, `content`) => `void` \| `Promise`\<`void`\>

Defined in: [src/bot.ts:39](../src/bot.ts#L39)

#### Parameters

##### chatItem

`AChatItem`

##### content

`MsgContent`

#### Returns

`void` \| `Promise`\<`void`\>

***

### options

> **options**: [`BotOptions`](bot.Interface.BotOptions.md)

Defined in: [src/bot.ts:38](../src/bot.ts#L38)

***

### profile

> **profile**: `Profile`

Defined in: [src/bot.ts:36](../src/bot.ts#L36)
