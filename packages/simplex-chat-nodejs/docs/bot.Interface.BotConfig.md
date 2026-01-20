[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [bot](Namespace.bot.md) / BotConfig

# Interface: BotConfig

Defined in: [src/bot.ts:37](../src/bot.ts#L37)

## Properties

### dbOpts

> **dbOpts**: [`BotDbOpts`](bot.Interface.BotDbOpts.md)

Defined in: [src/bot.ts:39](../src/bot.ts#L39)

***

### events?

> `optional` **events**: [`EventSubscribers`](api.TypeAlias.EventSubscribers.md)

Defined in: [src/bot.ts:46](../src/bot.ts#L46)

***

### onCommands?

> `optional` **onCommands**: \{\[`key`: `string`\]: (`chatItem`, `command`) => `void` \| `Promise`\<`void`\> \| `undefined`; \}

Defined in: [src/bot.ts:43](../src/bot.ts#L43)

#### Index Signature

\[`key`: `string`\]: (`chatItem`, `command`) => `void` \| `Promise`\<`void`\> \| `undefined`

***

### onMessage()?

> `optional` **onMessage**: (`chatItem`, `content`) => `void` \| `Promise`\<`void`\>

Defined in: [src/bot.ts:41](../src/bot.ts#L41)

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

Defined in: [src/bot.ts:40](../src/bot.ts#L40)

***

### profile

> **profile**: `Profile`

Defined in: [src/bot.ts:38](../src/bot.ts#L38)
