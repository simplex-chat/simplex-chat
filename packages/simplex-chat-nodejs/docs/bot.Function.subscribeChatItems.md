[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [bot](Namespace.bot.md) / subscribeChatItems

# Function: subscribeChatItems()

> **subscribeChatItems**(`bot`, `onMessage`, `commands`): `void`

Defined in: [src/bot.ts:111](../bot.ts#L111)

## Parameters

### bot

[`ChatApi`](api.Class.ChatApi.md)

### onMessage

((`chatItem`, `content`, `chat`) => `void` \| `Promise`\<`void`\>) \| `undefined`

### commands

\{\[`key`: `string`\]: ((`chatItem`, `command`, `chat`) => `void` \| `Promise`\<`void`\>) \| `undefined`; \}

## Returns

`void`
