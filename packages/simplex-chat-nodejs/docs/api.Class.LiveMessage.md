[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / LiveMessage

# Class: LiveMessage

Defined in: [src/api.ts:1129](../api.ts#L1129)

Text shown as a live message while it is written, e.g. a streamed reply.

## Constructors

### Constructor

> **new LiveMessage**(`chat`, `chatType`, `chatId`, `interval?`, `onError?`): `LiveMessage`

Defined in: [src/api.ts:1143](../api.ts#L1143)

#### Parameters

##### chat

`Pick`\<[`ChatApi`](api.Class.ChatApi.md), `"apiSendMessages"` \| `"apiUpdateChatItem"` \| `"apiDeleteChatItems"`\>

Chat API, or an object with its methods to send, update and delete messages.

##### chatType

`ChatType`

Chat type.

##### chatId

`number`

Contact or group ID.

##### interval?

`number` = `3000`

Minimum interval between updates of the live message, in milliseconds.

##### onError?

(`error`) => `void`

Error handler for sending, updating and deleting messages. Errors are logged when it is omitted.

#### Returns

`LiveMessage`

## Methods

### end()

> **end**(`text?`): `Promise`\<`void`\>

Defined in: [src/api.ts:1173](../api.ts#L1173)

Finalize the live message with the text, or with the last text when omitted.
A message without text is deleted. The next update starts a new live message.
The returned promise resolves after all queued commands complete.

#### Parameters

##### text?

`string` = `...`

#### Returns

`Promise`\<`void`\>

***

### update()

> **update**(`text`): `void`

Defined in: [src/api.ts:1155](../api.ts#L1155)

Set the text of the live message.
The first text is sent at once; later texts update the message at most once per interval.

#### Parameters

##### text

`string`

#### Returns

`void`
