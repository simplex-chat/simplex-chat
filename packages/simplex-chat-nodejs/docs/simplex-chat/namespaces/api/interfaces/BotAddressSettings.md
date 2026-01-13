[**simplex-chat**](../../../../README.md)

***

[simplex-chat](../../../../globals.md) / [api](../README.md) / BotAddressSettings

# Interface: BotAddressSettings

Defined in: [src/api.ts:23](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L23)

Bot address settings.

## Properties

### autoAccept?

> `optional` **autoAccept**: `boolean`

Defined in: [src/api.ts:28](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L28)

Automatically accept contact requests.

#### Default

```ts
true
```

***

### businessAddress?

> `optional` **businessAddress**: `boolean`

Defined in: [src/api.ts:41](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L41)

Business contact address.
For all requests business chats will be created where other participants can be added.

#### Default

```ts
false
```

***

### welcomeMessage?

> `optional` **welcomeMessage**: `MsgContent`

Defined in: [src/api.ts:34](https://github.com/simplex-chat/simplex-chat/blob/master/packages/simplex-chat-nodejs/src/api.ts#L34)

Optional welcome message to show before connection to the users.

#### Default

```ts
undefined (no welcome message)
```
