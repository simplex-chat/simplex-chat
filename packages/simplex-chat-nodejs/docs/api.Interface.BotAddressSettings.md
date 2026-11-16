[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [api](Namespace.api.md) / BotAddressSettings

# Interface: BotAddressSettings

Defined in: [src/api.ts:23](../api.ts#L23)

Bot address settings.

## Properties

### autoAccept?

> `optional` **autoAccept?**: `boolean`

Defined in: [src/api.ts:28](../api.ts#L28)

Automatically accept contact requests.

#### Default

```ts
true
```

***

### businessAddress?

> `optional` **businessAddress?**: `boolean`

Defined in: [src/api.ts:41](../api.ts#L41)

Business contact address.
For all requests business chats will be created where other participants can be added.

#### Default

```ts
false
```

***

### chatLayout?

> `optional` **chatLayout?**: `ChatLayout`

Defined in: [src/api.ts:43](../api.ts#L43)

***

### welcomeMessage?

> `optional` **welcomeMessage?**: `string` \| `MsgContent`

Defined in: [src/api.ts:34](../api.ts#L34)

Optional welcome message to show before connection to the users.

#### Default

```ts
undefined (no welcome message)
```
