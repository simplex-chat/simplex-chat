[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [core](Namespace.core.md) / loadLibrary

# Function: loadLibrary()

> **loadLibrary**(`backend`): `Promise`\<`void`\>

Defined in: [src/core.ts:13](../src/core.ts#L13)

Resolve (downloading on first use) and load libsimplex for the backend.
One libsimplex per process: the Haskell runtime is initialized once.

## Parameters

### backend

`Backend`

## Returns

`Promise`\<`void`\>
