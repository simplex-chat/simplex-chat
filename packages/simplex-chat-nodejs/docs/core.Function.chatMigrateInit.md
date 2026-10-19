[**simplex-chat**](README.md)

***

[simplex-chat](README.md) / [core](Namespace.core.md) / chatMigrateInit

# Function: chatMigrateInit()

> **chatMigrateInit**(`dbPath`, `dbKey`, `confirm`, `queueSize?`): `Promise`\<`bigint`\>

Defined in: [src/core.ts:30](../src/core.ts#L30)

Initialize chat controller

## Parameters

### dbPath

`string`

### dbKey

`string`

### confirm

[`MigrationConfirmation`](core.Enumeration.MigrationConfirmation.md)

### queueSize?

`number`

Size of internal queues, the core default is used when omitted.

## Returns

`Promise`\<`bigint`\>
