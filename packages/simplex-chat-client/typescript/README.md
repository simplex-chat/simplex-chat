# SimpleX Chat JavaScript client

## Quick start

```
npm i simplex-chat
```

```javascript
const ChatClient = require("simplex-chat").ChatClient
const chat = await ChatClient.create("ws://localhost:5225")
await processMessages(chat)
const user = await chat.apiGetActiveUser()

async function processMessages(chat) {
  for await (const m of chat.msgQ) {
    const msg = m instanceof Promise ? await m : m
    switch (msg.type) {
      case "newChatItem": // do something
        return
      default:
        console.log(msg)
    }
  }
}
```

## Documentation

Please refer to:

- available client API - [client.ts](./src/client.ts).
- available commands - `ChatCommand` type in [command.ts](./src/command.ts) - if some command is not created as a ChatClient method, you can pass any command object to `sendChatCommand` method, or if the type for some command is not available you can pass command string (same strings as supported in terminal/mobile API) to `sendChatCmdStr` method.
- available chat messages - `ChatResponse` type in [response.ts](./src/command.ts).

**Please note**: you should NOT use local display names that are supported in terminal app, as they can change when contact profile is updated and you can have race conditions - use commands that use chat IDs.

## Lisense

[AGPL v3](./LICENSE)
