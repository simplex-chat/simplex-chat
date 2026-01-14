# SimpleX Chat Node.js library

This library replaced now deprecated [SimpleX Chat WebRTC TypeScript client](https://www.npmjs.com/package/@simplex-chat/webrtc-client).

## Use cases

- chat bots: you can implement any logic of connecting with and communicating with SimpleX Chat users. Using chat groups a chat bot can connect SimpleX Chat users with each other.
- control of the equipment: e.g. servers or home automation. SimpleX Chat provides secure and authorised connections, so this is more secure than using rest APIs.
- any scenarios of scripted message sending.
- chat and chat-based interfaces.

Please share your use cases and implementations.

## Quick start: a simple bot

```
npm i simplex-chat@6.5.0-beta.4
```

Simple bot that replies with squares of numbers you send to it:

```javascript
(async () => {
  const {bot} = await import("simplex-chat")
  // if you are running from this GitHub repo:
  // const {bot} = await import("../dist/index.js")
  const [chat, _user, _address] = await bot.run({
    profile: {displayName: "Squaring bot example", fullName: ""},
    dbOpts: {dbFilePrefix: "./squaring_bot", dbKey: ""},
    options: {
      addressSettings: {welcomeMessage: "If you send me a number, I will calculate its square."},
    },
    onMessage: async (ci, content) => {
      const n = +content.text
      const reply = typeof n === "number" && !isNaN(n)
                    ? `${n} * ${n} = ${n * n}`
                    : `this is not a number`
      await chat.apiSendTextReply(ci, reply)
    }
  })
})()
```

If you installed this package as dependency, you can run this example with:

```sh
node ./node_modules/simplex-chat/examples/squaring-bot-readme.js`
```

If you cloned this repository, you can:

```
cd ./packages/simplex-chat-nodejs
npm install
npm run build
node ./examples/squaring-bot-readme.js
```

There is an example with more options in [./examples/squaring-bot.ts](./examples/squaring-bot.ts).

You can run it with: `npx ts-node ./examples/squaring-bot.ts`

## Documentation

The library docs are [here](./docs/README.md).

Library provides these modules:
- [bot](./docs/Namespace.bot.md): a simple declarative API to run a chat-bot with a single function call. It automates creating and updating of the bot profile, address and bot commands shown in the app UI.
- [api](./docs/Namespace.api.md): an API to send chat commands and receive chat events to/from chat core. You need to use it in bot event handlers, and for any other use cases.
- [core](./docs/Namespace.core.md): a low level API to the core library - the same that is used in desktop clients. You are unlikely to ever need to use this module directly.
- [util](./docs/Namespace.util.md): useful functions for chat events and types.


This library uses [@simplex-chat/types](https://www.npmjs.com/package/@simplex-chat/types) package with auto-generated [bot API types](../../bots/api/README.md).

## Supported chat functions

Library provides types and functions to:

- create and change user profile (although, in most cases you can do it manually, via SimpleX Chat terminal app).
- create and accept invitations or connect with the contacts.
- create and manage long-term user address, accepting connection requests automatically.
- send, receive, delete and update messages, and add message reactions.
- create, join and manage group.
- send and receive files.
- etc.

## License

[AGPL v3](./LICENSE)
