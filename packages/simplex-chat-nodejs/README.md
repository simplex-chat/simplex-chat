# SimpleX Chat Node.js library

## Use cases

- chat bots: you can implement any logic of connecting with and communicating with SimpleX Chat users. Using chat groups a chat bot can connect SimpleX Chat users with each other.
- control of the equipment: e.g. servers or home automation. SimpleX Chat provides secure and authorised connections, so this is more secure than using rest APIs.
- any scenarios of scripted message sending.
- chat interfaces.

Please share your use cases and implementations.

## Supported chat functions

Library provides types and functions to:

- create and change user profile (although, in most cases you can do it manually, via SimpleX Chat terminal app).
- create and accept invitations or connect with the contacts.
- create and manage long-term user address, accepting connection requests automatically.
- send, receive, delete and update messages, and add message reactions.
- create, join and manage group.
- send and receive files.
- etc.

## Quick start

```
npm i simplex-chat
```

Simple bot that replies with squares of numbers you send to it:

```javascript
(async () => {
  const {bot} = await import("simplex-chat")
  // if you are running from this GitHub repo:
  // const {bot} = await import("../dist/index.js")
  const [chat, _user] = await bot.run({
    profile: {displayName: "Squaring bot example", fullName: ""},
    dbOpts: {dbFilePrefix: "./squaring_bot", dbKey: ""},
    options: {
      addressSettings: {welcomeMessage: "If you send me a number, I will calculate its square."},
    },
    onMessage: async (ci, content) => {
      const n = +content.text
      const reply = typeof n === "number" && !isNaN(n) ? `${n} * ${n} = ${n * n}` : `this is not a number`
      await chat.apiSendTextReply(ci, reply)
    }
  })
})()
```

An example with more options is in [./examples/squaring-bot.ts](./examples/squaring-bot.ts).

You can run it with:

```sh
npm run build # only needed if you cloned from GitHub
npx ts-node ./examples/squaring-bot.ts
```

## Documentation

Library documentation is generated with typedoc.

Library provides these modules:
- core: provides a low level API to the same core library that is used in desktop clients. You are unlikely to need to use this module directly.
- api: provides an API for chat commands. You need to use it in bot event handlers and in any usage scenarios different from chat bots.
- bot: provides a declarative way to create a bot. It automates creating and updating of the bot profile, address and commands.
- util: functions for processing chat events and types.

This library uses [@simplex-chat/types](https://www.npmjs.com/package/@simplex-chat/types) package with auto-generated [bot API types](https://github.com/simplex-chat/simplex-chat/tree/stable/bots)

## License

[AGPL v3](./LICENSE)
