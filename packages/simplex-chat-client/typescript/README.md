# SimpleX Chat JavaScript client

This is a TypeScript library that defines WebSocket API client for [SimpleX Chat terminal CLI](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/CLI.md) that should be run as a WebSockets server on any port:

```bash
simplex-chat -p 5225
```

Client API provides types and functions to:

- create and change user profile (although, in most cases you can do it manually, via SimpleX Chat terminal app).
- create and accept invitations or connect with the contacts.
- create and manage long-term user address, accepting connection requests automatically.
- create, join and manage group.
- send and receive files.

## Use cases

- chat bots: you can implement any logic of connecting with and communicating with SimpleX Chat users. Using chat groups a chat bot can connect SimleX Chat users with each other.
- control of the equipment: e.g. servers or home automation. SimpleX Chat provides secure and authorised connections, so this is more secure than using rest APIs.

Please share your use cases and implementations.

## Quick start

```
npm i simplex-chat
npm run build
```

See the example of a simple chat bot in [squaring-bot.js](./examples/squaring-bot.js):

- start `simplex-chat` as a server on port 5225: `simplex-chat -p 5225 -d test_db`
- run chatbot: `node examples/squaring-bot`
- connect to chatbot via SimpleX Chat client using the address of the chat bot

## Documentation

Please refer to the available client API in [client.ts](./src/client.ts).

## License

[AGPL v3](./LICENSE)
