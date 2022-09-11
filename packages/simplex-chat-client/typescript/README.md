# SimpleX Chat JavaScript client

This is a TypeScript library that defines WebSocket API client for [SimpleX Chat terminal CLI](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/CLI.md) that should be run as a WebSockets server on any port:

```bash
simplex-chat -p 5225
```

Client API provides types and functions to:

- create and change user profile
- create and accept invitations ot connect with the contacts
- create and manage long-term user address
- create, join and manage group
- send and receive files

## Use cases

- chatbots: you can implement any logic of connecting with and communicating with SimpleX Chat users. Using chat groups chatbot can be used to connect SimleX Chat users
- control of the equipment: e.g. servers or home automation. As SimpleX Chat provides secure and authorised connections, this is more secure than using rest APIs.

Please share your use cases and implementations.

## Quick start

```
npm i simplex-chat
npm run build
```

See the example of a simple chat bot in [squaring-bot.js](./examples/squaring-bot.js):

- start `simplex-chat` as a server onport 5225: `simplex-chat -p 5225 -d test_db`
- run chatbot: `node examples/squaring-bot`
- connect to chatbot via SimpleX Chat client using the address of the chat bot

## Documentation

Please refer to the available client API in [client.ts](./src/client.ts).

## Lisense

[AGPL v3](./LICENSE)
