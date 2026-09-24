# SimpleX Calculator

A pocket calculator in a chat, built with the [SimpleX Chat Node.js library](../../packages/simplex-chat-nodejs/).

Each user who connects via the bot's business address gets their own chat with a calculator message. The keys are bot commands: tapping a key sends it to the bot, and the bot replaces the calculator message with the updated one.

## How it works

- Keys are applied left to right, without operator priority: `2 + 3 × 4 =` gives 20.
- `%` follows Apple's calculator: `100 + 15 %` shows 15, and `=` gives 115; `50 × 10 %` shows 0.1, and `=` gives 5.
- After `=`, the bot sends the calculation as a message, e.g. `2 + 2 = 4`.
- Tapped keys are deleted from the chat; the calculation messages stay as a log.
- Users can also send a number or an expression, e.g. `12 × 3 + 4`; the calculator shows the result.
- The calculator turns off after 10 minutes without use. Tap `/calc` or send an expression to turn it on.
- Apps that support commands made of symbols (chat protocol version 21) get keys like `/+`; older apps get keys like `/add`.

## Install & build

```bash
cd apps/simplex-calculator-bot
npm install
npm run build
```

The bot requires a `simplex-chat` library version that passes the chat API to event handlers (7.1.0-beta.4.1 or later). To run against the in-tree library:

```bash
# In packages/simplex-chat-nodejs
npm link

# In apps/simplex-calculator-bot
npm link simplex-chat
```

## Run

```bash
npm start
```

The bot prints its address on start. The database is stored in `./data`.

## Test

```bash
npm test
```
