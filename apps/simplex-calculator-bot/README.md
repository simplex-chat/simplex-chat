# SimpleX Calculator

A pocket calculator in a chat, built with the [SimpleX Chat Node.js library](../../packages/simplex-chat-nodejs/).

Each user who connects via the bot's business address gets their own chat with a calculator message. The keys are bot commands: tapping a key sends it to the bot, and the bot replaces the calculator message with the updated one.

## How it works

- Keys are applied left to right, without operator priority: `2 + 3 × 4 =` gives 20.
- `%` follows Apple's calculator: `100 + 15 %` shows 15, and `=` gives 115; `50 × 10 %` shows 0.1, and `=` gives 5.
- `C` clears the current number; the second `C` clears the whole calculation.
- The display shows up to 15 digits, without exponent; larger results show `Error`.
- After `=`, the bot sends the calculation as a message, e.g. `2 + 2 = 4`.
- Tapped keys are deleted from the chat; the calculation messages stay as a log.
- Users can also send messages instead of tapping keys:
  - a number or an expression, e.g. `12 × 3 + 4`, is computed and entered as the current number, so `25`, `/add`, `25`, `=` gives 50.
  - a key, e.g. `+`, `=`, `x`, `*`, `/`, `add` or `c`, works as the tapped key.
- The calculator turns off after 10 minutes without use: the number is removed from the display, the keys stay. Any key, number or `/calc` turns it on.
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

The end-to-end test runs a local SMP server with the TLS certificates from `tests/fixtures/tls`. On Linux, the test downloads `smp-server` from [simplexmq releases](https://github.com/simplex-chat/simplexmq/releases) to `node_modules/.cache` on the first run. To use another build, or on other systems, pass its path in `SMP_SERVER` variable:

```bash
SMP_SERVER=/path/to/smp-server npm test
```
