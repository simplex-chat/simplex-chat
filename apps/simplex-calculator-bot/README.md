# SimpleX Calculator

A calculator bot built with the [SimpleX Chat Node.js library](../../packages/simplex-chat-nodejs/).

Each user who connects via the bot's business address receives a calculator message with keys as commands. The bot replaces this message after each input.

- Keys are applied left to right, so `2 + 3 × 4 =` equals 20.
- `%` is computed as in Apple's calculator: `100 + 15 % =` equals 115.
- Tap `C` to clear the number, and `C` again to clear the calculation.
- Numbers are shown without exponent, up to 15 digits; larger results are shown as `Error`.
- You can also send numbers, expressions like `12 × 3 + 4`, which are entered as one number, and keys like `+`, `=`, `add` or `c`.
- The bot deletes tapped keys and sends each calculation after `=`.
- After 10 minutes without input, the bot turns the calculator off and discards the calculation; tap any key to turn it on.
- The bot sends keys like `/+` to apps with chat protocol version 21 or later, and keys like `/add` to older apps.

## Run

```bash
npm install
npm run build
npm start
```

The bot prints its address and keeps its data in `./data`.

To use the library from this repository, build it and run `npm install --no-save ../../packages/simplex-chat-nodejs` instead of `npm install`.

## Test

```bash
npm test
```

A local SMP server is started for the end-to-end test. On Linux, `smp-server` is downloaded from [simplexmq releases](https://github.com/simplex-chat/simplexmq/releases); to use another build, or on other systems, set `SMP_SERVER` to the path of `smp-server`.
