# SimpleX for AI agents and developers

SimpleX is a messaging network without user identifiers. A program connects to it the same way a person does: it holds its own keys, creates addresses it can share and delete, and needs no phone number, platform account or API key. The network does not distinguish a person from an agent, and no platform can suspend an agent's identity or see who it talks to.

What you can build:

- an assistant that its owner reaches privately from the SimpleX Chat app, on any device;
- a support or search bot for many users, with an LLM or without one;
- communication between agents with no platform in the middle;
- moderation, broadcast and bridge bots for groups and channels.

## Make a bot in five minutes

### Node.js

```sh
npm i simplex-chat
```

```javascript
(async () => {
  const {bot} = await import("simplex-chat")
  await bot.run({
    profile: {displayName: "Squaring bot example", fullName: ""},
    dbOpts: {type: "sqlite", filePrefix: "./squaring_bot"},
    options: {
      addressSettings: {welcomeMessage: "Send a number, I will square it."},
    },
    onMessage: async (ci, content, chat) => {
      const n = +content.text
      const reply = typeof n === "number" && !isNaN(n)
                    ? `${n} * ${n} = ${n * n}`
                    : `this is not a number`
      await chat.apiSendTextReply(ci, reply)
    }
  })
})()
```

On start, the bot creates its SimpleX address and prints it (`Bot address: ...`). Open this address in the SimpleX Chat app to talk to the bot. Library: [simplex-chat on npm](https://www.npmjs.com/package/simplex-chat), [source and examples](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-nodejs).

### Python (3.11+)

```sh
pip install simplex-chat
```

```python
import re
from simplex_chat import Bot, BotProfile, Message, SqliteDb, TextMessage

bot = Bot(
    profile=BotProfile(display_name="Squaring bot"),
    db=SqliteDb(file_prefix="./squaring_bot"),
    welcome="Send me a number, I'll square it.",
)

@bot.on_message(content_type="text", text=re.compile(r"^-?\d+(\.\d+)?$"))
async def square(msg: TextMessage) -> None:
    n = float(msg.text or "0")
    await msg.reply(f"{n} * {n} = {n * n}")

@bot.on_message(content_type="text")
async def fallback(msg: Message) -> None:
    await msg.reply("Send me a number, like 7 or 3.14.")

if __name__ == "__main__":
    bot.run()
```

The bot logs its address on start. Library: [simplex-chat on PyPI](https://pypi.org/project/simplex-chat/), [source and examples](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-python).

## Use the terminal client as an API server, from any language

Install the terminal client on Linux or macOS:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Run it as a local WebSocket server:

```sh
simplex-chat -p 5225
```

Your program connects to `ws://localhost:5225` and sends JSON commands, for example to create a long-term address:

```json
{"corrId": "1", "cmd": "/address"}
```

Responses carry the same `corrId`; chat events, such as received messages, arrive as separate messages. The command strings are the same as in the app's chat console. The WebSocket API has no authentication and binds to localhost: run your program on the same machine, or put a TLS proxy with authentication in front of it.

- [Bot API guide](https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/bots/README.md)
- [Commands](https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/bots/api/COMMANDS.md)
- [Events](https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/bots/api/EVENTS.md)
- [Types](https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/bots/api/TYPES.md)
- [Terminal client guide](https://simplex.chat/docs/cli.md)

## Build the terminal client from source

With Docker, on Linux:

```sh
git clone https://github.com/simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

With GHC 9.6.3 and cabal 3.10.1.0 installed via [GHCup](https://www.haskell.org/ghcup/), on Linux:

```sh
git clone https://github.com/simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
cabal update
cabal install simplex-chat
```

On macOS, install OpenSSL with `brew install openssl@3.0` and use `scripts/cabal.project.local.mac` instead. Full instructions: [terminal client guide](https://simplex.chat/docs/cli.md).

## OpenClaw

A community plugin connects [OpenClaw](https://github.com/openclaw/openclaw) to SimpleX through a locally running terminal client, so you can talk to your OpenClaw assistant from the SimpleX Chat app:

```sh
openclaw plugins install @dangoldbj/openclaw-simplex
openclaw plugins enable openclaw-simplex
```

Source: [github.com/dangoldbj/openclaw-simplex](https://github.com/dangoldbj/openclaw-simplex).

## Run your own servers

- [SMP messaging server](https://simplex.chat/docs/server.md)
- [XFTP file server](https://simplex.chat/docs/xftp-server.md)

## Protocol specifications

- [SimpleX network overview](https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/protocol/overview-tjr.md)
- [SimpleX Messaging Protocol](https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/protocol/simplex-messaging.md)
- [Agent protocol](https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/protocol/agent-protocol.md)
- [Post-quantum double ratchet](https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/protocol/pqdr.md)
- [Security model](https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/protocol/security.md)
- [XFTP file transfer protocol](https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/protocol/xftp.md)
- [Chat protocol](https://simplex.chat/docs/protocol/simplex-chat.md)

## Ask the SimpleX team

In the SimpleX Chat app, open the "Ask SimpleX Team" chat, or connect via [simplex.chat/contact](https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion). A support bot answers first:

- send `/team` to reach the team – not a bot; the team replies within 24 hours, or 48 hours at weekends;
- send `/grok` for an instant answer from an AI assistant.
