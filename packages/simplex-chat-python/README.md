# SimpleX Chat Python library

Python 3.11+ client for [SimpleX Chat](https://simplex.chat) bots. Equivalent to the [Node.js library](https://www.npmjs.com/package/simplex-chat).

## Install

```bash
pip install simplex-chat
```

The native `libsimplex` is downloaded lazily on first use. To pre-fetch:

```bash
python -m simplex_chat install                     # sqlite (default)
python -m simplex_chat install --backend postgres  # linux-x86_64 only
```

## Quick start

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

`bot.run()` blocks. The connection address is logged on startup — paste it into a SimpleX client to talk to the bot. `Ctrl+C` to stop.

Three decorators: `@bot.on_message(...)`, `@bot.on_command(name)`, `@bot.on_event(tag)`. Message handlers are first-match-wins in registration order, so register specific filters first and catch-alls last.

See [`examples/squaring_bot.py`](./examples/squaring_bot.py) for the full example.

## Runtime options

The native library runs on the Haskell runtime, started once per process with GC options `-A64m -H64m -xn` (`-A64m -H64m` on Windows). A process that needs less memory can replace them before the first `ChatApi`, `Client` or `Bot` starts:

```python
import simplex_chat

simplex_chat.set_runtime_options(["-A4m"])
```

A smaller allocation area (`-A`) uses less memory at the cost of more frequent garbage collection. Calling it after the runtime has started raises `RuntimeInitializedError`. An invalid option makes the runtime print an error and exit the process.

## Development

```bash
uv venv && source .venv/bin/activate
uv pip install -e '.[dev]'
ruff check && pyright && pytest tests/
```

Wire types under `src/simplex_chat/types/_*.py` are generated. Regenerate with `cabal test simplex-chat-test --test-options='--match Python'`.

## License

[AGPL-3.0](./LICENSE)
