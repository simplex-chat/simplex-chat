# SimpleX Chat Python library design

## Table of contents

- [What](#what)
- [Why](#why)
- [How](#how)
- [Architecture](#architecture)
- [Type generation](#type-generation)
- [Native lib loading](#native-lib-loading)
- [Public API](#public-api)
- [Distribution and CI](#distribution-and-ci)
- [Testing](#testing)
- [Open questions](#open-questions)

## What

A Python 3 client library `simplex-chat` on PyPI for SimpleX bots. Same capability as the Node.js library at `packages/simplex-chat-nodejs/`.

The user writes a Python script with decorator-registered handlers; the library does the rest:

```python
from simplex_chat import Bot, BotProfile, SqliteDb, TextMessage

bot = Bot(profile=BotProfile(display_name="Squarer"),
          db=SqliteDb(file_prefix="./bot"),
          welcome="Send a number, I'll square it.")

@bot.on_message(content_type="text")
async def square(msg: TextMessage) -> None:
    try:
        n = float(msg.text)
        await msg.reply(f"{n} * {n} = {n * n}")
    except ValueError:
        await msg.reply("Not a number.")

if __name__ == "__main__":
    bot.run()
```

`pip install simplex-chat`, run the script, done.

## Why

SimpleX has a Node.js library (`simplex-chat`) and Haskell-built native lib (`libsimplex.{so,dylib,dll}`) but no Python equivalent. Python is the dominant language for bot scripting, automation, and data integration. Without a Python client, those users either can't use SimpleX or have to bridge through Node.

The native `libsimplex` already exists as prebuilt artifacts (`simplex-chat/simplex-chat-libs` GitHub releases, one zip per platform/backend). The Haskell type metadata that drives the Node lib's TypeScript types is already in `bots/src/API/Docs/`. Both can be reused — adding Python bindings is mostly wiring, not a new system.

## How

Three pieces:

1. **Extend the existing Haskell type generator** in `bots/src/API/Docs/Generate/` to emit a Python types module alongside the existing TypeScript one. The metadata is the same; only the rendering changes. Already includes `pySyntaxText` (used today in COMMANDS.md docs) — just needs a Python codegen module.

2. **A new Python package** `packages/simplex-chat-python/` that wraps the prebuilt `libsimplex.*` via `ctypes`, downloading it on first use from the existing GitHub release. Async-only (`asyncio`), Python 3.11+. Single `Bot` class with decorator-registered handlers.

3. **One small CI job** appended to `.github/workflows/build.yml`, after the existing `release-nodejs-libs` job, that publishes the Python package to PyPI on each release tag. ~15 lines of YAML.

No new infrastructure: no separate libs build, no per-platform wheels, no PyPI size waiver, no second CI workflow. The libs zips that already exist for the Node lib are reused unchanged.

## Architecture

```
┌────────────────────────────────────────────────────────────┐
│  bots/src/API/Docs/                  (Haskell, existing)   │
│  ├── Types/Commands/Events/Responses                       │
│  ├── Syntax.hs   (already has pySyntaxText)                │
│  ├── Generate/TypeScript.hs          (existing)            │
│  └── Generate/Python.hs              ← new                 │
└────────────────────┬───────────────────────────────────────┘
                     │ tests/APIDocs.hs runs both generators
                     ▼ writes auto-gen Python type files
┌────────────────────────────────────────────────────────────┐
│  packages/simplex-chat-python/       (new)                 │
│                                                            │
│  Bot         ←── public API: decorators, lifecycle         │
│   └── ChatApi  ← escape hatch: raw command access          │
│       └── core  ← internal: typed FFI wrapper              │
│           └── _native  ← internal: ctypes + lazy DL        │
│                ↓                                           │
│              libsimplex.{so,dylib,dll}                     │
│              downloaded from simplex-chat-libs releases    │
└────────────────────────────────────────────────────────────┘
```

The split lets each layer be tested independently: `Bot`'s filter-routing without a real libsimplex (mock `api`), `core`'s JSON handling without ctypes (mock `_native`), `_native`'s download/ctypes work with a stub `.so`. Same shape as the Node lib (`bot.ts` → `api.ts` → `core.ts` → `simplex.cc`).

## Type generation

### New module: `bots/src/API/Docs/Generate/Python.hs`

Mirrors `Generate/TypeScript.hs` line-for-line. Reuses the existing data sources (`chatCommandsDocs`, `chatResponsesDocs`, `chatEventsDocs`, `chatTypesDocs`) and `pySyntaxText` from `Syntax.hs`. Output goes to `packages/simplex-chat-python/src/simplex_chat/types/` as four files: `_types.py`, `_commands.py`, `_responses.py`, `_events.py`.

### Type representation

Wire types are `TypedDict` + `Literal` discriminators (matches Node lib semantics — just shapes, no runtime cost; pyright narrows tagged unions correctly).

| Haskell | Python |
|---|---|
| `STRecord` | `class Foo(TypedDict)`; optional fields use `NotRequired[...]`. |
| `STUnion` / `STUnion1` | One `class Foo_<Tag>(TypedDict)` per member with `type: Literal["<tag>"]` discriminator. Type alias `Foo = Foo_A \| Foo_B \| …`. Tag alias `Foo_Tag = Literal["<tagA>", "<tagB>", …]`. |
| `STEnum` / `STEnum1` / `STEnum'` | Type alias `Foo = Literal["a", "b", "c"]`. |
| `ATPrim TBool` | `bool` |
| `ATPrim TString` | `str` |
| `ATPrim TInt`/`TInt64`/`TWord32` | `int` |
| `ATPrim TDouble` | `float` |
| `ATPrim TJSONObject` | `dict[str, object]` |
| `ATPrim TUTCTime` | `str` (ISO-8601, comment-annotated) |
| `ATOptional t` | `NotRequired[<t>]` in TypedDict fields; `<t> \| None` elsewhere |
| `ATArray {nonEmpty=False}` | `list[<elem>]` |
| `ATArray {nonEmpty=True}` | `list[<elem>]` with trailing `# non-empty` comment |
| `ATMap (PT k) v` | `dict[<k>, <v>]` |
| `ATDef` / `ATRef` | type name as forward-string reference `"<name>"` |

### Command serialization

Each command becomes a `TypedDict` plus a `<Type>_cmd_string(self) -> str` function. The function body is produced by `pySyntaxText` (which already emits Python expressions for the existing Markdown docs):

```python
class APICreateMyAddress(TypedDict):
    userId: int

def APICreateMyAddress_cmd_string(self: APICreateMyAddress) -> str:
    return '/_address ' + str(self['userId'])

APICreateMyAddress_Response = CR.UserContactLinkCreated | CR.ChatCmdError
```

### Field-naming convention

| Where | Style | Why |
|---|---|---|
| Auto-generated `types/_*.py` | **camelCase** | Round-trips JSON to/from libsimplex; the keys are the wire format. |
| Hand-written user-facing types (`SqliteDb`, `BotProfile`, `Message`, …) | **snake_case** | These are Python-side configs and wrappers, never reach `chat_send_cmd` directly. |
| `CryptoArgs` (`fileKey`, `fileNonce`) | **camelCase** | Returned by `chat_write_file` as JSON; round-trips wire format. |
| Method names | **snake_case** | PEP 8. |
| Type names | **PascalCase** | PEP 8 + Haskell parity. |

Generator must emit field names verbatim from `APIRecordField.fieldName'` — never transform.

### Wiring

Extend `tests/APIDocs.hs` with four `testGenerate` calls:

```haskell
describe "Python" $ do
  it "generate python commands"  $ testGenerate Py.commandsCodeFile  Py.commandsCodeText
  it "generate python responses" $ testGenerate Py.responsesCodeFile Py.responsesCodeText
  it "generate python events"    $ testGenerate Py.eventsCodeFile    Py.eventsCodeText
  it "generate python types"     $ testGenerate Py.typesCodeFile     Py.typesCodeText
```

`cabal test` regenerates all eight files (4 TS + 4 PY) and fails on drift — same enforcement loop that already governs the TypeScript files.

Add `API.Docs.Generate.Python` to `simplex-chat.cabal:572-580`.

## Native lib loading

### Approach: lazy download on first use

Single pure-Python wheel on PyPI (`simplex-chat`, ~100 KB). On first `Bot(...)` / `ChatApi.init(...)`, the library downloads the matching `libsimplex` zip from `simplex-chat/simplex-chat-libs` releases into a user cache, then `ctypes.CDLL`s it. Subsequent runs read from cache.

**Why not platform wheels?** Two reasons. First, simpler CI: one `python -m build` job vs a 5-platform matrix that has to download libs zips and rebuild wheels per platform. Second, the libs are already published as the source of truth (existing `release-nodejs-libs` job) — wheels would be a wrapper around those, adding nothing but build complexity. Tradeoff: first run requires internet; air-gap users set `SIMPLEX_LIBS_DIR=/path/to/libs`.

### Cache layout

```
~/.cache/simplex-chat/        # XDG_CACHE_HOME on Linux
└── v6.5.1/                   # = LIBS_VERSION
    ├── sqlite/libsimplex.so + libHS*.so
    └── postgres/libsimplex.so + libHS*.so
```

Platform-specific cache base: Linux `$XDG_CACHE_HOME`, macOS `~/Library/Caches/`, Windows `%LOCALAPPDATA%`.

### Version pinning

`src/simplex_chat/_version.py`:

```python
__version__ = "6.5.1"        # PEP 440 — bumped with each Python package release
LIBS_VERSION = "6.5.1"       # simplex-chat-libs release tag (no 'v' prefix)
```

`__version__` is read by hatchling for wheel metadata. `LIBS_VERSION` is read by `_native.py` for the download URL. Wrapper-only patch releases use a post-suffix (`__version__ = "6.5.1.post1"`, `LIBS_VERSION` unchanged). Same pattern as Node lib's `RELEASE_TAG = 'v6.5.1'`.

### `_native.py` responsibilities

1. **Detect platform.** `sys.platform` × `platform.machine()` → `linux-x86_64`, `linux-aarch64`, `macos-x86_64`, `macos-aarch64`, `windows-x86_64`. Unsupported combos raise immediately.
2. **Resolve backend** from the `Db` instance (`isinstance(db, SqliteDb)` → sqlite, `PostgresDb` → postgres). Module-level `threading.Lock` guards selection — first call wins for the process; subsequent call with a different backend raises (one libsimplex variant per process — Haskell RTS constraint).
3. **Resolve libs path.** If `SIMPLEX_LIBS_DIR` env is set, use it directly. Otherwise: `~/.cache/simplex-chat/v{LIBS_VERSION}/{backend}/`, downloading if missing.
4. **Download URL** (`LIBS_VERSION` is stored without 'v' prefix; URL re-adds it):

   ```
   https://github.com/simplex-chat/simplex-chat-libs/releases/download/v{LIBS_VERSION}/simplex-chat-libs-{platform}-{arch}{-postgres?}.zip
   ```

5. **Atomic install.** Download to sibling tempdir → `zipfile.extractall` → `os.replace` the `libs/` subdir into cache. The libs zip contains only regular files (no symlinks — `build.yml:751-781` builds it via `cp *.so` which resolves them), so plain `extractall` is sufficient. Two processes racing safely both extract; whichever rename lands first wins, contents identical.
6. **Load and init.** `ctypes.CDLL(libs_dir / libname)` once per process; declare `restype`/`argtypes` for the 8 FFI functions; call `hs_init_with_rtsopts` exactly once with `+RTS -A64m -H64m -xn --install-signal-handlers=no` (or without `-xn` on Windows — matches `cpp/simplex.cc:13-32`).
7. **Buffer ownership.** Haskell allocates result strings; caller must `free()` after copying. Declare `restype = c_void_p` (NOT `c_char_p`, which auto-converts to bytes and discards the pointer needed for free):

   ```python
   ptr = lib.chat_send_cmd(ctrl, cmd_bytes)
   if not ptr: raise RuntimeError("null result")
   try:    result = ctypes.string_at(ptr).decode("utf-8")
   finally: libc.free(ptr)
   ```

   `libc` is `ctypes.CDLL(None)` on Linux/macOS, `ctypes.CDLL("msvcrt")` on Windows. Mirrors `HandleCResult` in `cpp/simplex.cc:157-165`.

### Override / pre-fetch

```bash
# Skip download — for Docker / air-gapped
SIMPLEX_LIBS_DIR=/opt/simplex/libs python my_bot.py

# Pre-fetch in Dockerfile RUN step (avoids redundant download per container start)
python -m simplex_chat install --backend=sqlite
python -m simplex_chat install --backend=postgres
```

### Failure modes

| Condition | Behavior |
|---|---|
| Unsupported platform/arch | Raise on first FFI call with explicit list of supported combinations. |
| Postgres on non-Linux-x86_64 | Raise — matches existing constraint in `download-libs.js:15-18`. |
| Download network/HTTP error | Propagate `urllib.error.URLError` with the URL. |
| Two processes downloading simultaneously | Both extract to sibling temp dirs; rename is atomic; identical contents → safe. |
| Two `Bot()` / `ChatApi.init()` calls in same process with different backends | Second raises — one libsimplex variant per process. |
| Two `Bot()` instances same backend, same process | Permitted — each has its own controller (`chat_ctrl`). |

## Public API

See the [Architecture](#architecture) section for the layering. This section specifies each layer's surface.

### Construction

User-facing config types are `@dataclass(slots=True)`, snake_case fields.

```python
@dataclass(slots=True)
class SqliteDb:
    file_prefix: str
    encryption_key: str | None = None

@dataclass(slots=True)
class PostgresDb:
    connection_string: str
    schema_prefix: str | None = None

Db = SqliteDb | PostgresDb     # discriminated by isinstance()

@dataclass(slots=True)
class BotProfile:
    display_name: str
    full_name: str = ""
    short_descr: str | None = None
    image: str | None = None

@dataclass(slots=True)
class BotCommand:
    keyword: str
    label: str

class Bot:
    def __init__(
        self, *,
        profile: BotProfile,
        db: Db,
        welcome: str | T.MsgContent | None = None,
        commands: list[BotCommand] | None = None,        # None → []
        confirm_migrations: MigrationConfirmation = MigrationConfirmation.YES_UP,
        # behavioral toggles — mirror BotOptions in Node lib
        create_address: bool = True,
        update_address: bool = True,
        update_profile: bool = True,
        auto_accept: bool = True,
        business_address: bool = False,
        allow_files: bool = False,
        use_bot_profile: bool = True,
        log_contacts: bool = True,
        log_network: bool = False,
    ) -> None: ...

    @property
    def api(self) -> ChatApi: ...
```

### Handler registration

Three decorators. Filters are kwargs combined with **AND**; tuples within a kwarg are **OR**; arbitrary predicates use `when=`.

```python
class Bot:
    def on_message(self, *,
        content_type: T.MsgContent_Tag | tuple[T.MsgContent_Tag, ...] | None = None,
        text: str | re.Pattern | None = None,        # exact match or regex.search()
        chat_type: T.ChatType | tuple[T.ChatType, ...] | None = None,    # direct/group/local
        from_role: T.GroupMemberRole | tuple[T.GroupMemberRole, ...] | None = None,
        from_contact_id: int | tuple[int, ...] | None = None,
        from_member_id: int | tuple[int, ...] | None = None,
        group_id: int | tuple[int, ...] | None = None,
        when: Callable[[Message], bool] | None = None,
    ) -> Callable[[MessageHandler], MessageHandler]: ...

    def on_command(self, name: str | tuple[str, ...], *,
        args: str | re.Pattern | None = None,        # match command argument string
        chat_type: T.ChatType | tuple[T.ChatType, ...] | None = None,
        from_role: T.GroupMemberRole | tuple[T.GroupMemberRole, ...] | None = None,
        from_contact_id: int | tuple[int, ...] | None = None,
        group_id: int | tuple[int, ...] | None = None,
        when: Callable[[Message], bool] | None = None,
    ) -> Callable[[CommandHandler], CommandHandler]: ...

    # Multiple handlers per tag dispatch in registration order.
    def on_event(self, event: CEvt.Tag, /,
                 ) -> Callable[[EventHandler], EventHandler]: ...

    def use(self, middleware: Middleware) -> None: ...

MessageHandler = Callable[[Message], Awaitable[None] | None]
CommandHandler = Callable[[Message, ParsedCommand], Awaitable[None] | None]
EventHandler   = Callable[[CEvt.ChatEvent], Awaitable[None] | None]
```

`from_role` on direct chats: silent skip (not a runtime error).

### Message wrapper and content-narrowed types

`Message[C]` is generic in its content variant; concrete subclass aliases (`TextMessage`, `ImageMessage`, …) bind to the auto-generated `T.MsgContent_*` types. Decorator overloads narrow the handler parameter when `content_type` is a single `Literal`, so pyright sees the right concrete type.

```python
C = TypeVar("C", bound=T.MsgContent)            # bound covers the unparameterized case

@dataclass(slots=True, frozen=True)
class Message(Generic[C]):
    chat_item: T.AChatItem    # raw wire object — fields below this point are camelCase
    content: C                # narrowed when filter pins content_type
    bot: Bot

    @property
    def chat_info(self) -> T.ChatInfo: ...      # shortcut for chat_item["chatInfo"]
    @property
    def text(self) -> str | None: ...           # shortcut; non-Optional for TextMessage

    async def reply(self, text: str) -> Message: ...
    async def reply_content(self, content: T.MsgContent) -> Message: ...
    async def react(self, emoji: str) -> None: ...
    async def delete(self) -> None: ...
    async def forward(self, to: T.ChatRef) -> Message: ...

# Concrete narrowed aliases — exported from simplex_chat/__init__.py
TextMessage  = Message[T.MsgContent_Text]
ImageMessage = Message[T.MsgContent_Image]
FileMessage  = Message[T.MsgContent_File]
VoiceMessage = Message[T.MsgContent_Voice]
# … one per MsgContent variant

@dataclass(slots=True, frozen=True)
class ParsedCommand:
    keyword: str
    args: str
```

Decorator overloads (one per `T.MsgContent_*` variant — ~15 lines, hand-written in `bot.py`):

```python
class Bot:
    @overload
    def on_message(self, *, content_type: Literal["text"], **rest: Any
                  ) -> Callable[[Callable[[TextMessage], Awaitable[None] | None]], ...]: ...
    @overload
    def on_message(self, *, content_type: Literal["image"], **rest: Any
                  ) -> Callable[[Callable[[ImageMessage], Awaitable[None] | None]], ...]: ...
    # … one overload per MsgContent variant …
    @overload
    def on_message(self, *, content_type: tuple[T.MsgContent_Tag, ...] | None = None,
                   **rest: Any) -> Callable[[Callable[[Message], Awaitable[None] | None]], ...]: ...
```

`@bot.on_message(content_type="text")` → handler typed as `TextMessage`, so `msg.text: str` (non-Optional).

**Field-naming boundary in `Message`.** Wrapper properties (`msg.chat_info`, `msg.content`, `msg.text`) are snake_case. Descending into raw wire data via `msg.chat_item[...]` reverts to camelCase — same as accessing `T.AChatItem` returned by `bot.api`. Property shortcuts cover the common paths so most handlers never touch `chat_item` directly.

### Lifecycle

```python
class Bot:
    # Blocking convenience — runs asyncio.run(self.serve_forever()), installs SIGINT
    # via loop.add_signal_handler() (POSIX) or signal.signal() (Windows). Recommended for scripts.
    def run(self) -> None: ...

    # Embedding form — caller owns the loop and signal handling.
    async def __aenter__(self) -> Bot: ...
    async def __aexit__(self, *exc_info: object) -> None: ...

    # Concurrent calls raise RuntimeError("already serving"). Re-callable after a clean stop().
    async def serve_forever(self) -> None: ...

    # Marks bot for shutdown. Safe from signal handler, another coroutine, or another thread.
    def stop(self) -> None: ...
```

### Middleware

aiogram pattern. A class with `async __call__(handler, message, data)` wraps every **handler invocation** (per-message-per-matching-handler). `data: dict[str, object]` is the cross-cutting injection channel.

```python
class Middleware:
    async def __call__(self,
                       handler: Callable[[Message, dict[str, object]], Awaitable[None]],
                       message: Message,
                       data: dict[str, object]) -> None:
        await handler(message, data)
```

- **Invocation count.** A `newChatItems` event with N items × M matching handlers triggers N×M middleware calls. Per-event hooks should use `on_event`.
- **Exception propagation.** Handler exceptions propagate outward through the middleware stack. The outermost middleware can catch and swallow. Uncaught exceptions are logged via `logging.getLogger("simplex_chat")` and the chain moves to the next handler — the bot does not stop on individual handler errors. The receive loop only stops on a fatal `_native`/`core` error or explicit `bot.stop()`.
- **Order.** Registered via `bot.use(...)`. Called in registration order (first registered = outermost wrap).

### Event-loop semantics

`Bot` runs one event-receiver coroutine looping `chat_recv_msg_wait` (in `asyncio.to_thread`):

1. All `on_event(tag)` handlers for the event's `type` field — registration order, sequentially.
2. If event is `newChatItems`: for each chat item, run **all matching message handlers** (each through the middleware stack, in registration order). For each command-parseable text item, also run matching command handlers.

Handlers run **sequentially within an event**. Events are processed **sequentially**. Long-running work that shouldn't block the next event must `asyncio.create_task(...)` explicitly.

`bot.api.api_xxx(...)` calls are safe during `serve_forever` — same controller, serialized through `chat_send_cmd`. Calling them from inside a handler is the normal pattern (`msg.reply()` does exactly this).

### `ChatApi` (escape hatch)

Reached via `bot.api`. ~40 async methods, one per Node `apiXxx` (api.ts:344-958). Full enumeration deferred to implementation; representative examples:

```python
class ChatApi:
    @classmethod
    async def init(cls, db: Db,
                   confirm: MigrationConfirmation = MigrationConfirmation.YES_UP) -> ChatApi: ...
    async def start_chat(self) -> None: ...
    async def stop_chat(self) -> None: ...
    async def close(self) -> None: ...
    async def send_chat_cmd(self, cmd: str) -> CR.ChatResponse: ...
    async def recv_chat_event(self, wait_us: int = 5_000_000) -> CEvt.ChatEvent | None: ...

    async def api_create_user_address(self, user_id: int) -> T.CreatedConnLink: ...
    async def api_send_text_message(self, chat: T.ChatRef, text: str,
                                    in_reply_to: int | None = None) -> list[T.AChatItem]: ...
    async def api_get_chats(self, user_id: int, pagination: T.PaginationByTime,
                            query: T.ChatListQuery | None = None) -> list[T.AChat]: ...
    # ... etc
```

TS `apiCreateUserAddress` → Python `api_create_user_address` (PEP 8). Wire-format type names (`T.AChatItem`, `T.UserContactLink`, …) keep their Haskell/TS spelling to match JSON keys.

### Embedding example

```python
import asyncio
from simplex_chat import Bot, BotProfile, SqliteDb

async def main():
    async with Bot(profile=..., db=...) as bot:
        @bot.on_message(content_type="text")
        async def echo(msg):
            await msg.reply(msg.text)
        await asyncio.gather(bot.serve_forever(), other_task())

asyncio.run(main())
```

## Distribution and CI

### Project layout

```
packages/simplex-chat-python/
├── pyproject.toml              # hatchling, requires-python >= 3.11, no runtime deps
├── README.md
├── LICENSE                     # AGPL-3.0
├── src/simplex_chat/
│   ├── __init__.py             # exports Bot, BotProfile, BotCommand, SqliteDb, PostgresDb,
│   │                           # Message + TextMessage/ImageMessage/… aliases, ParsedCommand,
│   │                           # ChatApi, MigrationConfirmation, Middleware, ChatAPIError
│   ├── _version.py             # __version__ + LIBS_VERSION
│   ├── _native.py              # ctypes + lazy lib download (internal)
│   ├── __main__.py             # python -m simplex_chat install ...
│   ├── core.py                 # internal typed FFI wrapper
│   ├── api.py                  # ChatApi class — escape hatch
│   ├── bot.py                  # Bot class, decorators, Message wrapper, lifecycle
│   ├── filters.py              # filter kwarg compilation; predicate combinators
│   ├── util.py                 # stateless helpers (chat_info_ref, ci_content_text, reaction_text, …)
│   ├── py.typed                # PEP 561 marker
│   └── types/
│       ├── __init__.py         # re-exports T, CC, CR, CEvt
│       ├── _types.py           # AUTOGEN
│       ├── _commands.py        # AUTOGEN
│       ├── _responses.py       # AUTOGEN
│       └── _events.py          # AUTOGEN
├── examples/
│   └── squaring_bot.py
└── tests/
```

### `pyproject.toml`

```toml
[build-system]
requires = ["hatchling>=1.24"]
build-backend = "hatchling.build"

[project]
name = "simplex-chat"
description = "SimpleX Chat Python library for chat bots"
license = "AGPL-3.0"
authors = [{name = "SimpleX Chat"}]
requires-python = ">=3.11"
dynamic = ["version"]

[tool.hatch.version]
path = "src/simplex_chat/_version.py"

[tool.hatch.build.targets.wheel]
packages = ["src/simplex_chat"]
```

No runtime Python dependencies (ctypes, urllib, zipfile are stdlib).

### CI publishing

One job appended to `.github/workflows/build.yml`, after `release-nodejs-libs`:

```yaml
publish-python:
  needs: [release-nodejs-libs]
  if: startsWith(github.ref, 'refs/tags/v')
  runs-on: ubuntu-latest
  permissions: { id-token: write }     # OIDC, no API key
  steps:
    - uses: actions/checkout@v6
    - uses: actions/setup-python@v5
      with: { python-version: "3.11" }
    - run: pip install build && python -m build --wheel
      working-directory: packages/simplex-chat-python
    - uses: pypa/gh-action-pypi-publish@release/v1
      with: { packages-dir: packages/simplex-chat-python/dist }
```

Triggered by the same `vX.Y.Z` tag that already drives the desktop and libs releases.

### One-time setup

1. Verify PyPI package name `simplex-chat` is available; register it.
2. On PyPI project page, configure trusted publisher → repo `simplex-chat/simplex-chat`, workflow `build.yml`, job `publish-python`.

## Testing

Three levels:

1. **Codegen drift** — `tests/APIDocs.hs` adds Python generators alongside TypeScript. Same `testGenerate` mechanism enforces that committed `_types.py` etc. equal the generator output.

2. **Python unit tests** — `pytest`, no real libsimplex needed:
   - `test_native.py`: mock `urllib.request.urlretrieve` + `zipfile.ZipFile`; assert correct URL, atomic rename, cache hit on second call, override-env behavior, postgres-on-mac rejection.
   - `test_codegen.py`: import every type from `simplex_chat.types`, sanity-check that `T.ChatType` is `Literal[...]` of expected size, etc. Catches generator regressions.
   - `test_smoke.py`: build a fake `.so` (small C file with stub `chat_send_cmd` returning canned JSON, compiled per-test), point `SIMPLEX_LIBS_DIR` at it, run `Bot.__aenter__` → handler dispatch. Verifies FFI plumbing without real Haskell.

3. **Integration** — `examples/squaring_bot.py` runs against real libsimplex. Not in CI (needs network + persistent state).

## Open questions

1. **Linux ARM64.** Existing `simplex-chat-libs` releases ship `linux-x86_64`, `macos-x86_64`, `macos-aarch64`, `windows-x86_64` — no `linux-aarch64`. Python lib will fail with a clear message there. Adding it requires changes to the existing `release-nodejs-libs` job in `build.yml` (out of scope for this spec).

2. **`asyncio.to_thread` pool sizing.** Long-blocking `chat_recv_msg_wait` calls (default 5 s) pin executor threads. The default asyncio pool is unbounded but recycled. Bots running many concurrent chats may need a custom executor — first-pass uses `asyncio.to_thread`; document recommended pool sizing in README if it becomes a problem.
