# Make `--log-file` work with `-p` (WebSockets server mode)

Issue: [#7113](https://github.com/simplex-chat/simplex-chat/issues/7113) "Log doesn't work if -p is used".
Suggested branch: `nd/fix-server-log-file` · files `apps/simplex-chat/Server.hs`, `src/Simplex/Chat/Core.hs`.

## 1. Problem statement

```
simplex-chat -p 5225 --log-level debug --log-agent --log-file a.log --execute-log all
```

never creates or writes `a.log`. Removing `-p` makes the log file appear and fill. So `--log-file` works in the normal terminal CLI but is silently dead in server mode.

## 2. Root cause

`-p` does not just open a port — it switches the whole run mode. `simplexChatCLI'` (`Terminal/Main.hs:30-42`) branches on `chatServerPort`: with a port it calls `simplexChatServer` (the WebSockets server, `apps/simplex-chat/Server.hs`); without it, `runCLI → simplexChatTerminal`.

The `--log-file` file is only ever written by `runTerminalOutput` (`Terminal/Output.hs:144-168`), which exists **only on the terminal path**. It is the single, process-lifetime consumer of the controller's `outputQ`, and for each event it appends to the file when `logFilePath` is set:

```haskell
let printEvent = case logFilePath of
      Just path -> if either (const True) logEventToFile r_ then logResponse path else printToTerminal ct
      _ -> printToTerminal ct
```

The server path never runs `runTerminalOutput`. Worse, in `runChatServer` the only consumer of `outputQ` was `output`, which lived **inside the per-connection handler**:

```haskell
runLocalTCPServer started chatPort $ \sock -> do
  ...
  raceAny_ [send ws c, client c, output c, receive ws c]   -- output drains outputQ
```

So `outputQ` is drained **only while a WebSocket client is connected**. With the server merely running (the issue's repro — no client attached), nothing consumes the queue, so nothing is logged, and the startup subscription/host events never reach a file.

This was confirmed empirically (real DB with one live connection, `--log-level debug`):

| | WebSocket client connected? | `--log-file` written? |
|---|---|---|
| A | No | **No** (file never created) — reproduces the issue |
| B | Yes (`wscat` on `127.0.0.1`) | Yes — `connected to SMP host…` / `subscribed 1 connections…` |

The terminal path differs precisely because its output consumer (`runTerminalOutput`) runs for the whole process lifetime, independent of any UI peer.

Note: `--log-agent` / `--log-level` are a *separate* mechanism — global logger to **stderr** (`logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}`, `Chat.hs:142`). They never went to `--log-file`, before or after this fix. `--log-file` carries the filtered set of chat events (`logEventToFile`, `Controller.hs:1032`) plus all errors. This fix restores that set for server mode; it does not reroute agent logs.

## 3. Solution summary

Give the server a process-lifetime output consumer, mirroring the terminal, without changing client-delivery behaviour.

**`Core.hs`** — add `logResponseToFile`, a sibling of the existing `printResponseEvent` (which serializes the same way to stdout):

```haskell
logResponseToFile :: ChatConfig -> Either ChatError ChatEvent -> FilePath -> IO ()
logResponseToFile cfg r path =
  when (either (const True) logEventToFile r) $ do
    ts <- getCurrentTime
    tz <- getCurrentTimeZone
    let s = either (serializeChatError False cfg) (serializeChatResponse (Nothing, Nothing) cfg ts tz Nothing) r
    withFile path AppendMode $ \h -> hPutStr h s
```

Same filter (`either (const True) logEventToFile`), same serializers, same `withFile … AppendMode` append the terminal logger uses.

**`Server.hs`** — give the server a lifetime output consumer, but **only when a log file is set**, so the common no-log path is byte-for-byte the original code:

```haskell
runChatServer ChatServerConfig {chatPort, clientQSize} cc =
  case logFilePath cc of
    -- Without a log file, each client drains the controller output queue directly (as before).
    Nothing -> runServer $ outputQ cc
    -- With a log file, a single server-lifetime loop logs every event - including while no client
    -- is connected - and forwards it through clientQ for delivery.
    Just path -> do
      let ChatConfig {tbqSize} = config cc
      clientQ <- newTBQueueIO tbqSize -- same depth as outputQ, so logging is not bounded by clients
      raceAny_ [logEvents path clientQ, runServer clientQ]
  where
    runServer eventQ = do
      started <- newEmptyTMVarIO
      runLocalTCPServer started chatPort $ \sock -> do
        ...
        raceAny_ [send ws c, client c, output eventQ c, receive ws c] `finally` ...
    -- Logs each event before forwarding it (see "log-first" below). A failed write is caught
    -- so it cannot crash the server or drop connected clients.
    logEvents path clientQ = forever $ do
      out@(_, r) <- atomically . readTBQueue $ outputQ cc
      logResponseToFile (config cc) r path `catchAny` \e -> putStrLn ("log-file write error: " <> show e)
      atomically $ writeTBQueue clientQ out
    output eventQ ChatClient {sndQ} = forever $ do
      (_, r) <- atomically $ readTBQueue eventQ
      atomically $ writeTBQueue sndQ $ ACR ChatSrvResponse {corrId = Nothing, resp = CSRBody r}
```

`output` is parameterised over its source queue: `outputQ cc` directly in the no-log case (original behaviour), or the intermediate `clientQ` when logging. The `Just` branch is the only one that adds a thread, a queue, and the `raceAny_` wrap. `clientQ` matches `outputQ`'s depth so a late-connecting client still drains the same backlog.

## 4. Why this shape

- **Parity with the terminal path.** The bug is structural: the terminal has a lifetime output consumer, the server did not. The `Just` branch gives the server one, logging the exact same filtered event set via the same serializers and filter.
- **Surgical: zero impact when not logging.** Gating on `logFilePath` means the overwhelmingly common no-log server keeps the original topology unchanged — no extra thread, no queue hop, no exception coupling. All new risk is confined to the opt-in feature. (An always-on version was prototyped first and rejected after review for unnecessary blast radius.)
- **A log failure can never take down the server or drop clients.** `runChatServer` runs as `chat u cc` under `runSimplexChat`'s `waitEither_`, and `raceAny_ = withAsync … waitAnyCancel` propagates the first exception. An unguarded `withFile` error (bad path, full disk, log rotation) would therefore kill the controller and drop every client. The `catchAny` around `logResponseToFile` reduces a log-sink failure to one stderr line; draining continues, so client delivery is unaffected. (`catchAny` from `unliftio` is sync-only, so the async cancellation that shuts `logEvents` down still propagates.) Verified (test D below).
- **Log-first ordering is load-bearing.** `logEvents` writes the log *before* forwarding to `clientQ`. With no client connected, `clientQ` fills and the forward blocks; logging before the forward is what keeps the file written while no client is attached — the whole point of the fix. Forwarding first would stop logging once `clientQ` saturates.
- **Off the privacy-critical shared library.** The loop lives entirely in the CLI executable. The alternative — logging at event production in `toView_` — would put per-event file IO on the hot path of the library used by terminal, mobile, and bots, and would double-log in terminal mode (which already logs at consumption and routes events screen-vs-file via `logFilePath`). Rejected as far higher blast radius. `logResponseToFile` is in `Core.hs` (not `Server.hs`) only because the executable lacks the `time`/`View` deps the library already has — no new dependency.
- **Remote-host context is correctly `Nothing`.** `logResponseToFile` passes `Nothing` for the remote host, matching the terminal. This is exact, not a shortcut: `outputQ`'s sole writer is `toView_`, which enqueues `(Nothing, event)` (`Controller.hs:1684`); remote-host events go to a separate `remoteOutputQ`. So `outputQ` never carries a non-`Nothing` `RemoteHostId`, in either the terminal or the server path.
- **Minimal premise.** `logResponseToFile` takes a bare `ChatConfig` (like `printResponseEvent`/`printChatError`). `tbqSize` is bound via an explicit `ChatConfig {tbqSize}` pattern to avoid the `DuplicateRecordFields` ambiguity with `CoreChatOpts.tbqSize`. The `System.IO (IOMode (..), …)` import matches the sibling terminal logger's import.

## 5. Verification

Build: `cabal build exe:simplex-chat` — clean.

Against a throwaway DB holding one address (one subscribable queue), `--log-level debug`:

- **A — `--log-file`, no client:** `a.log` created on startup with `connected to SMP host …` / `subscribed 1 connections on server …`. (Was: no file.)
- **B — `--log-file` + `wscat` client:** the client still receives every event as JSON (`hostConnected`, `subscriptionStatus`) **and** `a.log` is written. Delivery unaffected.
- **C — no `--log-file` + `wscat` client (no-log path):** client receives all events — the original path is unchanged.
- **D — unwritable `--log-file` (`/nonexistent-dir/…`) + client:** server stays alive and the client still receives all events; the guaranteed `withFile` failure is caught, not fatal. (Confirms the `catchAny`.)

Unit-level: `logResponseToFile defaultChatConfig (Left (ChatError (CEInternalError "…"))) path` appends the serialized error and creates the file; called twice it appends twice (errors always pass the filter).

## 6. Known limitations / accepted trade-offs

- **Per-event open/close.** `logResponseToFile` does `withFile … AppendMode` per logged event, as the terminal logger does. This gives immediate durability (so `tail -f` works) at the cost of a syscall per event. Only `logEventToFile` events plus errors are written — high-frequency message events are not — so the steady-state rate is low; a sustained error storm on a slow disk is the pathological case. Opening the handle once (with explicit flush) is a possible follow-up, deliberately left out to match the terminal and keep the diff small.
- **Single-consumer fan-out (pre-existing).** With multiple clients, each event still goes to exactly one of them (they race on one queue), exactly as before this change. Not introduced here; noted so a reader does not assume `clientQ` broadcasts.
- **Duplication.** The predicate `either (const True) logEventToFile` and the serialize-to-file shape exist in both the terminal logger and `logResponseToFile`. At two sites this is left un-extracted; if a third appears, factor a shared `shouldLogToFile`/serializer.
