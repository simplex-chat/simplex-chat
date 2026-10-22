# Make `--log-file` work with `-p` (WebSockets server mode)

Issue: [#7113](https://github.com/simplex-chat/simplex-chat/issues/7113) "Log doesn't work if -p is used".
Branch: `nd/fix-server-log-file` · files `apps/simplex-chat/Server.hs`, `src/Simplex/Chat/Core.hs`.

## 1. Problem statement

```
simplex-chat -p 5225 --log-level debug --log-agent --log-file a.log --execute-log all
```

never creates or writes `a.log`. Removing `-p` makes the log file appear and fill. So `--log-file` works in the normal terminal CLI but is silently dead in server mode.

## 2. Root cause

`-p` does not just open a port — it switches the whole run mode. `simplexChatCLI'` (`Terminal/Main.hs`), unless `-e` is given, branches on `chatServerPort`: with a port it calls `simplexChatServer` (the WebSockets server, `apps/simplex-chat/Server.hs`); without it (and without `-e` or `--headless`), `runCLI → simplexChatTerminal`.

The `--log-file` file is only ever written by `runTerminalOutput` (`Terminal/Output.hs`), which exists **only on the terminal path**. It is the single, process-lifetime consumer of the controller's `outputQ`, and for each event it appends to the file when `logFilePath` is set:

```haskell
let printEvent = case logFilePath of
      Just path -> if either (const True) logEventToFile r_ then logResponse path else printToTerminal ct
      _ -> printToTerminal ct
```

The server path never runs `runTerminalOutput`. Also, in `runChatServer` the only consumer of `outputQ` is `output`, which lives **inside the per-connection handler**:

```haskell
runLocalTCPServer started chatPort $ \sock -> do
  ...
  raceAny_ [send ws c, client c, output c, receive ws c]   -- output drains outputQ
```

So `outputQ` is drained **only while a WebSocket client is connected** — a fix that logged in the per-connection handler would still miss everything while no client is attached.

On master the server never looks at `logFilePath` at all, so the file is not written whether or not a client is connected; events only go to connected clients. The terminal path differs because its output consumer (`runTerminalOutput`) runs for the whole process lifetime and writes the file.

Note: the agent log that `--log-agent` (or `-l debug`) enables is a *separate* mechanism — a global logger to **stderr** (`logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}`, `Chat.hs`). It never went to `--log-file`, before or after this fix. `--log-level` does affect `--log-file`: lower levels log more lines (the views have thresholds at error, warn, info and debug), and `-l info` or lower also turns on the host events; subscription status lines are logged at any level. `--log-file` carries the filtered set of chat events (`logEventToFile`, `Controller.hs`) plus errors, some of which are also filtered by level. This fix provides that set in server mode; it does not reroute agent logs.

## 3. Fix

When `--log-file` is set, `simplexChatServer` installs `ChatHooks.eventHook` on the server's config. The hook is called in `toView_` for every locally produced event before it is queued (`Controller.hs`), so each event is logged when the core produces it, whether or not a client is connected (until the existing no-client stall, when the core blocks on a full `outputQ` after `tbqSize` events); it returns the event unchanged.

```haskell
simplexChatServer chatPort cfg@ChatConfig {chatHooks} opts@ChatOpts {coreOptions = CoreChatOpts {logFile}} = do
  cfg' <- case logFile of
    Just path -> do
      logLock <- newMVar ()
      pure cfg {chatHooks = chatHooks {eventHook = Just $ logFileHook logLock path}}
    Nothing -> pure cfg
  simplexChatCore cfg' opts . const $ runChatServer defaultChatServerConfig {chatPort}
  where
    logFileHook logLock path cc r = do
      when (either (const True) logEventToFile r) $ do
        u_ <- readTVarIO $ currentUser cc
        withMVar logLock (\_ -> logResponseToFile u_ (config cc) r path) `catchAny` \e -> putStrLn ("log-file write error: " <> show e)
      pure r
```

The hook logs what terminal mode logs — the same filter (`logEventToFile` events and all errors), checked before taking the lock so other events never wait on a write — and `logResponseToFile` (`Core.hs`, next to `printResponseEvent`) formats them with the same view functions with the current user (so `[user: X]` prefixes match), appended with `withFile … AppendMode`. It lives in the library only because the CLI executable has no `time` dependency.

- Without `--log-file` nothing changes; with it, `runChatServer` and the client path are unchanged too.
- A failed write (bad path, full disk, removed directory) prints `log-file write error: …` on stdout and the event is still delivered; without `catchAny` the exception would surface in the core thread that produced the event.
- Terminal mode never gets the hook (it logs in `runTerminalOutput`), so nothing is logged twice.
- The hook runs on whichever core thread produced the event, so writes are serialised with a lock: GHC allows one open write handle per file in a process, and a second concurrent open would fail and drop that line.

## 4. Alternatives considered

| Option | Why not |
|---|---|
| A server-lifetime loop that reads `outputQ`, logs, and forwards through a second queue (the previous revision of this PR) | More code in `runChatServer` and an extra queue hop per event; with no client, logging stopped about `tbqSize` events before the core itself stalled. |
| Log in the per-client output loop | Nothing is logged while no client is connected — the reported case. |
| Log in the library's `toView_` for every mode | Terminal mode would log twice unless `runTerminalOutput` changed; affects bots and services. |
| Broadcast channel (`dupTChan`) | A late client would lose the backlog it gets today, or memory grows without bound. |
| Reject `--log-file` with `-p` | Leaves #7113 unfixed. |

`eventHook` is the documented extension point for extra event processing (`Controller.hs`), and the directory and badge services install their own hooks the same way.

## 5. Known differences from terminal mode

- Events and poll errors for a remote host that a `-p` server controls (`pollEvents`, `Remote.hs`) do not go through `eventHook`, so they are not logged; terminal mode logs them.
- While a remote host is the current host, terminal mode formats events with that host's user; the server always uses the local user.
- Events for a connected remote controller (`remoteOutputQ`) are logged here; terminal mode does not log them.
- A failed log write is reported and the server keeps running; in terminal mode it ends the CLI.
- Logged events are written to the file and still sent to clients; in terminal mode they go to the file instead of the screen.
- Lines are in the order the producing threads take the log lock, which can differ slightly from the order clients receive them.
- Log writes run on the core thread that produced the event (one open/append/close per logged event, as in terminal mode), so a hung log filesystem would stall the core at once.

## 6. Verification

CLI builds of the base and this branch, a private SMP server, a database with one address, `wscat` as the client: the reporter's command (no client) writes `connected to SMP host …` / `subscribed 1 connections on server …` (master: no file); with no client connected, each host event is in the file within about 0.2 s of happening; a client still gets every event, including the backlog when it connects late; without `--log-file` nothing changes; an unwritable path prints one error line per logged event and the server keeps running; the log is identical to terminal mode's, including the `[user: X]` prefix of a message error; SIGTERM/SIGINT exit in under 0.1 s.

`--headless`, `-e`, the bots, and the directory and badge services in service mode accept `--log-file` but ignore it (the directory and badge services' `--run-cli` mode already logs through `runTerminalOutput`); each could get the same hook in a separate PR, moved to the library.
