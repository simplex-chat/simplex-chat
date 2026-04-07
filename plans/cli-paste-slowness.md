# CLI terminal: event loss root cause analysis

## Two distinct problems

### Problem 1: Paste — TMVar capacity-1 bottleneck

When copy-pasting text, the capacity-1 `TMVar` event channel between the keyboard input reader and the consumer loop throttles stdin reading to terminal redraw speed.

**Root cause:** `events <- liftIO newEmptyTMVarIO` (`Platform.hsc:64`). Producer blocks on `putTMVar` after each event until consumer finishes redrawing. Consumer does a full terminal redraw per event (`Input.hs:161`).

**Fix:** Replace `TMVar` with `TQueue` in `Platform.hsc` (6 line changes on POSIX, matching changes on Windows). Decouples producer from consumer — stdin is drained at full speed regardless of redraw speed.

See previous analysis in git history for full details on this issue.

---

### Problem 2: Heavy load — `outputQ` backpressure blocks `agentSubscriber`

When the CLI is used as a heavy client (e.g., 1M connections), incoming chat events overwhelm the terminal display, causing cascading backpressure that blocks message acknowledgments and stalls the entire event processing pipeline.

**This is the more severe problem.** It causes actual message loss at the protocol level, not just UI slowness.

## Root cause: bounded `outputQ` + single-threaded `agentSubscriber`

### The queue chain

```
Network (SMP/XFTP connections)
  → agent internal queues
  → subQ (TBQueue, capacity 1024)          ← agent → chat boundary
  → agentSubscriber (single-threaded)       ← Commands.hs:4167
  → processAgentMessage                     ← Subscriber.hs:109
  → toView_ → writeTBQueue outputQ          ← Controller.hs:1528, BLOCKS when full
  → outputQ (TBQueue, capacity 1024)        ← Chat.hs:152
  → runTerminalOutput                       ← Output.hs:146
  → printToTerminal (acquires termLock)     ← Output.hs:298-303
  → terminal I/O (slow)
```

All queues are bounded `TBQueue` with default capacity 1024 (`Options.hs:226`). All writes use `writeTBQueue` which **blocks when full** — no events are dropped within the application, but backpressure cascades upstream.

### The blocking chain under heavy load

1. **Terminal I/O is the bottleneck.** `runTerminalOutput` (`Output.hs:146`) reads one event at a time from `outputQ`, acquires `termLock`, prints the message + redraws input, releases lock. Each iteration involves ANSI escape sequences, cursor manipulation, and `flush` syscalls. Throughput: ~hundreds of events/sec at best.

2. **`outputQ` fills up.** With 1M connections generating events, the arrival rate far exceeds terminal display speed. The 1024-element TBQueue fills in seconds.

3. **`toView_` blocks.** `Controller.hs:1528`: `writeTBQueue localQ (Nothing, event)` blocks when the queue is full. This call happens inside `processAgentMessage` → `processAgentMessageConn`, which runs within the `agentSubscriber` loop.

4. **`agentSubscriber` blocks — head-of-line blocking.** `Commands.hs:4164-4167`:
   ```haskell
   agentSubscriber = do
     q <- asks $ subQ . smpAgent
     forever (atomically (readTBQueue q) >>= process)
   ```
   Single-threaded. When `process` blocks on `toView_`, ALL events for ALL connections queue up behind it. Events for 1M other connections — including time-critical ACKs, keepalives, and handshakes — are stuck.

5. **ACKs are never sent.** The message receive path (`Subscriber.hs:1537-1540`) calls `toView` BEFORE `ackMsg`:
   ```haskell
   -- Inside withAckMessage's action:
   saveRcvChatItem' ...        -- save to DB (succeeds)
   toView $ CEvtNewChatItems ...  -- BLOCKS here (outputQ full)
   -- returns (withRcpt, shouldDelConns)

   -- After action returns (Subscriber.hs:1396-1397):
   ackMsg msgMeta ...           -- NEVER REACHED while toView blocks
   ```
   The developers explicitly acknowledge this at `Subscriber.hs:122-123`:
   > *without ACK the message delivery will be stuck*

6. **`subQ` fills up.** The agent can't deliver events to `subQ` (also capacity 1024) because `agentSubscriber` isn't reading. Agent-level processing stalls.

7. **Network-level failure.** Connections time out due to unprocessed keepalives and unacknowledged messages. Messages are lost at the protocol level.

### `termLock` contention worsens the bottleneck

`termLock` (`Output.hs:55`) is a `TMVar ()` mutex shared between:
- **Output thread** (`runTerminalOutput` → `printToTerminal`): acquires lock for each displayed message
- **Input thread** (`receiveFromTTY` → `updateInput`): acquires lock after each keystroke
- **Live prompt thread** (`blinkLivePrompt` → `updateInputView`): acquires lock every 1 second

Under heavy load, the output thread dominates the lock (constant stream of messages). The input thread is starved — user keystrokes are delayed. This also slows the output thread itself (lock contention overhead).

Note: `withTermLock` (`Output.hs:138-142`) is not exception-safe — no `bracket`/`finally`. If the action throws, the lock leaks and all threads deadlock.

### Error reporting also blocks

When `processAgentMessage` encounters an error, the error handler (`Commands.hs:4179`) calls `eToView'` → `toView_` → `writeTBQueue outputQ`. If `outputQ` is already full, even error reporting blocks. There is no escape path.

## Impact summary

| Load level | `outputQ` state | Effect |
|---|---|---|
| Light (few connections) | Nearly empty | No issues |
| Moderate (hundreds) | Partially filled | Occasional display lag |
| Heavy (thousands+) | Full (1024) | `toView_` blocks → `agentSubscriber` blocks → head-of-line blocking for ALL connections → ACKs delayed → message delivery stuck |
| Extreme (1M connections) | Permanently full | Cascading failure: all event processing stops, connections time out, messages lost at protocol level |

## Fix

The core fix: **`toView_` must never block the event processing pipeline on terminal display.**

Options (in order of simplicity):

1. **Make `outputQ` unbounded** — replace `TBQueue` with `TQueue` in `Chat.hs:152`. `writeTQueue` never blocks. Events accumulate in memory under heavy load but the event processing pipeline (including ACKs) is never stalled. Tradeoff: unbounded memory growth under sustained heavy load.

2. **Non-blocking write with drop** — use `tryWriteTBQueue` in `toView_`. When `outputQ` is full, drop the display event (or a coalesced summary). ACKs and network processing proceed unblocked. Tradeoff: some events not displayed, but none lost at protocol level.

3. **Separate ACK from display** — restructure `withAckMessage` to send ACK immediately after DB save, before `toView`. This decouples protocol correctness from display. `toView` can still block, but ACKs are always timely. Tradeoff: requires careful restructuring of the message processing path.

4. **Increase queue capacity** — increase `tbqSize` from 1024 to a larger value. Delays the problem but doesn't fix it. Under sustained heavy load, any finite queue eventually fills.
