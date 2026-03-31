# CLI paste slowness: root cause analysis

## Problem

simplex-chat CLI is slow when copy-pasting text to the terminal.

## Root cause

The input loop processes exactly one key event per iteration and performs a full terminal redraw after each, with no mechanism to batch pending events.

`src/Simplex/Chat/Terminal/Input.hs:161`:
```haskell
forever $ getKey >>= liftIO . processKey >> withTermLock ct (updateInput ct)
```

For every single key event the loop:
1. Reads ONE event from a capacity-1 `TMVar` channel
2. Processes it (updates `TerminalState`)
3. Performs a full terminal redraw (`updateInput`) -- hide cursor, set position, rewrite entire input string char by char, erase, restore cursor, show cursor, flush

When pasting N characters, the loop runs N times, each time redrawing the growing input string. This is **O(N^2)** total work.

## Redraw path

Each `updateInput` call (`Output.hs:308-327`) does:
```haskell
hideCursor
setCursorPosition ...
putStyled $ Styled [SetColor Foreground Dull White] acPfx
putString $ prompt <> inputString ts <> " "
eraseInLine EraseForward
setCursorPosition ...
showCursor
flush
```

`putString` (`TerminalT.hs:67-68`) writes each character individually:
```haskell
putString cs = forM_ cs (command . T.PutText . Text.singleton)
```

Each character generates a separate `Text.hPutStr IO.stdout` call via `termCommand`. For an input string of length K, that's K handle lock/unlock cycles and K `Text.singleton` allocations per redraw.

Since the input grows from 1 to N characters over N redraws, total characters rendered: **N(N+1)/2**.

## Contributing factors

### 1. TMVar event channel has capacity 1

`Platform.hsc:64`:
```haskell
events <- liftIO newEmptyTMVarIO
```

The input reader thread blocks after producing each event until the consumer finishes its full redraw cycle. Producer and consumer are in strict lock-step with no pipelining.

### 2. Double events for common characters

The decoder (`Decoder.hs:20-35`) combined with `specialChar` (`Platform.hsc:102-110`) produces TWO events for space, tab, newline, backspace, and delete:

- Space: `CharKey ' '` + `SpaceKey`
- Tab: `CharKey 'I' ctrlKey` + `TabKey`
- Newline: `CharKey 'J' ctrlKey` + `EnterKey`

The first event is typically a no-op in `updateTermState` (ctrl-modified CharKey falls to `otherwise -> pure ts`), but still triggers a full `updateInput` redraw.

Every space in pasted text = 2 full redraws instead of 1.

### 3. Tab triggers blocking DB queries

If pasted text contains tab characters, each `TabKey` event runs autocomplete SQL queries synchronously (`Input.hs:237-246`):
```haskell
TabKey -> do
  (pfx, vs) <- autoCompleteVariants user_  -- DB queries here
```

`autoCompleteVariants` calls `getNameSfxs_` (`Input.hs:328-330`) which runs `withTransaction` against the chat database, blocking the entire input loop.

### 4. No bracketed paste mode

The terminal library does not enable bracketed paste mode (`ESC[?2004h`). If the terminal emulator sends bracket sequences (`ESC[200~` / `ESC[201~`), the decoder silently consumes them with no events. The application has no way to detect paste and handle it as a batch.

## Impact estimate for paste of 500 chars (~20% spaces)

| Metric | Value |
|--------|-------|
| Loop iterations | ~600 (500 chars + ~100 extra SpaceKey events) |
| Total chars written to terminal | ~150,000 (O(N^2)) |
| `hFlush` syscalls | ~600 |
| `Text.singleton` allocations | ~150,000 |
| Handle lock/unlock cycles | ~150,000 |

## Fix

Batch pending events before redrawing: drain all available events from the channel, apply all state updates, then redraw once. This turns O(N^2) paste into O(N).

Concretely:
1. Replace `TMVar` with `TBQueue` (or use `tryTakeTMVar` loop) to allow reading multiple pending events
2. After `getKey`, loop with `tryTakeTMVar` to collect all pending events before calling `updateInput`
3. Apply all collected events to `TerminalState` in sequence, then redraw once

Optional further improvements:
- Use `putText` instead of `putString` in `updateInput` to avoid per-character `Text.singleton` overhead -- `putText` sends the whole `Text` as a single `PutText` command
- Filter out no-op events (e.g. `SpaceKey`, ctrl-modified CharKeys that fall through to `pure ts`) before they trigger redraws
- Enable bracketed paste mode to detect paste and handle it as a single insert operation
