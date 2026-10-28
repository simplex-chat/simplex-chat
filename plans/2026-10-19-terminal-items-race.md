# Chat console log: concurrent writes throw and lose rows

Issue #7755.

## Problem

When several rows are written to the chat console log at once while no console is open, the app can lose a row, leave the log above its cap, or, once the log is at its cap (200 rows, 500 with developer tools), throw `IndexOutOfBoundsException` (for example `toIndex = 201`). In `sendCmd` the exception is not caught: the command is not sent, or its response is lost, and the uncaught exception shows "View crashed" on Android; on desktop it shows the same alert or is only printed to stderr, depending on the caller. In the receiver and migration receivers it is caught and logged, and that event is dropped.

## Cause

The private `ChatModel.addTerminalItem(item, maxItems)` is the only writer of `terminalItems` in the app. It reads `terminalItems.value` four times without a lock: for the size check, twice on the `subList` line, and for the append. It posts the write to the main thread only while a console is shown (#4992); otherwise it runs on the caller's thread, and those threads run at the same time: `sendCmd` on the IO dispatcher, the receiver loop and the two migration receivers. So:
- another thread appending between the two reads on the `subList` line makes `toIndex` larger than the list it is applied to;
- two writers that read the same list each set their own new list, and one row is lost;
- two writers that both read the log size as one below its cap both append without trimming.

## Fix

`@Synchronized` on the private `addTerminalItem(item, maxItems)`. Each write is then one read-trim-append, on whichever thread it runs. The lock is the `ChatModel` object, which no other code locks. The locked code copies at most 500 references and performs at most two Compose state writes, with no I/O and no suspension.

## Approaches considered

- The issue's diff (a lock field and one snapshot trimmed with `drop`): also correct, but it adds a field and changes trimming: after developer tools are turned off it cuts the log from 500 to 200 rows at once, while today the log stays at 500 rows, dropping the oldest row for each new one, until the app restarts.
- Always posting writes to the main thread: every command and event would add a main-thread post while the console is closed, including during the bursts that trigger the race.
- A lock-free update (a compare-and-set loop, or `terminalItems` as an `AtomicReference` or `MutableStateFlow`): more code, and the last two change its type and its reader in `TerminalView.kt`.
- Clamping the `subList` bounds: no exception, but rows are still lost.

## Scope

Android and desktop (common code). Every call of the public `addTerminalItem` calls the locked function. The iOS app stores its log in an actor and has no such race. Other `ChatModel` state written from several threads without a lock is not changed here.
