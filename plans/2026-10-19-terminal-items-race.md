# Chat console log: concurrent writes crash and lose rows

Issue #7755.

## Problem

`ChatModel.addTerminalItem(item, maxItems)` reads `terminalItems.value` for the size check, twice on the `subList` line (the list and its size), and again for the append, without a lock. While no chat console is open, it runs on the caller's thread: `sendCmd` on the IO dispatcher, the receiver loop, and the migration receivers, several at once. (With a console open, writes are posted to the main thread.)

Two concurrent calls can:
- crash with `IndexOutOfBoundsException: toIndex = 201` in `subList`: another thread appended between the two reads of `terminalItems.value` in the `subList` line;
- lose a row: both read the same list and the second write replaces the first;
- keep the list above the cap.

When it happens in `sendCmd`, the exception is not caught there: on Android it shows "View crashed", and the command is not sent (when logging the command) or its result is lost (when logging the response). On the receiver and migration paths it is caught and logged, and the received event is dropped.

## Fix

`@Synchronized` on the private `addTerminalItem(item, maxItems)`, which is the only writer of `terminalItems`. Every write is then a single read-trim-append, on whichever thread it runs; the console still does not hop to the main thread while closed (#4992).

## Alternatives considered

- The issue's diff (a lock field plus a single snapshot trimmed with `drop`): also correct, but it adds a field and changes trimming: after developer tools are turned off (cap 500 → 200) it cuts the log to 200 at once, where master keeps the log at that size (trimming one row per new row) until the app restarts.
- Always posting the write to the main thread: correct, but every command and event would pay a coroutine and main-thread post while the console is closed, during the event bursts that trigger this.
- A lock-free compare-and-set or `MutableStateFlow.update`: no precedent in this code, changes the reader or the type.
- Clamping the `subList` bounds: stops the crash but still loses rows.

## Not included

- iOS keeps the log in an actor, so it has no such race; its rows can appear out of order because they are logged from unstructured tasks (cosmetic).
- #7415 (console row keys) touches other files; together they fix the duplicate-key crash, this crash and the lost rows.

## Testing

To be done before the PR is opened, and replaced here with what was observed: a JVM harness against the built desktop jars (not committed), where several threads call `addTerminalItem` with the cap at 200, on master and on this fix; desktop GUI regression of the chat console (commands, responses, rows added while it is closed, the cap, the console window); Android smoke test.
