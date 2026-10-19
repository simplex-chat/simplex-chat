# Chat console log: concurrent writes crash and lose rows

Issue #7755.

## Problem

`ChatModel.addTerminalItem(item, maxItems)` reads `terminalItems.value` for the size check, twice on the `subList` line (the list and its size), and again for the append, without a lock. While no chat console is open, it runs on the caller's thread: `sendCmd` on the IO dispatcher, the receiver loop, and the migration receivers, several at once. (With a console open, writes are posted to the main thread.)

Two concurrent calls can:
- crash with `IndexOutOfBoundsException: toIndex = 201` in `subList`: another thread appended between the two reads of `terminalItems.value` in the `subList` line;
- lose a row: both read the same list and the second write replaces the first;
- keep the list above the cap.

When it happens in `sendCmd`, the exception is not caught there, and most callers don't catch it either: on Android it then shows "View crashed", and the command is not sent (when logging the command) or its result is lost (when logging the response). On the receiver and migration paths it is caught and logged, and the received event is dropped.

## Fix

`@Synchronized` on the private `addTerminalItem(item, maxItems)`, which is the only writer of `terminalItems`. Every write is then a single read-trim-append, on whichever thread it runs; the console still does not hop to the main thread while closed (#4992).

## Alternatives considered

- The issue's diff (a lock field plus a single snapshot trimmed with `drop`): also correct, but it adds a field and changes trimming: after developer tools are turned off (cap 500 → 200) it cuts the log to 200 at once, where master keeps the log at that size (trimming one row per new row) until the app restarts.
- Always posting the write to the main thread: correct, but every command and event would pay a coroutine and main-thread post while the console is closed, during the event bursts that trigger this.
- A lock-free compare-and-set: either a retry loop over a Compose mutable snapshot's apply, or changing `terminalItems` to an `AtomicReference` or a `MutableStateFlow`, which changes its type and its reader (`TerminalView.kt`); both are more code than one annotation.
- Clamping the `subList` bounds: stops the crash but still loses rows.

## Not included

- iOS keeps the log in an actor, so it has no such race; its rows can appear out of order because they are logged from unstructured tasks (cosmetic).
- #7415 (console row keys) touches other files; together they fix the duplicate-key crash, this crash and the lost rows.

## Testing

- JVM harness (not committed) against the desktop jars, 8 threads calling `addTerminalItem` with the cap at 200: master 3e6fd3836 lost rows in 843 of 2000 runs and threw 742 exceptions (the first: `IndexOutOfBoundsException: toIndex = 201`) in one flood round; this fix lost none in 2000 runs and threw none in 135 flood rounds, ending at exactly 200 rows.
- Desktop GUI, master and this fix, same results: a command and its response, the `/sql` rejection, rows added while the console is closed, trimming of the oldest rows once the log is past its cap, the floating console during incoming messages, opening and closing the console during a flood, sending and receiving messages; no crashes.
