# Chat console crash on duplicate list keys

## Problem

The chat console (Android, desktop) crashes with `IllegalArgumentException: Key "(…, …)" was already used`. Its rows are keyed by `item.id to item.createdAtNanos`, i.e. the creation time in milliseconds and `System.nanoTime()`. Terminal items are created on several threads: commands in `sendCmd` on the IO dispatcher, received events in the receiver coroutine (also on the IO dispatcher), and items created on the main thread. Two items created within one clock tick get the same key — on different threads, or on one thread when the clock is coarse (a command and its response are created back to back) — and the next measure pass throws. On desktop this shows the "View crashed" alert and closes the console.

`nanoTime` never goes backwards but is not unique; how often ties happen depends on the clock's granularity (finer on Linux x86 than on Windows or ARM). #4812 added this key (with `createdAtNanos`); it also keeps rows in place while new items arrive.

## Fix

Key the rows by a per-item number from a shared `AtomicLong`, assigned when the item is created. The generator is declared before `sampleData` in the companion object, so the sample items read an initialised generator. `createdAtNanos` had no other reader and is removed.

## Alternatives considered

- Reuse `id` as the counter: items constructed directly (e.g. `sampleData`) would bypass it, and `id` would stop meaning the creation time.
- No key: no crash, but when scrolled up, new items shift the visible rows (which #4812's key also prevents).
- Number items when they are added to the list: the duplicate values come from creation, so this still needs an atomic counter and is larger.
- Stronger time-based keys (thread id, identity hash, UUID): uniqueness by chance, or the same counter at higher cost.

## Not included

- iOS keys console rows by a `Date` too; duplicates there do not crash.
- `ChatModel.addTerminalItem` updates the list from several threads without a lock, so an item can be lost; it cannot create duplicate keys.

## Testing

Desktop AppImage, master vs this PR. A duplicate needs two items created within one clock tick, so the repro runs the app with a small `LD_PRELOAD` library that rounds the monotonic clock to 1 ms: on master, a few `/sql chat select 1` commands in the chat console show "View crashed"; the PR build shows the rows. Regression with the normal clock on both builds: autoscroll, scroll position kept while scrolled up, row details and Share, the row cap (200, or 500 with developer tools), rows added while the console is closed, the console in its own window. Android APK: smoke test of the console.
