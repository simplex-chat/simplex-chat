# Fix desktop UI freeze caused by clipboard polling; remove dead `clipboardHasText` state

## Problem

User report: the Linux desktop app becomes extremely slow — every scroll or button press takes ~9 seconds. The lag disappears while the clipboard is filled and returns when it is emptied, reproducibly triggered by KeePassXC's clipboard auto-clear (10s safety timeout). Manual clearing via desktop tools does not trigger it.

## Root cause

`SetupClipboardListener()` (desktop actual, `common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/Utils.desktop.kt`) ran `chatModel.clipboardHasText.value = clipboard.hasText()` once at composition and then every 1 second in a `LaunchedEffect` — i.e. on the Compose main dispatcher, which on desktop is the AWT Event Dispatch Thread that processes all input and rendering.

The call chain of `hasText()` (verified in Compose 1.8.2 / skiko 0.9.4 bytecode):

```
PlatformClipboardManager.hasText()
  → org.jetbrains.skiko.ClipboardManager_hasText()
  → ClipboardManager_getText()
  → java.awt.datatransfer.Clipboard.getData(DataFlavor.stringFlavor)
```

So "does the clipboard have text" was answered by fetching the full clipboard contents over an X11 selection conversion, every second. On X11, `getData` blocks in `sun.awt.X11.XSelection.waitForSelectionNotify()` until the selection owner replies or `sun.awt.datatransfer.timeout` expires — **default 10 000 ms** (OpenJDK `UNIXToolkit.getDatatransferTimeout()`).

When the X11 CLIPBOARD selection owner does not answer conversion requests — the state KeePassXC's auto-clear leaves behind — each poll blocked the EDT for the full 10 s, then the loop slept 1 s and blocked again. The EDT was therefore blocked ~10 of every ~11 seconds; a random click or scroll waited ~9 s on average, exactly as reported. Any responsive clipboard owner (any normal app copying text) made the polls instant again, which is why filling the clipboard "cured" it.

## Why deletion (not a workaround) is correct

`chatModel.clipboardHasText` has had **zero readers since July 2024**: its only ever consumer — a conditional paste icon over the chat list search field — was removed in #4398 (commit `3e623684b`). Everything else was write-only plumbing for that state:

- desktop 1s poll (`Utils.desktop.kt`) — the freeze source;
- Android `addPrimaryClipChangedListener` (`Utils.android.kt`, added in #3529);
- Android `onResume` re-read of `hasPrimaryClip()` (`MainActivity.kt`, #3758) — a workaround for Android 10+ denying clipboard access to backgrounded apps, needed only to keep the state fresh for that same paste icon;
- the `expect`/`actual` declarations and the `App.kt` call site;
- the `ChatModel.clipboardHasText` field itself.

Verified before removal:

- No direct reads anywhere in the repo (all source sets, all modules), on any remote branch, and none in any upstream PR (GitHub code search: only the writers + one spec table row).
- No indirect access: no reflection over `ChatModel`, no `"clipboardHasText"` string literal, no wholesale snapshot observation, no serialization (`ChatModel` is `@Stable`, not `@Serializable`), nothing crosses the mobile↔desktop remote protocol, no Gradle/lint/ProGuard/codegen references, no tests.
- Removing the `App.kt` call cannot affect sibling composition: the composable emits no UI node (Android: bare `DisposableEffect`; desktop: state writes only), and there is no `key()`/`movableContentOf`/positional logic nearby.
- Android side effects: none. `hasPrimaryClip()` does not read clip contents (no Android 12+ access toast); this was the only primary-clip listener (no ordering concerns); the `onResume` coroutine `Job` was never captured or awaited.
- All remaining paste features are unaffected because they read the clipboard directly at the moment of user action, not via this state: "tap to paste link" (`NewChatView.kt`), Ctrl+V in the composer (`PlatformTextField.desktop.kt`), "paste desktop address" (`ConnectDesktopView.kt`), migration link paste (`MigrateToDevice.kt`).

After this change the desktop app touches the X11 clipboard only on explicit user paste, so no background clipboard state can stall the UI. (A paste attempt while the selection owner is unresponsive can still block once, up to the AWT timeout — inherent to AWT's synchronous X11 clipboard, out of scope.)

## Change

Delete the dead mechanism end to end:

- `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Utils.kt` — `expect fun SetupClipboardListener()`
- `common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/Utils.desktop.kt` — desktop actual (the polling loop) + unused imports
- `common/src/androidMain/kotlin/chat/simplex/common/views/helpers/Utils.android.kt` — Android actual (clip listener) + unused imports
- `common/src/commonMain/kotlin/chat/simplex/common/App.kt` — call site
- `common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt` — `clipboardHasText` field
- `android/src/main/java/chat/simplex/app/MainActivity.kt` — `onResume` clipboard block + unused imports
- `spec/state.md` — stale table row
- `spec/client/navigation.md` — stale "sets up clipboard listener" step in the MainScreen description

## Verification

- `:common:compileKotlinDesktop`, `:common:compileReleaseKotlinAndroid`, `:android:compileReleaseKotlin` pass.
- Repo-wide grep for `clipboardHasText`/`SetupClipboardListener` returns no references outside this document.
- Linux AppImage and Android APK built from this branch.
