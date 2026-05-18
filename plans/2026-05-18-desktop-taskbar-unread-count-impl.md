# Desktop taskbar unread count — implementation plan

Companion to `2026-05-18-desktop-taskbar-unread-count-spec.md`. The spec defines what the user sees and why. This plan defines what the diff looks like.

## Surface

`DesktopApp.kt:124-130`:

```kotlin
Window(state = windowState,
       visible = simplexWindowState.windowVisible.value,
       icon = painterResource(MR.images.ic_simplex),
       onCloseRequest = { handleCloseRequest(closedByError) },
       onKeyEvent = { ... },
       title = "SimpleX") {
```

Compose Multiplatform's `androidx.compose.ui.window.Window` is a composable. Its `title: String` parameter, when it changes between recompositions, propagates to the backing `ComposeWindow.title`, which is an AWT `Frame.title` — and `Frame.setTitle` is the canonical mechanism every JVM uses to update the OS taskbar / window-list entry on Linux, Windows, and macOS. No new platform glue, no JNI, no native bridge.

The change is local to that one parameter. Everything else in `AppWindow` stays.

## Counting logic — one composable, two paths

In `AppWindow` (`DesktopApp.kt`), introduce a derived integer that re-evaluates whenever the inputs change, then format the title from it:

```kotlin
val unreadTotal by remember {
  derivedStateOf {
    // Active profile — apply per-chat mute filter exactly, the same rule
    // that drives Chat.unreadTag (ChatModel.kt:1383-1387).
    val active = ChatModel.chats.value.sumOf { c ->
      when (c.chatInfo.chatSettings?.enableNtfs) {
        MsgFilter.All -> c.chatStats.unreadCount
        MsgFilter.Mentions -> c.chatStats.unreadMentions
        else -> 0  // MsgFilter.None or null (no settings — treated as muted)
      }
    }
    // Other profiles — non-active, not hidden, not profile-level-muted.
    // Same non-active-profile filter the tray uses (DesktopTray.kt:62-68) plus
    // a hidden-profile skip to match the profile picker (UserPicker.kt:60).
    val others = ChatModel.users.sumOf { u ->
      if (!u.user.activeUser && !u.user.hidden && u.user.showNtfs) u.unreadCount else 0
    }
    active + others
  }
}
val title = if (unreadTotal > 0) "SimpleX [$unreadTotal]" else "SimpleX"
```

Then pass `title = title` into `Window(...)`. The bracketed format is hard-coded — no string resource, because the literal characters `[` `]` and ASCII digits do not localize. The plain "SimpleX" branch matches the existing hard-coded literal.

### Why the asymmetry between active and others

`ChatModel.chats` proxies `chatsContext.chats`, which always holds whichever profile is currently active — non-active profiles do not have their chats loaded into the desktop client. For them we use `UserInfo.unreadCount`, which is mute-filtered at every mutation point: the SQL in `getUserInfo` (`src/Simplex/Chat/Store/Profiles.hs:175-196`) filters by `enable_ntfs` on load, and the Kotlin event dispatcher gates the real-time increment at `SimpleXAPI.kt:2786-2789` on `cInfo.ntfsEnabled(cItem)` — the same per-chat predicate. So `users[non-active].unreadCount` is always equal to "messages this user wants to be notified about," with no `listUsers` round-trip required after each new message.

The active profile takes a different real-time path: the dispatcher's `if (active(r.user))` branch (`SimpleXAPI.kt:2778-2785`) calls `addChatItem`, and `addChatItem` in turn calls `increaseUnreadCounter(rhId, currentUser.value!!)` at `ChatModel.kt:559-562` with **no** `ntfsEnabled` gate. Every `RcvNew` item — including ones in muted chats — bumps `users[active].unreadCount` by 1, and the inflation is only reconciled when the next `listUsers` refresh runs the mute-filtered SQL again. The per-chat walk in the taskbar derivation bypasses that drift entirely: it reads `chatStats.unreadCount`/`unreadMentions` (raw per-chat counters that are accurate by construction) and applies the `enableNtfs` filter at compute time, producing a number that is correct under both new messages and mute toggles.

### Why `derivedStateOf`

Both `ChatModel.users` (a `SnapshotStateList<UserInfo>` from `mutableStateListOf`) and `ChatModel.chats.value` (a `SnapshotStateList<Chat>` reached via a `State<List<Chat>>` wrapper) are observable Compose state. `derivedStateOf` re-runs the lambda whenever any read of those snapshots is invalidated, and only emits a new value when the computed integer actually differs — so individual chat-list edits that don't shift the total (e.g. a message arriving in a fully-muted chat) re-run the lambda but do not cause the title to recompose. The `remember { ... }` keeps the derivation alive across recompositions.

### Reading `chats` from a composable

`ChatModel.chats` is declared as `val chats: State<List<Chat>> = chatsContext.chats` (`ChatModel.kt:134`). `chatsContext` is itself a `val` (not reassigned). On profile switch and on `startChat`'s "running" branch, `ChatsContext.updateChats` (`ChatModel.kt:468-488`; `SimpleXAPI.kt:585-587`) calls the `replaceAll` extension (`ChatModel.kt:3329`), which constructs a fresh `SnapshotStateList<Chat>` and assigns it to `chats.value`. `derivedStateOf` reads `ChatModel.chats.value` each time it recomputes, so it always observes the active profile's current chats — both the new-list assignment and per-chat edits (like `chats[i] = chat.copy(...)` at `ChatModel.kt:556`) invalidate the derivation.

## Files changed

| File | Change |
|---|---|
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt` | Replace `title = "SimpleX"` (line 130) with a `title = title` parameter computed from a `derivedStateOf { … }` block immediately above the `Window(…)` call. `MsgFilter` is reachable via the existing wildcard `import chat.simplex.common.model.*` (line 16); no new import is needed because we qualify the enum constants (`MsgFilter.All`, etc.). |

That is the only file changed. The two file-chooser dialog titles (`DesktopApp.kt:136, 147, 158`) and the developer Terminal window title (line 213) remain as-is per the spec's scope.

No new resources, no new strings, no `build.gradle.kts` change, no `commonMain` change, no Android-side change.

## Specific things to watch

- **Title at startup.** Before the active profile's chats load, `ChatModel.chats.value` is empty and the active-profile contribution is 0. If `ChatModel.users` is already populated (via `listUsers`), the title reflects only the non-active-profiles sum during that window; when the active chats arrive, the title updates to include the active-profile contribution. If neither is populated yet, the title is plain "SimpleX". Each population is a `replaceAll`/`addAll` invocation followed by one Compose frame; `derivedStateOf` re-computes once per frame, so intermediate empty states are not user-observable.
- **Profile switch transient.** `changeActiveUser_` (`SimpleXAPI.kt:643-661`) writes the new currentUser, then refreshes `ChatModel.users` via `listUsers` (lines 653-655), then awaits `apiGetChats` for the new profile, and finally calls `updateChats` which `replaceAll`s `chats.value`. There is a brief window between the `users` refresh and the chats replacement (~one round-trip to the core) where `chats.value` still holds the OLD profile's chats but `users[OLD].activeUser` has already flipped to `false` — so the OLD profile contributes once via `active` (its old chat list) and once via `others` (`users[OLD].unreadCount`, just refreshed). The title can transiently appear inflated during a profile switch and settles correctly once `updateChats` runs. Reviewer-visible behavior, but invisible to users in normal use; aligning the refresh order (load new chats before flipping the users list) is out of scope for this PR.
- **Chat controller stopped.** When `chatRunning.value == false`, no new messages arrive; the existing model state is shown verbatim. The title does not get a "stopped" decoration — that is the responsibility of the in-app stopped banner, not the taskbar.
- **Programmatic title overwrites.** Nothing else in the desktop code writes to the main `ComposeWindow`'s title. Grep on `\.title = ` inside `desktopMain` returns exactly one hit — `DefaultDialog.desktop.kt:148` setting `this.title = title` on a `FileDialog` instance (a separate transient AWT window, not the main window) — so there is no risk of an out-of-band write racing with the composable.
- **No interaction with the tray code.** `DesktopTray.kt`'s `unread` derivation is independent and continues to use its existing rule (sum `UserInfo.unreadCount`, skip non-active muted profiles). Both surfaces read `ChatModel.users` and recompute on the same edits; no shared mutable state.

## Why not share a helper with the tray

The two surfaces produce the same total in steady state but differ in two transient places:

1. **Active profile, between `listUsers` refreshes.** The tray uses `UserInfo.unreadCount` for the active profile too — the same in-memory aggregate that the increment at `ChatModel.kt:560` drifts upward on every received item regardless of `enable_ntfs`. The taskbar walks `ChatModel.chats` and gates each chat's contribution on `enableNtfs` at compute time, so a mute toggle or a message in a muted chat is reflected immediately rather than at the next `listUsers` round-trip.
2. **Non-active hidden profiles, on paper.** The tray's lambda is `if (!showNtfs && !activeUser) 0 else unreadCount` — no explicit `User.hidden` filter. The taskbar adds one. The core enforces "hidden user always muted when inactive" (`CEHiddenUserAlwaysMuted`; `View.hs:333` matches on `(showNtfs && isNothing viewPwdHash) || activeUser`), so the tray's `!showNtfs` already covers every non-active hidden profile and the observed counts coincide. The taskbar's `!hidden` is a redundant-but-explicit safety belt: it documents the predicate at the call site rather than relying on the reader to know the core invariant.

Two independent rule differences. Extracting a shared helper today would either parameterize over both — which is just two functions sitting in the same file — or force one of the surfaces to adopt the other's rule, which is a deliberate product decision that belongs in its own PR (see spec, "Out of scope: Updating the tray-icon tooltip / tray-dot logic"). The taskbar derivation lives next to its consumer in `DesktopApp.kt` for now.

## Diff shape

- One file changed.
- ~15 lines added (the `derivedStateOf` block + the title-format expression).
- 1 line modified (the `title = "SimpleX"` parameter).
- 0 lines removed.

This is a behavior-change commit: one logical addition, one surface, one file. It is reviewable in under three minutes.

## Test plan

Manual verification on each platform (Linux KDE Plasma, Windows 11, macOS) using a single profile and then a multi-profile setup. The taskbar / window-list entry is the visual surface — verify by reading it.

1. **Fresh install, no unread.** App opens. Title bar reads "SimpleX". Taskbar / dock entry reads "SimpleX".
2. **Receive an unmuted message.** Title transitions to "SimpleX [1]" as soon as the message arrives, both in the title bar and in the taskbar / window-list / Alt-Tab switcher entry. Receive a second: "SimpleX [2]".
3. **Read the chat.** Title returns to "SimpleX".
4. **Mute a chat with unread.** Mute the chat (`All` → `None`) while it has 3 unread messages. Title decrements by 3.
5. **Partially-mute (Mentions).** Chat has 5 unread, 1 of which mentions you. Set `All` → `Mentions`. Title goes from "SimpleX [5]" to "SimpleX [1]". Reply to the mention. Title returns to "SimpleX". Receive a regular message (not a mention): title stays "SimpleX". Receive a mention/reply: "SimpleX [1]".
6. **Mark unread (`unreadChat` only, zero items).** Long-press an empty chat → Mark unread. Title does *not* change. The chat-list shows the unread indicator but the taskbar suffix does not increment (per spec, "Skipped categories").
7. **Second profile, unmuted.** Add Profile B. Switch back to Profile A. Send a message from another device to Profile B (to a non-muted chat). Title in Profile A's window transitions to "SimpleX [1]" immediately — `users[B].unreadCount` is incremented in real time at `SimpleXAPI.kt:2786-2789` when `cInfo.ntfsEnabled(cItem)` passes. Send a second message: "SimpleX [2]". Switch to Profile B, read the chat. Title returns to "SimpleX".
8. **Second profile, muted.** Set Profile B's `showNtfs = false` (the profile-level mute, the muted-icon row in the picker). With Profile A active, receive a message to B. Title does **not** change. The profile picker still shows the badge in B's row (with the muted-color), but the taskbar — like the tray icon — is silent for muted non-active profiles. Re-enable B's notifications (`showNtfs = true`); title transitions to "SimpleX [1]" without any new message.
9. **Hidden profile.** Hide Profile B (set a passphrase). With Profile A active, receive a message to B. Title does *not* change. The profile picker also does not show B.
10. **Switch active profile.** With unread in both A (3, active, unmuted) and B (2, non-active, not hidden), title reads "SimpleX [5]". Switch to B. Title reads "SimpleX [5]" (now via B-active-path + A-non-active-path).
11. **File chooser dialog.** Trigger a file dialog (send a file). The dialog's title bar reads "SimpleX" (no count), per scope. The main window's title bar still reads "SimpleX [N]" if applicable.
12. **Stop the chat controller.** Settings → stop chat. The title shows whatever the model currently has; no count change from the stop itself.

The Linux test should be done on at least one non-GNOME desktop (Plasma / XFCE / Cinnamon) to verify the taskbar/window-list entry — stock GNOME's overview is the same `Frame.title` surface as the rest, so it works without the tray probe.
