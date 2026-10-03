# Desktop taskbar unread count — implementation plan

Companion to `2026-05-18-desktop-taskbar-unread-count-spec.md`.

## Change

One file: `DesktopApp.kt`. The `Window` composable's `title` parameter (was the
literal `"SimpleX"`) becomes a value derived from the unread count. Compose
propagates it to `ComposeWindow` / AWT `Frame.title` — the OS taskbar /
window-list title on every platform. No native glue.

## Code

In `AppWindow`, above the `Window(...)` call:

```kotlin
val unreadTotal by remember {
  derivedStateOf {
    val active = ChatModel.chats.value.sumOf { c ->
      when (c.chatInfo.chatSettings?.enableNtfs) {
        MsgFilter.All -> c.chatStats.unreadCount
        MsgFilter.Mentions -> c.chatStats.unreadMentions
        else -> 0
      }
    }
    val others = ChatModel.users.sumOf { u ->
      if (!u.user.activeUser && !u.user.hidden && u.user.showNtfs) u.unreadCount else 0
    }
    active + others
  }
}
val windowTitle = if (unreadTotal > 0) "SimpleX [$unreadTotal]" else "SimpleX"
```

`derivedStateOf` recomputes only when `ChatModel.chats`/`users` change and the
total actually differs. `MsgFilter` comes from the existing wildcard import.

## Why two paths

- **Active profile** — walk `ChatModel.chats` (only the active profile's chats
  are loaded) and apply per-chat mute at compute time. `users[active].unreadCount`
  is not reliable here — the active-profile increment is not mute-gated.
- **Other profiles** — use `UserInfo.unreadCount`; the core's SQL and the event
  dispatcher both keep it mute-filtered.

## Test plan

Manual, per platform — verify by reading the taskbar / window-list entry:

- No unread → `SimpleX`. Receive unmuted message → `SimpleX [1]`, then `[2]`.
- Read the chat → `SimpleX`.
- Mute a chat with unread → count drops by that chat's unread.
- `Mentions` mode → only mentions count.
- Second non-muted profile receives a message → count increments.
- Muted or hidden profile receives a message → no change.
- File-chooser dialog title stays `SimpleX`.
