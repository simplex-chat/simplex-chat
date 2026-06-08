# Desktop taskbar unread count — product spec

## What

The desktop app's OS window/taskbar title changes from `SimpleX` to `SimpleX [N]`
when there are unread, non-muted messages. `N` is the total across all profiles,
mute-filtered; the suffix disappears when `N` is 0.

The title shows in the taskbar / dock / window-list / Alt-Tab on Linux, Windows,
and macOS — visible from any workspace at zero attention cost.

Scope: the main window only. File-chooser dialogs and the developer Terminal
window keep their own titles.

## Why

Desktop already has OS toasts (disappear after seconds) and the tray icon (binary
dot; count hidden in a hover tooltip). Neither gives a glanceable count to a user
working in another window. The taskbar title is the canonical place for it.

## Counting rules

`N` = active-profile count + other-profiles count.

**Active profile** — sum over its chats, by each chat's `enableNtfs`:

| `enableNtfs` | counted |
|---|---|
| `All` | `chatStats.unreadCount` |
| `Mentions` | `chatStats.unreadMentions` |
| `None` | 0 |

**Other profiles** — each non-active profile that is not hidden and not
profile-muted (`showNtfs`) contributes its `UserInfo.unreadCount`, which the core
already keeps mute-filtered.

Not counted: hidden profiles, profile-muted non-active profiles, fully-muted
chats, and the `unreadChat` flag (marked-unread chats with no messages).

## Out of scope

- macOS Dock badge — separate API, own PR.
- Tray icon / tooltip logic — left unchanged.
- A setting to disable it — muting chats already removes the count.
- Per-platform format variants — `SimpleX [N]` everywhere.
