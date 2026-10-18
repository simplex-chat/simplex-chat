# Desktop: unread count in the window title

## What

The desktop app's main window title changes from `SimpleX` to `SimpleX [N]`
when there are unread messages the user would be notified about; the suffix
disappears when `N` is 0.

The window title shows in the window's own title bar, in Windows and
Linux taskbars and window switchers that show window titles, and in macOS
Mission Control (the macOS Dock and Cmd-Tab show the app name).

Scope: the main window only. File-chooser dialogs and the developer Terminal
window keep their own titles.

## Why

Desktop already has OS notifications (they disappear after seconds) and, for
users who chose "minimize to tray", the tray icon (a dot; the count is in a
hover tooltip). Neither gives a glanceable count to a user working in another
window.

## Counting rules

`N` = active-profile count + other-profiles count.

**Active profile** — sum over its loaded chats, by each chat's `enableNtfs`:

| `enableNtfs` | counted |
|---|---|
| `All` | `chatStats.unreadCount` |
| `Mentions` | `chatStats.unreadMentions`, at most `chatStats.unreadCount` |
| `None` | 0 |

**Other profiles** — each non-active profile that is not hidden and not
profile-muted (`showNtfs`) contributes its `UserInfo.unreadCount` (the core
computes it with chat mute applied; the app keeps it up to date for chats that
notify).

Not counted: hidden non-active profiles, profile-muted non-active profiles,
fully muted chats, and the `unreadChat` flag (chats marked unread with no
unread messages). Like the chat list, the active profile's part covers only
the loaded chats (up to 5000), and while the app is switched to a connected
mobile the profiles are that mobile's.

The mentions count is capped by the unread count because the app lowers
`unreadCount` but not `unreadMentions` in `decreaseCounterInPrimaryContext`
(e.g. when an unread mention is deleted); without the cap the title would
keep counting a mention after the chat has no unread messages, until the
chat is opened or the chats are reloaded (with other unread messages left,
the stale mention still counts, as the chat list's @ badge shows it). The model drift itself is a separate bug.

Unread messages in member support chats ("Chat with admins") are not
counted for the active profile (the chat list's unread badge excludes
them; the row shows a flag icon instead), except
messages arriving while the user is a pending member, which are counted
until the chat is opened or the chats are reloaded. By the code, other profiles'
`UserInfo.unreadCount` includes them (the core's per-profile count has no
support-scope filter), so the total could change when switching
profiles; not verified by a test.

## Change

One file, `DesktopApp.kt`: the `Window` composable's `title` (was
`"SimpleX"`) is derived from the count in a `derivedStateOf` over
`ChatModel.chats` and `ChatModel.users`. Compose passes it to the AWT
`Frame` title; no native code. The title notifies observers only when the
total changes.

The two parts of the count come from different sources because the active
profile's `users[active].unreadCount` goes up on every unread message
in a loaded chat (outside support chats, unless the user is a pending
member) regardless of chat mute; the per-chat walk applies each chat's mode.

While switching profiles, `users` is updated before the new profile's chats
are loaded, so for that moment the old profile's chats are counted as active
and (unless it is hidden or muted) again as another profile, and the new
profile's are not counted.

## Privacy

The title is visible in the window's title bar and readable by other
software (any X11 client, other processes on Windows, screen-share window
pickers, activity trackers), and it is shown
while SimpleX Lock has locked the app. The count, not the content, is
exposed; whether that needs a setting or should be hidden while locked is a
maintainer decision.

## Out of scope

- macOS Dock badge — separate API.
- Tray icon / tooltip logic — left unchanged, so the tooltip can show a
  different number for the active profile.
- A setting to turn it off.
- Per-platform format variants — `SimpleX [N]` everywhere.
