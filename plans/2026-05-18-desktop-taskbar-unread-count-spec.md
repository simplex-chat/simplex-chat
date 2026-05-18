# Desktop taskbar unread count — product spec

## What

When the SimpleX desktop app has unread, non-muted notifications, the operating system's window/taskbar title for the app changes from

> SimpleX

to

> SimpleX [N]

where `N` is the total unread count across the user's profiles, applying each chat's mute setting: a partially-muted chat (`Mentions`) contributes only its mention/reply count, a fully-muted chat (`None`) contributes nothing, an unmuted chat (`All`) contributes its full unread count. The same per-mute-mode dispatch the in-app chat list uses to decide whether each chat shows a badge, applied to produce a number instead of a yes/no. The bracketed suffix disappears when `N` drops to zero — typically when the user reads or mutes the remaining unread chats. See "Counting rules" below for the exact formula.

The visible surface is the OS-provided window title, which appears in:

- Linux: the title bar of the SimpleX window and the per-app entry in the taskbar / dock / window-list of every desktop environment (KDE Plasma, GNOME, XFCE, etc.).
- Windows: the window title bar and the per-app button on the taskbar / task switcher (Alt-Tab).
- macOS: the window title bar and the per-window entry in the Window menu (the Dock badge on macOS is unaffected by window title — that is a separate API, out of scope here).

Scope: the main SimpleX window only. The transient file-chooser dialogs (`DesktopApp.kt:136, 147, 158`) and the optional developer Terminal window (`DesktopApp.kt:213`) keep their own titles unchanged — they are not "the app" and adding a count to them would be misleading.

## Why

We already ship two unread surfaces on desktop:

1. **OS notifications** — toasts via `displayNotificationViaLib` (TwoSlices).
2. **Tray icon** — binary unread dot (`ic_simplex_tray_dot.svg`) plus a numeric tooltip ("SimpleX — N unread") added in commit `3c82c6c91`.

Neither tells a user who is in another window how many messages are waiting. The notification toasts disappear after a few seconds. The tray icon's dot is binary — the count lives only in the tooltip, which requires the user to hover over the tray icon to see it. The window title and taskbar entry, on the other hand, are visible in every workspace, in every window switcher, in the OS's own "what apps want my attention" surface — and they update at zero attention cost from the user.

Adding a numeric suffix to the taskbar gives users the information they already had on the tray icon (the count) on a surface they already glance at (the taskbar / window-list / Cmd-Tab). It is the canonical place to put per-app unread counts on every desktop platform we ship.

The existing tray-tooltip count and this taskbar count come from the same notion of "unread" and agree in steady state. The active-profile path differs in transient states — see "Out of scope: Updating the tray-icon tooltip / tray-dot logic" below.

## How

### Counting rules

The total is the sum of two parts:

**Active profile** — walk its chats; per chat, count according to that chat's notification setting (`ChatSettings.enableNtfs`):

| Chat's `enableNtfs` | What is counted | Source |
|---|---|---|
| `All` (unmuted) | all unread messages | `chatStats.unreadCount` |
| `Mentions` (partially muted) | unread mentions and replies only | `chatStats.unreadMentions` |
| `None` (fully muted) | nothing | 0 |

This parallels the existing per-chat `Chat.unreadTag` rule (`ChatModel.kt:1383-1387`), which decides whether a chat shows an unread badge in the chat list — both dispatch on the same `enableNtfs` cases. The difference: `unreadTag` produces a boolean, the taskbar produces a number; and the taskbar excludes the `unreadChat`-only case that `unreadTag` does include (see "Skipped categories" below). Note that the chat-list badge itself displays the *raw* `chatStats.unreadCount` regardless of mute mode (only the badge color reflects the mute); the taskbar's per-chat contribution is the mute-filtered number described in the table above, not the badge number.

**Other profiles (non-active)** — every non-active profile that is not hidden *and* not profile-level-muted contributes its full `UserInfo.unreadCount`. Profile-level mute (`User.showNtfs == false`) silences a profile entirely from cross-profile surfaces: the profile picker still shows the per-row badge with a different color so the user can see "there is unread over there", but the taskbar — like the existing tray icon (`DesktopTray.kt:62-68`) — does not contribute that profile's unread to the global count. Muting a profile is the user saying "I do not want this surfaced to me right now"; the taskbar honors that the same way the tray does.

Skipped categories:

- **Hidden profiles** (those gated behind a passphrase, `User.hidden == viewPwdHash != null`) — never counted, matching the profile picker's filter `users.filter { u -> u.user.activeUser || !u.user.hidden }`. A hidden profile that the user has unlocked into the *active* slot is no longer "hidden from this surface" and is counted via the active-profile path above (per-chat with mute filter), just like the picker shows it.
- **Profile-level muted non-active profiles** (`!user.activeUser && !user.showNtfs`) — see above. The active profile is always counted regardless of `showNtfs`, because being active overrides the profile-level mute (`UserLike.showNotifications = activeUser || showNtfs`); per-chat mute still applies within it.
- **Fully-muted chats** in the active profile (`enableNtfs == None`) — see table above.
- **The `unreadChat` flag** (manually-marked-unread chats with zero items) — not counted. The taskbar suffix is a *number*, not a state, and a "marked unread, no messages" chat contributes 0 to a numeric sum just as it does in the tray tooltip today.

### Asymmetry between the active profile and the others

For the active profile we have the full chat list in memory; we walk it and apply the per-chat mute filter directly off the live `Chat.chatStats` and `chatInfo.chatSettings`. For non-active profiles we do not have their chats loaded — we use the per-profile aggregate `UserInfo.unreadCount`. Both produce mute-filtered numbers, but via different mechanisms:

1. **Non-active `UserInfo.unreadCount` is mute-filtered at every mutation point.** On load from core, `getUserInfo` (`src/Simplex/Chat/Store/Profiles.hs:175-196`) sums chat-item rows with a SQL `WHERE` that mirrors `MsgFilter` exactly: for direct contacts only `enable_ntfs = 1 OR NULL` (i.e. `All`), for groups only `enable_ntfs = 1 OR NULL OR (enable_ntfs = 2 AND user_mention = 1)` (i.e. `All` or `Mentions`-mode mentions). In real time, the Kotlin event dispatcher at `SimpleXAPI.kt:2786-2789` increments `users[r.user].unreadCount` only when `cInfo.ntfsEnabled(cItem)` passes — the same filter, applied at the call site before the increment runs. So a message arriving for a non-active profile in a `None` chat is silently dropped from the aggregate; one in an `All` chat increments it; one in a `Mentions` chat increments only if it is a mention/reply. The value stays correct without depending on `listUsers`.
2. **It agrees with the tray's non-active rule.** The tray uses `!showNtfs && !activeUser → 0` (no explicit `.hidden` filter); the taskbar uses `!hidden && !activeUser && showNtfs ? unreadCount : 0`. The two formulations are not the same set on paper, but the core enforces "hidden user always muted when inactive" (`src/Simplex/Chat/View.hs:333`, `CEHiddenUserAlwaysMuted`) — a non-active hidden profile always has `showNtfs == false` from the core's perspective. So the tray's `!showNtfs` already absorbs every non-active hidden profile, and the two non-active sets coincide. The taskbar's `!hidden` clause is an explicit safety belt that does not depend on the core invariant being upheld for every code path that ever sets `showNtfs`.

Why walk the active profile's chats instead of using `UserInfo.unreadCount` for it too? Because the *active* profile takes a different real-time path. The active-user branch of the dispatcher (`SimpleXAPI.kt:2778-2785`) calls `addChatItem`, and inside `addChatItem` the call to `increaseUnreadCounter` (`ChatModel.kt:559-562`) fires for every `RcvNew` item with **no** `ntfsEnabled` gate. So `users[active].unreadCount` drifts upward whenever the active profile receives messages in muted chats, and is only reconciled on the next `listUsers` refresh. The per-chat walk avoids the drift: it reads `chatStats.unreadCount` and `unreadMentions` (which track raw chat-item state) and gates them at compute time on the chat's `enableNtfs`, producing a number that is correct under both new messages *and* mute toggles.

### Behavior in observable scenarios

| Event | Effect on title |
|---|---|
| App starts, two unread messages in the active profile's unmuted chat | "SimpleX [2]" appears as soon as the chat list finishes loading. |
| User opens the chat and reads the messages | Title returns to "SimpleX" as `chatStats.unreadCount` decrements. |
| User mutes a chat with unread messages (`All` → `None`) | Those messages stop contributing; title decrements. |
| User partially-mutes a chat (`All` → `Mentions`) with unread non-mention messages and one unread mention | Contribution drops from `unreadCount` to `unreadMentions` (1). |
| A non-active, non-hidden profile receives a message in a non-muted chat | Title increments immediately. The dispatcher at `SimpleXAPI.kt:2786-2789` increments `users[X].unreadCount` in real time when `cInfo.ntfsEnabled(cItem)` passes (the same per-chat mute filter the core uses). For `Mentions`-mode chats only mention/reply items trigger the increment; for `None`-mode chats no increment fires, so the title stays unchanged. |
| A hidden profile receives a message | Title is unchanged. The profile picker also hides this. |
| User switches active profile from A to B | Title recomputes for B's active-profile path and A's non-active path. |
| Chat controller stopped (no live messages) | Title shows whatever the model currently holds; no special "stopped" state. |
| Count exceeds 4 digits ("SimpleX [12345]") | Rendered verbatim. No truncation or "+99". OS may ellipsize in the taskbar; the title bar shows full. |

### Out of scope

- **macOS Dock badge.** That is a separate API (`NSApp.dockTile.badgeLabel`) and a separate UX surface (a red badge on the Dock icon, not the window title). A Dock badge belongs in its own PR — it has different platform constraints, different positioning, and the question of whether it should mirror the tray dot's binary indicator or the taskbar's number is its own design decision.
- **Updating the tray-icon tooltip / tray-dot logic.** The taskbar count and the tray-tooltip count agree in steady state but differ in two transient places:
  - **Active profile, between `listUsers` refreshes.** The tray uses `UserInfo.unreadCount` for the active profile too — the same in-memory aggregate that drifts upward on every received chat item regardless of `enable_ntfs` (`ChatModel.kt:560`). The taskbar walks the active profile's chats and applies the per-chat `enableNtfs` filter at compute time. When the user mutes a chat or receives a message in a muted chat, the taskbar reflects the correct count immediately; the tray's tooltip is corrected only on the next `listUsers` round-trip (startup, profile switch, picker open, hide/unhide). The steady-state values agree.
  - **Non-active hidden profiles, on paper.** The tray's lambda is `if (!showNtfs && !activeUser) 0 else unreadCount` — it does not explicitly filter `User.hidden`. The taskbar does. In practice the core enforces "hidden user always muted when inactive" (`src/Simplex/Chat/View.hs:333`, `CEHiddenUserAlwaysMuted`), so a non-active hidden profile always has `showNtfs == false` and both surfaces zero it. The rules differ on paper; the observed counts do not.

  Aligning the tray's lambda to the taskbar's (or vice versa) is its own product decision and belongs in its own PR. This PR introduces the taskbar count alongside the existing tray and leaves the tray rule untouched.
- **Per-platform title format variants** (e.g. "SimpleX (3)" on macOS, "(3) SimpleX" on Windows). One format on all platforms is simpler, easier to test, and easier to localize. We use `"SimpleX [N]"` everywhere.
- **Configurability.** No setting to turn this off. It is the same information the tray already exposes, presented on a surface that is always visible. A user who does not want a count anywhere can already mute their chats.
- **Counting `unreadChat`** (manually-marked-unread chats with zero items). The taskbar suffix is a number; this flag is a state. Leaving it out keeps the suffix faithful to "messages waiting".
