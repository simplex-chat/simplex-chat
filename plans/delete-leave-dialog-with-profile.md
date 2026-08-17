# Show chat name in delete / leave / clear confirmation dialogs

## Goal

The current delete-contact, delete-group, delete-channel, leave-group,
leave-channel and clear-chat confirmations are generic. From a long
chat list, swiping on a row and triggering one of these actions opens
a dialog whose title is "Delete group?", "Leave channel?", "Clear
conversation?" — with no indication of *which* chat is the target. A
user can easily act on the wrong chat.

The fix: include the chat's display name in the dialog body, on a line
of its own above the existing warning text. Nothing else changes —
same title, same warning text, same buttons, same colors, same dialog
shape. No profile picture, no layout changes, no new helpers, no new
translation strings.

We deliberately do NOT reuse the open-chat-link alert layout (centered
profile image + name + open-chat button). That layout is the *invite*
flow's identity; repurposing it for destructive confirmations would
confuse the two flows visually. The minimum change that solves the
"which chat?" problem is putting the name in the body text.

## Why body, not title; why no new strings

The title carries the action ("Delete group?", "Leave channel?"). The
body carries the consequences ("Group will be deleted for all
members…"). The chat name belongs with the body — it is the subject
of the consequence, not part of the question.

Adding the name to the title would require new format-string variants
(`delete_group_named_question` etc.) and per-locale re-translation.
Putting the name on its own line in the body is a pure code change —
the existing translated warnings are concatenated with the chat name
in code:

```
Tech Talk

Group will be deleted for all members - this cannot be undone!
```

The display name appears first because the user wants to confirm
*which* chat before reading *what* will happen. The blank line between
the name and the warning makes the name visually distinct.

## Current state

### Multiplatform (Kotlin / Android / Desktop)

All eight dialogs go through `AlertManager.shared.showAlertDialog` or
`showAlertDialogButtonsColumn`:

- `deleteGroupDialog` — `views/chat/group/GroupChatInfoView.kt:182`
- `leaveGroupDialog` — `views/chat/group/GroupChatInfoView.kt:222`
- `clearChatDialog` — `views/chat/ChatInfoView.kt:492`
- `deleteContactOrConversationDialog` — `views/chat/ChatInfoView.kt:248`
- `deleteActiveContactDialog` — `views/chat/ChatInfoView.kt:304`
- `deleteContactWithoutConversation` — `views/chat/ChatInfoView.kt:361`
- `deleteNotReadyContact` — `views/chat/ChatInfoView.kt:417`
- `deleteContactConnectionAlert` — `views/chatlist/ChatListNavLinkView.kt:772`
  (deletes a pending contact connection; takes a `PendingContactConnection`
  whose `displayName` reflects any custom name the user set)

Call sites (chat-info screens, chat-list swipe / overflow, contact
list) funnel through these dispatcher functions.

### iOS (Swift)

Two SwiftUI patterns are used:

- SwiftUI `Alert` with `primaryButton: .destructive` / `.cancel()`:
  - `deleteGroupAlert` — `Views/ChatList/ChatListNavLink.swift:567`,
    `Views/Chat/Group/GroupChatInfoView.swift:835`
  - `leaveGroupAlert` — `Views/ChatList/ChatListNavLink.swift:622`,
    `Views/Chat/Group/GroupChatInfoView.swift:872`
  - `clearChatAlert` — `Views/ChatList/ChatListNavLink.swift:600`,
    `Views/Chat/ChatInfoView.swift:577`,
    `Views/Chat/Group/GroupChatInfoView.swift:858`
- SwiftUI `ActionSheet`:
  - `deleteContactOrConversationDialog` —
    `Views/Chat/ChatInfoView.swift:1177`
  - `deleteContactWithoutConversation` —
    `Views/Chat/ChatInfoView.swift:1324`
  - `deleteNotReadyContact` — `Views/Chat/ChatInfoView.swift:1348`

`Alert(message:)` accepts `Text`, and `ActionSheet(message:)` (an
existing optional parameter not used today) accepts `Text` too — so
the name can be added by composing the existing message string with
`"\n\n"` and the chat name. No widget changes.

## Design

| Dialog | Body today | Body after |
|---|---|---|
| Delete group | `Group will be deleted for all members – this cannot be undone!` | `Tech Talk` + blank line + existing text |
| Delete channel | `Channel will be deleted for all subscribers – this cannot be undone!` | `SimpleX news` + blank line + existing text |
| Leave group | `You will stop receiving messages from this group. …` | `Tech Talk` + blank line + existing text |
| Clear chat | `All messages will be deleted – this cannot be undone! …` | `Alice` + blank line + existing text |
| Delete contact (entry sheet) | *(no body today — title only + buttons)* | `Alice` (becomes the body) |
| Delete contact (active variant) | `Contact will be deleted – this cannot be undone!` | `Alice` + blank line + existing text |
| Confirm contact deletion (not-ready / no-conversation) | `Contact will be deleted – this cannot be undone!` | `Alice` + blank line + existing text |

Title text is unchanged in every case. Existing titles
(`delete_contact_question`, `confirm_delete_contact_question`, etc.)
keep their semantic distinction — the "Confirm contact deletion?"
title still appears for the not-ready / no-conversation paths.

### Which name to use: `displayName`, not `chatViewName`

The chat list row labels chats with `cInfo.chatViewName`
(`ChatPreviewView.kt:87`), defined as:

```kotlin
val chatViewName: String
  get() = localAlias.ifEmpty { displayName + (if (fullName == "" || fullName == displayName) "" else " / $fullName") }
```

The dialog uses `chatInfo.displayName` (and `groupInfo.displayName`
for the leave dialog). For most chats these are identical:

- If `localAlias` is set, both resolve to the alias.
- If `displayName == fullName` (or `fullName` is empty), both resolve
  to `displayName`.

For a contact with distinct display name and full name (no alias),
the row would show `alice / Alice Smith` while the dialog shows
`alice`. Acceptable: `displayName` is the recognizable identifier,
shorter, and the dialog format (single line above the warning)
benefits from concision. Two-part identifiers in the dialog would
crowd the layout.

### `clearNoteFolderDialog` is excluded

The local notes folder is a single-instance object — there is only
one per user — and its existing warning text already names it
unambiguously. Adding the display name on its own line would be
pure redundancy. Skipped.

## Changes

### Multiplatform (Kotlin)

Each dispatcher function changes one argument: the `text =` parameter
passed to `AlertManager.shared.showAlertDialog` /
`showAlertDialogButtonsColumn`. The new value is the chat name + two
newlines + the existing message text:

```kotlin
text = "${chatInfo.displayName}\n\n${generalGetString(messageId)}",
parseHtml = false,
```

`parseHtml = false` is a new boolean parameter added to both alert
helpers. It bypasses `escapedHtmlToAnnotatedString` so the
user-controlled `displayName` is rendered as literal text, never
interpreted as HTML markup (`<b>`, `<font>`, `&`, etc.). The default
remains `true`; only our delete-confirmation dispatchers opt out.

For `leaveGroupDialog` the source is `groupInfo.displayName` (the
function already takes `groupInfo` — no signature change needed,
no caller updates needed).

For `deleteGroupDialog`, also `groupInfo.displayName`, for consistency
with `leaveGroupDialog` (both have `groupInfo` already in scope).

For `deleteContactOrConversationDialog`, which has no `text =`
parameter today, add `text = chatInfo.displayName` (no concatenation
needed — the dialog had no body text before).

### iOS

Each of the eight call sites changes one argument: the `message:`
parameter passed to `Alert(…)` or `ActionSheet(…)`. The new value
composes the chat name with the existing localized message string:

```swift
message: Text("\(chat.chatInfo.displayName)\n\n\(existingMessage)"),
```

For the three `ActionSheet` sites that have no `message:` today, add
`message: Text(chat.chatInfo.displayName)`.

## Out of scope

- Profile picture / avatar in any of these dialogs — excluded by
  decision: the open-chat-link alert owns that layout, and reusing
  it for destructive confirmations conflates two semantically
  different flows.
- The pre-existing wording divergence between Kotlin's
  `clear_chat_question` ("Clear chat?") and iOS's "Clear
  conversation?". Both platforms keep their existing titles.
- Refactoring the iOS duplication between `ChatListNavLink` and
  `GroupChatInfoView` / `ChatInfoView` (pre-existing `// TODO reuse
  this and clearChatAlert with ChatInfoView` at
  `GroupChatInfoView.swift:834`).
- "Delete invitation" at `ChatListNavLink.swift:236` — goes through
  `deleteChat(chat)` directly with no confirmation dialog. No dialog
  to modify.
- Bolding the chat name on its own line. SwiftUI `Text` concatenation
  supports `.bold()`; Jetpack Compose `AlertDialog` text is a single
  string. Keep both platforms unstyled for parity.

## Verification

Per platform, exercise every entry point and confirm the dialog body
reads `<chat name>` on its own line followed by a blank line followed
by the existing warning:

- Android & Desktop:
  - Chat list swipe — direct contact, group, channel, business chat
    → delete / clear / leave actions. (Note folder's clear dialog
    is intentionally unchanged.)
  - Chat info screens — "Delete contact" / "Delete group" / "Delete
    channel" / "Clear conversation" / "Leave …" rows.
  - Contact list (`ContactListNavView.kt:148`) — "Delete contact"
    action.
  - The multi-option contact-delete path: entry dialog (now has a
    name body where it had none) → toggle dialog (name above the
    warning) → success.
- iOS:
  - Same matrix from chat list swipe and chat info screens.
  - Action-sheet contact-delete paths show the name as the
    `message:` line.

Edge cases:

- Long chat name (40+ chars) — alert containers wrap automatically;
  body now occupies 3+ lines (name on 2, blank line, warning on 1+).
  Confirm via a chat renamed to a long string.
- Special characters in name (emoji, RTL text, double quotes) —
  render literally because the substitution is string concatenation,
  not format expansion. A contact named `Bob "the builder"` displays
  as `Bob "the builder"` on its own line. No quoting/escaping issue.
- Empty `displayName` would render an empty first line above the
  warning. In practice `displayName` is non-empty (the `NamedChat`
  interface enforces it via `localAlias.ifEmpty { profile.displayName }`);
  no defensive trimming added.

Diff-level checks:

- `git diff strings.xml` and `git diff '*Localizable.strings'` show
  zero hunks. The change is pure code.
- `git diff --stat` shows each platform touched in 2–4 files:
  the dispatcher file(s) on Kotlin (`ChatInfoView.kt`,
  `GroupChatInfoView.kt`), and the SwiftUI views holding the
  alert/sheet builders on iOS.
- Behavior is unchanged. Cancel returns to the prior screen;
  confirm performs the same destructive API call as before.
