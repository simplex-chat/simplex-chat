# Implementation plan — chat name on its own line in delete/leave/clear dialogs

Follows the product spec in
[`delete-leave-dialog-with-profile.md`](./delete-leave-dialog-with-profile.md).

Pure code change — zero string additions, zero new helpers, zero
signature changes. Each call site edits one argument: the `text =` /
`message:` value gains `"${displayName}\n\n"` prepended to the
existing localized warning (or, where there is no current body, the
chat name becomes the new body).

One commit per platform.

## Commit 1 — Kotlin

**Files touched:**
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/helpers/AlertManager.kt`
  — adds `parseHtml: Boolean = true` to `showAlertDialog` and
  `showAlertDialogButtonsColumn`. When `false`, the body text is wrapped
  as `AnnotatedString` and routed through the existing AnnotatedString
  `AlertContent` overload, which does NOT call
  `escapedHtmlToAnnotatedString`. Default stays `true` so existing
  callers are unaffected.
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupChatInfoView.kt`
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt`
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListNavLinkView.kt`
  — adds the previously-missed `deleteContactConnectionAlert`
  dispatcher to the coverage (pending contact connections).

Every Kotlin call site that prepends the chat name sets
`parseHtml = false`, so `displayName` is never HTML-interpreted.

### 1.1 — `deleteGroupDialog` (`GroupChatInfoView.kt:182`)

```diff
 fun deleteGroupDialog(chat: Chat, groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
   val chatInfo = chat.chatInfo
   val titleId = /* unchanged */
   val messageId = /* unchanged */
   AlertManager.shared.showAlertDialog(
     title = generalGetString(titleId),
-    text = generalGetString(messageId),
+    text = "${groupInfo.displayName}\n\n${generalGetString(messageId)}",
     confirmText = generalGetString(MR.strings.delete_verb),
     onConfirm = { /* unchanged */ },
     destructive = true,
   )
 }
```

### 1.2 — `leaveGroupDialog` (`GroupChatInfoView.kt:222`)

```diff
 fun leaveGroupDialog(rhId: Long?, groupInfo: GroupInfo, chatModel: ChatModel, close: (() -> Unit)? = null) {
   val titleId = /* unchanged */
   val messageId = /* unchanged */
   AlertManager.shared.showAlertDialog(
     title = generalGetString(titleId),
-    text = generalGetString(messageId),
+    text = "${groupInfo.displayName}\n\n${generalGetString(messageId)}",
     confirmText = generalGetString(MR.strings.leave_group_button),
     onConfirm = { /* unchanged */ },
     destructive = true,
   )
 }
```

Signature unchanged. No caller updates. `groupInfo.displayName` is
already available on the existing parameter (`ChatModel.kt:2142`).

### 1.3 — `clearChatDialog` (`ChatInfoView.kt:492`)

```diff
 fun clearChatDialog(chat: Chat, close: (() -> Unit)? = null) {
   AlertManager.shared.showAlertDialog(
     title = generalGetString(MR.strings.clear_chat_question),
-    text = generalGetString(MR.strings.clear_chat_warning),
+    text = "${chat.chatInfo.displayName}\n\n${generalGetString(MR.strings.clear_chat_warning)}",
     confirmText = generalGetString(MR.strings.clear_verb),
     onConfirm = { controller.clearChat(chat, close) },
     destructive = true,
   )
 }
```

### 1.4 — Contact-delete dispatchers (`ChatInfoView.kt`)

Four functions. `deleteContactOrConversationDialog` (line 248) has
no existing `text =`, so the chat name becomes the new body. The
other three already have a `text =`, so the name is prepended.

All four already have `contact: Contact` as a parameter, so
`contact.displayName` is used directly (same value as
`chat.chatInfo.displayName` for a direct chat, shorter expression).

```diff
 // deleteContactOrConversationDialog — line 248
 private fun deleteContactOrConversationDialog(chat: Chat, contact: Contact, chatModel: ChatModel, close: (() -> Unit)?) {
   AlertManager.shared.showAlertDialogButtonsColumn(
     title = generalGetString(MR.strings.delete_contact_question),
+    text = contact.displayName,
     buttons = { /* unchanged */ }
   )
 }
```

```diff
 // deleteActiveContactDialog — line 304
 private fun deleteActiveContactDialog(chat: Chat, contact: Contact, chatModel: ChatModel, close: (() -> Unit)? = null) {
   val contactDeleteMode = mutableStateOf<ContactDeleteMode>(ContactDeleteMode.Full())
   AlertManager.shared.showAlertDialogButtonsColumn(
     title = generalGetString(MR.strings.delete_contact_question),
-    text = generalGetString(MR.strings.delete_contact_cannot_undo_warning),
+    text = "${contact.displayName}\n\n${generalGetString(MR.strings.delete_contact_cannot_undo_warning)}",
     buttons = { /* unchanged */ }
   )
 }
```

Same diff for `deleteContactWithoutConversation` (line 361) and
`deleteNotReadyContact` (line 417) — both use
`delete_contact_cannot_undo_warning`. Neither takes `contact` as
a parameter, so the name is read via `chat.chatInfo.displayName`
(which resolves to `contact.displayName` because these dispatchers
are only reached for `ChatInfo.Direct` chats). Their titles
(`confirm_delete_contact_question`) stay unchanged — the
not-ready / no-conversation paths keep their distinct title.

## Commit 2 — iOS

**Files touched:**
- `apps/ios/Shared/Views/ChatList/ChatListNavLink.swift`
- `apps/ios/Shared/Views/Chat/ChatInfoView.swift`
- `apps/ios/Shared/Views/Chat/Group/GroupChatInfoView.swift`

### 2.1 — `deleteGroupAlert` (two locations)

`Views/Chat/Group/GroupChatInfoView.swift:835` and
`Views/ChatList/ChatListNavLink.swift:567` get the same diff.
`deleteGroupAlertMessage(_:)` already returns a `Text` containing
the localized warning — concatenate to it.

```diff
 private func deleteGroupAlert() -> Alert {
     let label: LocalizedStringKey = /* unchanged */
     return Alert(
         title: Text(label),
-        message: deleteGroupAlertMessage(groupInfo),
+        message: Text(chat.chatInfo.displayName) + Text(verbatim: "\n\n") + deleteGroupAlertMessage(groupInfo),
         primaryButton: .destructive(Text("Delete")) { /* unchanged */ },
         secondaryButton: .cancel()
     )
 }
```

`Text(chat.chatInfo.displayName)` resolves to `Text(_ content: some StringProtocol)`
(the runtime-string overload — no localization lookup, matches
codebase convention: `ChatView.swift:984`, `ChatInfoToolbar.swift:49`,
`SettingsView.swift:540`). `Text(verbatim: "\n\n")` is the literal
separator, matching the codebase convention that reserves
`verbatim:` for fixed punctuation (`ContextItemView.swift:88` is
the textbook example: `Text(chatLink.displayName) + Text(verbatim: " - ")`).
The third term `Text(messageLabel)` keeps the existing
`LocalizedStringKey` lookup.

### 2.2 — `leaveGroupAlert` (two locations)

`Views/Chat/Group/GroupChatInfoView.swift:872` and
`Views/ChatList/ChatListNavLink.swift:622`:

```diff
 private func leaveGroupAlert() -> Alert {
     let titleLabel: LocalizedStringKey = /* unchanged */
     let messageLabel: LocalizedStringKey = /* unchanged */
     return Alert(
         title: Text(titleLabel),
-        message: Text(messageLabel),
+        message: Text(chat.chatInfo.displayName) + Text(verbatim: "\n\n") + Text(messageLabel),
         primaryButton: .destructive(Text("Leave")) { /* unchanged */ },
         secondaryButton: .cancel()
     )
 }
```

### 2.3 — `clearChatAlert` (three locations)

`Views/Chat/ChatInfoView.swift:577`,
`Views/Chat/Group/GroupChatInfoView.swift:858`,
`Views/ChatList/ChatListNavLink.swift:600`:

```diff
 private func clearChatAlert() -> Alert {
     Alert(
         title: Text("Clear conversation?"),
-        message: Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
+        message: Text(chat.chatInfo.displayName) + Text(verbatim: "\n\n") + Text("All messages will be deleted - this cannot be undone! The messages will be deleted ONLY for you."),
         primaryButton: .destructive(Text("Clear")) { /* unchanged */ },
         secondaryButton: .cancel()
     )
 }
```

### 2.4 — Contact-delete action sheets

Three functions in `Views/Chat/ChatInfoView.swift`. None currently
pass `message:` to `ActionSheet`; we add it. `ActionSheet`'s
`message:` is an optional second parameter that SwiftUI already
supports.

All three functions have `contact: Contact` in scope. Use bare
`Text(contact.displayName)` (resolves to the `StringProtocol`
overload, no localization lookup, matches codebase convention).
Add only the name as `message:` — these ActionSheets had no
message before, so adding any additional warning would be new
behavior beyond the stated goal.

**`deleteContactOrConversationDialog`** (line 1177):

```diff
 private func deleteContactOrConversationDialog(
     _ chat: Chat, _ contact: Contact, _ dismissToChatList: Bool,
     _ showAlert: @escaping (SomeAlert) -> Void,
     _ showActionSheet: @escaping (SomeActionSheet) -> Void,
     _ showSheetContent: @escaping (SomeSheet<AnyView>) -> Void
 ) {
     showActionSheet(SomeActionSheet(
         actionSheet: ActionSheet(
             title: Text("Delete contact?"),
+            message: Text(contact.displayName),
             buttons: [ /* unchanged */ ]
         ),
         id: "deleteContactOrConversationDialog"
     ))
 }
```

**`deleteContactWithoutConversation`** (line 1324):

```diff
     showActionSheet(SomeActionSheet(
         actionSheet: ActionSheet(
             title: Text("Confirm contact deletion?"),
+            message: Text(contact.displayName),
             buttons: [ /* unchanged */ ]
         ),
         id: "deleteContactWithoutConversation"
     ))
```

**`deleteNotReadyContact`** (line 1348) — same:

```diff
     showActionSheet(SomeActionSheet(
         actionSheet: ActionSheet(
             title: Text("Confirm contact deletion?"),
+            message: Text(contact.displayName),
             buttons: [ /* unchanged */ ]
         ),
         id: "deleteNotReadyContact"
     ))
```

### 2.5 — `DeleteActiveContactDialog` sheet (line 1282) unchanged

The secondary multi-option sheet is reached only after the user
confirms "Delete contact" in the previous action sheet — which now
shows the name. The sheet itself remains as-is.

## Verification

For each platform, exercise every entry point and confirm the
body reads `<chat name>` on its own line followed by the existing
warning:

- Android & Desktop:
  - Chat list swipe — direct contact, group, channel, business chat
    → delete / clear / leave. (Note folder's clear dialog is
    intentionally unchanged — `clearNoteFolderDialog` excluded.)
  - Chat info screens — "Delete contact" / "Delete group" / "Delete
    channel" / "Clear conversation" / "Leave …" rows.
  - Contact list (`ContactListNavView.kt:148`) — "Delete contact"
    action shows the name in entry-point dialog and toggle dialog.
  - Multi-option contact-delete path: entry dialog (now has a name
    body where it had none) → toggle dialog (name above the
    warning) → success.
- iOS:
  - Same matrix from chat list swipe and chat info screens.
  - Action-sheet contact-delete paths show the name as the
    `message:` line on iPhone and iPad.

Edge cases:

- Long chat name — alert containers wrap automatically; the body
  occupies 3+ lines. Confirm with a chat renamed to ~40 characters.
- Special characters (emoji, RTL, double quotes) — render literally
  via string interpolation, no format-substitution involved.
- Empty `displayName` — does not occur in practice (`NamedChat`
  enforces non-empty via `localAlias.ifEmpty { profile.displayName }`).

Diff-level checks:

- `git diff '*strings.xml' '*Localizable.strings'` returns zero
  hunks. Pure code change.
- `git diff --stat` shows ~5 files total: two Kotlin dispatcher
  files, three iOS view files.
- Cancel/confirm flows behave exactly as before — same API calls,
  same model updates, same navigation.

## Out of scope

- Profile picture / avatar in dialogs — excluded by product decision.
- Refactoring the iOS duplication between `ChatListNavLink` and
  `GroupChatInfoView` / `ChatInfoView` (pre-existing `// TODO` at
  `GroupChatInfoView.swift:834`).
- Pre-existing wording divergence between Kotlin's "Clear chat?"
  and iOS's "Clear conversation?". Both platforms keep their
  titles.
- "Delete invitation" at `ChatListNavLink.swift:236` — has no
  confirmation dialog (direct call to `deleteChat(chat)`); nothing
  to modify.
- Bolding the chat name. SwiftUI `Text + Text` supports `.bold()`
  on the first term, but Jetpack Compose `AlertDialog` text is a
  single unstyled string — keeping both unstyled preserves parity.
