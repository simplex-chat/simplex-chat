# Desktop: chat info shows the previous chat's security code and misses group members after switching chats

Fixes #5135.

## Problem

On desktop, open contact A's info, then "Verify security code". Click contact B in the chat list without closing the page. The right pane goes back to the info, now for B. Opening "Verify security code" again shows A's code under the title "B is not verified". "Mark verified" then fails with "Incorrect security code!", so the user sees what looks like a tampered connection.

The same happens in other cases:
- Group A's info → "Group preferences" → click group B: B's info shows "1 members" without B's other members.
- Group A's info → "Group preferences" → click a contact: the contact's info has no "Verify security code" row, because no code is loaded.
- Without any page open, switching between a contact and a group with the info pane shown: contact A → group B shows "1 members", and contact A → group B → contact B shows A's code under B's name.

## Cause

`ChatView` loads the info data before opening the pane (`preloadedContactInfo`, `preloadedCode`, `preloadedLink`, and members through `setGroupMembers`). The pane's state starts from these values. `KeyChangeEffect(chatInfo.id)` reloads them when the shown chat changes.

`KeyChangeEffect` treats the key it sees on its first composition as the starting key, and runs only when the key changes after that. Switching between two chats of the same type while the info pane is composed changes the key, so the data is reloaded.

Whenever the pane's contact or group branch is composed fresh for another chat, its state starts from the values last loaded for another chat, and `KeyChangeEffect` takes the new chat as its starting key and never runs:
- `ModalManager.end` composes only its top modal (and, during a transition, the one leaving). With "Verify security code" or "Group preferences" on top, the info pane is not composed. Switching chats calls `ModalManager.end.closeModalsExceptFirst()` (`ChatListView.kt`, except when the top page is a secondary chat), so the info pane is composed again, now for chat B.
- The contact and group cases are separate branches of the pane, so a switch between a contact and a group composes the other branch fresh.

The pane keeps A's code, connection info and group link, and the members of group B are not loaded (switching chats clears them; channels reload their members on open).

## Fix

Remember which chat the preloaded values belong to, `preloadedChatId`. Reload when the shown chat is a different one, instead of only when the key changes during one composition:

```kotlin
LaunchedEffect(chatInfo.id) {
  if (chatInfo.id == preloadedChatId) return@LaunchedEffect
  preloadedChatId = null
  // load as before, updating the preloaded values
  preloadedChatId = chatInfo.id
}
```

Both the contact branch and the group branch get this change.

`preloadedChatId` is cleared while a load runs, because the load overwrites the preloaded values: if it is cancelled halfway, for example by switching back to the previous chat, that chat no longer matches them and is reloaded.

Effect:
- The pane reloads whenever it shows a chat other than the one its values were last fully loaded for. This covers every case above, a switch to another chat before the preload finishes, and a reload interrupted by opening a page.
- When it shows that same chat, as on the first open or Back from a page opened over it, nothing is reloaded, as before.
- Android: the info pane is a full-screen modal and a chat change normally closes it, so usually only the second rule applies there.

Security: the core checks the code when verifying (`APIVerifyContact`), so the stale screen could not mark a contact verified; it showed a false "Incorrect security code!".

Alternatives considered:
- Close all right-pane modals on a chat switch. Rejected: the info pane is meant to stay open next to the new chat.
- Reload on every composition. Rejected: this doubles the member load on the first open; the preload exists so that a large group's info opens already filled.

## Verification

Desktop, master prebuild vs this branch, two contacts and two groups (one owned by each contact).

Changed:

| # | Check | master | this branch |
|---|---|---|---|
| T1 | Contact A info → Verify → switch to B → Verify | A's code under B's name | B's code |
| T3 | Group A info → Group preferences → switch to group B | "1 members", B's owner missing | "2 members" |
| T6 | Contact A info → Verify → switch to group B | "1 members" | "2 members" |
| T7 | Group A info → Group preferences → switch to contact B | no "Verify security code" row | B's code |
| T9 | After T1, "Mark verified" | "Incorrect security code!" | B verified |
| X1 | Contact A info (nothing on top) → switch to group B | "1 members" | "2 members" |
| X2 | Then switch to contact B → Verify | A's code | B's code |

Unchanged:

| # | Check | master | this branch |
|---|---|---|---|
| T2 | Contact A info → switch to B → Verify | B's code | same |
| T4 | Group A info → switch to group B | "2 members" | same |
| T5 | Verify → Back → Verify (same contact) | A's code | same |
| T8 | A info → Verify → B → A → Verify | A's code | same |

The codes were compared with each peer's own `/code` output.

Android (Waydroid), master vs this branch, identical: contact info → Verify → Back → Verify shows the peer's code both times; group info members, also after Group preferences → Back.
