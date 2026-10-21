# Desktop: chat info shows the previous chat's security code and misses group members after switching chats

Fixes #5135.

## Problem

On desktop, open contact A's info, then "Verify security code". Click contact B in the chat list without closing the page. The right pane goes back to the info, now for B. Opening "Verify security code" again shows A's code under the title "B is not verified". "Mark verified" then fails with "Incorrect security code!", so the user sees what looks like a tampered connection.

The same happens in other cases:
- Group A's info → "Group preferences" → click group B: B's info shows "1 members" without B's other members.
- Group A's info (A has a link) → "Group link" → click group B (no link): B's info shows "Group link" with A's link.
- Group A's info → "Group preferences" → click a contact: the contact's info has no "Verify security code" row, because no code is loaded.
- Without any page open, switching between a contact and a group with the info pane shown: contact A → group B shows "1 members", and contact A → group B → contact B shows A's code under B's name.
- With the info pane shown, contact A → contact B: until B's info is reloaded, the pane shows A's values, and "Verify security code" clicked in that moment opens A's code. Group A → group B likewise shows A's group link: "Group link" opens and shares A's link as B's. From a group without a link to one with a link, it shows "Create group link", which fails with "Error creating group link".
- Click info on group A, then another chat and back to A before the info has loaded: A's info shows only the user as a member.

## Cause

`ChatView` loads the info data before opening the pane (`preloadedContactInfo`, `preloadedCode`, `preloadedLink`, and members through `setGroupMembers`). The pane's state starts from these values. `KeyChangeEffect(chatInfo.id)` reloads them when the shown chat changes.

`KeyChangeEffect` treats the key it sees on its first composition as the starting key, and runs only when the key changes after that. Switching between two chats of the same type while the info pane is composed changes the key, so the data is reloaded, but until the reload returns the pane shows the previous chat's values: the contact branch's state is remembered without a key, and the group branch's link starts from the previous group's link.

Whenever the pane's contact or group branch is composed fresh for another chat, its state starts from the values last loaded for another chat, and `KeyChangeEffect` takes the new chat as its starting key and never runs:
- `ModalManager.end` composes only its top modal (and, during a transition, the one leaving). With "Verify security code" or "Group preferences" on top, the info pane is not composed. Switching chats calls `ModalManager.end.closeModalsExceptFirst()` (`ChatListView.kt`, except when the top page is a secondary chat), so the info pane is composed again, now for chat B.
- The contact and group cases are separate branches of the pane, so a switch between a contact and a group composes the other branch fresh.

The pane keeps A's code, connection info and group link, and the members of group B are not loaded (switching chats clears them; channels reload their members on open).

The preload itself also loses the members: `setGroupMembers` skips storing them when another chat is open when they arrive, and every chat switch clears them. After info on group A → another chat → back to A before the preload finishes, the pane opens for A, the chat the values were preloaded for, so nothing is reloaded.

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

Both the contact branch and the group branch get this change. The group branch also reloads when the members are not loaded (`!chatModel.membersLoaded.value`), which covers the preload that lost them.

`preloadedChatId` is cleared while a load runs, because the load overwrites the preloaded values: if it is cancelled halfway, for example by switching back to the previous chat, that chat no longer matches them and is reloaded.

Until the reload returns, the pane shows none of the previous chat's loaded info:
- The contact info, security code and group link start empty when `preloadedChatId` is not the shown chat (`remember(chatInfo.id)`, so a switch inside the same branch also starts over). With no code, the "Verify security code" row is hidden, as it already was when the code was not loaded.
- Until the link is loaded (`linkLoaded`, passed to `GroupChatInfoView` as `groupLinkLoaded`), the "Create group link" / "Group link" row is hidden, the channel "Link" button is disabled, and a channel owner sees no channel link rows. With no link yet, the row and the button would open the link page, which creates a link when it gets none; for a group that already has one, the core creates a server queue before failing with a duplicate link error. The owner's "Channel link" and "SimpleX name" rows need the loaded link; the channel's QR code, "Share link" and "Share via chat", which an owner sees only when no link is loaded for the channel, would otherwise show until the "Channel link" row replaces them.

A link created, changed or deleted on the "Group link" page finishes in the background. It is stored only while the stored values belong to the group it came from, so after a switch it can't replace the next group's link.

Effect:
- The pane reloads whenever it shows a chat other than the one its values were last fully loaded for, or a group whose members are not loaded. Together with the empty start above, this covers every case above, a switch to another chat before the preload finishes, and a reload interrupted by opening a page.
- When it shows that same chat, as on the first open or Back from a page opened over it, nothing is reloaded, as before (unless the group's members were cleared by a switch).
- Android: the info pane is a full-screen modal and a chat change normally closes it, so usually only the second rule applies there.

Security: the core checks the code when verifying (`APIVerifyContact`), so the stale screen could not mark a contact verified; it showed a false "Incorrect security code!".

Alternatives considered:
- Close all right-pane modals on a chat switch. Rejected: the info pane is meant to stay open next to the new chat.
- Reload on every composition. Rejected: this doubles the member load on the first open; the preload exists so that a large group's info opens already filled.
- Start the group link empty and show "Create group link" until it loads. Rejected: clicking it in that moment, for a group that has a link, leaves an unused server queue and shows an error.
- Make `preloadedChatId` observable state instead of adding `linkLoaded`. Rejected: `preloadedChatId` is cleared while any load runs, including the reload of the same group when its members are not loaded, so a link row derived from it would disappear during that reload; `linkLoaded` follows the existing `connectionLoaded` pattern of member info.

## Verification

Desktop, master prebuild (0002582) vs this branch before its last commit (93881883c, the channel owner's link rows), the same scripts on fresh profiles: two contacts, two groups owned by them; then the user is made admin of both groups, and a third group where the user is a member is added. Security codes were compared with each peer's own `/code` output.

Changed:

| # | Check | master | this branch |
|---|---|---|---|
| T1 | Contact A info → Verify → switch to B → Verify | A's code under B's name | B's code |
| T9 | After T1, "Mark verified" | "Incorrect security code!" | B verified |
| T3 | Group A info → Group preferences → switch to group B | "1 members", B's owner missing | "2 members" |
| T6 | Contact A info → Verify → switch to group B | "1 members" | "2 members" |
| T7 | Group A info → Group preferences → switch to contact B | no "Verify security code" row | B's code |
| X1 | Contact A info (nothing on top) → switch to group B | "1 members" | "2 members" |
| X2 | Then switch to contact B → Verify | A's code | B's code |
| R6 | Group A (has a link) → "Group link" page → switch to group B (no link) | "Group link" | "Create group link" |

Unchanged:

| # | Check | master | this branch |
|---|---|---|---|
| T2 | Contact A info → switch to B → Verify | B's code | same |
| T4 | Group A info → switch to group B | "2 members" | same |
| T5 | Verify → Back → Verify (same contact) | A's code | same |
| T8 | A info → Verify → B → A → Verify | A's code | same |
| R1 | First open of contact info | Verify row, the contact's code | same |
| R2 | Info of a group where the user is a member | no link row, members listed | same |
| R3 | First open of a group the user admins, no link | "Create group link" | same |
| R4 | Create a link → Back → "Group link" → open it | the same link | same |
| R5 | Pane open: group A (has a link) → group B → group A | B: "Create group link", A: "Group link" | same |
| R7 | Group preferences → Back | members listed | same |
| R8 | Delete the link → Back | "Create group link" | same |

Both: no crashes; no `E:` log lines other than `apiGetGroupLink … groupLinkNotFound` for groups without a link.

While the info loads (desktop; test-only builds of the branch with only the `preloadedChatId` fix (before) and with the branch before its last commit (after), both with `delay(3000)` added before the pane's reload and after the preload's `setGroupMembers`, to make the moment observable; locally it lasts milliseconds):

| # | Check | before | after |
|---|---|---|---|
| W1 | Contact A info → click B | "Verify security code" shown; it opens A's code under "B is not verified" | no Verify row until B's code is loaded, then B's code |
| W2 | Group A (has a link) info → click group B (no link) | "Group link" shown; it opens A's link as B's | no link row until loaded, then "Create group link" |
| W3 | Group B info → click group A (has a link) | "Create group link" shown; tapping it fails with "Error creating group link" (duplicateGroupLink) | no link row until loaded, then "Group link" |
| M1 | Click info on group A → click B → click A while it loads | "1 members" | "2 members" |

Android (Waydroid debug APK), the branch with only the `preloadedChatId` fix vs the branch before its last commit, identical: contact info → Verify → Back → Verify shows the peer's code both times; group info lists both members, also after Group preferences → Back; in a group the user admins, "Create group link" → Back → "Group link" → the link, members intact.

The last commit, desktop: test-only builds of the branch before and after it, both with `delay(3000)` before the pane's link load. An owner with channel X's info open clicks their channel Y, which has a link: before it, Y's QR code, "Share link" and "Share via chat" show during the load; after it, no link rows until the "Channel link" row appears. A subscriber sees the channel's QR code at once (tested after it; the condition is unchanged for non-owners). The final code against a later master prebuild (built 2026-10-17): the security code after a contact switch, "Mark verified", the members after Group preferences → another group, the group link rows (as in the first table), messages each way in a direct chat and a group; no errors in the logs other than `groupLinkNotFound`.
