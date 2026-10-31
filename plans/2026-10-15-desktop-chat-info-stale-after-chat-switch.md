# Desktop: chat info shows the previous chat's data after switching chats

Fixes #5135.

## Problem

With chat info open in the right pane on desktop, clicking another chat can leave the pane showing the previous chat's data:
- Contact A's info → "Verify security code" → click contact B → "Verify security code": A's code is shown under "B is not verified", and "Mark verified" fails with "Incorrect security code!".
- Group A's info → "Group preferences" → click group B: B's info shows "1 members" and A's group link.
- Switching between a contact and a group with the info shown: the same, without any page on top.
- Contact A → contact B, or group A → group B, with the info shown: until B's info is reloaded, "Verify security code" opens A's code, and "Group link" opens and shares A's link as B's; "Create group link" for a group that has a link fails with "Error creating group link".
- Info on group A → another chat → back to A before the info has loaded: A's info shows only the user as a member.

## Cause

Before the app opens the pane, `ChatView` loads the contact info, security code, group link and group members into preloaded values, and the pane's state is initialised from them. They are reloaded in `KeyChangeEffect(chatInfo.id)` only when the key changes after the first composition of the pane's branch.

The branch is composed again for another chat in two cases, and then nothing is reloaded:
- On a chat switch the app calls `ModalManager.end.closeModalsExceptFirst()` (`ChatListView.kt`), so a page on top of the pane ("Verify security code", "Group preferences", "Group link") is closed and the pane is composed for the new chat.
- The contact and group cases are separate branches, so after a switch between a contact and a group the app composes the other branch.

When the key does change, the contact branch's state is remembered without a key and the group link is initialised from the previous group's link, so the previous chat's values are shown until the reload returns.

The app clears the group members on every chat switch, and members are stored by `setGroupMembers` only while their group is the open chat. After info on group A → another chat → A, the members list is empty, and the pane opens for A, the chat its values were preloaded for, so nothing is reloaded.

## Fix

`preloadedChatId` stores the id of the chat the preloaded values belong to. The app reloads them in `LaunchedEffect(chatInfo.id)` when the shown chat's id differs from `preloadedChatId`, or when the shown chat is a group whose members are not loaded (`!chatModel.membersLoaded.value`). `preloadedChatId` is cleared while a load runs and set when it completes, so a cancelled load is redone.

Until the reload completes, none of the previous chat's values are shown:
- The contact info, security code and group link are initialised empty for another chat (`remember(chatInfo.id)`); without a code the "Verify security code" row is hidden.
- While `linkLoaded` (passed to `GroupChatInfoView` as `groupLinkLoaded`) is false, the "Create group link" / "Group link" row and a channel owner's channel link rows are hidden, and the channel "Link" button is disabled.

A link created, changed or deleted on the "Group link" page is stored in the preloaded values only while they belong to the group of that page.

The app skips the reload on the first open and on Back for the same chat, as before, unless the group's members were cleared.

## Scope

- `ChatView.kt`: the info pane in `ChatView`.
- `GroupChatInfoView.kt`: the `groupLinkLoaded` parameter of `GroupChatInfoView`, `GroupChatInfoLayout` and `ChannelLinkActionButton`.
- Desktop. Android uses the same code; its info is a full-screen modal over the chat list, and the app closes it on a notification tap.
- iOS is unchanged: the contact info, security code and group link are loaded when its info views appear, and the group members before its `ChatView` shows the group info.
