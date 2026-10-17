# Chat info and the chat menu show and save another chat's mute, receipts and member search

Follow-up of #7726.

## Problem

On desktop the chat info pane stays open when you click another chat in the list, and then shows that chat. Parts of it kept the state of the chat it showed before:

- Mute: muted contact A's info → click unmuted contact B. B's button says "Unmute"; tapping it applies A's next mode, so B stays unmuted. The same for groups (whose button cycles "Mute" → "Mute all" → "Unmute").
- Muting a chat from the chat list menu while its info pane is open leaves the pane's button unchanged.
- Group "Send receipts": set to "no" in group A, then click group B: B shows "no" although its setting is the default.
- Member search: type a search in group A's member list (shown above 8 members besides you), then click group B: B's members are filtered by A's search. If B has 8 members or fewer besides you, it has no search field, so its members are hidden with no way to clear the filter.

The same kind of stale state shows elsewhere:

- The mute item of the chat header ⋮ menu keeps the mute state of the first chat it showed: after a chat switch it offers that chat's next mode (desktop; on both platforms when opening a member's direct chat from a group), and it misses muting done elsewhere, such as from the chat info.
- In the contact info pane, mute and "Send receipts" undo each other: mute, then change receipts → the contact is unmuted again; in the other order the receipts change is lost.
- With a search typed in a group of 9 members besides you, a member leaving hides the search field but its filter keeps hiding members.

All were reproduced on master with the GUI (screenshots in the PR).

Scope checked:
- Not reproduced, not changed: a local alias typed just before switching chats is kept; a favorite set from the chat list survives a mute from the pane; a verified security code survives a mute from the pane.
- Other views that can be the first right-pane modal also stay open on a chat switch (member info, message info, the toolbar's "Group link", "Add members"). They keep showing the chat they were opened for, and their actions apply to that chat, so nothing is mixed between chats; not changed here.
- iOS shows chat info as a sheet over the chat, so the chat can't change while it is open.

## Cause

- `MuteButton` (contact and group) and the chat menu's mute item keep the mute mode in `remember { mutableStateOf(...) }` without a key, plus a `derivedStateOf` reading that state. The info pane and `ChatView` are not recreated per chat on desktop, so the remembered mode stays that of the first chat they showed, and nothing updates it when the mode changes outside them.
- The group's `sendReceipts` is `remember { ... }` without a key (the contact's is keyed by contact id).
- The member search text is `stateGetOrPut("searchText")` in the pane's `ModalData`, shared by every chat the pane shows. The filter applies whatever the text is, while the field is shown only above 8 members.
- The contact info pane keeps `chat = remember(contact.id) { ... }`, a copy taken when it opened, and builds the settings it saves from `chat.chatInfo`; `updateChatSettings` then writes that copy back, so a second change reverts the first.

## Fix

- Info pane mute buttons: compute the next mode from the chat's own setting (`contact.chatSettings.enableNtfs`, `groupInfo.chatSettings.enableNtfs`) and drop the local state. `updateChatSettings` updated that state only after the API call succeeded, at the same time as the chat in the model, so the label changes at the same moment as before.
- Chat menu mute item: compute the next mode from the chat's setting. The remembered state stays only because `changeNtfsState` takes it as a parameter; nothing reads it.
- Group receipts: `remember(groupInfo.id)`, as for contacts. The local state stays because the setter updates it before the API call returns.
- Member search: keep the text per chat (`stateGetOrPut("searchText:${chat.id}")`, so Back from a page opened over the pane still keeps it), and apply the filter only while the field is shown (`showMemberSearch`, one value for both).
- Contact info saves: build the saved settings from the current chat (`chatModel.getChat(chat.id) ?: chat`), as the wallpaper save already does.

Four commits: the info pane (desktop), the chat menu, the contact pane saves, the member filter.

Alternatives considered:
- Key the mute state on the chat's setting (`remember(enableNtfs)`). Rejected: in the chat menu an in-flight toggle for chat A then wrote its result into the state object reused by chat B with the same mode, showing a wrong label.
- Make the contact pane's whole `chat` live. Rejected for locality: it also changed the Search button's enabled state, the header and the delete dialog choice while the pane is open.
- Clear the search text on every chat switch. Rejected: one frame of the old filter, and the search is lost on switching back.
- Show the search field whenever there is text. Rejected: a search field would appear in small groups.

Not changed (same on master): group receipts set just before switching away and back may show the old value until the save returns; two settings changes saved before the first returns can overwrite each other.

## Verification

Desktop, master prebuild vs this branch, the same test plan on fresh profiles (two contacts; a group of 10 members and one of 2).

Changed (✱ = the bugs):

| # | Check | master | this branch |
|---|---|---|---|
| B1 | Muted contact A's info → click unmuted contact B | "Unmute"; one tap leaves B unmuted | "Mute"; one tap mutes B |
| B2 | The same with groups | "Unmute"; one tap leaves group B unmuted | "Mute"; one tap → "Mute all" |
| B3 | Group A receipts "no" → click group B | "no" (B is default) | "default (yes)" |
| B4 | B's info open, mute B from the chat list menu | pane stays "Mute" | pane shows "Unmute" |
| B5 | Member search in group A (10 members) → click group B (2 members) | B lists only you, no search field | B lists you and its owner |
| B6 | Contact info: mute, then receipts "no", reopen | unmuted, "no" (mute lost) | muted, "no" |
| B6b | Contact info: receipts "no", then mute, reopen | muted, "default (yes)" (receipts lost) | muted, "no" |
| B7 | Unmuted contact B → click muted A (no info open), ⋮ menu | "Mute" (the first chat's state) | "Unmute" |
| B7b | Mute in the info or the chat list menu, then ⋮ menu | "Mute" | "Unmute" |
| B8 | Search typed in group of 10, one member leaves (9) | no field, filter still hides members | no field, all members listed |

Unchanged (identical on both builds): first open of contact and group info (mute labels match the chat); mute cycles in the info pane (contact Mute ↔ Unmute; group Mute → Mute all → Unmute → Mute); mute cycles in the ⋮ menu (contact; groups differ only by B7's fix: on master the group menu kept the contact's two-step cycle from the first chat it showed); mute and unmute from the chat list menu; receipts set in contact and group info persist after reopening; member search in a group of 10: filter, clear, and Back from a member's info keeps the search; favorite from the chat list; local alias kept across chat switches. No crashes; no `E:` log lines other than `apiGetGroupLink … groupLinkNotFound` for groups without a link.

Android (Waydroid debug APKs, master prebuild vs this branch, each check from the same app snapshot): contact info mute then receipts, and receipts then mute: master loses the first change, this branch keeps both; ⋮ menu after muting in the info: master "Mute", this branch "Unmute"; member search typed, a member leaves (9 members): master keeps filtering with no field (the owner hidden), this branch lists all members. Identical on both: mute cycles in the info and in the ⋮ menu (contact and group), receipts persist, member search filter, clear, and Back from a member's info. No app crashes.
