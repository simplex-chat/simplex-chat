# A page that opens after a load opens twice on a double click

Follow-up to #7725 ("Async openers" in its plan).

## Problem

These openers start a load (`openChat`, `setGroupMembers`, reading and parsing a file) and push their page only when it finishes. Until then nothing covers or disables the opener, and their page is not on top of the pane yet, so a second click during the load starts a second load, and each pushes its own page. #7725's slide-in block doesn't apply: it starts once the pushed page is drawn.

| Opener | Page |
|---|---|
| Group chat: reports bar; support bar as a member; a pending member's support chat opened when the chat opens | reports / support chat (right pane) |
| Group info: "Chat with admins", "Member reports"; rows of "Chats with members"; member info "Chat with member" | support / reports chat |
| Group info: "Invite" button and "Invite members" row | Invite members |
| Channel info: "Subscribers" / "Owners & contributors", "Chat relays" | members / relays list |
| Android: a `.md` file tapped twice while it loads | `.md` viewer |

A doubled support or reports chat is also stale: both loads share the one `chatModel.secondaryChatsContext`, so only one of the pages gets new items.

## Fix

- Each opener checks and sets an `AtomicBoolean` before it starts the load and clears it when its job ends, after the page is pushed (`invokeOnCompletion`), or in the two suspend helpers when the load ends (`finally` around `openChat` only; their callers run on the main thread, so the page is pushed in the same step). A click while the flag is set is ignored.
  - `showMemberSupportChatView` and `showGroupReportsView` share `secondaryChatOpening`, since they share the secondary chat context. Every support/reports opener goes through one of them.
  - `addGroupMembers` (the button and the chat menu) and the group-info `addMembers` lambda share `addGroupMembersOpening`.
  - The channel members and relays rows have one flag each.
- The flag is cleared in the step that pushes the page, but the page is drawn only at the next frame, and until then the opener still receives clicks; on Android a press that starts before that frame and ends after it is also a click on the opener. A click in that gap starts a second load, which pushes a second page. So the code of the following openers also returns early when their page (for the support/reports openers, any secondary chat) is already on top of the right pane:
  - The Invite members, channel members and channel relays pages have new ids (`ADD_GROUP_MEMBERS`, `CHANNEL_MEMBERS`, `CHANNEL_RELAYS`). Each job returns before its load when its page is on top, and its flag is then cleared as before. A job clears its flag after it has pushed its page, so when a later click sets the flag, the page is already on top. `addGroupMembers` checks only when it does not close the right pane first, so the chat menu's "Invite members" is unchanged.
  - The four support/reports openers ("Chat with admins", "Member reports", a member row of "Chats with members", member info "Chat with member") check for a secondary chat on top in their coroutine, right before calling the helper. The check and the helper's compare-and-set run on the main thread with no suspension point between them. The check is not in the helpers, so the pending member's support chat opened when a chat opens is unchanged.
- `.md`: the replace of a viewer on top (#7725, desktop only) now applies on Android too, so the second load of a double tap replaces the first viewer. On Android a viewer is also on top when a file opened on receipt finishes downloading over a viewer opened meanwhile; it now replaces that viewer too.

## Not fixed

- Double saves and actions (profile, group profile, preferences, welcome message, member "Message", "Fix connection", conditions): not pages; left out by choice.
- "Create group" and the channel's "Continue": separate PR.
- On desktop connected to a mobile, saving a file not yet fetched from the mobile twice opens the Save dialog twice: a system dialog, not a page.
- A load that never returns keeps its opener inert until it does (on master each click started another load that also never returned).
- The flags are app-wide, not per chat: a click on one support/reports opener while another one loads is ignored, including the pending member's support chat opened at start, and the same holds for "Invite" / "Invite members" and the channel rows across groups. The page of the load in progress still opens, as on master.
- The support/reports chats and the members-section "Invite members" row load on the view's scope: when that view is closed during the load, the load is cancelled and opens nothing, but the opener stays inert until the load returns. The "Invite" button, the chat menu and the channel rows load on the background API scope and still open their page after their view is closed, as on master.
- The "Invite" button when incognito (it opens the group link) loads its link before opening it and has no flag (#7745 changes that button).
- The chat menu "Invite members", which closes the right pane first and never doubled, shares the flag, so it is ignored while a group-info "Invite" load runs.

## Effect

- Desktop and Android: a click on these openers (all but `.md`) during their load, or while their page is on top, opens no second page; a click on the reports or support bar once its chat is open closes it, as before. Single clicks are unchanged.
- Android: a `.md` viewer on top is replaced by one opened from a second tap or from a finished download, instead of stacking; with two different files tapped before either has loaded, the one that finishes loading last is shown, and Back returns to the chat, as on desktop.
