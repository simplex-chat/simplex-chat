# A page opens again on every click of the button that opened it

Fixes #6270.

## Problem

Issue #6270 reports one case: on desktop (Linux), clicking "Learn more" repeatedly in a pending connection opens the page again on every click. Each copy then needs its own Back. The same bug has two causes, and both occur across the app:

1. **The button stays clickable in another pane.** On desktop the window has a left pane (chat list, `ModalManager.start`), a center pane (chat or `ModalManager.center`) and a right pane (`ModalManager.end`). Each pane draws only the top modal of its own stack. When a button in one pane opens a page in another, the button stays visible and clickable, and every click pushes another copy.
2. **A fast double click during the slide-in.** A modal slides in over 250 ms. Until it covers the button that opened it, the button still takes clicks, so a double click opens two copies. Pages slide in on Android and in the desktop left pane; in the desktop center and right panes the first page never slides, the third and later do, and the second does only after an earlier page there has slid out.

Reproduced on the master prebuild (a932c82), R8 on the stable prebuild (master has the same code):

| # | Steps | Clicks | Backs needed |
|---|---|---|---|
| R1 | Pending connection: "Learn more" or "Incognito" | 3 | 3 |
| R2 | Your chat profiles: "+ Add profile" (form opens in the center) | 3 | 3 |
| R3 | "Chat with admins" open on the right (a reports chat opens the same way; not tried); in the group chat, message menu → "Info" | 3 | 3 |
| R4 | Direct chat banner: profile bio "Read more" | 3 | 3 |
| R5 | Attach a file over 1 GB; "Large file" alert → "Support SimpleX" | 3 | 3 |
| R6 | `.md` file clicked in a support chat on the right (viewer opens in the center) | 3 | 3 |
| R7 | What's New opened in the center after an update → "Create your address" → "Address settings" | 3 | 3 |
| R8 | No profile and no linked mobile: picker → "Create chat profile" | 3 | 3 |
| D1 | Settings: double-click "Appearance" (80 ms, 150 ms) | 2 | 2 |
| D2 | Settings: double-click "Network & servers" (80 ms) | 2 | 2 |
| D3 | User menu: double-click "Your chat profiles" (80 ms) | 2 | 2 |

Controls that don't stack on master: message "Info" with nothing open on the right, a double click on "Group preferences" when group info is the only page on the right, a double click on the new-chat pencil.

## Audit

Every `ModalManager.{start,center,end,fullscreen}.show*` call on master (about 170) was classified by where its button is drawn on desktop and whether that button stays clickable after the page opens. Wrapper functions and every caller of each view were followed. The cases above are all those found. All other call sites are covered by the page they open, guarded already, or open a fullscreen modal, which covers every pane.

Existing guards: close the target pane first (`ChatListNavLinkView` "Set contact name", the pending-connection branch of `ChatView`), skip if a page with that id is open (`BadgesSupportSimplexView`, `BADGE_REDEEM_CODE`), toggles (the chat info button, the profile picker's incognito info), and the desktop overlay in `DesktopScreen` that closes left-pane pages on a click in the center (it guards the onboarding cards there).

## Fix

### Double click: block clicks on the page being covered (`ModalManager.showInView`)

`AnimatedContent` draws each page in a child with its own enter or exit transition. When a modal opens, the child sliding out (`targetState == PostExit`, below the new count) also draws an empty `pointerInput` box over itself. That child is the page being covered, which is the page that opened the new one when the button is in the same pane, or, for the first modal of a pane, the empty child, whose box covers the chat list or chat under the pane. The rest of the pane is under the page sliding in, whose `Surface` already takes clicks. Nothing is blocked when a page closes, as on master.

This fixes the double click for every animated modal on both platforms, with no change at the call sites.

### Buttons in another pane

Each site uses an existing guard, or "replace" (an on-top check followed by a close and a show), chosen by where its button can be and what the page shows:

| Site | Guard | Why this one |
|---|---|---|
| `ContactConnectionInfoView` "Learn more", "Incognito" (R1) | desktop: close the right pane first | The button is never in the right pane on desktop. On Android the view can itself be the open modal ("Set contact name"), so the call is desktop only. |
| The large-file and badge alerts' "Support SimpleX" (R5), which call `openBadgesView` | desktop: skip if `BADGES` is open | Badges can also be open under other right-pane pages, so the right pane can't be closed. `BADGES` is already set on this modal. Badges sub-pages stack above it, so the check is "open", not "on top": while an open Badges page is covered by another page, these two buttons do nothing. The check is at the two alerts, not in `openBadgesView`, so the "Larger files, longer" row in message Info, which its Badges page covers, is unchanged. On Android the page covers its button. |
| `UserProfilesView` "+ Add profile" (R2), `UserPicker` "Create chat profile" (R8) | skip if `CREATE_PROFILE` is on top | Closing the center would also close What's New when it is open there, and clear `chatModel.secondaryChatsContext` while a support chat is open on the right (`closeModals()` always clears it). Both clicks open the same empty form, so the open form, with anything already typed, is kept. Both check before asking for authentication, and the picker row also hides the picker, as the form does when it opens. "On top" rather than "open": with no chat selected the center stops drawing its page stack once its last page has closed, so a closed page can stay in its list. |
| `showFullProfileDescription` "Read more" (R4) | replace if `PROFILE_DESCRIPTION` is on top | Also opened from info pages in the right pane. The text depends on which link was clicked. |
| `showMarkdownFile` (R6) | desktop: replace if `MARKDOWN_FILE` is on top, on the main thread | The opener can be a support chat in the right pane, which must stay. Another file must show its own text. The file is read on a background thread, so the close and show run on the main thread, where the pane reads its stack. On Android the viewer covers its file once it has loaded; taps during the load are the async case under Not fixed. |
| `showItemDetails` message "Info" (R3) | when a secondary chat is open, replace if `CHAT_ITEM_INFO` is on top | Info already closes the right pane first unless a secondary chat is open there. In that case it was pushed over the support or reports chat on every click. The first Info over the support chat still opens over it, and Back still returns to it. The job now runs on the chat's UI scope with only the load on a background thread, so the close and show run on the main thread, as `showReports` does. |

"Replace" closes and shows in one main-thread event, so the new page is drawn in the same place as the old one, from a lambda of the same call site, and Compose would keep the old page's state. That includes `AppBarTitle`'s `LaunchedEffect(title)`, so when the title is unchanged it would not reach the new page's app bar. Each replaced page, and each #6270 row, wraps its content in `key(this)`, the `ModalData` of that show, so it starts fresh.

The "on top" checks use a new `isTopModalOpen`, which looks at the visible top modal (the same index as `isLastModal`). A modal closed with animation stays in `modalViews` until its slide-out ends, and `isLastModalOpen` still counts it: "replace" during those 250 ms would then call `closeModal()` on a page already closing, and `closeModal()` would close the page under it. `isLastModalOpen` and its callers are unchanged.

### Not fixed

- **R7, What's New "Create your address".** The address page opened from What's New in the center pushes its own pages (Address settings, SimpleX name, 1-time link, Learn more) to the left pane, beside it. Opening it in the left pane instead, where they would cover it, makes it disposed while covered: on Back it is composed again, its `autoCreateAddress` effect runs again (re-creating an address the user just deleted) and its "share with contacts" state resets. The Settings → What's New path and Android already have that flaw. Fixing it needs `UserAddressView` to keep that state across disposal, which a follow-up PR (#7756) does.
- Async openers that load data before showing a page, e.g. the reports and support bars, the add-members, channel-members and relays rows in group info, and on Android a `.md` file tapped twice while it loads. Two clicks within the load time can still push two pages. This is a different mechanism from the slide-in. It reproduces on master (reports bar, "Invite", "Invite members", "Member reports", "Chats with members" rows) and is fixed in a follow-up PR (#7757).
- On desktop, two different `.md` files clicked before either has loaded: the one that finishes loading last is shown, as on master; Back now returns to the chat instead of the other file.
- `OpenWhenReceivedEffect`: a file opened when its download finishes is not a repeated click. An image opens a second fullscreen viewer over an open one, as before. On desktop a `.md` file opened this way now replaces an open `.md` viewer, so Back returns to the chat instead of the other file.
- Keyboard and accessibility activation are not pointer input and are not blocked during a slide.
- The `BADGES` check (`hasModalOpen`) also counts a page that is sliding out, so on desktop a click within 250 ms of closing that page, if it slid out, does nothing; a click after the slide-out works.
- A different page opened from the same view stacks over the first one, for example bio "Read more" over contact info, and Back returns to the first page. That is normal navigation. Of the buttons this change touches, only #6270's two rows, "Incognito" and "Learn more", replace each other.

## Effect

- Desktop: each fixed button opens its page once. A repeated click keeps the open page or replaces it with what was clicked. The on-top check doesn't depend on which button opened the page, so a bio, `.md` viewer or Info opened from one place is also replaced by one opened from another (for example a member's bio by a business chat banner's "Read more"), and Back then returns to the page under it. Message Info that replaces Info with nothing else on the right also starts fresh now: master kept the previous page's selected tab and, when both messages had the same title, its app bar showed none.
- Both platforms: while a modal slides open, the page it covers in its pane, or the view under a pane's first modal, takes no new pointer input (clicks, drags, wheel scroll, hover) for 250 ms. On desktop the first page of the center and right panes appears without a slide, so it has no such window.
- Both platforms: a keyed page (bio, `.md` viewer, Info, #6270's pages) opened again while the previous one is still sliding out after Back now starts fresh; master reused the closing page's remembered state (for example Info's selected tab), and its app bar showed no title when the title was unchanged.
- Android: the slide window also covers modals the app opens itself (What's New at start), and a double-tapped toggle (chat info, the profile picker's incognito info, and the reports and support-chats bars when the second tap comes after their page is pushed) now stays open; a second tap during the reports or support-chat load still opens a second page (see Not fixed). Apart from these and the fresh state above, the per-site checks act only on desktop, where the button stays visible.

## Verification

Desktop: the same steps on the master prebuild and this branch, before and after, plus the areas the change touches (info and support chats in the right pane, profiles, badges, markdown viewer, Settings navigation and Back). Android (Waydroid): double tap on list rows, Back during a transition, and the touched flows.
