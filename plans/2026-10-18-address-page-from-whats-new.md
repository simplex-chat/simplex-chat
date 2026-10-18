# What's New "Create your address" stacks pages and loses address state

Follow-up to #7725 (R7 in its plan), stacked on its branch.

## Problem

1. **Pages stack on desktop (R7).** What's New, shown in the center pane after an update, opens the address page ("Create your address" on the v6.4.1 page) in the center too. The address page opens its own pages in the left pane (Learn more, SimpleX name, Create 1-time link, Address settings), beside it, so every click on one of those rows pushes another copy there.
2. **Two address pages on desktop.** With What's New in the center and an address page opened from the user menu in the left pane, "Create your address" opens a second address page in the center, which creates the address.
3. **Two editors of the same settings on desktop.** With the address page in the center and Address settings on the left, each caches the address settings when it opens and saves its own copy, so saving one discards the other's change (for example, a welcome message saved on the left is wiped by toggling "Business address" in the center).
4. **State lost when the page is covered (both platforms).** A covered page is removed from composition and composed again on Back. The address page then re-ran its `LaunchedEffect(autoCreateAddress)` and rebuilt "Share with contacts" from its parameter: after Delete address, opening one of its pages and going back created the address again, and after turning sharing on in Address settings the page showed it off. This happens on the Settings → What's New path, on Android, and would happen on desktop as soon as the page opens in the left pane.

## Fix

- `UserAddressView` becomes `ModalData.UserAddressView` and keeps `shareViaProfile` and a one-shot auto-create flag in `stateGetOrPut`, as `UserAddressSettings` and `NewChatView` keep theirs. All four callers already show it inside a `ModalData`. The flag is cleared before `createAddress()`, which runs on the background API scope, so a create is never cut short.
- What's New opens the address page in `ModalManager.start` with a new id, `USER_ADDRESS`, as the user menu does, so its own pages cover it. This also removes problem 3: both pages are in one stack.
- On desktop the link does nothing while a `USER_ADDRESS` page is open in the left pane. The user-menu row gets the same id. The check is also needed for the page the link opened itself: it now opens beside the link instead of covering it, and the link stays visible during the create request, after a failed create and after Delete address. It is "open", not "on top", as with `BADGES` in #7725: the link also does nothing while the address page is covered by one of its own pages.

## Not fixed

- The short/full link toggle and a running create's progress indicator still reset when the page is covered and comes back.
- The user-menu row and the onboarding card still open an address page without checking for one. The user-menu row is covered by the left pane while a page is open there; the desktop onboarding card is in the center, where a click closes the left pane instead, and it is hidden while What's New is open.
- The `hasModalOpen` check also counts an address page that is sliding out, so the link does nothing for 250 ms after Back from it.
- On desktop, a page already open in the left pane is covered by the address page opened from What's New, and is composed again on Back: its unsaved input is lost (for example an edited profile name), and a page that cancels work when disposed (a connection in progress in New chat) cancels it.
- Disconnecting a linked mobile closes the center and right panes, not the left one, so an address page from What's New covered by one of its own pages now stays open, showing the desktop's address with the mobile profile's "Share with contacts" value (the `TODO` in `UserAddressView`). Before, the user-menu page re-read that value when composed again.

## Effect

- Desktop: from What's New in the center, the address page opens in the left pane, beside What's New, over whatever the left pane shows (the chat list or other pages, to which Back then returns), and its pages open over it. The link opens it once. What's New stays on the page it was on, and can be closed while the address page stays open.
- Both platforms: the address page, from any caller, no longer re-creates a deleted address, sends a second create, or resets "Share with contacts" when it is composed again: after being covered, or on Android after the activity is recreated (for example on rotation). A failed auto-create is not retried then.
- Android: the address page opens where it did (the panes are one stack); only the state fix applies.
