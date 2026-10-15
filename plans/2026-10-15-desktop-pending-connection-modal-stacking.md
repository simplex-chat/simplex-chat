# Desktop: "Learn more" and "Incognito" open a new copy on every click in a pending connection

Fixes #6270.

## Problem

On desktop, open a pending connection from the chat list, for example your own unused one-time link ("invited to connect"). Click "Learn more" several times. Each click opens another copy of the "One-time invitation link" page in the right pane, and the user has to press Back once per click to close them. The "Incognito" row of an incognito connection behaves the same way.

## Cause

`ContactConnectionInfoView` has two callers:

- `ChatView.kt`, in the `ChatInfo.ContactConnection` branch: opening the connection from the chat list.
- `ChatListNavLinkView.kt`, `ContactConnectionMenuItems`: the "Set contact name" menu item, through `ModalManager.center.showModalCloseable`.

On desktop, both put the view in the center pane. Its two rows push a modal onto `ModalManager.end` with no check:

- `learnMore` → `ModalManager.end.showModalCloseable { AddContactLearnMore(close) }`
- the incognito row → `ModalManager.end.showModal { IncognitoView() }`

`ModalManager.end` is the right pane and composes only its top modal. The center pane stays visible and clickable, so every click adds one more modal.

On Android, `start`, `center` and `end` are the same shared full-screen manager (`ModalView.kt`, the `ModalManager` companion object). The new page covers the row once it opens, so the row can't be clicked again.

## Fix

On desktop only, close the right-pane modals before showing the page, in both click handlers:

```kotlin
if (appPlatform.isDesktop) ModalManager.end.closeModals()
ModalManager.end.showModalCloseable { close -> AddContactLearnMore(close) }
```

`ChatView` already does the same when a contact connection opens: it calls `ModalManager.end.closeModals()`. `ContactConnectionInfoView.kt` already branches on `appPlatform.isDesktop` in its `DisposableEffect`.

Why the call is desktop-only: on Android, after "Set contact name", the connection view is itself the open modal of the shared manager. `closeModals()` would close the view too, and Back would then go to the chat list instead of the view. It would also leave `chatModel.showingInvitation` set, because the view's `onDispose` skips clearing it while a modal is open. Waydroid showed this regression with the unguarded call.

Alternatives considered:
- Ignore the click while a right-pane modal is open. Rejected: with "Incognito" open, "Learn more" would do nothing.
- Toggle (close if open), like the chat info button. Rejected: clicking "Learn more" while "Incognito" is open would close the pane instead of showing "Learn more".
- Check a `ModalViewId` with `isLastModalOpen`, the mechanism `ComposeContextProfilePickerView` uses. Rejected: it adds two enum values, and "Learn more" opened over "Incognito" would still stack.

Effect:
- Desktop: one page at most. Clicking the other row replaces the page instead of stacking. Before, Back from "Learn more" opened over "Incognito" returned to "Incognito"; now it closes the pane. A replaced page now appears without the slide-in animation, the same as a first open.
- Android: unchanged. The new line does not run there.

The New chat sheet's "Learn more" (`AddContactLearnMoreButton`) and the incognito info buttons in `NewChatView` and `AddGroupView` use `ModalManager.start`, which covers the button, so they don't stack and are not changed.

## Verification

Desktop: the same steps on the master prebuild and on this branch.

| # | Check | master | this branch |
|---|---|---|---|
| T1 | "Learn more" ×3 on own one-time link, then Back | still open, 3 Backs | closed after 1 Back |
| T2 | "Incognito" ×3 on own incognito link, then Back | still open, 3 Backs | closed after 1 Back |
| T3 | "Incognito", then "Learn more", then Back | Back reveals "Incognito" | pane empty |
| T4 | "Learn more" ×3 on a joined connection ("You accepted connection"), then Back | still open, 3 Backs | closed after 1 Back |
| T5 | "Learn more" open, switch to another chat | pane closed | same |
| T6 | "Learn more" content | same texts | same |
| T7 | QR code still shown after closing "Learn more" | shown | same |
| T8 | New chat sheet → "Learn more" → Back | back on the 1-time link tab | same |
| T9 | Contact info open, then open the pending connection | info closed | same |
| T10 | "Set contact name" → "Learn more" ×3, then Back | still open, 3 Backs | closed after 1 Back; connection view stays |

Android (Waydroid), master and this branch:

| # | Check | master | this branch |
|---|---|---|---|
| A1 | Open the connection → "Learn more" → Back, twice | Back returns to the connection view | same |
| A2 | "Set contact name" → "Learn more" → Back | Back returns to the connection view | same |
