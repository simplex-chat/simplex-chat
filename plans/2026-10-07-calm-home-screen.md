# Calm home screen (Android and desktop)

An optional home screen that shows one shape instead of the chat list. Off by default. When it is off, nothing changes.

## Problem

1. People who find the chat list too busy (older users, people new to phones) asked for a simpler app in [#1342](https://github.com/simplex-chat/simplex-chat/issues/1342). This plan is a small, self-contained step that needs no new core API and no new assets.
2. It is not always clear which profile is active ([#1678](https://github.com/simplex-chat/simplex-chat/issues/1678)). The home screen should show which profile is active without reading anything.
3. Screen privacy: the chat list shows names and message previews to anyone glancing at the phone. The calm home shows no names until you hold it.

## What the user sees

Settings, Appearance, Interface section: a new toggle "Calm home screen", next to "Reachable app toolbars".

When it is on, the chat list area shows:

- One shape, 128dp, centred horizontally and 56% down the screen. Drawn in code, no images.
  - Light themes: near-black `#0B0C12` with a soft white glint at the top left (26% x 15% ellipse, rotated -28 degrees, alpha 0.2).
  - Dark and SimpleX themes: black with a 1.5dp rim in `rgba(112,240,249,0.3)`.
  - Black theme: `#1C1D22` with the same rim, so it reads as a dot on the pure black background, not as a ring.
- The shape tells which profile is active, by its position among visible (not hidden) profiles sorted by profile id, so the order does not change when the profile picker reorders its list: circle, rounded square, rounded diamond, capsule, then repeat. Switching profile morphs between shapes.
- A quiet "All chats" text button at the bottom edge of the content area (top edge when the app toolbar is at the bottom). It shows today's chat list. The way back is a small button with the profile's shape in the list toolbar, or Back. Nothing is drawn over the list rows.
- Everything is laid out inside the content area between the toolbar and the system bars. On short screens (phone in landscape, small desktop window) the shape moves up so the text below it never meets "All chats", and the peek card keeps a usable height and covers the shape instead of being clipped.
- The app toolbar stays as it is, with the profile picker and settings. The floating new chat button is hidden on the calm home because double tap replaces it.

## Behaviour

- At rest the shape is perfectly still. No blinking, no squash animation.
- Breathing (scale 1 to 1.13, ease in-out) when the active profile has chats waiting. A chat is waiting when it has an unread badge by the existing `Chat.unreadTag` rule and its notifications are not off: direct chats and groups set to all messages count any unread message, groups set to mentions count only unread mentions. So the shape agrees with notifications and badges. Contact requests are not counted here (see below).
  - Period: 1 waiting 2.6s, 2 waiting 1.9s, 3 or more 1.35s. A change of period keeps the current phase, and when the last chat is read the breath eases back to rest, so the shape never jumps.
  - The animation runs only while something is waiting and the home is visible: not while a chat is open on Android, and not while the desktop window is out of focus. When it cannot move (out of focus, or system animations turned off on Android), a still ring around the shape says that someone is waiting.
- Press feedback: on touch down the shape squashes slightly (x 1.07, y 0.9, 0.12s ease out) and springs back once with a small overshoot (0.42s). While swiping sideways it leans 26dp toward the finger.
- An 8dp muted dot below the shape when another visible profile with notifications on has unread messages.
- Hold (320ms, or right click on desktop) to peek: a card above the shape with the first waiting chat: avatar, name, "Contact" / "Group, you were mentioned" / "You joined incognito", the latest message, and "Maya knows you as **Alex**" or "Jonas knows you as **MellowPilot**, incognito", next to a small mark in the profile's own shape (an indigo ring when incognito). The incognito name of a contact is loaded with `apiContactInfo` and is only shown for the chat it was loaded for.
  - The message is the last received item, shown like the chat list preview: nothing for deleted, moderated or live messages, and in a group set to mentions only the message that mentions you.
  - The card follows a chat, not a position, so a new message arriving while you hold does not swap the card or change which chat opens. Footer: "Slide up to reply" and "1 of N, slide sideways" or "Let go to close".
  - While holding: drag up past 80dp opens the chat (same functions as chat list rows, `directChatAction` / `groupChatAction`), drag sideways past 70dp shows the next waiting chat, release closes the card.
  - With nothing waiting, the card shows your profile, "This is who new chats will see" and "Swipe sideways to be <other profiles>".
- Swipe the still shape sideways (past 50dp, not after a hold) to switch to the next or previous visible profile. The switch is the same function the profile picker uses (`switchToUser`, moved out of `UserPicker` so both call it). One switch at a time; the new name shows briefly with "N waiting here" or "Nobody waiting here" only if the switch succeeded.
- Double tap opens the existing new chat sheet.
- Single tap (after the double tap timeout) shows "You are Alex" with "Nobody is waiting" or "Your other profile has messages", or "Hold to read" when chats are waiting.
- Pending contact requests (a contact request, or a contact whose request is still to be accepted): a small dot circles the shape (11s per turn, radius 104dp), and rests at the top when motion is off. Tapping it, or a single tap on the shape when nothing else is waiting, opens the existing contact request dialog, or the chat where that request is accepted.
- When chat is stopped, opening chats, requests, new chat and profile switching are disabled, as in the chat list and profile picker.

## Privacy rules

- No names or message text are shown until the user holds the shape.
- Notification preview mode "Hidden": the peek shows "Someone" with no avatar and no text, no incognito name, and "Let go to close" instead of "1 of N". Breathing always uses the 2.6s period and counts are replaced by "Someone is waiting", so nothing tells how many people wrote.
- Message text in the peek is shown only when chat previews are on and the notification preview mode shows messages.
- Hidden profiles never affect the shape, the dot, the swipe order or the profile names in the peek.
- Muted chats and groups without a mention do not make the shape breathe.

## Accessibility

- The shape is a button with a content description built from its state: who you are, how many are waiting (or "Someone is waiting" when previews are hidden), whether another profile has messages, and whether a contact request is waiting.
- Activating the shape opens the first waiting chat, or says who you are when nobody is waiting. The short message under the shape is a polite live region, so it is read out.
- Custom actions replace the gestures: Open next waiting chat, Switch profile, New chat, All chats.
- With system animations off (Android), the shape does not breathe and shows a still ring instead.
- "All chats" is a real button. The request dot is a 44dp button with its own description.

## What does not change

- With the toggle off (default) the chat list, toolbar and floating button are exactly as before.
- No core, database or protocol changes. No new dependencies, no new images.
- Strings are added only to `MR/base/strings.xml`; translations come through Weblate.

## Out of scope

- iOS. The same design can follow in SwiftUI as a separate change.
- Bot permission requests (only contact requests are shown).
- Any proprietary art or illustrations.
- Keyboard shortcuts on desktop beyond the accessibility actions.

## Files

- `views/chatlist/CalmHomeView.kt` (new): `CalmHomeView` reads `ChatModel` and passes plain values to the stateless `CalmHomeLayout`.
- `views/chatlist/ChatListView.kt`: `ChatListWithLoadingScreen` shows `CalmHomeView` instead of `ChatList` when the toggle is on, the floating button is hidden, and the toolbar gets the way back.
- `views/chatlist/UserPicker.kt`: the profile switch body moved into `switchToUser`, called by the picker and the calm home.
- `model/SimpleXAPI.kt`: `calmHome` preference, default false.
- `Appearance.android.kt`, `Appearance.desktop.kt`: the toggle.
- `MR/base/strings.xml`: new strings.

## How it was tested

- `:desktop:compileKotlinJvm` builds with no new warnings. The Android target was not compiled here (no Android SDK on the test machine); `Appearance.android.kt` has a one-line change identical to the desktop one.
- `CalmHomeLayout` was rendered headlessly with `ImageComposeScene` (throwaway harness, not committed) inside a content area with a toolbar gap, in light, dark, black and SimpleX themes: rest, mid breath, all four profile shapes, other profile dot, request orbit, peek for a direct chat, an incognito chat, a group mention, your own profile and hidden previews, the still ring without motion, the "You are Alex" message, the bottom toolbar layout and a phone in landscape. Sizes, colours, timings and copy were checked against the approved web prototype.
- `CalmHomeLayout` interactions were exercised with Compose Desktop UI tests (throwaway, not committed), using fake chats and recording callbacks: hold to peek and release to close, drag up to open the chat on the card, drag sideways to the next waiting chat, swipe the still shape to switch profile in both directions, double tap for new chat, single tap for "You are Alex" or "Hold to read", the request dot, "All chats", the content description and custom actions, and the peek with hidden previews ("Someone", no message text, no count) using the real `calmChatPeek`.
- The desktop app was built with `:desktop:createDistributable` for a manual try.
- Not yet checked on a device: Android touch, right click, real profile switching, the incognito name lookup, TalkBack, and both toolbar modes on Android. These need a run on a real phone and on desktop before review.
