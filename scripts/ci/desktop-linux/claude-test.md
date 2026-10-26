# Test: channel message signing and channel moderators in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words and can't read icons; when the text and the picture disagree, trust the picture. "Above" means a smaller y.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- After onboarding, the chat list is in the left pane (x 1 to 389) and chats open in the right pane. An empty chat list shows only its toolbar, with the profile picture at about 27,730 and a pencil (New chat) at about 363,730: the search field "Search or paste SimpleX link" appears once there is a chat. The toolbar is at the bottom of the left pane.
- The channel uses the app's preset chat relays, on the internet. Creating it and joining it wait for them: wait up to 120 s there (`ui wait "<text>" 120`).

## How to do things in the app

- **Send a message:** click the message field at the bottom of the right pane (its placeholder is "Broadcast" for the channel's owner, "Message" for others), `ui type "<text>"`, then `ui key Return`. Check the message then shows in the chat.
- **Channel info:** click the channel name at the top of the right pane. Press Escape to close it and other panes (preferences, member info, a message's info) and get back to the chat.
- **Message menu:** `ui right-click <x> <y>` on the message's text (centre from `ui find`) opens its menu ("Info", "Moderate", …).
- **Signatures are read as text, never from the seal icons:** a message's "Info" shows "Signed" (your own message), "Signed & verified" (someone else's, checked) or "Signature missing".
- **Switch profile:** click the profile picture (about 27,730); the menu lists your other profiles by name: click the one you want. The chat list then shows that profile's chats; click "ci signing" in it.
- **Add a profile:** profile picture → "Your chat profiles" → "Add profile". On "Create profile", click the field about 40 px below the text "Enter your name:", type the name, and click the "Create profile" button (the lower of the two "Create profile" texts; the upper one is the title). The new profile becomes the active one; press Escape until the chat list shows.
- **Join by the copied link** (from a profile without chats): the pencil (about 363,730) → "Paste link" → "Tap to paste link". An alert names the channel with "Open channel": click it. The channel opens with "Tap Join channel" and a "Join channel" bar at the bottom: click the bar, and wait up to 120 s until the bottom of the chat shows "you are subscriber".

## The test

The parts build on each other: run them in this order. cisub must join only after cimod has become a moderator (B2), so that cimod learns about cisub.

**S. Setup.**

1. (S1) Onboarding. Each screen waits for the one before it; wait for a text before clicking it. The first screen has "Create your profile": click it. On "Create profile", click the field "Enter profile name", type `citester`, and click "Create profile". The next screen offers a database passphrase: click "Use random passphrase", then "OK" in the alert that follows. Click "Continue", then "Accept" on the screen after it. Wait up to 60 s for the chat list: the right pane shows "Talk to someone".
2. (S2) Click the pencil, then "Create public channel". Click the field about 40 px below the text "Channel name", type `ci signing`, and click the "Create public channel" button (the lower of the two texts with that name). Wait up to 120 s for "Share link": the channel opens and its "Channel link" pane shows over it. If "N/M relays active" stays below M for 120 s with N at least 1, click "Continue", and "Continue" in the alert. On "Channel link", click "Continue". Check: the chat "ci signing" is open.
3. (S3) Send `before signing`. Check: it shows in the chat.

**A. "Sign messages" (as citester).**

1. (A1) Channel info → "Channel preferences". Check: the first row is "Sign messages", and its footer says "Do not require signing messages." (the switch is off).
2. (A2) Click the switch of "Sign messages", at the right end of its row. Check: the alert "Sign messages?" says "Messages sent before will show "signature missing" to subscribers who join later." and has "Enable" and "Cancel". Click "Cancel". Check: the footer says "Do not require signing messages." again.
3. (A3) Click the switch again, then "Enable". Check: the footer says "Require signing messages.". Click "Save and notify channel subscribers", then press Escape until the chat shows. Check: the chat shows "Sign messages: on".
4. (A4) Send `after signing`, right-click it → "Info". Check: the info says "Signed".
5. (A5) Channel info → "Channel preferences". Check: the footer of "Chat with admins" says "Prohibit chats with admins." (saving in A3 did not turn it on). Press Escape until the chat shows.

**J. A subscriber joins.**

1. (J1) Channel info → "Channel link" → "Share link". Check: the toast "Copied to clipboard". Press Escape until the chat list shows.
2. (J2) Add the profile `cimod`, and as cimod join by the copied link. Check: "you are subscriber".
3. (A6) As cimod in "ci signing": click the message "before signing". Check: the alert "Signature missing" says "The message may have been sent before the channel required signing, or a relay forwarded it unsigned." Close it, right-click "before signing" → "Info". Check: "Signature missing". Then right-click "after signing" → "Info". Check: "Signed & verified".

**B. A moderator.**

1. (B1) Switch to citester → "ci signing" → channel info → "Subscribers" → "cimod". Click the "Change role" row. Check: the list offers exactly "subscriber", "contributor" and "moderator" (no admin, no owner).
2. (B2) Click "moderator", then "Change" in the alert "Change role?". Check: cimod's role shows moderator, and the section above it is titled "Moderator". Press Escape until the chat shows. Check: the chat shows "you changed role of cimod to moderator".
3. (B3) Switch to cimod → "ci signing". Check: the chat shows "changed your role to moderator". Send `moderator post`. Check: it shows in the chat.
4. (J3) Add the profile `cisub`, and as cisub join by the same link (it is still on the clipboard; if not, copy it again as citester as in J1). Check: "you are subscriber".
5. (J4) Switch to citester → "ci signing" → channel info → "Subscribers" → "cisub" → "Change role" → "contributor" → "Change". Check: cisub's role shows contributor, in the section titled "Contributor".
6. (J5) Switch to cisub → "ci signing". Send `contributor post`. Check: it shows in the chat.
7. (B4) Switch to cimod → "ci signing" → channel info. Check: it shows "Subscribers"; click it: the list includes "cisub".
8. (B5) Click "cisub". Check: its info offers "Block for all" (don't click it yet). Press Escape until the chat shows.
9. (B6) Right-click "contributor post" → "Moderate" → "Delete" in the alert "Delete member message?". Check: the message shows "moderated by cimod" in its place.
10. (B7) Channel info → "Subscribers" → "cisub" → "Block for all" → "Block for all" in the alert "Block subscriber for all?". Check: cisub's info now offers "Unblock for all". Press Escape until the chat shows. Check: the chat shows "you blocked cisub".

You have at most 300 turns, and the whole test fits in about 150 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Create your profile" -- wait "Enter profile name" -- click-text "Enter profile name" -- type citester -- shot profile`
- `ui do click 363 730 -- click-text "Create public channel" 10 -- wait "Channel name" -- shot new-channel`
- `ui do click-text "Broadcast" -- type "after signing" -- key Return -- wait "after signing" -- shot after-signing`
- `ui do click 27 730 -- click-text "Your chat profiles" 10 -- click-text "Add profile" 10 -- wait "Enter your name" -- shot add-profile`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped. Take a shot at each check, named after its step (`ui shot A2`).

## Report

Write `report.md` in your working directory as you go, so a session that is cut short still leaves its results: one row per step (S1 to S3, A1 to A5, J1, J2, A6, B1 to B3, J3 to J5, B4 to B7), each check you ran as PASS or FAIL with its evidence (the text OCR read, screenshot names), and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the steps that failed.

## The ui helper
