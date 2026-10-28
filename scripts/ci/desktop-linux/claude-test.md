# Test: switching profile clears the chat list search in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words and can't read icons; when the text and the picture disagree, trust the picture. "Above" means a smaller y.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- After onboarding, the chat list is in the left pane (x 1 to 389) and chats open in the right pane. An empty chat list shows only its toolbar, with the profile picture at about 27,730 and a pencil (New chat) at about 363,730: the search field "Search or paste SimpleX link" appears once there is a chat. The toolbar is at the bottom of the left pane.
- Check the chat list and the search field in the left pane only: the chat open in the right pane can show the same names.

## How to do things in the app

- **Create a group:** the pencil (about 363,730) → "Create group". Click the field about 40 px below the text "Enter group name", type the name, and click "Create group" (the button below the field). If "Skip inviting members" shows, click it. Press Escape until the chat list shows; the group stays open in the right pane, since Escape doesn't close a chat.
- **Type in the search field:** click "Search or paste SimpleX link" and `ui type "<text>"`. Escape clears the field, and so does the clear button at its right end: from typing until the next profile switch, press no Escape and don't click that button. A typed word of 5 or more letters, such as `alpha`, also shows "Connect to alpha.testing" near the field: don't click it. Before switching profile, check that the field still shows the text.
- **Switch profile:** click the profile picture (about 27,730); the menu lists your other profiles by name: click the one you want. The chat list then shows that profile's chats.
- **Add a profile:** profile picture → "Your chat profiles" → "Add profile". On "Create profile", click the field about 40 px below the text "Enter your name:", type the name, and click the "Create profile" button (the lower of the two "Create profile" texts; the upper one is the title). The new profile becomes the active one; press Escape until its chat list shows (for a profile without chats, the right pane shows "Talk to someone").

## The test

The steps build on each other: run them in this order. Names are compared ignoring case.

1. (S1) Onboarding. Each screen waits for the one before it; wait for a text before clicking it. The first screen has "Create your profile": click it. On "Create profile", click the field "Enter profile name", type `citester`, and click "Create profile". The next screen offers a database passphrase: click "Use random passphrase", then "OK" in the alert that follows. Click "Continue", then "Accept" on the screen after it. Wait up to 60 s for the chat list: the right pane shows "Talk to someone".
2. (P1) As citester, create the group `ci alpha`. Check: the chat list lists "ci alpha" and shows the search field "Search or paste SimpleX link".
3. (P2) Add the profile `ciother` (it becomes the active profile), and as ciother create the group `ci beta`. Check: ciother's chat list lists "ci beta" and not "ci alpha".
4. (P3) Switch to citester. Type `alpha` in the search field. Check: the list shows "ci alpha".
5. (P4) With `alpha` still in the field, switch to ciother. Check: the search field is empty (it shows its placeholder "Search or paste SimpleX link"), the list shows "ci beta", and "No chats found" is not shown.
6. (P5) As ciother, type `beta` in the search field, then switch to citester. Check: the search field is empty and the list shows "ci alpha".
7. (P6) As citester, type `alpha` in the search field. Then add the profile `cinew`: it has no chats, so the right pane shows "Talk to someone". Then switch back to citester. Check: the search field is empty and the list shows "ci alpha".

You have at most 200 turns, and the whole test fits in about 80 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Create your profile" -- wait "Enter profile name" -- click-text "Enter profile name" -- type citester -- shot profile`
- `ui do click 363 730 -- click-text "Create group" 10 -- wait "Enter group name" -- shot new-group`
- `ui do click-text "Search or paste SimpleX link" -- type alpha -- shot P3`
- `ui do click 27 730 -- click-text "Your chat profiles" 10 -- click-text "Add profile" 10 -- wait "Enter your name" -- shot add-profile`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped. Take a shot at each check, named after its step (`ui shot P4`).

## Report

Write `report.md` in your working directory as you go, so a session that is cut short still leaves its results: one row per step (S1, P1 to P6), each check you ran as PASS or FAIL with its evidence (the text OCR read, screenshot names), and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the steps that failed.

## The ui helper
