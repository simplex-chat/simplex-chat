# Test: switching profile clears the chat list search in the SimpleX Chat Android app

You are testing the SimpleX Chat Android app, which runs in an Android emulator on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text on screen, one element per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1080×2400). The text comes from the app's accessibility tree, so it is spelled exactly, but it can list an element that another screen covers: when the text and the picture disagree, trust the picture. "Above" means a smaller y. `ui tree` also lists text fields and buttons that have no text.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- `ui type` types into the field that has the focus: tap a field before typing into it. `ui key BACK` is Android's back button.
- Besides the chats, the chat list shows the app's own cards as list items until they are closed: "New 1-time link / Paste link / Scan" (once there is a conversation), "Bottom bar / Top bar", and "Invest in SimpleX Chat!". They are part of the list, not something drawn over it: leave them open.
- The toolbar is at the bottom: the profile picture at its left end, "Start new chat" (the round button) in it. A profile without conversations sees the onboarding pages ("Talk to someone") in place of the list, without the search field.

## How to do things in the app

- **Create a group:** click "Start new chat", then "Create group". The name field under "Enter group name:" has the focus: type the name and click "Create group". Wait up to 30 s for "Invite members", which shows once the group exists. Then press `ui key BACK` one at a time, looking at the screen after each, until the chat list shows (the search field "Search or paste SimpleX link"): a BACK on the chat list itself leaves the app. If the app was left, `ui relaunch` restarts it and waits for the chat list.
- **Type in the search field:** click "Search or paste SimpleX link" and `ui type "<text>"`. While the keyboard is open, one `ui key BACK` only closes the keyboard; once it is closed, BACK clears the search field, and so does the field's clear button, and `ui relaunch` too. From typing until the next profile switch, press BACK at most once, only to close the keyboard when it covers the toolbar, and use neither the clear button nor `ui relaunch`. A typed word of 5 or more letters, such as `alpha`, also shows "Connect to alpha.testing" near the field: don't click it. Before switching profile, check that the field still shows the text.
- **Switch profile:** click the profile picture at the left end of the toolbar to open the profile picker. In `ui tree` the picture is a "profile image placeholder", like the pictures of the chats in the list (and perhaps of the picker while it is closed): take the one nearest the bottom left corner of the screen (y under 2400), check its place in the screenshot, and click its centre with `ui click`. The picker lists your profiles by name and offers "Your chat profiles": click the other profile's name. The chat list then shows that profile's chats.
- **Add a profile:** open the profile picker → "Your chat profiles" → "Add profile". On "Create profile" the name field under "Enter your name:" has the focus: type the name, and click the "Create profile" button below the fields (the lower of the two "Create profile" texts; the upper one is the title). The new profile becomes the active one; press `ui key BACK` one at a time until its chat list shows (for a profile without conversations: "Talk to someone").

## The test

The steps build on each other: run them in this order. Names are compared ignoring case.

**Onboarding.** Each screen waits for the one before it; wait for a text before clicking it.

1. The first screen has "Get started": click it.
2. Click the field "Enter profile name…", type `citester`, and click "Create profile".
3. On "Your network", click "Continue".
4. The app explains instant notifications in an alert: click "Allow". Android then asks whether the app may always run in the background: click "Allow" again.
5. Click "Accept" on the conditions screen.
6. Wait up to 60 s for the chat list, which shows "Talk to someone" while there are no conversations. Take a shot.

**P. Switching profile clears the search.**

1. (P1) As citester, create the group `ci alpha`. Check: the chat list lists "ci alpha" and shows the search field "Search or paste SimpleX link".
2. (P2) Add the profile `ciother` (it becomes the active profile), and as ciother create the group `ci beta`. Check: ciother's chat list lists "ci beta" and not "ci alpha".
3. (P3) Switch to citester. Type `alpha` in the search field. Check: the list shows "ci alpha".
4. (P4) With `alpha` still in the field, switch to ciother. Check: the search field is empty (it shows its placeholder "Search or paste SimpleX link"), the list shows "ci beta", and "No chats found" is not shown.
5. (P5) As ciother, type `beta` in the search field, then switch to citester. Check: the search field is empty and the list shows "ci alpha".
6. (P6) As citester, type `alpha` in the search field. Then add the profile `cinew`: it has no conversations, so it shows the onboarding pages "Talk to someone". Then switch back to citester. Check: the search field is empty and the list shows "ci alpha".

You have at most 200 turns, and the whole test fits in about 90 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Get started" -- click-text "Enter profile name" 30 -- type citester -- shot profile`
- `ui do click-text "Create profile" -- wait Continue 60 -- shot your-network`
- `ui do click-text "Start new chat" -- click-text "Create group" 10 -- wait "Enter group name" -- shot new-group`
- `ui do click-text "Search or paste SimpleX link" -- type alpha -- shot P3`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped. Take a shot at each check, named after its step (`ui shot P4`).

## Report

Write `report.md` in your working directory as you go, so a session that is cut short still leaves its results: one row for the onboarding and one per step (P1 to P6), each check you ran as PASS or FAIL with its evidence, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the checks that failed.

## The ui helper
