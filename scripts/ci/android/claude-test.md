# Test: chat list cards and banners while searching in the SimpleX Chat Android app

You are testing the SimpleX Chat Android app, which runs in an Android emulator on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text on screen, one element per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1080×2400). The text comes from the app's accessibility tree, so it is spelled exactly, but it can list an element that another screen covers: when the text and the picture disagree, trust the picture. "Above" means a smaller y. `ui tree` also lists text fields and buttons that have no text.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- `ui type` types into the field that has the focus: tap a field before typing into it. `ui key BACK` is Android's back button.
- Besides the chats, the chat list shows the app's own cards as list items until they are closed: "New 1-time link / Paste link / Scan" (once there is a conversation), "Bottom bar / Top bar", and "Invest in SimpleX Chat!". They are part of the list, not something drawn over it: leave them open.
- The search field's clear button is a "Close button" in the field's row, shown only while the field has text; it also closes the keyboard. The cards have close buttons with the same name: never tap those, which would close a card for good. Take the clear button's position from `ui text`, the "Close button" at the same y as the field's text, and tap it with `ui click <x> <y>`.

## The test

The steps build on each other: run them in this order.

**Onboarding.** Each screen waits for the one before it; wait for a text before clicking it. The first screen has "Get started": click it. Click the field "Enter profile name…", type `citester`, and click "Create profile". On "Your network", click "Continue". The app explains instant notifications in an alert: click "Allow". Android then asks whether the app may always run in the background: click "Allow" again. Click "Accept" on the conditions screen. Wait up to 60 s for the chat list, which shows "Talk to someone" while there are no conversations.

1. (C1) Click "Start new chat" (the round button in the bottom toolbar), then "Create group". The name field under "Enter group name:" has the focus: type `ci cards` and click "Create group". Wait for "Invite members", which shows once the group exists, then run `ui relaunch`: it restarts the app and waits for the chat list. Check: "ci cards" is listed, with the search field "Search or paste SimpleX link" below it; note which of the cards show: the picker ("Bottom bar", "Top bar"), the connect card ("New 1-time link", "Paste link / Scan") and "Invest in SimpleX Chat!". At least the picker or the connect card must show; if neither does, C1 fails and the later checks prove nothing.
2. (C2) Click the search field and type `ci`. Check: "ci cards" is still listed; none of the cards noted in C1 shows.
3. (C3) Clear the field with its clear button. Check: the field shows "Search or paste SimpleX link" again, and the cards noted in C1 are back.
4. (C4) Click the search field and type `zzz`. Check: "No chats found", and no cards or banners.
5. (C5) Clear the field. Run `ui layout top`: it restarts the app with the toolbar at the top. Check: the search field is in the top half of the screen (y under 1200), and "ci cards" is listed; note which cards show. Then repeat C2 and C3 there. Check: the same as C2 and C3.

You have at most 150 turns, and the whole test fits in about 40 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Get started" -- click-text "Enter profile name" 30 -- type citester -- shot profile`
- `ui do click-text "Create profile" -- wait Continue 60 -- shot your-network`
- `ui do click-text "Start new chat" -- click-text "Create group" 10 -- wait "Enter group name" -- shot new-group`
- `ui do click-text "Search or paste SimpleX link" -- type ci -- shot C2-reachable`
- `ui do layout top -- shot chat-list-top`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped. Take a shot at each check, named after its step and layout (`ui shot C2-reachable`).

## Report

Write `report.md` in your working directory as you go, so a session that is cut short still leaves its results: one row per step (C1 to C5), each check you ran as PASS or FAIL with its evidence, the cards C1 and C5 noted, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the steps that failed.

## The ui helper
