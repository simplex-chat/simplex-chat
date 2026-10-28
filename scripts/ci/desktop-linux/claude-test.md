# Test: chat list cards and banners while searching in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words and can't read icons; when the text and the picture disagree, trust the picture. "Above" means a smaller y.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- After onboarding, the chat list is in the left pane (x 1 to 389) and chats open in the right pane. An empty chat list shows only its toolbar, with the profile picture at about 27,730 and a pencil (New chat) at about 363,730: the search field "Search or paste SimpleX link" appears once there is a chat. In the default "Reachable app toolbars" layout, the toolbar and the search field are at the bottom of the left pane.
- Besides the chats, the chat list shows the app's own cards as list items, in the left pane, until they are closed: the toolbar picker "Bottom bar / Top bar", the connect card "New 1-time link / Paste link" (once there is a conversation), and the crowdfunding banner "Invest in SimpleX Chat!". They are part of the list, not something drawn over it: leave them open, and never click their x.
- The search field's clear button is an x at the right end of the field, shown only while the field has text. OCR doesn't read it: take its position from the screenshot. Escape also clears the field.

## The test

The steps build on each other: run them in this order.

**Onboarding.** Each screen waits for the one before it; wait for a text before clicking it. The first screen has "Create your profile": click it. On "Create profile", click the field "Enter profile name", type `citester`, and click "Create profile". The next screen offers a database passphrase: click "Use random passphrase", then "OK" in the alert that follows. Click "Continue", then "Accept" on the screen after it. Wait up to 60 s for the chat list: the right pane shows "Talk to someone".

1. (C1) Click the pencil (about 363,730) to open New chat, then "Create group". Click the name field, about 40 px below the text "Enter group name", type `ci cards`, and click "Create group". If "Skip inviting members" shows, click it. The group opens in the right pane, and stays there: press Escape until the left pane shows the chat list again. Check: "ci cards" is listed in the left pane, with the search field below the list; note which of the cards show in the left pane: the picker ("Bottom bar", "Top bar"), the connect card ("New 1-time link", "Paste link") and "Invest in SimpleX Chat!". At least the picker or the connect card must show; if neither does, C1 fails and the later checks prove nothing.
2. (C2) Click the search field and type `ci`. Check: "ci cards" is still listed; none of the cards noted in C1 shows in the left pane.
3. (C3) Clear the field with its x. Check: the field shows "Search or paste SimpleX link" again, and the cards noted in C1 are back.
4. (C4) Click the search field and type `zzz`. Check: the left pane shows "No chats found", and no cards or banners.
5. (C5) Clear the field. Switch to the top layout: click the profile picture (about 27,730) to open the menu, open "Settings", then "Appearance", and turn off the switch of "Reachable app toolbars", at the right end of its row (about x 335). Press Escape until the chat list shows again. Check: the search field is at the top of the left pane (y under 200), and "ci cards" is listed; note which cards show. Then repeat C2 and C3 there. Check: the same as C2 and C3.

You have at most 150 turns, and the whole test fits in about 50 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Create your profile" -- wait "Enter profile name" -- click-text "Enter profile name" -- type citester -- shot profile`
- `ui do click-text "Create profile" -- wait "Use random passphrase" 60 -- shot passphrase`
- `ui do click 363 730 -- click-text "Create group" 10 -- wait "Enter group name" -- shot new-group`
- `ui do click-text "Search or paste SimpleX link" -- type ci -- shot C2-bottom`
- `ui do click 27 730 -- click-text Settings 10 -- click-text Appearance 10 -- wait "Reachable app toolbars" -- shot appearance`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped. Take a shot at each check, named after its step and layout (`ui shot C2-bottom`).

## Report

Write `report.md` in your working directory as you go, so a session that is cut short still leaves its results: one row per step (C1 to C5), each check you ran as PASS or FAIL with its evidence (the text OCR read, screenshot names), the cards C1 and C5 noted, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the steps that failed.

## The ui helper
