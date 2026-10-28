# Test: online search in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words; when the text and the picture disagree, trust the picture. "Above" means a smaller y.

## What is set up

- The app is already running, past onboarding and set up as below: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- The app has a profile and one chat, a group of its own called "seeded notes". The chat list is in the left pane (x 1 to 389). Its toolbar and the search field "Search or paste SimpleX link" are at the bottom: the default "Reachable app toolbars" layout.
- A test directory lists a group, #ci_group ("CI seeded group", a red avatar with a white G), and a channel, #ci_channel ("CI seeded channel", a green avatar with a white C). Their avatars are as large as the app makes them, so the directory returns one of them per response: a search first shows one result and the load row "Load more · 1 left" (OCR may misread the middle dot), which loads the other.
- A test server resolves the SimpleX name `seeded.testing` to the address of a contact, ci_contact ("CI seeded contact", a blue avatar with a white N), and the app uses only that server to resolve names. A search never resolves names: only "Connect to …" does.
- Directory search is off until the first search's alert enables it (Settings > Privacy & security > "Directory search").

## The test

Run all of A and B in the default layout, with the toolbar at the bottom. Then move the toolbar to the top and repeat A and B with only the checks marked **(both layouts)**: the two layouts lay the list out differently, but draw the rows themselves the same way.

**A. Search in Directory.**

1. Click the search field and type `ci seed`. A row "Search in Directory" appears. Take a shot: in the first layout, check that the chat list still shows its banner "Invest in SimpleX Chat", the "Bottom bar" / "Top bar" card and the "New 1-time link" / "Paste link" cards. Then click "Search in Directory".
2. In the first layout the alert "Enable Directory search?" asks first, with "Cancel" and "Enable". Click "Cancel": nothing is searched (no results, and "Search in Directory" still shows). Click "Search in Directory" again and then "Enable". In the second layout no alert may show.
3. Wait for results, up to 60 s. If "Search failed, tap to retry" shows, tap it once and note it.
4. After the first page, note the load row's text ("Load more" and how many are left). Use it until both #ci_group and #ci_channel are listed, and note how many pages that took.
5. Check, noting the evidence (screenshot names, positions):
   - each result's row shows its name, its description, its count ("2 members" or "1 subscriber") and "Open" at its right end;
   - the count's text is smaller than the description's above it: compare `ui ink "1 subscriber"` with `ui ink "CI seeded channel"`, and look at the screenshot;
   - each shows its avatar, filling the picture: run `ui avatar "#ci_group" red` and `ui avatar "#ci_channel" green`, and look at the screenshot;
   - that banner and those cards are hidden while the results show;
   - **(both layouts)** one list without headers: no "Directory" or "Names" header, and no "Search in Directory" while the results show;
   - **(both layouts)** no load row is left at the end, and nothing is cut off, overlapping or garbled.
6. In the first layout, right-click #ci_group (`ui right-click <x> <y>` at its name): a menu with "Open" shows. Take a shot, then press Escape to close it without choosing Open.
7. Press Escape to clear the search field. In the first layout, take a shot and check that the banner and the cards show again.
8. In the first layout, click the search field and type `zqxvjk`, which no chat and no listing matches. Before searching, take a shot and check that "Search in Directory" is offered and "No chats found" is not shown anywhere. Click "Search in Directory", wait about 12 s, and take a shot: no results, and no "Search in Directory". "No chats found" doesn't show here even then, because the chat open in the right pane ("seeded notes") stays listed. Note both screenshot names, then press Escape to clear the search field.

**B. The Search key.**

1. Click the search field and type `seeded`. A row "Connect to seeded.testing" appears.
2. Press Enter (`ui key Return`): on desktop it is the Search key. No alert may show (Directory search is on).
3. Wait up to 60 s for the results, and use the load row until both directory entries are listed.
4. Check, noting the evidence:
   - **(both layouts)** the local chat "seeded notes" and the directory's #ci_group and #ci_channel, each with "Open"; no "Names" header and no row for ci_contact, as the search resolves no names;
   - **(both layouts)** "Connect to seeded.testing" stays shown next to the results;
   - **(both layouts)** the order: with the toolbar at the bottom the list reads upwards from the search field (the local chat nearest the field, then the results); with the toolbar at the top it reads downwards from the field in the same order.
5. Press Escape to clear the search field.

**Between the layouts:** click the profile picture at the left end of the bottom toolbar (about 27,730) to open the menu, open "Settings", then "Appearance", and turn off the switch of "Reachable app toolbars", at the right end of its row (about x 335). Press Escape until the chat list shows again; its search field should now be at the top.

**C. Opening from the results, and no duplicates.** Run it once, at the end, in the layout you are in then (the layouts draw these the same way). Clear the search field (Escape) before each step.

1. Search `ci seed` with "Search in Directory" (no alert asks). Click "Open" in the #ci_channel row: the channel opens in the right pane, not yet joined, with "Tap Join channel", its subscriber count both under its name in the toolbar and in the banner, and a "Join channel" button, without any alert ("Open channel" must not show on the way). Click "Join channel" and wait up to 60 s for "you are subscriber". Without clearing the search, take a shot: the chat list now lists ci_channel (the chat open in the right pane), so the results must no longer list #ci_channel (#ci_group stays). Then press Escape until the chat list shows (the channel may stay open in the right pane).
2. Type `ci_channel` and press Enter, and wait about 12 s for the search to finish (nothing on the screen marks its end): the chat list shows your chat ci_channel, and no result #ci_channel and no note about the page; only the load row shows if there are more pages.
3. Click "seeded notes" in the chat list, so that it, not ci_channel, is the chat open in the right pane. Type `seeded` and click "Search in Directory". A name narrows the chat list to the chats with that name and the chat open in the right pane, so the list doesn't show ci_channel by itself. Among the results (use the load row until #ci_group shows) ci_channel is a chat row like the ones in the chat list: its name without "#", its last message and time, and no "Open"; #ci_group keeps "Open". There is no "Names" header. Click the ci_channel row: no alert (no "You are a subscriber"), and the channel opens in the right pane.
4. Type `zqxvjk` and press Enter: once the search finishes, no results. On desktop the chat open in the right pane stays listed, so "No chats found" may not show.
5. Type `cinone`: a row "Connect to cinone.testing" shows. Press Enter: no alert about the name, no "Names" header, and "Connect to cinone.testing" stays; the search finds no directory entries.
6. Open New chat with the pencil: at the right end of the bottom toolbar (about 363,730) when the toolbar is at the bottom; with the toolbar at the top, a round blue button at the bottom right of the chat list pane (about 341,710). Type `ci seed` in its search field and click "Search in Directory". In its results, one group without a header, with the contacts that match first (use the load row until #ci_group shows), ci_channel is a chat row like a contact's (its name without "#", no "Open"), and #ci_group keeps "Open". Close the sheet: Escape clears its search, then its back arrow (top left of the sheet) closes it.

You have at most 120 turns, and the whole test fits in about 80 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example, in the first layout:

- `ui do click-text "Search or paste SimpleX link" -- type "ci seed" -- wait "Search in Directory" -- shot bottom-typed`
- `ui do click-text "Search in Directory" -- wait "Enable Directory search" -- click-text "Cancel" -- shot bottom-consent-cancelled`
- `ui do click-text "Search in Directory" -- wait "Enable Directory search" -- click-text "Enable" -- wait "Open" 60 -- shot bottom-directory-1`
- `ui do click-text "Load more" -- wait "#ci_group" 60 -- shot bottom-directory-2` (wait for whichever result is missing)
- `ui do avatar "#ci_group" red -- avatar "#ci_channel" green`
- `ui do key Escape -- click-text "Search or paste SimpleX link" -- type seeded -- wait "Connect to seeded.testing" -- key Return -- wait "Open" 60 -- shot bottom-search-key-1`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped.

## Report

Write `report.md` in your working directory: for each layout and each of A and B, and for C, each check you ran as PASS or FAIL with its evidence, the number of pages and retries, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the checks that failed.

## The ui helper
