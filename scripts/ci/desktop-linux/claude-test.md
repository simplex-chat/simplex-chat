# Test: online search in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words; when the text and the picture disagree, trust the picture. "Above" means a smaller y.

## What is set up

- The app has a profile and one chat, a group of its own called "seeded notes". The chat list is in the left pane (x 1 to 389). Its toolbar and the search field "Search or paste SimpleX link" are at the bottom: the default "Reachable app toolbars" layout.
- A test directory lists a group, #ci_group ("CI seeded group", a red avatar with a white G), and a channel, #ci_channel ("CI seeded channel", a green avatar with a white C). Their avatars are as large as the app makes them, so the directory returns one of them per response: a search first shows one result and "Show more", which loads the other.
- A test server resolves the SimpleX name `seeded.testing` to the address of a contact, ci_contact ("CI seeded contact", a blue avatar with a white N), and the app uses only that server to resolve names.

## The test

Run all of A and B in the default layout, with the toolbar at the bottom. Then move the toolbar to the top and repeat A and B with only the checks marked **(both layouts)**: the two layouts lay the list out differently, but draw the rows themselves the same way.

**A. Search in Directory.**

1. Click the search field and type `ci seed`. A row "Search in Directory" appears. Take a shot: in the first layout, check that the chat list still shows its banner "Invest in SimpleX Chat", the "Bottom bar" / "Top bar" card and the "New 1-time link" / "Paste link" cards. Then click "Search in Directory".
2. The alert "Search in Directory?" asks before searching. In the first layout click "Search"; in the second, "Search and don't show again".
3. Wait for results, up to 60 s. If "Search failed, tap to retry" shows, tap it once and note it.
4. Use "Show more" until both #ci_group and #ci_channel are listed, and note how many pages that took.
5. Check, noting the evidence (screenshot names, positions):
   - each result's row shows its name, its description, its count ("2 members" or "1 subscriber") and "Join group" (#ci_group) or "Join channel" (#ci_channel);
   - the count's text is smaller than the description's above it: compare `ui ink "1 subscriber"` with `ui ink "CI seeded channel"`, and look at the screenshot;
   - each shows its avatar, filling the picture: run `ui avatar "#ci_group" red` and `ui avatar "#ci_channel" green`, and look at the screenshot;
   - that banner and those cards are hidden while the results show;
   - **(both layouts)** the "Directory" header is above its results;
   - **(both layouts)** no "Show more" is left, and nothing is cut off, overlapping or garbled.
6. Press Escape to clear the search field. In the first layout, take a shot and check that the banner and the cards show again.

**B. The full search.**

1. Click the search field and type `seeded`. A row "Connect to seeded.testing" appears.
2. Press Enter (`ui key Return`): on desktop it is the Search key. In the first layout the same alert asks first: click "Search". In the second layout it should not ask.
3. Wait up to 60 s for the results, and use "Show more" until both directory entries are listed.
4. Check, noting the evidence:
   - **(both layouts)** three parts: the local chat "seeded notes"; a "Names" header with one row for ci_contact, showing `(@seeded.testing)` and "CI seeded contact"; and the "Directory" header with #ci_group and #ci_channel;
   - "Connect to seeded.testing" is gone once the name is found;
   - ci_contact shows its avatar: `ui avatar "ci_contact" blue`;
   - **(both layouts)** each header is above all of its part's rows;
   - **(both layouts)** the order of the parts: with the toolbar at the bottom the list reads upwards from the search field (the local chat nearest the field, then Names, then Directory furthest up); with the toolbar at the top it reads downwards from the field in the same order.
5. Press Escape to clear the search field.

**Between the layouts:** click the profile picture at the left end of the bottom toolbar (about 27,730) to open the menu, open "Settings", then "Appearance", and turn off the switch of "Reachable app toolbars", at the right end of its row (about x 335). Press Escape until the chat list shows again; its search field should now be at the top.

You have at most 60 turns, and the whole test fits in about 40 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example, in the first layout:

- `ui do click-text "Search or paste SimpleX link" -- type "ci seed" -- wait "Search in Directory" -- shot bottom-typed`
- `ui do click-text "Search in Directory" -- wait "Search and don't show again" -- click-text "Search" -- wait "Join" 60 -- shot bottom-directory-1`
- `ui do click-text "Show more" -- wait "#ci_group" 60 -- shot bottom-directory-2` (wait for whichever result is missing)
- `ui do avatar "#ci_group" red -- avatar "#ci_channel" green`
- `ui do key Escape -- click-text "Search or paste SimpleX link" -- type seeded -- wait "Connect to seeded.testing" -- key Return -- wait "Search and don't show again" -- click-text "Search" -- wait ci_contact 60 -- shot bottom-full-1`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped.

## Report

Write `report.md` in your working directory: for each layout and each of A and B, each check you ran as PASS or FAIL with its evidence, the number of pages and retries, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the checks that failed.

## The ui helper
