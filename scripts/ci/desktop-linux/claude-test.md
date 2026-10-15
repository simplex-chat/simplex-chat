# Test: directory search in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words; when the text and the picture disagree, trust the picture.

## What is set up

- The app has a profile and shows its chat list. The toolbar and the search field "Search or paste SimpleX link" are at the bottom of the left pane: the default "Reachable app toolbars" layout.
- The app uses a test directory that lists a group, #ci_group ("CI seed group", red avatar), and a channel, #ci_channel ("CI seed channel", green avatar). Their avatars are as large as the app makes them, so the directory returns one of them per response: a search first shows one result and "Show more", which loads the other.

## The test

Do it twice: first in the default layout, then with the toolbar at the top.

1. Click the search field and type `ci seed`. A row "Search in Directory" appears; click it.
2. The alert "Search in Directory?" asks before searching. The first time, click "Search"; the second time, "Search and don't show again".
3. Wait for results, up to 60 s. If "Search failed, tap to retry" shows, tap it once and note it.
4. Use "Show more" until both #ci_group and #ci_channel are listed, and note how many pages that took.
5. Check, noting the evidence (screenshot names, positions):
   - each result's row shows its name, its description, its member or subscriber count, and "Join group" (#ci_group) or "Join channel" (#ci_channel);
   - each shows its avatar: run `ui avatar "#ci_group" red` and `ui avatar "#ci_channel" green`, and look at the screenshot;
   - the "Directory" header is above its results;
   - no "Show more" is left, and nothing is cut off, overlapping or garbled.
6. After the first layout: press Escape to clear the search. Click the profile picture at the left end of the bottom toolbar (about 27,730) to open the menu, open "Settings", then "Appearance", and turn off the switch of "Reachable app toolbars", at the right end of its row. Press Escape until the chat list shows again; its search field should now be at the top. Then repeat steps 1 to 5.

You have at most 60 turns. Chain steps whose outcome you can predict with `ui do`, for example `ui do click-text "Show more" -- wait "#ci_channel" 60 -- shot top-page-2`, and look at the screen after each chain.

## Report

Write `report.md` in your working directory: for each layout, each check of step 5 as PASS or FAIL with its evidence, the number of pages and retries, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check passed in both layouts, otherwise `CLAUDE-TEST: FAIL` followed by the checks that failed.

## The ui helper
