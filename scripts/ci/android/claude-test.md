# Test: onboarding, the chat list and sending images in the SimpleX Chat Android app

You are testing the SimpleX Chat Android app, which runs in an Android emulator on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text on screen, one element per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1080×2400). The text comes from the app's accessibility tree, so it is spelled exactly, but it can list an element that another screen covers: when the text and the picture disagree, trust the picture. "Above" means a smaller y. `ui tree` also lists text fields and buttons that have no text.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- `ui type` types into the field that has the focus: tap a field before typing into it. `ui key BACK` is Android's back button.
- Besides the chats, the chat list shows the app's own cards as list items until they are closed: "New 1-time link / Paste link / Scan" (once there is a conversation), "Bottom bar / Top bar", and "Invest in SimpleX Chat!". They are part of the list, not something drawn over it: leave them open.
- The emulator's Pictures folder has two images for part D: `test-card.jpg`, a TV test card 1280×720 (colour bars, a large circle, "1080" at the top and "TVB" at the bottom), and `simplex-logo.png`, the dark blue and turquoise SimpleX logo on a transparent background.

## The test

**A. Onboarding.** Each screen waits for the one before it; wait for a text before clicking it.

1. The first screen has "Get started": click it.
2. Click the field "Enter profile name…", type `citester`, and click "Create profile".
3. On "Your network", click "Continue".
4. The app explains instant notifications in an alert: click "Allow". Android then asks whether the app may always run in the background: click "Allow" again.
5. Click "Accept" on the conditions screen.
6. Wait up to 60 s for the chat list, which shows "Talk to someone" while there are no conversations. Take a shot.

**B. A group, and the chat list with the toolbar at the bottom.**

1. Click "Start new chat" (the round button in the bottom toolbar), then "Create group". The name field under "Enter group name:" has the focus: type `seeded notes` and click "Create group". Wait for "Invite members", which shows once the group exists.
2. Run `ui relaunch`: it restarts the app and waits for the chat list. Take a shot.
3. Check, noting the evidence (screenshot names, positions):
   - "seeded notes" is listed;
   - the search field "Search or paste SimpleX link" is in the bottom half of the screen (y over 1200), and "Add list" is just above it;
   - nothing is cut off, overlapping or garbled.

**Between the layouts:** run `ui layout top`: it restarts the app with the toolbar at the top.

**C. The chat list, toolbar at the top.** Check, noting the evidence:

- "seeded notes" is still listed;
- the search field is in the top half of the screen (y under 1200), and "Add list" is just below it;
- nothing is cut off, overlapping or garbled. "Start new chat" is now a floating button at the bottom right, drawn over whatever the list shows there: note what it covers, but that alone is not a failure.

**D. Sending images in "seeded notes".**

1. Click "seeded notes" to open the group. Click "Attach" (left of the message field), then "Image". Android opens a picker that is not part of the app (the photo picker, or Files): find `test-card.jpg` (under Pictures, Images or Recent; its thumbnail shows the test card) and choose it, then confirm the choice if the picker has a button for it ("Add", "Select", "Done").
2. Back in the app, the compose area shows the image above the message field before it is sent. Take a shot. Check: the preview shows the whole test card, upright, not stretched.
3. Click "Send Message". Check: the chat shows the sent test card. Click the image: it opens full screen, and "1080" and "TVB" are sharp and readable. Take a shot, then `ui key BACK`.
4. Repeat 1 to 3 with `simplex-logo.png`. Check: the preview and the sent image show the whole logo, and the area around its letters shows the chat's background, not black or grey.
5. Check that the chat shows both images after `ui relaunch` and opening "seeded notes" again.

You have at most 90 turns, and the whole test fits in about 45 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Get started" -- click-text "Enter profile name" 30 -- type citester -- shot profile`
- `ui do click-text "Create profile" -- wait Continue 60 -- shot your-network`
- `ui do click-text "Start new chat" -- click-text "Create group" 10 -- wait "Enter group name" -- shot new-group`
- `ui do relaunch -- shot chat-list-bottom`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped.

## Report

Write `report.md` in your working directory: for each of A, B, C and D, each check you ran as PASS or FAIL with its evidence, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the checks that failed.

## The ui helper
