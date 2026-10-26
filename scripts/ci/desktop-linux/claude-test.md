# Test: onboarding, the chat list and sending images in the SimpleX Chat desktop app

You are testing the SimpleX Chat desktop app, which runs on a virtual display on this machine. Drive it only with the `ui` helper, through Bash, one command per call: `ui <command> [args]`. Anything else is refused; don't look for ways around that, and if something blocks the test, report it. The commands are listed at the end.

Your working directory is where your output goes. `ui shot <name>` saves `screens/NN-<name>.png` and prints the text read from it, one phrase per line after its centre `x,y` in screen pixels. Read the PNG to see the screen (1440×900, the app window at the top left). OCR can misread or split words; when the text and the picture disagree, trust the picture. "Above" means a smaller y.

## What is set up

- The app is already running, on its first screen, with a new, empty database: never run `ui start` or `ui stop`. Begin with `ui shot start` to see it.
- After onboarding, the chat list is in the left pane (x 1 to 389), and the right pane shows the empty chat list's cards. An empty chat list shows only its toolbar, with the profile picture at about 27,730 and a pencil at about 363,730: the search field "Search or paste SimpleX link" appears once there is a chat. In the default "Reachable app toolbars" layout, the toolbar and the search field are at the bottom of the left pane.
- The folder `images` in your working directory has two images for part D: `test-card.jpg`, a TV test card 1280×720 (colour bars, a large circle, "1080" at the top and "TVB" at the bottom), and `simplex-logo.png`, the dark blue and turquoise SimpleX logo on a transparent background. You can Read them to compare with what the app shows.

## The test

**A. Onboarding.** Each screen waits for the one before it; wait for a text before clicking it.

1. The first screen has "Create your profile": click it.
2. On "Create profile", click the field "Enter profile name", type `citester`, and click "Create profile".
3. The next screen offers a database passphrase: click "Use random passphrase", then "OK" in the alert that follows.
4. Click "Continue", then "Accept" on the screen after it.
5. Wait up to 60 s for the chat list: the right pane shows "Talk to someone". Take a shot.

**B. A group, and the chat list with the toolbar at the bottom.**

1. Click the pencil (about 363,730) to open New chat, then "Create group". Click the name field, about 40 px below the text "Enter group name", type `seeded notes`, and click "Create group". If "Skip inviting members" shows, click it.
2. The group opens in the right pane, and stays there. Press Escape until the left pane shows the chat list again, then take a shot.
3. Check, noting the evidence (screenshot names, positions):
   - "seeded notes" is listed in the left pane;
   - the search field "Search or paste SimpleX link" is at the bottom of the left pane (y over 600), and "Add list" is just above it;
   - nothing is cut off, overlapping or garbled.

**Between the layouts:** click the profile picture at the left end of the bottom toolbar (about 27,730) to open the menu, open "Settings", then "Appearance", and turn off the switch of "Reachable app toolbars", at the right end of its row (about x 335). Press Escape until the chat list shows again; its search field should now be at the top.

**C. The chat list, toolbar at the top.** Check, noting the evidence:

- "seeded notes" is still listed;
- the search field is at the top of the left pane (y under 200), and "Add list" is just below it;
- nothing is cut off, overlapping or garbled. The pencil is now a floating button at the bottom right of the left pane, drawn over whatever the list shows there: note what it covers, but that alone is not a failure.

**D. Sending images in "seeded notes".**

1. Click "seeded notes" in the left pane to open the group in the right pane. Click the attachment button, the paperclip icon at the left end of the message field at the bottom of the right pane, then "Image". A file dialog opens: click its "File Name" field, type the absolute path of `images/test-card.jpg` in your working directory, and press Return.
2. Back in the app, the compose area shows the image above the message field before it is sent. Take a shot. Check: the preview shows the whole test card, upright, not stretched.
3. Press Return or click the send button at the right end of the message field. Check: the chat shows the sent test card. Click the image: it opens larger, and "1080" and "TVB" are sharp and readable. Take a shot, then close the image with Escape.
4. Repeat 1 to 3 with `images/simplex-logo.png`. Check: the preview and the sent image show the whole logo, and the area around its letters shows the chat's background, not black or grey.

You have at most 90 turns, and the whole test fits in about 45 if you chain steps whose outcome you can predict with `ui do` and look at the screen after each chain. For example:

- `ui do click-text "Create your profile" -- wait "Enter profile name" -- click-text "Enter profile name" -- type citester -- shot profile`
- `ui do click-text "Create profile" -- wait "Use random passphrase" 60 -- shot passphrase`
- `ui do click 363 730 -- click-text "Create group" 10 -- wait "Enter group name" -- shot new-group`
- `ui do click 27 730 -- click-text Settings 10 -- click-text Appearance 10 -- wait "Reachable app toolbars" -- shot appearance`

`ui do` stops at the first command that fails, so a failed chain tells you where it stopped.

## Report

Write `report.md` in your working directory: for each of A, B, C and D, each check you ran as PASS or FAIL with its evidence, and anything else unexpected. End your final message with one line: `CLAUDE-TEST: PASS` if every check you ran passed, otherwise `CLAUDE-TEST: FAIL` followed by the checks that failed.

## The ui helper
