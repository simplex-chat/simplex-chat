# Device test session: SimpleX Chat iOS

You are testing the SimpleX Chat iOS app in an iPhone 17 simulator (iOS 26.5, screen 402 x 874 points) on a GitHub Actions runner. You run the cases of the device test plan (at the end of this prompt) that the section "This session" names, in the layout it names, and write a report. You cannot change the app or the runner; anything wrong you find is a finding for the report.

## Tools

- **Bash runs only the `ui` helper**, one command per call: `ui tap "Create profile"`, `ui find Settings`. Any other program, a pipe, a redirection, `;` or `&&` is denied. Its commands are listed under "The ui commands" below.
- **Read, Write and Edit work only in the current directory.** Screenshots go to `shots/` (look at the `-view.jpg` files, they are smaller), and you write `report.md` here. This whole prompt, with the plan, is also `prompt.md` here: read it again when you are unsure, and after the conversation has been compacted.

## What you need to know about the app and the helper

- **Layouts.** With the one-hand UI (reachable toolbar, the default) the chat list is drawn from the bottom up: the toolbar (your profile image and the "Compose" pencil) is at the bottom, the search field just above it (y about 754), the list tags ("Add list" when there are no lists) above the field, and chats stack upwards from there. With the top bar the toolbar and the search field are at the top. A profile without conversations sees onboarding pages ("Talk to someone") in place of the list, without the search field. Stay in the layout the session names; the app starts in it, and `ui relaunch` keeps it.
- **Positions.** "Above" means a smaller y. Judge order and position from the tree's coordinates, and look at a screenshot when you judge layout (truncation, overlap, images).
- **The search field** has no label; tap it with `ui tap-value "Search or paste SimpleX link"` when empty, or by its text. To clear it, tap the field so it is focused, then "Cancel" next to it ("Cancel" exists only while the field is focused; it also hides the keyboard); tapping the "Close" image inside the field did not clear it in an earlier run. Type text as one quoted argument (`ui type "ci seed"`). The keyboard capitalizes the first letter and autocorrects words (an earlier run saw "ci" become "Ci" and a typed name become another word when a button was tapped), so check the field's value in the tree after typing, and before typing check that the field shows its placeholder. The keyboard's Search key is `ui key return`; the keyboard's keys are below the screen and not listed in the tree, and the screenshots do not show the keyboard.
- **Pasting**: `ui clipboard "<link>"`, tap the empty field once to focus it and once more to open its menu, then tap its "Paste" item (two elements carry that label: use `ui tap-xy` on the one `ui find Paste` shows in the menu).
- **Disabled elements** end with "disabled" in the tree.
- **Ambiguous labels.** When `ui tap` says several elements match a label (seen for "Paste", "Channels", "Account"), tap the one you mean with `ui tap-xy` at its centre, taken from `ui find`.
- **Hidden elements.** The tree always contains the closed user picker: the profile name, "Create SimpleX address", "Chat preferences", "Your chat profiles", "Use from desktop", "Settings", at coordinates on the screen even while it is hidden. Do not tap them unless you opened the picker by tapping your profile image in the toolbar (`Image "Account"`, 32 x 32; tap its centre with `ui tap-xy`).
- **Settings → Reset all hints:** open the picker, "Settings", scroll to the bottom, the app version row ("v…"), "Developer", "Reset all hints". Go back with the navigation back buttons and close Settings by swiping it down.
- **New chat** is the "Compose" button (the pencil) in the toolbar. With the top bar the toolbar is hidden while the search field is focused.
- **Joining** (the app's normal flow, from `NewChatView.swift`; an earlier session took its first step for a failure). Tapping a group or channel the profile does not know yet, from a link or a list, shows an alert with its name (and subscriber count for a channel) and "Cancel" / "Open channel" or "Open group": that is the start of joining, not a sign that the entry is known. "Open …" opens the chat, not yet joined, with "Tap Join channel" in it and a "Join channel" / "Join group" bar at the bottom; tapping that bar joins (a channel then shows "you are subscriber"; a group may stay "connecting"). A contact works the same way: "Open new chat", then the chat's connect button. An entry already opened, joined or connecting shows the same kind of alert, plus your role ("You are a subscriber", "You are a member") once joined. So each case that joins uses the entry named for it under "This session", which no earlier case has touched. Opened and joined chats appear in the chat list, and the profile is no longer "without conversations".
- **A second profile:** open the picker, "Your chat profiles", add a profile; a new profile has no conversations. Switch back to the first profile when done.
- **Text size:** `ui text-size accessibility-large`, and back to `ui text-size medium` afterwards. Only one phone size is available.
- **If the app gets stuck**, `ui relaunch` restarts it (search text is cleared, everything else stays).
- Waiting: prefer `ui wait <text> <secs>` over repeated trees; `ui wait` returns at once when the text is already shown.

## The test backend

When "This session" gives the test directory's address, the runner also runs a test backend: a chat relay, a SimpleX Directory with the groups and channels listed under "Test directory" below, a server that resolves SimpleX names, and contacts that accept connections. `ui directory stop|start` stops and starts the directory (after `ui directory start`, wait about 15 s before using it), and `ui listings` prints what it lists.

**SimpleX names**: the names server's address is under "This session"; the names it knows are under "Test directory" → "SimpleX names" (all end in `.testing`; a bare word of 5 or more letters is completed with `.testing`, so `cinews` is `cinews.testing`). Set the app up for it once, before the first case that needs names (if an earlier session in this run did it, check it instead: the server is listed under "Your servers"):

1. Open the picker (your profile image), "Settings", "Network & servers".
2. Under "Preset servers", open each operator ("SimpleX Chat", "Flux"), turn "To resolve names" off, and go back. Otherwise the app may ask the real servers, which do not know the test names.
3. "Your servers" → "Add server" → "Enter server manually". Put the names server's address in the address field by pasting it (typing it would let the keyboard capitalize it), check it in the tree, and go back.
4. In "Your servers", open the new server and set "To receive" to "no", "For private routing" to "no" and "To resolve names" to "yes" (each is a picker), then go back.
5. On "Network & servers", tap "Save servers" (and "Save" if asked). Close Settings.

## How to run the cases

- Run each case of the session's list, in the plan's order unless a case needs another first.
- The plan's tables (ID, Do, Expect) are at the end of `prompt.md` in the current directory: read the row before running a case and do exactly what it says. An earlier session did not find them and filed other actions under some IDs.
- For each case, do what the plan says, decide pass or fail against the plan's "Expect" column, and take at least one `ui shot` at the moment that shows the result, named after the case and layout without a number, for example `ui shot S1-reachable` (the helper numbers the files).
- Pass only what you saw: quote the evidence (element labels, values and coordinates from the tree, or what a screenshot shows). If an expectation cannot be checked here, say which part and why, and mark the case passed for the parts you checked only if those parts are the substance of the case; otherwise skipped.
- **Skip** any case that needs something this runner lacks, saying exactly what is missing. Do not skip a case only because it takes several steps.
- Other app bugs you notice are findings: write what you did, what you saw, what you expected, and the screenshot.
- Mind the session's turn limit under "This session": every `ui` command and every file edit is a turn.

## The report

Write `report.md` in the current directory as you go, so a session that is cut short still leaves its results. **Before the first case, create it with the Results table already holding one row for every case of the session's list, Result `todo`**, and fill a row in as soon as its case is done. Before each case, and always after the conversation has been compacted, read `report.md` to see which cases are left (and `prompt.md` for anything else you need). The session is finished only when no row says `todo`. Its sections:

1. `# Device test report: <layout>` and one line on the app state at the start.
2. `## Results`: a table with one row per case of the session's list: `| ID | Result | Evidence | Screenshots |`, Result being pass, fail or skipped (with the reason).
3. Any section the plan asks for.
4. `## Findings`: app bugs, each with steps, what you saw, what you expected, screenshots.
5. `## Harness notes`: anything about `ui` or the runner that got in the way.

End the session by making sure `report.md` is complete and no row says `todo`; your final message is a three-line summary: passed, failed and skipped counts, and the most important finding, then one more line: `CLAUDE-TEST: PASS` if no case failed, otherwise `CLAUDE-TEST: FAIL` and the IDs of the cases that failed.
