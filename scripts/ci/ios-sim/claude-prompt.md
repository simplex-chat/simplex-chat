# Device test session: SimpleX Chat iOS, online search

You are testing the SimpleX Chat iOS app in an iPhone 17 simulator (iOS 26.5, screen 402 x 874 points) on a GitHub Actions runner. The feature under test is online search: from the chat list and from New chat, the app sends the search text to a SimpleX Directory and shows its listed groups and channels. You run the cases of the device test plan (at the end of this prompt) that the section "This session" names, in the layout it names, and write a report. You cannot change the app or the runner; anything wrong you find is a finding for the report.

## Tools

- **Bash runs only the `ui` helper**, one command per call: `ui tap "Search in Directory"`, `ui find Directory`. Any other program, a pipe, a redirection, `;` or `&&` is denied. Its commands are listed under "The ui commands" below.
- **Read, Write and Edit work only in the current directory.** Screenshots go to `shots/` (look at the `-view.jpg` files, they are smaller), and you write `report.md` here. This whole prompt, with the seeded directory and the plan, is also `prompt.md` here: read it again when you are unsure, and after the conversation has been compacted.

## What you need to know about the app and the helper

- **Layouts.** With the one-hand UI (reachable toolbar, the default) the chat list is drawn from the bottom up: the search field is near the bottom (y about 754), the list tags ("Add list" when there are no lists) or the online search rows are just above it, and chats and results stack upwards from there. With the top bar the search field is at the top, the tags above it, and "Search in Directory" and "Connect to …" appear below it. Stay in the layout the session names; the app starts in it, and `ui relaunch` keeps it.
- **Positions.** "Above" means a smaller y. Judge order and position from the tree's coordinates, and look at a screenshot when you judge layout (truncation, overlap, avatars).
- **The search field** has no label; tap it with `ui tap-value "Search or paste SimpleX link"` when empty, or by its text. To clear it, tap "Cancel" next to the field (it also hides the keyboard): in an earlier run, tapping the "Close" image inside the field did not clear it, and the next text was typed after the old one. Before typing, check that the field shows its placeholder; after typing, check the field's value in the tree, as the keyboard may capitalize or correct words. The keyboard's Search key is `ui key return`; keyboard keys are below the screen and not listed in the tree.
- **Pasting** (C4, W3): `ui clipboard "<link>"`, tap the empty field once to focus it and once more to open its menu, then `ui tap Paste`.
- **Directory search.** "Search in Directory" (a row) searches the directory; the first time after the hints are reset the alert "Search in Directory?" asks first, with "Cancel", "Search" and "Search and don't show again" (`ui tap Search` taps the alert's button). Results come as a heading `Heading "Directory"`, then per entry an avatar image, `"#name"`, the description, "N members" or "N subscriber(s)", and "Join group" or "Join channel" as separate texts. A failed search shows "Search failed, tap to retry"; more results show "Show more". A search takes a few seconds (it sets up an encrypted connection) and times out after 10 s. A channel's row shows the subscriber count the directory publishes for it (`publicMemberCount`, 1 for the seeded channels), not the listings' members column, which counts every member record (owner, relay, directory: 3), so the two differ by design.
- **Order with the one-hand UI.** Results read from the bottom like the chats: the first result is nearest the search field, each "Show more" adds its page above the earlier ones, and "Show more" sits right under the "Directory" heading. That is the current design; what must hold is that the heading is above all of the part's rows.
- **Disabled elements** end with "disabled" in the tree. To see "Show more" while its page loads, stop the directory first (`ui directory stop`), tap "Show more" and look at it within 10 s, then start the directory again.
- **Hidden elements.** The tree always contains the closed user picker: the profile name, "Create SimpleX address", "Chat preferences", "Your chat profiles", "Use from desktop", "Settings", at coordinates on the screen even while it is hidden. Do not tap them unless you opened the picker by tapping your profile image in the toolbar (`Image "Account"`, 32 x 32; tap its centre with `ui tap-xy`).
- **Settings → Reset all hints:** open the picker, "Settings", scroll to the bottom, the app version row ("v…"), "Developer", "Reset all hints". Go back with the navigation back buttons and close Settings by swiping it down.
- **New chat** is the "Compose" button (the pencil) in the toolbar.
- **Counting requests.** `ui requests` tells how many directory searches the app sent; compare before and after an action to prove that something was or was not sent (C6, C9, D12, D13).
- **The test directory** can be stopped and started (`ui directory stop|start`). Use it instead of turning networking off (D8, D9; the simulator shares the runner's network). After `ui directory start`, wait about 15 s before searching again; while it is stopped, searches fail after 10 s.
- **Text size** (D3): `ui text-size accessibility-large`, and back to `ui text-size medium` afterwards. Only one phone size is available.
- **Joining.** Tapping a directory row starts the usual join flow; joining a channel works (the test relay is online), joining a group may stay "connecting". Joined chats then appear in the chat list, and the profile is no longer "without conversations".
- **A second profile** (D14, and D15 once the first profile has chats): open the picker, "Your chat profiles", add a profile; a new profile has no conversations. Switch back to `citester` when done.
- **If the app gets stuck**, `ui relaunch` restarts it (search text and results are cleared, everything else stays).
- Waiting: prefer `ui wait <text> <secs>` over repeated trees.

## How to run the cases

- Run each case of the session's list, in the plan's order unless a case needs another first (for example D15 needs a profile without conversations, U1 and U2 need a joined channel from D6).
- For each case, do what the plan says, decide pass or fail against the plan's "Expect" column, and take at least one `ui shot` at the moment that shows the result, named after the case and layout, for example `ui shot D2-reachable`.
- Pass only what you saw: quote the evidence (element labels, values and coordinates from the tree, or what a screenshot shows). If an expectation cannot be checked here, say which part and why, and mark the case passed for the parts you checked only if those parts are the substance of the case; otherwise skipped.
- **Skip** the N cases (no name-resolving server here), and any case that needs something this runner lacks, saying exactly what is missing. Known: U3 (needs a listed group joined through another link), W7, W8 and G1 (need contacts with SimpleX names), W10 (needs a notification from another profile), W11 unless "Archived contacts" is shown. Do not skip a case only because it takes several steps.
- **The Directory header** must be above its results in this layout (D2, D4, D8 and every screen with results): record the heading's y and the first and last result's y each time, and note whether `Heading "Directory"` comes before or after the result texts in the tree's order.
- **Avatars and the response size limit** (part of D2 and D7): search `ci avatar`. Six entries match; each has an avatar about the size the app makes, and a directory response must fit 10737 bytes, so expect about one entry per page. Tap "Show more" until it disappears and record which entries each page added. Compare with the page-by-page table under "Test directory" below: every entry reachable, none twice, none silently missing. Look at the screenshots: does every row show its picture (not the placeholder icon), is any row drawn wrong?
- Other app bugs you notice are findings: write what you did, what you saw, what you expected, and the screenshot.

## The report

Write `report.md` in the current directory as you go: create it after the first case and update it after every case, so a session that is cut short still leaves its results. Its sections:

1. `# Device test report: <layout>` and one line on the app state at the start.
2. `## Results`: a table with one row per case of the session's list: `| ID | Result | Evidence | Screenshots |`, Result being pass, fail or skipped (with the reason).
3. `## Directory header`: per screen with results, the heading's y and the first and last result's y, and the heading's place in the tree order.
4. `## Avatars and the size limit`: what each "Show more" added, compared with the table below, and how the avatars looked.
5. `## Findings`: app bugs, each with steps, what you saw, what you expected, screenshots.
6. `## Harness notes`: anything about `ui` or the runner that got in the way.

End the session by making sure `report.md` is complete; your final message is a three-line summary: passed, failed and skipped counts, and the most important finding.
