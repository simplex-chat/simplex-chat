# Device test plan: iOS online search (f/directory-search)

Run these on an iOS simulator or device on the Mac. Each case says what to do and what you should see. The case IDs map to the run's acceptance criteria (AC1–AC8) and review fixes (R1-x).

## Setup

1. **Build** `f/directory-search` (HEAD `e7678b634` or later) in Xcode on a simulator. Use a small one (iPhone SE) and a large one, or switch Dynamic Type to a large size for the layout cases.
2. **A directory the app can reach with service requests.** The app sends every search to `DIRECTORY_SERVICE_LINK` (`apps/ios/Shared/Model/DirectorySearch.swift:14`), which is the live SimpleX Directory. Searches against the live directory fail with `ASENotDRAddress` until it runs `/_rotate_address_keys <bot user id>`. Pick one:
   - **Live directory:** rotate its address keys first. The published link does not change.
   - **Test directory:** run `simplex-directory-service` against your own servers (it turns service requests on by itself). Create its address with `/ad pq_ratchet=on`, take the short link, and point `DIRECTORY_SERVICE_LINK` at it in your local build only. Do not commit that change.
3. **Directory content** (most useful with a test directory):
   - at least one listed channel and one listed regular group;
   - enough entries for more than one page (entries with large images fit about one per page);
   - one entry with a long name and a two-line description;
   - one entry whose name contains markdown, e.g. `*bold* [x](https://example.com)`;
   - one channel the test profile has already joined.
4. **SimpleX names** (for the Names cases): the test profile needs a server that resolves names. You also need a channel with a verified `#name` and a contact address with an `@name`, each at least 5 characters long (a bare word is completed with `.testing`). Without these, only cases N4 and N7 can run.
5. **Consent alert:** Settings → "Reset all hints" brings it back between runs.
6. Run every chat-list case in **both toolbar modes** (one-hand UI on and off).

## Chat list: search rows and consent

| ID | Do | Expect |
|----|----|--------|
| C1 | Empty search field | Tags in the slot above the field (one-hand) or above as usual (top bar); no "Search in Directory" row |
| C2 | Type `ne` | One-hand: "Search in Directory" replaces the tags above the field. Top bar: tags stay, "Search in Directory" appears below the field |
| C3 | Type a 5+ letter word, e.g. `alice` | "Connect to alice.testing" and "Search in Directory" (same slots as C2) |
| C4 (AC4) | Paste a SimpleX link | One-hand: the **tags stay** in the slot (before this change the slot was empty). Neither mode offers "Search in Directory"; the link connects as before |
| C5 (AC8) | After resetting hints, tap "Search in Directory" | Alert "Search in Directory?": the text goes to SimpleX Directory, and if it is a SimpleX name, also to your server that resolves names. Actions: Cancel / Search / Search and don't show again |
| C6 | In C5, choose Cancel | Nothing is sent: no spinner, no results |
| C7 | Choose "Search and don't show again", then search again | No alert the second time; "Reset all hints" brings it back |
| C8 | Type text, press the keyboard **Search** key | The key is labelled Search; it runs the same directory search (alert first time) |
| C9 | Whitespace only, then press Search or tap the row | Nothing happens |

## Chat list: directory results (AC1–AC3)

| ID | Do | Expect |
|----|----|--------|
| D1 | Search for a term that matches listed groups | Spinner in the search field after about 1 s. While loading: **no "Directory" header**, no "No chats found" overlay |
| D2 | Results arrive | A "Directory" header row, then rows: avatar, `#` name, the directory's SimpleX name (if any), description up to 2 lines, member or subscriber count, and **"Join channel"** for channels or **"Join group"** for groups at the trailing edge. "Join …" rows are for entries you don't have; an entry you already have is a chat row (U2) |
| D3 (R1-4) | Long-named entry, small phone, large text | The verb stays on one line; the name and description truncate instead |
| D4 | Scroll the list, especially in one-hand mode | The "Directory" header scrolls with the rows and is not pinned to the screen edge |
| D5 | Entry with markdown in its name or description | Shown literally: no bold, no tappable link |
| D6 | Tap a directory row | The usual join flow (channel or group alert); the keyboard hides |
| D7 | More entries than one page | A "Show more" row; tapping it appends the next page, is disabled while loading, and disappears at the end. No entry appears twice |
| D8 | Turn networking off, search | After about 10 s: "Search failed, tap to retry" under the "Directory" header. Turn networking on, tap it: the page loads |
| D9 | Fail a "Show more" (network off), then retry | The retry continues from the next page; it does not start over |
| D10 (AC3) | Search for nonsense, e.g. `zqxvjk`, with no local chat matching | No Directory part, no "No results" row; the centred "No chats found" appears once the search finishes |
| D11 | Search for nonsense that matches a local chat name | Only the local chats; no Directory part, no overlay |
| D12 | Start a search, change the text before it returns | Results and spinner clear at once; the old reply never appears |
| D13 | Start a search, clear the field | The spinner stops; nothing appears later |
| D14 | With results shown, switch profile | Results disappear |
| D15 | Profile with no conversations (onboarding pages shown) | The search bar is shown with the onboarding pages (below them with the one-hand UI, above them with the top bar). A search replaces them with the results while it runs or shows rows; a search that finds nothing shows a "No chats found" row; clearing the text brings the onboarding pages back |

## Chat list: no duplicates (AC6)

| ID | Do | Expect |
|----|----|--------|
| U1 | Search for the name of a channel you joined (it is also listed) | It appears once, as your local chat; no Directory row for it |
| U2 | Search for a word only in that channel's description, so the local list does not show it | It appears in the Directory part as your chat's row (as in the chat list: name without `#`, last message, time), with no "Join channel"; tapping it opens the channel, with no alert |
| U3 | A listed regular group you joined through a different link (or by invitation) | It shows twice, local and Directory. This is the documented limit, not a regression |
| U4 | Search for the name of a joined channel that is the page's only result, so the list shows it as your chat | Under the "Directory" header: "All results on this page are already in your chats" (grey), and "Show more" when there are more pages (with the one-hand UI it sits right under the header, as in D7); nothing loads more pages by itself |

## Chat list: names (AC5)

| ID | Do | Expect |
|----|----|--------|
| N1 | Type a bare name registered only as a channel (e.g. `news1`), press **Search** | Directory results as above, plus a "Names" header above the Directory part with one row: avatar, profile name in bold, `(#news1…)` in grey, description. The "Connect to …" row disappears |
| N2 | Type `@alice.testing` (registered contact address), press Search | One Names row for the contact |
| N3 | Bare name registered as both a contact and a channel | Up to two Names rows |
| N4 (R1-6 note) | Name that is not registered, press Search | **No alert**, no Names row; "Connect to …" stays; directory results still come |
| N5 | Name of a contact you already have, shown in the local list | No Names row for it |
| N6 | A channel found both by name and in the directory | Shown once, as the Names row. If it also shows in Directory, note it (link encoding differs; nothing should ever be hidden wrongly) |
| N7 | Profile without a name-resolving server, press Search on a name | No "None of your servers…" alert; just no Names row |
| N8 | After names arrive, tap "Search in Directory", "Show more" or retry | The Names rows stay |
| N9 | Tap "Search in Directory" (not the key) with a name candidate | Directory only; no Names part |
| N10 | Tap a Names row | The usual connect flow (contact or channel alert). If the directory page is still loading, it stops, and the Names rows stay (also on the onboarding screen) |
| N11 | Start a connect (tap a result, "Loading profile…"), and while it runs press Search | The spinner stays the connection's; when both finish, no spinner is left behind |

## New chat (AC7)

| ID | Do | Expect |
|----|----|--------|
| W1 | Open New chat (pencil), type text | "Search in Directory" below the field; the action rows (1-time link, scan, create group/channel) hide as before |
| W2 | Type a 5+ letter word | "Connect to … " and "Search in Directory" below the field |
| W3 | Paste a link | No "Search in Directory"; the link connects as before |
| W4 | Press the keyboard **Search** key | "Names" and "Directory" sections after "Your Contacts" (which shows only while a contact matches), with uppercase grey headers like it; first time, the same consent alert |
| W5 | Search with no contact matching | "No filtered contacts" is hidden while searching and when there are results; shown when nothing is found anywhere |
| W6 | Tap a Directory or Names result | The sheet closes and the join or connect flow runs |
| W7 | Name of a contact listed in "Your Contacts" | No Names row for it |
| W8 | Name of a known contact whose display name does not contain the typed text | It shows as a Names row (documented gap); tapping opens the existing chat |
| W9 | Close the sheet while a search runs | The spinner stops; the chat list is unaffected |
| W10 (R1-1) | Sheet open with results, tap a notification from another profile | The results disappear |
| W11 | New chat → Archived contacts, type and press return | The key is "return", not Search; no online rows; empty states as before |
| W12 | In New chat, search the directory for a word in the description of a channel or group you joined | In the Directory section it is a chat row like a contact's (avatar, name without `#`, a star if favourite, a mask if incognito), with no "Join …"; tapping it closes the sheet and opens the chat |

## Regression checks

| ID | Do | Expect |
|----|----|--------|
| G1 | Typing a known contact's SimpleX name on the chat list | The list narrows to it while typing, as before |
| G2 | Tag filters, unread filter, clearing the search | Unchanged |
| G3 | Tap the pencil while a chat-list search runs | The chat-list search is cancelled (spinner stops) |
| G4 | Pull to refresh (reconnect servers) | Unchanged |
