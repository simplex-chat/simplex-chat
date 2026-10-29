# Device test plan: iOS online search

Run these on an iOS simulator or device on the Mac. Each case says what to do and what you should see. The case IDs map to the run's acceptance criteria (AC3–AC9).

## Setup

1. **Build** this branch in Xcode on a simulator. Use a small one (iPhone SE) and a large one, or switch Dynamic Type to a large size for the layout cases. The long-press menu (D15, W8) needs iOS 16 or later; on iOS 15 there is no menu.
2. **A directory the app can reach with service requests.** The app sends every search to `DIRECTORY_SERVICE_LINK` (`apps/ios/Shared/Model/DirectorySearch.swift`), which is the live SimpleX Directory. Searches against the live directory fail with `ASENotDRAddress` until it runs `/_rotate_address_keys <bot user id>`. Pick one:
   - **Live directory:** rotate its address keys first. The published link does not change.
   - **Test directory:** run `simplex-directory-service` against your own servers (it turns service requests on by itself). Create its address with `/ad pq_ratchet=on`, take the short link, and point `DIRECTORY_SERVICE_LINK` at it in your local build only. Do not commit that change.
3. **Directory content** (most useful with a test directory):
   - at least one listed channel and one listed regular group;
   - enough entries for more than one page (entries with large images fit about one per page);
   - one entry with a long name and a two-line description;
   - one entry whose name contains markdown, e.g. `*bold* [x](https://example.com)`;
   - one channel the test profile has already joined.
4. **SimpleX names** (for "Connect to …", C3, C11 and G5): the test profile needs a server that resolves names, and a channel with a verified `#name` and a contact address with an `@name`, each at least 5 characters long (a bare word is completed with `.testing`). The search itself never resolves names.
5. **Directory search switch:** Settings → Privacy → "Directory search" (in the Chats section), off by default. While it is off, every search asks first; turn it off again to repeat the consent cases.
6. Run every chat-list case in **both toolbar modes** (one-hand UI on and off).

## Chat list: search rows and consent

| ID | Do | Expect |
|----|----|--------|
| C1 | Empty search field | Tags in the slot above the field (one-hand) or above as usual (top bar); no "Search in Directory" row |
| C2 | Type `ne` | One-hand: "Search in Directory" replaces the tags above the field. Top bar: tags stay, "Search in Directory" appears below the field |
| C3 | Type a 5+ letter word, e.g. `alice` | "Connect to alice.testing" and "Search in Directory" (same slots as C2) |
| C4 | Paste a SimpleX link | One-hand: the tags stay in the slot. Neither mode offers "Search in Directory"; the link connects as before |
| C5 (AC4) | With "Directory search" off, tap "Search in Directory" | Alert "Enable Directory search?", message "Search text is sent to SimpleX Directory, each search separately and without your profile.", actions Cancel and Enable |
| C6 (AC4) | In C5, choose Cancel | Nothing is sent: no spinner, no results, the request count does not change; "Search in Directory" is still shown |
| C7 (AC4) | Choose Enable | The search runs; Settings → Privacy → "Directory search" is now on; later searches, by the row or the key, do not ask; turning the switch off brings the alert back |
| C8 (AC3) | Type text, press the keyboard **Search** key | The key is labelled Search; it runs the same directory search as the row (the alert first while the switch is off) |
| C9 | Whitespace only, then press Search or tap the row | Nothing happens |
| C10 (AC3) | Search, then look at the rows by the field while the results load and show; then change the text | "Search in Directory" is hidden while the results for the current text load or show; it comes back when the text changes |
| C11 (AC3) | Type a 5+ letter name of a listed channel, e.g. `cinews`, press **Search** | Directory results only: no "Names" part, no name resolution. "Connect to cinews.testing" stays shown next to the results (one-hand: above the field; top bar: below it) |

## Chat list: results (AC5–AC8)

| ID | Do | Expect |
|----|----|--------|
| D1 | Search for a term that matches listed groups | Spinner in the search field after about 1 s. While loading: no header, no "No chats found" overlay |
| D2 (AC5) | Results arrive | One list without headers (no "Directory", no "Names"): your chats first, then the results. A result row: avatar the size of a chat row's, `#` name, the directory's SimpleX name (if any), description up to 2 lines, member or subscriber count, and **"Open"** at the trailing edge. No "Join channel" or "Join group" |
| D3 | Long-named entry, small phone, large text | "Open" stays on one line; the name and description truncate instead |
| D4 | Entry with markdown in its name or description | Shown literally: no bold, no tappable link |
| D5 (AC6) | Tap a channel's result (or its "Open") | The keyboard hides, the spinner shows in the field ("Loading profile…"), then the channel opens, not yet joined, with "Tap Join channel" and the "Join channel" bar, and with **no "Open channel?" alert**. Its subscriber count shows in the toolbar and in the banner under the name. A group's result opens the same way, with "Tap Join group" |
| D6 (AC7) | More entries than one page | The last row of the results is "Load more · N left" (N from the directory) or "Load more" (when the directory sends no count), with a plus-magnifier icon; one-hand: it is the top row of the results, farthest from the field. Tapping it appends the next page; while it loads the row is grey and disabled; it disappears at the end. No entry appears twice |
| D7 (AC7) | Stop the directory, search | After about 10 s: "Search failed, tap to retry" with a circular arrow, where the results go. Start the directory, tap it: the page loads |
| D8 (AC7) | Fail a "Load more" (directory stopped), then retry | The row turns into "Search failed, tap to retry"; the retry continues from the next page; it does not start over |
| D9 | Search for nonsense, e.g. `zqxvjk`, with no local chat matching | No result rows; the centred "No chats found" appears once the search finishes |
| D10 | Search for nonsense that matches a local chat name | Only the local chats; no overlay |
| D11 | Start a search, change the text before it returns | Results and spinner clear at once; the old reply never appears |
| D12 | Start a search, clear the field | The spinner stops; nothing appears later |
| D13 | With results shown, switch profile | Results disappear |
| D14 | Profile with no conversations (onboarding pages shown) | The search bar is shown with the onboarding pages (below them with the one-hand UI, above them with the top bar). A search replaces them with the results while it runs or shows rows; a search that finds nothing shows a "No chats found" row; clearing the text brings the onboarding pages back |
| D15 (AC8) | Long press a result | A menu with one item, "Open", over a lifted preview of the row (the row without its "Open"), drawn upright in both layouts. Choosing Open does what a tap does; dismissing the menu opens nothing |
| D16 (AC5) | Manual only: a full-link entry you are already joining (joined, not yet connected) | Its caption says "Already joining" in place of the count, with nothing at the trailing edge; tapping it shows the usual "already joining" sheet or alert |
| D17 | With a chat in the list, type `zqxvjk` (no chat or listing matches it), and look before searching; then tap "Search in Directory" | Before the search: "Search in Directory" is offered and "No chats found" is not shown anywhere; after the search finds nothing, the centred "No chats found" shows |

## Chat list: your chats (AC5)

| ID | Do | Expect |
|----|----|--------|
| U1 | Search for the name of a channel you joined (it is also listed) | It appears once, as your local chat; no result row for it |
| U2 | Search for a word only in that channel's description, so the local list does not show it | It appears as your chat's row (as in the chat list: name without `#`, last message, time), before the result rows, with no "Open"; tapping it opens the channel, with no alert |
| U3 | A listed regular group you joined with `/c` and its listed link; one you were invited to or joined through another link | The `/c`-joined group shows as your chat, with no "Open". The invited or other-link group shows as a result with "Open" beside its own chat row: the documented limit, not a regression |
| U4 | Search for the name of a joined channel that is the page's only result, so the list shows it as your chat | Only the chat's own row, no note about the page, and the load row when there are more pages; nothing loads more pages by itself |

## New chat (AC9)

| ID | Do | Expect |
|----|----|--------|
| W1 | Open New chat (pencil), type text | "Search in Directory" below the field; the action rows (1-time link, scan, create group/channel) hide as before |
| W2 | Type a 5+ letter word | "Connect to … " and "Search in Directory" below the field |
| W3 | Paste a link | No "Search in Directory"; the link connects as before |
| W4 | Press the keyboard **Search** key | The directory results, after the alert while the switch is off; "Connect to …" stays; "Search in Directory" hides while the results show |
| W5 | Results arrive | One card without a header: the contacts that match, your chats, then the results with 30-pt avatars and "Open", the load row last; no "Your Contacts" header while results show |
| W6 | With at least one contact in the profile, type `zqxvjk` (no contact or listing matches it) | The empty state is hidden while searching, while results show and while "Search in Directory" is offered; after the search finds nothing, "No filtered contacts" ("No contacts" in a profile without contacts) |
| W7 | Tap a result or its "Open" | The sheet closes and the chat opens prepared, with no alert |
| W8 | Long press a result | A menu with "Open", as in D15 |
| W9 | Close the sheet while a search runs | The spinner stops; the chat list is unaffected |
| W10 | Sheet open with results, tap a notification from another profile | The results disappear |
| W11 | New chat → Archived contacts, type and press return | The key is "return", not Search; no online rows; empty states as before |
| W12 | In New chat, search the directory for a word in the description of a channel or group you joined | In the results card it is a chat row like a contact's (avatar, name without `#`, a star if favourite, a mask if incognito), with no "Open"; tapping it closes the sheet and opens the chat |

## Regression checks

| ID | Do | Expect |
|----|----|--------|
| G1 | Typing a known contact's SimpleX name on the chat list | The list narrows to it while typing, as before |
| G2 | Tag filters, unread filter, clearing the search | Unchanged |
| G3 | Tap the pencil while a chat-list search runs | The chat-list search is cancelled (spinner stops) |
| G4 | Pull to refresh (reconnect servers) | Unchanged |
| G5 | Tap "Connect to …" for a registered name | The name resolves and the usual contact or channel alert shows, as before |
