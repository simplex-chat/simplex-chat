# Device test plan: switching profile clears the chat list search

The app as the scripted smoke test leaves it: the profile "citester", without conversations. The cases run in the order of the table and build on each other: a group "ci alpha" of citester (P1), a second profile "ciother" with its group "ci beta" (P2), and a third profile "cinew" without conversations (P6). Do not reorder them.

## Setup

- **Creating a group:** "Compose" → "Create group" → `ui tap-value "Enter group name…"`, type the name, check the field's value in the tree (the keyboard may capitalize it: then fix it), tap the "Create group" button (the list row below the name field, not the title; use `ui tap-xy` if the label is ambiguous). The next screen invites members: tap "Skip" (top right). The group's chat opens; go back to the chat list with the navigation back buttons.
- **Typing in the search field:** tap it with `ui tap-value "Search or paste SimpleX link"` (it shows this placeholder when empty), `ui type "<text>"`, and check the field's value in the tree. Then close the keyboard with `ui key return`: the text stays in the field. In these cases never tap "Cancel" or the field's "Close" image, which clear the field, and don't run `ui relaunch` between typing and the next switch, which clears it too: the cases check that switching profile clears it. A typed word of 5 or more letters, such as `alpha`, also shows a row "Connect to alpha.testing" near the field: do not tap it. Before switching profile, check that the field still shows the text.
- **Switching profile:** open the picker with your profile image (`Image "Account"`), then tap the other profile's name in the row at the top of the picker (`ui find <name>`, then `ui tap-xy`; the picker's elements are in the tree even while it is closed, so open it first). The chat list then shows that profile's chats.
- **Adding a profile:** picker → "Your chat profiles" → "Add profile" → tap the field "Enter your name…" (`ui tap-value "Enter your name…"`), type the name, check the field's value in the tree (the keyboard may capitalize it: then fix it), tap "Create profile". The new profile becomes the active one; close "Your chat profiles" by swiping it down.
- **Names** are compared ignoring case: "Ci alpha" in the list is "ci alpha".

| ID | Do | Expect |
|----|----|--------|
| P1 | As citester, create the group `ci alpha` | The chat list lists "ci alpha" and shows the search field "Search or paste SimpleX link" |
| P2 | Add the profile `ciother` (it becomes the active profile); as ciother create the group `ci beta` | ciother's chat list lists "ci beta" and not "ci alpha" |
| P3 | Switch to citester. Type `alpha` in the search field | The list shows "ci alpha" (the text filters it) |
| P4 | With `alpha` still in the field, switch to ciother | The search field is empty (its value is the placeholder "Search or paste SimpleX link"), and the list shows "ci beta"; no "No chats found" |
| P5 | As ciother, type `beta` in the search field, then switch to citester | The search field is empty and the list shows "ci alpha" |
| P6 | As citester, type `alpha` in the search field; then add the profile `cinew` (a profile without conversations shows the onboarding pages "Talk to someone"); then switch back to citester | Back on citester the search field is empty and the list shows "ci alpha" |
