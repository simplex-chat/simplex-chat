# Device test plan: chat list cards and banners while searching

The app as the scripted smoke test leaves it: the profile "citester", without conversations, so the chat list shows the onboarding pages. C1 creates a group, "ci cards", which gives the chat list its search field and its cards; the cases after it build on it. The top-bar session starts from the state the reachable session left: "ci cards" exists, and `ui layout` cleared the search field.

## Setup

- **Creating a group:** "Compose" (the pencil in the toolbar) → "Create group" → `ui tap-value "Enter group name…"`, `ui type "ci cards"`, and check the field's value in the tree (the keyboard may capitalize it: then fix it) → tap the "Create group" row (the button below the field, not the screen's title; use `ui tap-xy` if the label is ambiguous). The next screen offers to invite members: tap "Skip" if it shows, then leave the group's chat with the navigation back button. If you can't get back to the chat list, `ui relaunch` returns to it.
- **The search field** is just above the toolbar with the one-hand UI (y about 754, the list tags above it) and at the top with the top bar. Tap it with `ui tap-value "Search or paste SimpleX link"` while it is empty. Its clear button is the `Image "Close"` (an x in a circle) at the right end of the field, shown only while the field has text: tap its centre with `ui tap-xy`, taken from `ui find Close` (the element in the field's row). If the field still shows the text after that, write it under "Harness notes" and clear it with "Cancel" next to the field.
- **The cards** are list rows around the chats, not drawn over them; leave them open (never tap their x):
  - the toolbar picker, a capsule with "Bottom bar" and "Top bar", the row nearest the search field;
  - the connect card, two halves "New 1-time link" and "Paste link / Scan", beyond the chats, on the side away from the search field;
  - the crowdfunding banner "Invest in SimpleX Chat!" ("Crowdfunding on Wefunder"), between the picker and the chats, shown only when the App Store country is the US or unknown, as it may be in the simulator;
  - a badge banner (a title such as "Your badge expired"), in the same place, only for a profile with a badge: not expected here.
- Judge what shows from the tree and a screenshot: a card counts as shown when its text is in the tree at a y on the screen and the screenshot shows it.

| ID | Do | Expect |
|----|----|--------|
| C1 | As citester (after onboarding), create a group `ci cards` (Setup) and go back to the chat list | The list shows "ci cards", the search field, and cards around the chats: note in the report which of the "Bottom bar / Top bar" picker, "New 1-time link / Paste link / Scan" and "Invest in SimpleX Chat!" show. At least the picker or the connect card shows; if neither does, the case fails and C2 to C4 prove nothing |
| C2 | Tap the search field and type `ci` (check the field's value) | "ci cards" is still listed; none of the cards or banners noted in C1 shows |
| C3 | Clear the field with its clear (x) button | The field shows its placeholder again, and the cards noted in C1 are back |
| C4 | Tap the search field and type `zzz` | "No chats found"; no cards or banners |
| C5 | In the top-bar session: check "ci cards" is listed (if it is not, create it as in C1), note which cards show as in C1, then repeat C2 and C3 | The search field is at the top; the same as C2 and C3: while `ci` is typed, "ci cards" is listed and none of the noted cards shows; after clearing, they are back |
