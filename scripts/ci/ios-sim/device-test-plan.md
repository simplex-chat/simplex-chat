# Device test plan: channel message signing, channel moderators and sending images

The app as the scripted smoke test leaves it: the profile "citester", without conversations. The cases run in the order of the tables and build on each other: one channel, "ci signing", owned by citester, and two more profiles in this app, "cimod" and "cisub", that join it by its link. Do not reorder them: cisub must join only after cimod has become a moderator (B2), so that cimod learns about cisub.

## Setup

- The channel uses the app's preset chat relays. Creating it and joining it wait for the relays: use `ui wait "<text>" 120` there, and take a shot if a wait fails.
- **Sending a message:** in the chat, tap the message field at the bottom (find it in `ui tree`: the text view next to the send button; for the channel's owner its placeholder is "Broadcast"), `ui type "<text>"`, then tap the send button at the right end of the field. Check the message then shows in the chat.
- **Channel info:** tap the channel name at the top of the chat (`ui find "ci signing"`, the element with the smallest y; tap it with `ui tap-xy`). Go back with the navigation back buttons; close a sheet by swiping it down.
- **Message menu:** `ui long-press <x> <y>` on the message's text (centre from `ui find`) opens its menu ("Info", "Moderate", …). "Info" opens the message's details; close them by swiping down.
- **Signatures are read as text, never from the seal icons:** a message's "Info" shows "Signed" (your own message), "Signed & verified" (someone else's, checked) or "Signature missing".
- **Switching profile:** open the picker with your profile image (`Image "Account"`), then tap the other profile's name in the row at the top of the picker (`ui find <name>`, then `ui tap-xy`; the picker's elements are in the tree even while it is closed, so open it first). The chat list then shows that profile's chats; open "ci signing" from it.
- **Adding a profile:** picker → "Your chat profiles" → "Add profile" → tap the field "Enter your name…" (`ui tap-value "Enter your name…"`), type the name, check the field's value in the tree (the keyboard may capitalize it: then fix it), tap "Create profile". The new profile becomes the active one; close "Your chat profiles" by swiping it down.
- **Joining by the copied link** (from a profile without conversations): "Compose" → "Scan / Paste link" → "Tap to paste link". If iOS asks to allow pasting, tap "Allow Paste". An alert names the channel with "Open channel": tap it; the channel opens with "Tap Join channel" and a "Join channel" bar at the bottom: tap the bar, and wait up to 120 s until the chat shows "you are subscriber".

| ID | Do | Expect |
|----|----|--------|
| S2 | "Compose" → "Create public channel" → `ui tap-value "Enter channel name…"`, type `ci signing` (check the value) → tap the "Create public channel" button (the list row, not the title; use `ui tap-xy` if the label is ambiguous) → wait up to 120 s for "Channel link"; if "%d/%d relays active" stays below the total for 120 s with at least 1 active, tap "Continue" and "Continue" in the alert → on "Channel link" tap "Continue" (top right) | The channel "ci signing" opens as its chat |
| S3 | Send `before signing` | "before signing" shows in the chat as sent |
| A1 | Channel info → "Channel preferences" | The first row is the switch "Sign messages", off; its footer says "Do not require signing messages." |
| A2 | Turn "Sign messages" on | Alert "Sign messages?" with the message "Messages sent before will show "signature missing" to subscribers who join later." and the buttons "Enable" and "Cancel". Tap "Cancel": the switch is off again |
| A3 | Turn "Sign messages" on again, tap "Enable", then "Save and notify subscribers"; go back to the chat | After "Enable" the switch stays on (footer "Require signing messages."); after saving and going back, the chat shows the event "Sign messages: on" |
| A4 | Send `after signing`, then long-press it → "Info" | The message's info says "Signed" |
| A5 | Channel info → "Channel preferences" | "Chat with admins" is still off (its footer says "Prohibit chats with admins."); saving in A3 did not turn it on |
| J1 | Channel info → "Channel link" → "Share link" → in the share sheet tap "Copy" (`ui find Copy`; if it is not in the tree, `ui tap-xy` at its position in the screenshot) → close the sheet and go back to the chat list | The link is copied (the sheet closes or shows it was copied) |
| J2 | Add the profile `cimod`; as cimod, join by the copied link | The chat "ci signing" shows "you are subscriber" |
| A6 | As cimod in "ci signing": tap the message "before signing"; then long-press it → "Info"; then long-press "after signing" → "Info" | Tapping "before signing" shows the alert "Signature missing" with "The message was sent before the channel started signing messages, or it is fake."; its info says "Signature missing"; the info of "after signing" says "Signed & verified" |
| B1 | Switch to citester → "ci signing" → channel info → "Subscribers" → "cimod" → tap the "Change role" row | The role menu lists exactly "subscriber", "contributor" and "moderator" (no admin, no owner) |
| B2 | Choose "moderator" → "Change role?" → "Change"; go back to the chat | cimod's info shows the role moderator under the section header "Moderator" (any case); the chat shows "you changed role of cimod to moderator" |
| B3 | Switch to cimod → "ci signing"; send `moderator post` | The chat shows "changed your role to moderator"; "moderator post" shows as sent |
| J3 | Add the profile `cisub`; as cisub, join by the same link (it is still on the clipboard; if not, copy it again as citester as in J1) | cisub's chat "ci signing" shows "you are subscriber" |
| J4 | Switch to citester → "ci signing" → channel info → "Subscribers" → "cisub" → "Change role" → "contributor" → "Change" | cisub's role shows contributor, under the section header "Contributor" |
| J5 | Switch to cisub → "ci signing"; send `contributor post` | "contributor post" shows as sent |
| B4 | Switch to cimod → "ci signing" → channel info | The info shows "Subscribers"; its list includes "cisub" |
| B5 | Open "cisub" in that list | cisub's info offers "Block for all" (do not tap it yet) |
| B6 | Back in the chat, long-press "contributor post" → "Moderate" → "Delete member message?" → "Delete" | The message shows "moderated by cimod" in its place |
| B7 | Channel info → "Subscribers" → "cisub" → "Block for all" → "Block subscriber for all?" → "Block for all" | cisub's info now offers "Unblock for all"; the chat shows "you blocked cisub" |

## Sending images

These cases run after the channel cases: first switch to citester and go back to its chat list. The photo library has, besides the simulator's own photos, the images of these cases: a TV test card 1280×720 (JPEG: colour bars, a large circle, "1080" at the top and "TVB" at the bottom), the dark blue and turquoise SimpleX logo on a transparent background (PNG), and, when the runner could make it, the same test card turned by 90° to portrait (HEIC). In the photo picker, identify each by its thumbnail. The paperclip button left of the message field opens the "Attach" dialog; the send button is the arrow at the right end of the field; find their labels with `ui tree`.

| ID | Do | Expect |
|----|----|--------|
| I1 | Tap "Compose", then "Create group"; type `image notes` in the field "Enter group name…", check the value, tap "Create group", then "Skip" if it shows; open the group's chat if it is not open | The chat of "image notes", with the message field and the paperclip button enabled |
| I2 | Tap the paperclip button, "Choose from library", the test card's thumbnail, then the picker's "Add" button; `ui shot I2-preview`; tap the send button; tap the sent image; `ui shot I2-full`; close the image | Before sending, the compose area shows the whole test card, upright and not stretched; after sending, the chat shows it; opened, it fills the screen width and "1080" and "TVB" are sharp and readable |
| I3 | As I2, with the SimpleX logo | The preview and the sent image show the whole logo; the area around its letters shows the chat's background, not black or grey |
| I4 | As I2, with the portrait test card; skipped when the library has no portrait test card | The preview and the sent image show the test card upright in portrait, as the picker's thumbnail does |
