# Device test plan: smoke test and sending images

The app as the scripted smoke test leaves it: a profile created through onboarding, without conversations. A branch that tests a feature replaces this plan with its own (same table format) and names its cases in `claude-scope`.

| ID | Do | Expect |
|----|----|--------|
| S1 | Look at the screen: `ui tree`, then `ui shot S1-<layout>` | The chat list of a profile without conversations: the heading "Talk to someone", the cards "Let someone connect to you" and "Connect via link or QR code", and the toolbar with your profile image (`Image "Account"`, 32 x 32) and "Compose": at the bottom of the screen with the one-hand UI, at the top with the top bar |
| S2 | Tap "Let someone connect to you", then "Back" | The page "Create your link" with the cards "Invite someone privately" and "Create your public address" and a "Back" button; after "Back", "Talk to someone" again |
| S3 | Open the profile picker by tapping the toolbar's profile image, tap "Settings", then close Settings by swiping it down | The picker shows your profile name and "Settings"; Settings opens with "Your settings" and "Network & servers"; after closing, "Talk to someone" again |

## Sending images

The photo library has, besides the simulator's own photos, the images of these cases: a TV test card 1280×720 (JPEG: colour bars, a large circle, "1080" at the top and "TVB" at the bottom), the dark blue and turquoise SimpleX logo on a transparent background (PNG), and, when the runner could make it, the same test card turned by 90° to portrait (HEIC). In the photo picker, identify each by its thumbnail. The paperclip button left of the message field opens the "Attach" dialog; the send button is the arrow at the right end of the field; find their labels with `ui tree`.

| ID | Do | Expect |
|----|----|--------|
| I1 | Tap "Compose", then "Create group"; type `image notes` in the field "Enter group name…", check the value, tap "Create group", then "Skip" if it shows; open the group's chat if it is not open | The chat of "image notes", with the message field and the paperclip button enabled |
| I2 | Tap the paperclip button, "Choose from library", the test card's thumbnail, then the picker's "Add" button; `ui shot I2-preview`; tap the send button; tap the sent image; `ui shot I2-full`; close the image | Before sending, the compose area shows the whole test card, upright and not stretched; after sending, the chat shows it; opened, it fills the screen width and "1080" and "TVB" are sharp and readable |
| I3 | As I2, with the SimpleX logo | The preview and the sent image show the whole logo; the area around its letters shows the chat's background, not black or grey |
| I4 | As I2, with the portrait test card; skipped when the library has no portrait test card | The preview and the sent image show the test card upright in portrait, as the picker's thumbnail does |
