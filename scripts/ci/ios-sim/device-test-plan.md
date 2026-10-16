# Device test plan: smoke test

The app as the scripted smoke test leaves it: a profile created through onboarding, without conversations. A branch that tests a feature replaces this plan with its own (same table format) and names its cases in `claude-scope`.

| ID | Do | Expect |
|----|----|--------|
| S1 | Look at the screen: `ui tree`, then `ui shot S1-<layout>` | The chat list of a profile without conversations: the heading "Talk to someone", the cards "Let someone connect to you" and "Connect via link or QR code", and the toolbar with your profile image (`Image "Account"`, 32 x 32) and "Compose": at the bottom of the screen with the one-hand UI, at the top with the top bar |
| S2 | Tap "Let someone connect to you", then "Back" | The page "Create your link" with the cards "Invite someone privately" and "Create your public address" and a "Back" button; after "Back", "Talk to someone" again |
| S3 | Open the profile picker by tapping the toolbar's profile image, tap "Settings", then close Settings by swiping it down | The picker shows your profile name and "Settings"; Settings opens with "Your settings" and "Network & servers"; after closing, "Talk to someone" again |
