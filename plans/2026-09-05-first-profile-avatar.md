# Profile created with no active local profile: avatar stored as description, bio dropped

`createProfileInNoProfileSetup` in `views/WelcomeView.kt` (shared code; the path is reached on desktop only).

## The defects

On the "Create profile" screen reached when no local chat profile is active (after deleting the only, or last visible, profile, or with a linked mobile in use and no local profile):

1. The chosen profile picture is written to the profile's **description** instead of its image, so the profile has no avatar and contacts see the base64 image text as the profile's description (shown as a description preview with "Read more").
2. The bio typed on the same screen is discarded.

## The cause

`Profile`'s parameters are `displayName, fullName, shortDescr, description, image, …`. The call passed the image positionally:

```kotlin
Profile(displayName.trim(), "", null, image)   // 4th argument is description
```

It was correct when written in #6891; #7256 later inserted `description` before `image` in `Profile` and did not update this call. Both are `String?`, so it still compiles.

The bio (`shortDescr`) has never been passed on this path: #6055 passed it only to `createProfileInProfiles`, and #6086 later showed the field on this screen without passing it here.

`CreateProfile` calls `createProfileInNoProfileSetup` when `chatModel.localUserCreated` is not `true`; otherwise it calls `createProfileInProfiles`, which passes both correctly. The flag is `false` when no active local profile was found at start or the core last reported none (`apiGetActiveUser`). The migration's temporary database can also report none, but that state is short-lived and can't reach this screen. While the flag is `null` the app shows the splash screen, so this screen cannot be opened. This happens on desktop only:

- from the user picker's "Create chat profile" row;
- from "Your chat profiles" → "+ Add profile" right after deleting the last visible profile on that screen: on desktop the screen stays open after the delete (`UserProfilesView.kt:346-363`, and `App.kt:161-163` does not show the user picker while it is open), and its "+ Add profile" row opens `CreateProfile` (the `addUser` lambda at `UserProfilesView.kt:49-56`, `WelcomeView.kt:164-167`);
- from "Your chat profiles" → "+ Add profile" while a linked mobile is in use.

Onboarding uses `createProfileOnboarding`.

## The fix

Pass the image by name and the bio as `shortDescr`, as `createProfileInProfiles` does:

```kotlin
- Profile(displayName.trim(), "", null, image)
+ Profile(displayName.trim(), "", shortDescr.trim().ifEmpty { null }, image = image)
```

`createProfileInNoProfileSetup` takes the bio as a parameter, and its only caller passes the screen's `shortDescr`.

## Alternatives considered

- Reorder `Profile`'s fields: it follows the core's field order (plus `localAlias`), and positional callers would silently shift.
- Call `createProfileInProfiles` from this branch: it creates the profile on the linked mobile when one is in use and does not switch the UI to the local profile. It never sets `localUserCreated` itself; its `startChat` sets it when chat is not running, e.g. a desktop started with no profile. Such a desktop never sends `StartChat` to the core (the core requires a user), so `startChat` sees chat as not running and sets the flag (`SimpleXAPI.kt:707`). When there are no users yet it also sets a different onboarding stage and never closes the screen.
- Share one `Profile` builder between the two create functions, which now build the same profile: a refactor, a separate PR if wanted.
- Require named arguments everywhere / repair existing profiles: too broad for one screen.
- Fix only the avatar and leave the bio for a separate PR: the same function, two lines; fixed together.

## Testing

Desktop: delete the only chat profile and, in the same session, create one with a name, a picture and a bio, from "Create chat profile" in the user picker and from "+ Add profile" on the still-open "Your chat profiles" screen. Master: no avatar, the description holds the image text, no bio. PR: the picture is the avatar, the bio is set, no description. Regression: creating a further profile (user picker → Your chat profiles → +), no picture and a bio of only spaces, the bio length limit. Android does not reach this path.

GUI-tested on master and on the fix build: both routes after deleting the last profile, "Create chat profile" in the user picker and "+ Add profile" on "Your chat profiles", and the controls (adding a profile while another exists, first-run onboarding). The unusual inputs were tested on the fix build. The linked-mobile route is untested (no mobile can be linked to a test desktop here).
