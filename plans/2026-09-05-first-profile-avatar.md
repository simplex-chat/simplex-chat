# The first chat profile's avatar is stored as its description, and its bio is lost

`createProfileInNoProfileSetup` in `views/WelcomeView.kt` (shared code; the path is reached on desktop only).

## The defects

On the "Create profile" screen reached when there is no local chat profile yet:

1. The chosen profile picture is written to the profile's **description** instead of its image, so the profile has no avatar and contacts see the base64 image text as the profile's description (shown as a description preview with "Read more").
2. The bio typed on the same screen is discarded.

## The cause

`Profile`'s parameters are `displayName, fullName, shortDescr, description, image, …`. The call passed the image positionally:

```kotlin
Profile(displayName.trim(), "", null, image)   // 4th argument is description
```

It was correct when written in #6891; #7256 later inserted `description` before `image` in `Profile` and did not update this call. Both are `String?`, so it still compiles.

The bio (`shortDescr`) has never been passed on this path: #6055 wired it only into `createProfileInProfiles`, and #6086 later showed the field on this screen without wiring it here.

`CreateProfile` calls `createProfileInNoProfileSetup` when `chatModel.localUserCreated` is not `true`, i.e. when there is no active local profile, which happens on desktop only: from the user picker's "Create chat profile" row, or from "Your chat profiles" → add while a linked mobile is in use; otherwise it calls `createProfileInProfiles`, which passes both correctly. Onboarding uses `createProfileOnboarding`.

## The fix

Pass the image by name and the bio as `shortDescr`, as `createProfileInProfiles` does:

```kotlin
- Profile(displayName.trim(), "", null, image)
+ Profile(displayName.trim(), "", shortDescr.trim().ifEmpty { null }, image = image)
```

`createProfileInNoProfileSetup` takes the bio as a parameter, and its only caller passes the screen's `shortDescr`.

## Alternatives considered

- Reorder `Profile`'s fields: it mirrors the core's field order, and positional callers would silently shift.
- Require named arguments everywhere / repair existing profiles: too broad for one screen.
- Fix only the avatar and leave the bio for a separate PR: the same function, two lines; fixed together.

## Testing

Desktop: with a database but no local chat profile, open "Create chat profile", set a name, a picture and a bio, create. Master: no avatar, the description holds the image text, no bio. PR: the picture is the avatar, the bio is set, no description. Regression: creating a further profile (Settings → Your chat profiles → add), no picture and a bio of only spaces, the bio length limit. Android does not reach this path.
