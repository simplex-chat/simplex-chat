# The first chat profile's avatar is stored as its description

`createProfileInNoProfileSetup` in `views/WelcomeView.kt`.

## The defect

Choosing a profile picture when creating the first chat profile silently loses it. The
image is written to the profile's **description** instead, so the avatar never appears and
the bio holds a base64 image that is never displayed either.

## The cause

`Profile`'s parameter order is:

```kotlin
data class Profile(
  override val displayName: String,
  override val fullName: String,
  override val shortDescr: String?,
  val description: String? = null,
  override val image: String? = null,
  ...
```

so the fourth positional argument is `description`, not `image`:

```kotlin
Profile(displayName.trim(), "", null, image)   // -> description = image
```

Both are `String?`, so this type-checks and there is no warning. Nothing downstream
validates that a description looks like a description, and the UI simply renders no avatar
— there is no error to notice.

## Why it survived

The path is awkward to reach. `createProfileInNoProfileSetup` is selected only by the
`else` branch of `CreateProfile`'s submit, i.e. when `chatModel.localUserCreated.value` is
not `true`:

```kotlin
if (chatModel.localUserCreated.value == true) {
  createProfileInProfiles(...)     // named `image =` — correct
} else {
  createProfileInNoProfileSetup(...)   // positional — the bug
}
```

Its only caller is the user picker's **"Create chat profile"** row, which is shown when
`chatModel.desktopNoUserNoRemote` — desktop, with a database but no local user, not
connected to a remote host. Ordinary onboarding does not go through it, and neither does
Settings → Add profile.

The sibling constructions are all correct, which is why the bug is isolated:
`createProfileInProfiles` passes `image = image`, and `createProfileOnboarding` passes no
image at all.

## The fix

Use the named argument:

```kotlin
- Profile(displayName.trim(), "", null, image)
+ Profile(displayName.trim(), "", null, image = image)
```

One word. No behaviour change on any other path.

## Testing

On desktop, with a database but no local chat profile, open the user picker and use
"Create chat profile". Set a picture and a bio, create, then check the profile: the picture
must be the avatar and the bio must be the bio. Before the fix the avatar is absent.

## Notes

Found while writing #7329, which adds a fourth `Profile` construction on this screen; that
one uses `image =` from the start. Kept separate because this defect is pre-existing and
independent of that feature.
