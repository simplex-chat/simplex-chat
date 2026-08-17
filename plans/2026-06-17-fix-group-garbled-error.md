# Fix garbled error when saving group profile (member admission)

## Problem

Saving a group profile change — e.g. enabling member admission (Review = "All") from Group preferences → Member admission — can fail with an unreadable alert:

```
chat.simplex.common.model.API$Error@3ea295c.err
```

The user sees an object reference instead of the actual error, so there is no way to tell what went wrong.

## Cause

In `apiUpdateGroup` the `API.Error` branch builds the alert message with `"$r.err"` (`SimpleXAPI.kt:2292`). In a Kotlin string template `"$r.err"` interpolates `r.toString()` — and `API.Error` has no custom `toString`, so it yields `chat.simplex.common.model.API$Error@<hash>` — then appends the literal text `.err`. The meaningful message (`r.err.string`) is never read.

This surfaces whenever the core rejects the update. A concrete trigger is a **desynced member role**: the client shows the Save controls because `groupInfo.isOwner` is true, but the core's `assertUserGroupRole gInfo GROwner` (`Commands.hs:3840`) disagrees and returns `CEGroupUserRole`. The display bug then hides which error it was.

## Fix

Render the error message instead of the object reference:

```kotlin
AlertManager.shared.showAlertMsg(generalGetString(errorTitle), "${r.err.string}")
```

One-line change in `SimpleXAPI.kt`. This is the only occurrence of the `"$r.err"` pattern in the codebase. The underlying core rejection is unchanged — but it is now shown clearly to the user.
