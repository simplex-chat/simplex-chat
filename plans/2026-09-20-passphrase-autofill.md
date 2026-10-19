# Passphrase fields not recognised by password managers (Android)

PR #7612, branch `nd/passphrase-autofill`.

## Problem

> The passphase isn't detected as a password box for any password manager so it refuses to autofill.

`PassphraseField` (`DatabaseEncryptionView.kt`) was never classified as a password, so no manager offered to fill or save the database passphrase. While verifying this, a second defect turned up: the raw passphrase could be copied to the clipboard from the selection toolbar and through accessibility `ACTION_COPY`.

## Root cause (Compose 1.8.2, from bytecode)

- Semantic autofill is enabled (`ComposeUiFlags.isSemanticAutofillEnabled = true`). `PopulateViewStructure.populate` builds each node from semantics. It sets autofill hints only from `SemanticsProperties.ContentType`, and `inputType = 0x81` plus `dataIsSensitive` only from `password()` or a `"password"` hint.
- The legacy `BasicTextField` never sets `ContentType`. It sets `password()` only when `visualTransformation is PasswordVisualTransformation`. The hand-rolled `VisualTransformation { … }` lambda fails that check, so the field reached managers with no hints and no password `inputType`.
- The same `is PasswordVisualTransformation` check gates Copy/Cut in the selection toolbar and the `CopyText`/`CutText` accessibility actions. Both copy `value.getSelectedText()`, which is the raw passphrase.

## Fix

`PassphraseField`:
- uses `PasswordVisualTransformation('*')`. The mask looks the same, and it restores `password()` semantics and the copy/cut gates.
- takes `contentType: ContentType? = ContentType.Password` and applies it with `.semantics { if (contentType != null) this.contentType = contentType }`.

`newPasswordContentType()` (expect/actual in `platform/Modifier*.kt`) returns `NewPassword + Password` on Android and `NewPassword` on desktop, where `ContentType` has no `plus`. Desktop has no OS autofill, so it is inert there.

| Screen | Current | New | Confirm |
| --- | --- | --- | --- |
| Change passphrase (`DatabaseEncryptionView`, current field shown) | `password` | `newPassword` | `newPassword` |
| First passphrase (`DatabaseEncryptionView` with no current field, `SetupDatabasePassphrase`) | — | `newPassword`+`password` | `newPassword`+`password` |
| Unlock / migrate (`DatabaseErrorView`, `MigrateToDevice`, `MigrateFromDevice`) | `password` | — | — |
| Hidden profile password (`HiddenProfileView`), unhide/delete (`UserProfilesView`) | none | none | none |

Why the hints vary:
- **First passphrase:** KeePassDX does not recognise a form whose every field is `newPassword`, so these fields also carry `password`.
- **Change form:** adding `password` to the new fields here gives three identical `password` fields, and the manager stops offering to save.
- **Hidden profile:** these fields carry no `ContentType`, so no password manager offers to save them. A stored entry would reveal that the hidden profile exists. `AndroidAutofillManager.onEndApplyChanges` commits the session, which raises the save prompt, only after the last node carrying `ContentType` leaves the screen (`isRelatedToAutoCommit` = has `ContentType`), so leaving these screens never commits. Compose 1.8.2 cannot mark a node not important for autofill (`populate` never calls `setImportantForAutofill`). The fields still carry `password()`, so a manager may still offer to fill them.

The app never calls `AutofillManager.commit()`. Committing on submit was tried and reverted: it ended the session while the fields were still shown, and autofill stayed dead until the app restarted.

## Verification

- **Semantics** (desktop Compose test `~/passphrase-review/semtest`, a two-field A/B): with the fix the node has `ContentType`, `Password = true`, and no `CopyText`. Without the fix it has no `ContentType`, `Password = false`, and `CopyText` present. `EditableText` is masked either way; autofill never received the raw value.
- **Device, structure** (stub `AutofillService` dumping `AssistStructure`, `~/passphrase-review/harness`, on an earlier `newPassword`-only build): the passphrase fields arrived as `EditText` with `inputType=0x81` and their hints.
- **Device, KeePassDX:** the hint combinations in the table were settled by testing with KeePassDX. It fills and offers to save on the change and first-passphrase forms, and the save prompt appears when the screen is closed.
- **Not yet checked on a device:** the hidden-profile screens (no `ContentType`) showing no save prompt.

## Limitations

- KeePassDX offers to fill only the last password field in the form (Confirm rather than New). New and Confirm emit identical hints and `inputType`, so this has to be fixed in KeePassDX.
- The framework still commits when the activity finishes (`AutofillManager.onActivityFinishing`) unless the service set `FLAG_DONT_SAVE_ON_FINISH`. A session opened by focusing a hidden-profile field could therefore still prompt to save if the activity finishes before that session ends.
- With a hardware keyboard or mouse, Ctrl+C and the Android right-click menu still copy, because those paths don't check the transformation (an upstream Compose gap). Only `BasicSecureTextField` blocks copying outright.
- The Compose text-toolbar "Autofill" item does nothing for the legacy `BasicTextField`, because `requestAutofillAction` is never assigned. Also upstream.

## Out of scope

- `DefaultConfigurableTextField` (`DefaultBasicTextField.kt:163–166`), which backs the SOCKS proxy password (`NetworkAndServers.kt:525`), has the same lambda and no `ContentType`. It has the same clipboard exposure, and the same two-line fix applies.
- iOS `PassphraseField` (`DatabaseEncryptionView.swift:304`) is a `SecureField` without `.textContentType`, which is the same gap.
