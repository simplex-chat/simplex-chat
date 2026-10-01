# Fix passphrase field not recognised as a password box by password managers

Branch: `nd/passphrase-autofill` (to create, base `master`) — unrelated to the current working branch `nd/qr-recognise-wrong-type`.

## 1. Problem statement

> The passphase isn't detected as a password box for any password manager so it refuses to autofill. This is for all password managers.

Android. The database passphrase field (`PassphraseField`) is never offered for autofill: password managers see the field but cannot classify it as a password, so they decline to fill it. Affects all 11 `PassphraseField` call sites — database encryption, onboarding passphrase setup, database error recovery, migrate to/from device, hidden profile, user profiles.

Desktop is unaffected in practice (no OS autofill framework). iOS has a separate, analogous gap — see §5.

A second, unreported defect was found in the same function while verifying the fix: the raw passphrase can be copied to the system clipboard, both from the text selection toolbar and via accessibility `ACTION_COPY`. See §4b.

## 2. Solution summary

Declare the field's content type so Compose emits autofill hints (fixes the report, §4a), and use the stdlib password transformation so foundation recognises the field as a password and withholds copy/cut from the selection toolbar and from accessibility actions (narrows the clipboard leak, §4b).

```diff
+import androidx.compose.ui.autofill.ContentType
+import androidx.compose.ui.semantics.contentType
+import androidx.compose.ui.semantics.semantics
@@ fun PassphraseField(
       )
+      .semantics { contentType = ContentType.Password }
       .focusRequester(focusRequester),
@@
     visualTransformation = if (showKey)
       VisualTransformation.None
     else
-      VisualTransformation { TransformedText(AnnotatedString(it.text.map { "*" }.joinToString(separator = "")), OffsetMapping.Identity) },
+      PasswordVisualTransformation('*'),
```

Total diff: 1 file, +5 / −1.

## 3. Root cause

Compose Multiplatform 1.8.2 → androidx.compose.ui / foundation 1.8.2. `ComposeUiFlags.isSemanticAutofillEnabled` is initialised to `true` (`ComposeUiFlags.<clinit>`: `iconst_1; putstatic`), so the semantics-driven autofill path is live:

```
AndroidComposeView.onProvideAutofillVirtualStructure
  → AndroidAutofillManager.populateViewStructure
    → PopulateViewStructure_androidKt.populate   (walks the whole semantics tree)
```

`PassphraseField` uses the legacy value-based `BasicTextField`. Two properties the autofill structure is built from were therefore never set:

1. **No `ContentType`.** Legacy `BasicTextField` never sets `SemanticsProperties.ContentType` — the only foundation class referencing it is `BasicSecureTextFieldKt` (the new `TextFieldState` API). In `populate`, `setAutofillHints(...)` is reachable **only** from the `ContentType` branch (offset 1150).
2. **No `password()`.** `CoreTextFieldKt` derives its password flag as `visualTransformation instanceof PasswordVisualTransformation` (offset 4260). The hand-rolled `VisualTransformation { … }` lambda fails that `instanceof`, so `CoreTextFieldSemanticsModifierNode.applySemantics` never called `SemanticsPropertiesKt.password()`.

The node handed to the autofill service was therefore `className = android.widget.EditText`, `autofillType = TEXT`, **no `autofillHints`, no `inputType`, `dataIsSensitive = false`** — nothing for a password manager, hint-based or heuristic, to key on.

`KeyboardType.Password` (already present) only configures the IME; it never reaches the autofill structure.

## 4. The fix in detail, and why this shape

### 4a. `contentType` — this is what fixes the reported bug

`ContentType.Password` maps to the hint string `"password"` (= `View.AUTOFILL_HINT_PASSWORD`; `ContentType$Companion.<clinit>`).

In `populate` the sensitivity flag is

```
isSensitive(local 24) = password()(local 11) || contentTypeHints.contains("password")(local 23)      offsets 1376–1391
```

and it gates both `setDataIsSensitive(true)` (offset 1398) and `setInputType(129)` = `TYPE_CLASS_TEXT | TYPE_TEXT_VARIATION_PASSWORD` (offset 1698). So `contentType` **alone** yields all three signals: hints, sensitive, password inputType.

Placement: `CoreTextFieldKt` offsets 5088–5161 show the caller's `modifier` and `CoreTextFieldSemanticsModifier` are `.then()`-chained onto the same layout node, so `contentType` merges into the text field's own `SemanticsConfiguration` instead of forming a sibling node. Applied unconditionally, not gated on `showKey`, so the hint survives the show/hide toggle.

Cross-platform: `setContentType` and `ContentType.Password` both exist in `ui-desktop-1.8.2.jar`, so commonMain compiles for desktop (where it is an inert no-op). No `@OptIn` required — `ExperimentalComposeUiApi` appears 0 times in the `SemanticsPropertiesKt` constant pool.

### 4b. `PasswordVisualTransformation('*')` — prevents the raw passphrase reaching the clipboard

This is a second, independent defect, found by checking every consumer of the password flag rather than only the autofill path.

Not a UI change: decompiled `PasswordVisualTransformation.filter()` returns `TransformedText(AnnotatedString(mask.toString().repeat(text.length)), OffsetMapping.Identity)` — same mask character, same identity offset mapping as the lambda it replaces. Keeping the explicit `'*'` (rather than the `'\u2022'` default) preserves the current appearance exactly.

Two separate gates in foundation test `visualTransformation is PasswordVisualTransformation`, and the hand-rolled lambda fails both:

1. **Floating selection toolbar** — `TextFieldSelectionManager$showSelectionToolbar$1` offsets 84–128:

   ```kotlin
   val isPassword = visualTransformation is PasswordVisualTransformation
   val copy = if (!value.selection.collapsed && !isPassword) { { copy() } } else null
   ```

   With the lambda, `isPassword` is false, so the passphrase field offers **Copy** and **Cut** in the text selection toolbar.

2. **Accessibility actions** — `CoreTextFieldKt` offset 4260 derives `isPassword` the same way and passes it to `CoreTextFieldSemanticsModifierNode`, whose `applySemantics` gates the actions on it (offsets 246–313):

   ```kotlin
   if (!value.selection.collapsed) {
     if (!isPassword) {
       copyText { … }
       if (enabled && !readOnly) cutText { … }
     }
   }
   ```

   `SemanticsActions.CopyText` is advertised on the node as `AccessibilityNodeInfo.ACTION_COPY` (`16384`; a11y delegate lines 1633/1644, dispatched at 3087). The field also exposes `SetSelection` (`applySemantics` offset 182), so an accessibility service can select all and copy with no user interaction.

Both routes call `TextFieldSelectionManager.copy$foundation_release`, whose body (`TextFieldSelectionManager$copy$1`, offsets 71–89) is:

```
TextFieldValueKt.getSelectedText(value)   // reads value.annotatedString — the RAW passphrase
  .toClipEntry()
Clipboard.setClipEntry(…)
```

So the **raw database passphrase** can be placed on the system clipboard. On Android 10+ the clipboard is readable by the foreground app and the default IME, and is captured by IME clipboard history (e.g. Gboard) and clipboard managers.

**This fix narrows the exposure, it does not eliminate it.** Enumerating every caller of `TextFieldSelectionManager.copy` gives four routes, and only some consult the transformation type:

| Route | Reachable on | Gated by `PasswordVisualTransformation`? |
| --- | --- | --- |
| Floating selection toolbar | Android + desktop | **Yes**, both (`showSelectionToolbar$1` offsets 84–128; desktop equivalent gates identically) |
| Accessibility `ACTION_COPY` / `ACTION_CUT` | Android | **Yes** (`applySemantics` offsets 246–313) |
| Right-click / stylus context menu | Android (mouse), desktop | Desktop **yes** (`ContextMenu_desktopKt$textManager$1`); Android **no** — `TextFieldSelectionManager_androidKt$contextMenuBuilder$1` gates only on `getEditable()` and `selection.collapsed` |
| Ctrl+C / Ctrl+X key command | Android (hardware keyboard), desktop | **No** — `TextFieldKeyInput$process$2` calls `copy$foundation_release` at offset 228 and the whole `TextFieldKeyInput*` set has 0 references to `Password` or `VisualTransformation`, on both Android and desktop |

All of these gates are evaluated on the *current* transformation, so they apply only while the passphrase is hidden. With the eye toggle on, `visualTransformation` is `VisualTransformation.None` and copy/cut return — which is correct and unavoidable: in that state `editableText = transformedText.text` is the raw passphrase, so anything reading semantics can see it directly anyway.

On a touch-only Android phone — the reported configuration — the toolbar and accessibility routes are the reachable ones, and both close. The residual routes need a hardware keyboard or a mouse. They are upstream Compose gaps, not something this change introduces or can fix locally; closing them would require either `BasicSecureTextField` (which disables cut/copy outright via `DisableCutCopy`) or an upstream fix. Worth an upstream issue; out of scope here.

It also restores `password()` semantics, which reaches `AndroidComposeViewAccessibilityDelegateCompat` offset 901 → `AccessibilityNodeInfoCompat.setPassword(true)`, so TalkBack treats the field as a password. That part is metadata only and gates nothing.

**Not affected, despite an earlier assumption to the contrary:** the autofill/assist structure never carried the passphrase. `CoreTextFieldSemanticsModifierNode.applySemantics` sets `inputText = value.annotatedString` (raw) and `editableText = transformedText.text` (masked) as *different* properties (offsets 0–19); `populate` reads `EditableText` for `setAutofillValue` (local 10, offset 389) and never reads `InputText` (`getInputText`: 0 occurrences), and the accessibility delegate likewise reads `getEditableText` 6 times and `getInputText` 0 times. The masked `"*****"` was all that was ever exposed there. The clipboard route above is the only disclosure path, and it is independent of autofill.

### Alternatives considered

- **`contentType` only, leave the lambda.** Byte-minimal (+4 / −0) and fully fixes the *reported* bug. Rejected once §4b was found: it would knowingly leave the raw passphrase copyable to the clipboard via the selection toolbar and via accessibility `ACTION_COPY`.
- **`ContentType.NewPassword` on the create/confirm call sites.** More accurate — it is what prompts managers to offer *saving* a generated passphrase. Rejected here: requires a new parameter threaded through all 11 call sites. Worth a follow-up.
- **Migrate to `BasicSecureTextField`.** Sets `ContentType` and password semantics automatically, but is the new `TextFieldState` API — a rewrite of `PassphraseField` and its state plumbing. Far beyond a bug fix.

Regression risk: both changes are declarative semantics/transformation swaps. No logic, no state, no layout or measurement change.

## 5. Scope verification — other instances of the bug class

Scanned all Kotlin source sets for masking / password-typed text fields (`KeyboardType.Password`, `PasswordVisualTransformation`, `VisualTransformation {`):

- **`PassphraseField`** (`DatabaseEncryptionView.kt`) — fixed here.
- **`DefaultConfigurableTextField`** (`DefaultBasicTextField.kt:163–166`) — **identical defect on both counts**: the same hand-rolled `VisualTransformation` lambda and the same missing `contentType`. Backs the SOCKS proxy password field (`NetworkAndServers.kt:525`). It therefore also offers Copy/Cut on a password field and exposes `ACTION_COPY` over the raw value (§4b). Not fixed here only because this change was scoped to the reported field — but since §4b is a disclosure issue rather than a cosmetic one, this should be a deliberate decision, not an omission. Same two-line change applies.
- **App passcode** (`PasswordEntry.kt`) — structurally immune: a custom digit grid plus a `Text`, not an editable text field, so it emits no editable semantics node and never enters the autofill structure.

iOS has the analogous gap, not addressed here: `PassphraseField` in `apps/ios/Shared/Views/Database/DatabaseEncryptionView.swift:334` is a `SecureField` with no `.textContentType(.password)`, and it swaps `TextField`/`SecureField` for the show/hide toggle. `grep textContentType apps/ios` → 0 hits.

## 6. Verification status

Static only — all findings above are from decompiled 1.8.2 bytecode (`javap`), not from documentation or memory. **Not yet done:**

- ~~a compile of the project~~ **done 2026-09-23**: `:common:compileDebugKotlinAndroid` and `:android:compileDebugKotlin` both succeed. No diagnostic mentions `contentType`, `semantics` or `PasswordVisualTransformation`; in particular no opt-in error, empirically confirming `@OptIn(ExperimentalComposeUiApi::class)` is not required. The only warning touching the file is the pre-existing deprecated `KeyboardOptions(autoCorrect = …)` constructor at line 335, which this change does not touch. An APK could not be produced on the `nd/qr-recognise-wrong-type` branch: `:android:buildCMakeDebug` fails with `undefined symbol: chat_check_link`, because the prebuilt `libsimplex.so` predates that core symbol — unrelated to this change, and it occurs after all Kotlin and dex tasks have completed;
- an end-to-end dump of the `AssistStructure` from a stub `AutofillService` confirming `autofillHints=["password"]` and `inputType=129` (§4a). The real APK is arm64-only, so an emulator check needs a minimal repro APK rather than the app itself;
- ~~a device check of §4b~~ **done 2026-09-24**, see below.

## 7. Empirical verification (2026-09-24)

Both halves were verified by a desktop Compose test (`/home/user/passphrase-review/semtest`) that composes two otherwise-identical `BasicTextField`s differing only by this change, and reads the fetched semantics node. `CoreTextField` is common code, so node composition is identical on Android; the Android-only `populate()` step remains bytecode-proven (and is ungated).

| property on the text field's node | with fix | without fix |
| --- | --- | --- |
| `ContentType` | `Password` (type=2) | `null` |
| `ContentDataType` | Text | Text |
| `EditableText` | `*********` (masked) | `*********` |
| `InputText` | `secret123` (raw) | `secret123` |
| `Password` flag | **true** | **false** |
| `CopyText` action (with a selection) | **absent** | **present** |

Consequences, now established rather than inferred:

- **§4a** — `ContentType` lands on the *same* node as `EditableText`. That was the only unverified link; `populate()` emits `setAutofillHints(["password"])`, `inputType=0x81` and `dataIsSensitive=true` whenever it is present. The field is now advertised as a password box.
- **§4b** — without the fix the node exposes the `CopyText` action (→ `AccessibilityNodeInfo.ACTION_COPY`, which copies `value.getSelectedText()`, the raw passphrase). With the fix it is suppressed. Confirmed with a non-collapsed selection; with a collapsed selection neither field exposes it, since the `!selection.collapsed` guard precedes the password check.

**Not established:** that any particular password manager will now offer to fill. KeePassDX was observed offering nothing even after the fix, which is consistent with it having no entry associated with the debug package `chat.simplex.app.debug`. Field labelling and entry matching are separate concerns; this change fixes the former only.

## 8. On-device results (2026-09-27)

Measured with a purpose-built stub `AutofillService` (`/home/user/passphrase-review/harness`, package `h.autofilldump`) that dumps the real `AssistStructure`. This closed the last gap: everything before was bytecode or desktop-only.

Dump of the 2-field change-passphrase screen (no passphrase set), `newPassword`-only build:

```
activity: ComponentInfo{chat.simplex.app.debug/chat.simplex.app.MainActivity}
 cls=EditText hints=[newPassword] inputType=0x81 afType=1 focused=true   <- New passphrase
 cls=EditText hints=[newPassword] inputType=0x81 afType=1 focused=false  <- Confirm
 cls=EditText hints=[-]           inputType=0x0  afType=1 focused=false  <- not a PassphraseField
```

- **The fix works.** Both fields advertise `AUTOFILL_HINT_NEW_PASSWORD` and `inputType=0x81` (`TYPE_CLASS_TEXT|TYPE_TEXT_VARIATION_PASSWORD`). The reported bug — field not recognised as a password box — is resolved.
- The third `EditText` cannot be a `PassphraseField`: `inputType=0` is impossible for one, since `PasswordVisualTransformation` forces `password()` semantics and hence `setInputType(0x81)`. Its much lower semantics id indicates the chat-list search field, still composed beneath the settings modal.

### Why `NewPassword` alone was not enough

Observed KeePassDX behaviour:

| form | hints present | offer |
| --- | --- | --- |
| 3 fields (passphrase set) | `password`, `newPassword` x2 | Confirm only |
| 2 fields (no passphrase) | `newPassword` x2 | **none** |
| 2 fields, after widening | `newPassword+password` x2 | Confirm |

A form with no `password` hint anywhere is not recognised at all. Hence the call sites use `ContentType.NewPassword + ContentType.Password` (`plus` unions the hint sets, emitting `["newPassword","password"]`) rather than `NewPassword` alone. This fixed the silent first-time-setup case.

### Known remaining limitation (not fixable in this app)

Only the **last** password-ish field in structure order receives the offer. New and Confirm emit byte-identical hints and `inputType`, differing only in `focused`, so nothing the app exposes distinguishes them — KeePassDX is selecting positionally. Any hint shared between the two resolves to Confirm because it is later in the tree; the only way to move the offer to New is to remove `password` from Confirm, which (per the table above) silences Confirm rather than adding New. Getting both would require KeePassDX to include both field ids in its dataset. Worth reporting upstream to KeePassDX with the dump above.

### Incidental findings

- The "Autofill"/"Auto completion" item in Compose's text selection toolbar is a **dead no-op** for the legacy `BasicTextField`: it calls `requestAutofillAction?.invoke()`, and `requestAutofillAction` is never assigned on that path (only `TextFieldSelectionState`, the new `TextFieldState` API, sets it). Upstream Compose 1.8.2 bug, unrelated to this change.
- `FLAG_SECURE` (set by default via `privacyProtectScreen`) does **not** block autofill; Android's autofill deliberately ignores it.

## 9. Final configuration and session findings (2026-09-29)

Arrived at by on-device testing with KeePassDX; earlier sections record intermediate states that were superseded.

### Hints as shipped

| form | current | new | confirm |
| --- | --- | --- | --- |
| change passphrase (3 fields) | `password` | `newPassword` | `newPassword` |
| first-time setup (2 fields, no current field) | — | `newPassword` + `password` | `newPassword` + `password` |

`ContentType.plus` unions the hint sets. The combined form is needed only when no field carries plain `password`: a form whose every field is `newPassword` is not recognised by KeePassDX at all, so first-time setup offered nothing. When the current-passphrase field is present it supplies `password`, giving the canonical change-password shape, and adding `password` to the new fields there **breaks the save prompt** — three indistinguishable `password` fields leave the manager unable to identify which holds the new value. Hence the conditional.

`SetupDatabasePassphrase` and `HiddenProfileView` are always new-password-only forms, so they use the combined hints unconditionally.

### Do not commit the autofill session explicitly

An explicit `AutofillManager.commit()` on successful submit was tried and **reverted**. Compose 1.8.2 commits by itself: `AndroidAutofillManager.onEndApplyChanges` fires `commit()` once `currentlyDisplayedIDs` becomes empty, i.e. when the last `ContentType` node leaves the screen. The save prompt therefore appears **when leaving the passphrase screen**, which is correct.

Committing on submit instead breaks things badly: it ends the session while the fields are still displayed, and Compose does not start a new one for them, so taps stop producing any autofill UI until the app process restarts. Symptoms seen while that code was in place: dead fields after Update, intermittent save prompts, and forms that only worked after reopening the app. Removing the explicit call fixed all of them.

### Remaining limitations (upstream, not fixable here)

- Autofill UI occasionally requires an app restart to reappear. Compose 1.8.2 does not reliably re-register nodes after a session is committed.
- Only the **last** password-ish field in the form receives the offer (Confirm, not New). Both emit byte-identical hints and `inputType`, so KeePassDX is selecting positionally. Worth reporting upstream to KeePassDX.
