# Recognise the wrong QR code type in every scanner

Supersedes the earlier `plan-unify-qr-scanning.md` / `-impl.md` (PR #7044), which
proposed replacing every screen's parser with a single universal `QRCodeType`
classifier. That was more change than the goal needs. This plan is the minimal
version: keep every scanner's parser and success path; only add a fallback when a
scan is rejected.

## Problem

Each QR scanner accepts exactly one kind of code and silently rejects everything
else with a generic "invalid" message:

- New chat → connection link
- Network & servers → SMP/XFTP server address
- Migrate device → migration file link
- Use from desktop → desktop address (`xrcp:/…`)
- Verify security code → a contact's security code

If you scan a valid code on the wrong screen (e.g. a contact link while verifying a
security code) you get "Invalid …" with no hint that the code is fine, just scanned
in the wrong place.

## Approach

When a scanner's own parser rejects a scan, run a small fallback that tries the
*other* known parsers and reports what the code actually is. No parser is replaced;
no scanner signature changes; the fallback lives in one shared file per platform.

- `identifyQRCode(text)` — the "megaparser". Tries, in order, the parsers each
  scanner already uses: connection link (markdown `SimplexLink`, whole-string),
  server address (`parseServerAddress`), migration file link
  (`strHasSimplexFileLink`), desktop address (`xrcp:/` prefix). Returns the first
  match or "none". Because the calling scanner's own parser already failed, this
  naturally reports "any option except the one expected".
- On a match: alert **"Wrong QR code"** with `"This is a <kind>.\n\n<where to scan it>."`
- On no match: **"Wrong QR code" / "The code you scanned is not a SimpleX link QR
  code."**, except the security-code screen ("Incorrect security code!" — there the
  code is the right kind but the wrong value) and migration ("Invalid link", shared
  with its paste path).

A contact's security code has no scheme, so it is matched by shape (`isSecurityCode`:
2+ space-separated groups of digits, 32+ digits total — the `unwords . chunks 5 .
show . os2ip` form). It is detected on every scanner **except** the security-code
scanner itself, which passes `detectSecurityCode = false`: there a scan of the right
shape but wrong value must stay "Incorrect security code!", not "scan it there".

## Files

Kotlin (Android + desktop), shared `commonMain`:
- `views/newchat/WrongQRCode.kt` (new) — `ScannedQRCode`, `identifyQRCode`,
  `showWrongQRCodeAlert(text, orElse)`, `DESKTOP_ADDRESS_SCHEME`.
- `views/newchat/NewChatView.kt`, `chat/ScanCodeView.kt`,
  `migration/MigrateToDevice.kt` (makes `strHasSimplexFileLink` `internal`),
  `remote/ConnectDesktopView.kt`, `usersettings/networkAndServers/ScanProtocolServer.kt`
  — failure branch calls `showWrongQRCodeAlert`; desktop gains a `xrcp:/` pre-check.
- `resources/MR/base/strings.xml` — 11 new strings; none removed.

iOS mirror (same structure):
- `Views/NewChat/WrongQRCode.swift` (new) — `identifyQRCode`, `wrongQRCodeMessage`,
  `notSimplexQRCodeMessage`, `wrongQRCodeAlert`, `desktopAddressScheme`,
  `strHasSimplexFileLink` (moved out of `MigrateToDevice.swift`).
- The five scanner views present the wrong-type alert through **their own** alert
  state (`SomeAlert` / view alert enum), not a global manager, so it shows within
  the scanner's own presentation. Server and desktop gain a local pre-check
  (`parseServerAddress` / `xrcp:` prefix) so they too report the wrong type.
  `project.pbxproj` registers the new file.

## Parser ordering (correctness)

Markdown is checked first, then server / migration / desktop. Verified safe against
`src/Simplex/Chat/Markdown.hs`: a `SimplexLink` is only produced for connection-request
URIs (`http/https/simplex:` that parse as `AConnectionLink`); file links have no
markdown handling, and `xrcp:`/`smp:` are not treated as URIs. So migration, desktop
and server codes never match the markdown branch — order does not misclassify.

## Edge cases

- Relay links classify as a connection link; the "where to scan" line is omitted
  (a relay address can't be used to connect from New chat), so the alert shows only
  "This is a SimpleX relay address."
- A SimpleX *name* is not a `SimplexLink`, so it stays "unknown" on the connection
  scanner — same as before (names only ever worked via paste).
- Android: the failure branch returns `false` (unchanged), so the scanner keeps
  scanning with the existing 1s debounce; iOS scanners are `.oncePerCode`.

## Verification

- Kotlin: `./gradlew :common:compileKotlinDesktop` — builds clean.
- iOS: **not compiled** in this environment (no Xcode toolchain) — needs a Mac/CI
  build. "Where to scan" wording should be checked against the live iOS navigation.

## Not done / out of scope

- No "Connect" action on the alert — informational only.
