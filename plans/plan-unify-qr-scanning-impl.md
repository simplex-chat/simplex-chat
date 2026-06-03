# Implementation work plan: unify QR scanning

Companion to **`plan-unify-qr-scanning.md`** (the design — *what/why*). This file is the
*how/order*: atomic commits, exact files, and acceptance criteria. Do not restate the design;
when in doubt about behaviour, that doc is authoritative.

> **Status (as-built, branch `nd/qr-scan-clarity`).** The `- [ ]`/`- [x]` boxes below are the
> work order; current state:
> - **Phase 1 (Kotlin): DONE & committed.** C0.1–C1.9 implemented; `:common:compileKotlinDesktop`
>   green; `ParseQRCodeTest` passes. **C1.1 is partial** — only the FFI-free fixtures (migration,
>   desktop, ordering, trim) are in the JVM test; the connection/server/unknown + short-link
>   fixtures need the Android instrumented suite (not added).
> - **Phase 2 (iOS): code WRITTEN in source; project wired, not yet compiled here.**
>   C2.0a/C2.0/C2.2/C2.4–C2.8/C2.9 are done (both new Swift files exist + all 5 call sites
>   migrated + dead `strIsSimplexLink` removed), and `QRCodeType.swift` / `QRCodeScan.swift` are
>   now registered in `SimpleX.xcodeproj/project.pbxproj` (PBXFileReference + PBXBuildFile + the
>   NewChat group + the app target's Sources phase) — **needs a Mac/Xcode build to confirm**.
>   **Still open:** **C2.1** (XCTest) not written; **C2.3** iOS strings remain inline
>   `NSLocalizedString`/`Text` literals (not externalized into `Localizable.xcstrings` — a
>   `genstrings` step on the Mac; breadcrumb wording still to review).

## Conventions

- **One logical change per commit**; build + tests green after every commit (`git bisect`-able).
- **Structure separated from behaviour**: signature/move-only changes (e.g. threading `close`,
  exposing a `private` fn) are their own commits, before the behavioural swap.
- **Order**: pure additions first (type, classifier, test, strings, **then** alert — the alert
  references the new strings, so strings must land first), then one migration commit per screen,
  then subtraction (dead helpers).
- **Platforms**: land Kotlin end-to-end first (Phase 1), then mirror on iOS (Phase 2). Kotlin is
  the reference; iOS has documented divergences (design doc → "iOS verification").
- Commit-message summaries imperative, ≤ ~70 chars, e.g. `qr: add QRCodeType + parseQRCode`.

## Decisions (settled — do not re-litigate)

- `parseQRCode` lives in **`views/newchat/`** (next to `strConnectTarget`), NOT `model/`, to avoid
  a view→model inversion (design finding #6).
- No `QRCodeKind` enum — acceptance is `expected: KClass<out QRCodeType>` (Kotlin) / a closure (iOS).
- `QRCodeType.ServerAddress` carries only `text`; build `UserServer` in `onMatch`. `parseQRCode`
  takes no `rhId`.
- Wrong-type alert returns **true** (dedup; show once). `onMatch` keeps each screen's own
  success/failure side effects.
- Action button only for ConnectionLink (non-relay) **and only when `currentUser != null`**.
- `handleScan`'s `rhId` is in scope **only at `ConnectView` and `ScanProtocolServer`**; pass
  **`null`** at the migration / desktop / verify scanners (they aren't host-scoped). `rhId` is
  used solely for the Connect button → `planAndConnect`. **iOS has no `rhId`** — its helper
  threads `theme: AppTheme` instead.

## As-built deltas vs this plan (branch `nd/qr-scan-clarity`, off `master`)

The plan was verified against `stable`; the branch is off `master`, which is **behind** stable.
The code therefore differs from the plan text in these (intentional, documented) ways:

1. **Classifier (master):** no `strConnectTarget`/`ConnectTarget`/SimplexName on master, so
   `parseQRCode` uses `parseToMarkdown` (`size == 1`) + `Format.SimplexLink.linkType` — see the
   updated C1.0 above. There is **no address-name case** on master, so `ConnectionLink.linkType`
   is always non-null and ConnectView gains no "names now accepted" improvement.
2. **iOS uses a small `enum QRCodeKind`** (`connectionLink/serverAddress/migrationLink/
   desktopAddress/code`) for `expected`, not a closure: Swift has no `KClass`, and the alert
   needs the expected kind for the Unknown-on-server message. This is the Code↔Unknown pairing
   the design flagged for Kotlin (finding #4), but it's unavoidable on iOS.
3. **ConnectView `onMatch`** wraps `planAndConnect(rhId, qr.text, close = close).await()` in
   `withContext(Dispatchers.Default){…}` to preserve the original `verifyAndConnect` threading.
4. **C1.9 (remove `verifyOnly`/`verifyAndConnect`) is folded into the ConnectView commit** —
   same file, and the removal is a direct consequence of that migration. `connect()` stays (paste).
5. **Kotlin migration `onMatch` reuses `checkUserLink` unchanged** (its inner `strHasSimplexFileLink`
   recheck is kept as harmless redundancy, not dropped). iOS sets `.linkDownloading` directly.
6. **`ScanProtocolServer` param order is `(rhId, close, onNext)`** (not `…, onNext, close`) so the
   existing trailing-lambda caller keeps binding to `onNext`.
7. **All `File:line` references throughout this plan are the stable-verified locations** and are off
   by a few lines on `master` (e.g. `strHasSimplexFileLink` ~:730 not :727; `ProtocolServersView`
   `showModalCloseable` ~:286 not :284; migration scanner ~:208; desktop scanner ~:358). They point
   at the right symbol, just not the exact master line.

Verification: Kotlin `:common:compileKotlinDesktop` is green and `ParseQRCodeTest` passes. **iOS
is NOT compiled** (no Xcode here) — and the two new Swift files still need adding to
`SimpleX.xcodeproj`, plus the iOS breadcrumb wording reviewed.

---

## Phase 0 — Kotlin prep (structure only)

- [x] **C0.1 — expose the file-link parser.** `strHasSimplexFileLink` is `private` in
  `views/migration/MigrateToDevice.kt:727`. Make it non-private (or move to a shared
  `views/newchat` util). Pure visibility change, no behaviour. Build.
- [x] **C0.2 — thread `close` into scanners that lack it.** The wrong-type Connect/Open button
  needs to dismiss the scanner. Add a `close: () -> Unit` parameter to:
  - `ScanProtocolServer` — this is **`expect`/`actual`**, so update **4 declarations**: the
    `expect` (`ScanProtocolServer.kt:13`), the **android** actual (`ScanProtocolServer.android.kt:10`),
    the **desktop** actual (`ScanProtocolServer.desktop.kt:7`), and `ScanProtocolServerLayout`
    (`ScanProtocolServer.kt:16`); then pass the modal `close` from `ProtocolServersView.kt:284`
    (`showModalCloseable { close -> … }`) — the **same** `close` already given to `addServer`.
  - `ScanDesktopAddressView(sessionAddress)` (private, commonMain) → add `close`, pass
    `ConnectDesktopView`'s `close`.
  - MigrateToDevice scanner lambda — capture the migration view's `close` (or `{}` — see C1.6;
    on that screen no action button appears, so `{}` is acceptable).
  Signature-only refactor; behaviour identical (callers already had `close` in scope). Build.

## Phase 1 — Kotlin core (pure additions)

- [x] **C1.0 — add `QRCodeType` + `parseQRCode`.** New file `views/newchat/QRCodeType.kt`
  (this is the source of truth for the code; nothing calls it yet; build):

```kotlin
sealed class QRCodeType {
  abstract val text: String
  data class ConnectionLink(override val text: String, val linkType: SimplexLinkType?): QRCodeType()
  data class ServerAddress (override val text: String): QRCodeType()
  data class MigrationLink (override val text: String): QRCodeType()
  data class DesktopAddress(override val text: String): QRCodeType()
  data class SecurityCode  (override val text: String): QRCodeType()   // contact verification code
  data class Unknown       (override val text: String): QRCodeType()
}

const val DESKTOP_ADDRESS_SCHEME = "xrcp:/"

fun parseQRCode(raw: String): QRCodeType {              // no rhId; FFI parse only (needs core to test)
  val t = raw.trim()
  return when {
    strHasSimplexFileLink(t)             -> QRCodeType.MigrationLink(t)
    t.startsWith(DESKTOP_ADDRESS_SCHEME) -> QRCodeType.DesktopAddress(t)
    else -> {
      // master has no strConnectTarget/ConnectTarget (no SimplexName); accept only when the
      // whole string is exactly one SimpleX link and keep the parsed link to read its type.
      // linkType is therefore always non-null here on master.
      val md = parseToMarkdown(t)
      val link = if (md?.size == 1) md[0].format as? Format.SimplexLink else null
      when {
        link != null                  -> QRCodeType.ConnectionLink(t, link.linkType)
        parseServerAddress(t) != null -> QRCodeType.ServerAddress(t)
        isSecurityCode(t)             -> QRCodeType.SecurityCode(t)   // long hex/decimal digits, no scheme
        else                          -> QRCodeType.Unknown(t)
      }
    }
  }
}

// A contact's verification code QR is the raw code, no scheme. Core builds it as
// `verificationCode = unwords . chunks 5 . show . os2ip` = DECIMAL digits grouped in 5s separated
// by SPACES, so strip whitespace, then require decimal digits only (show emits no hex). Checked
// last, so real links/addresses never reach here. NB: qr.text keeps the spaces, so verifyCode
// (core noSpaces-normalises) still matches. Canonical fixture: tests/ChatTests/Direct.hs.
internal fun isSecurityCode(t: String): Boolean {
  val digits = t.filterNot { it.isWhitespace() }
  return digits.length >= 32 && digits.all { it in '0'..'9' }
}
```
- [x] **C1.1 — unit test `parseQRCode`** (load-bearing). One fixture per kind + garbage, and
  **both** connection encodings: full (`simplex:/contact#…`, `https://simplex.chat/group#…`) AND
  short (`https://<smp-server>/a#…` for contact/group/invitation) → all `ConnectionLink`; a
  migration `simplex:/file…` → `MigrationLink`; an `xrcp:/…` → `DesktopAddress`; an `smp://…`/
  `xftp://…` → `ServerAddress`; a `stun:`/`turn:` and a channel web URL and a numeric security
  code → `Unknown`. **Harness caveat:** the new logic is the *ordering*; the parsers
  (`strConnectTarget`→`chat_parse_markdown`, `parseServerAddress`→`chat_parse_server`) are JNI/FFI
  into core, so this is **not** a pure-JVM unit test — it must run where the native lib is loaded
  (the app's instrumented/integration suite). If no such harness exists, gate this work on adding
  one; do NOT skip the test (classification is the whole design).
- [x] **C1.2 — add strings** to `resources/MR/base/strings.xml` (see Appendix A). Add keys only;
  translations land via the project's usual flow. Must precede C1.3 — the alert code references
  these `MR.strings.*`, so adding the alert first would not compile. Build.
- [x] **C1.3 — add the alert + gate.** In `views/newchat/QRCodeType.kt` (or a sibling). The gate:

```kotlin
suspend fun handleScan(
  rhId: Long?, raw: String, expected: KClass<out QRCodeType>,
  close: () -> Unit, onMatch: suspend (QRCodeType) -> Boolean,
): Boolean {
  val qr = parseQRCode(raw)
  return if (expected.isInstance(qr)) onMatch(qr)
         else { showWrongQRCodeAlert(rhId, qr, expected, close); true }   // true: Android dedups → alert shows once
}
```

  `showWrongQRCodeAlert(rhId, scanned, expected, close)` + the per-kind `description`/`whereToScan`
  metadata implement the design's message rules (recognised-wrong → `wrong_qr_code` +
  `"<desc>\n\n<where>"`; relay → `relay_address_alert_title` + `relay_address_alert_message`; Unknown → `invalid_qr_code` +
  expected-aware msg; **no-user** → no button + `qr_where_no_user`). Connect button dismisses the
  scanner first, then connects:
  `onConfirm = { close(); withBGApi { planAndConnect(rhId, qr.text, close = null) } }`.
  Nothing calls these yet. Build.

## Phase 1 — Kotlin migrations (one screen per commit, behavioural)

Each: replace the screen's bespoke validate+alert with one `handleScan` call; keep the screen's
own success/failure side effects in `onMatch`; verify the accepted-kind flow is unchanged and a
wrong-kind scan now shows the explaining alert. (The `File:line` references below are the
stable-verified locations; exact lines differ slightly on `master` — e.g. the migration scanner
is ~:208 and the desktop scanner ~:358.)

- [x] **C1.4 — ConnectView** (`NewChatView.kt:654`). `handleScan(rhId, text,
  QRCodeType.ConnectionLink::class, close) { qr -> withContext(Dispatchers.Default) { planAndConnect(rhId, qr.text, close = close).await() } }`
  (the `withContext` preserves `verifyAndConnect`'s threading; `close = close` MUST be named —
  `planAndConnect`'s 3rd positional param is `linkOwnerSig`). Removes the inline `verifyOnly` +
  `invalid_qr_code` block. (On stable this would also start accepting address-names; **master has
  no name case**.)
- [x] **C1.5 — ScanProtocolServer** (`ScanProtocolServer.kt:19`). `ServerAddress::class`; `onMatch`
  builds `UserServer(remoteHostId = rhId, null, qr.text, false, null, false, false)` and calls
  `onNext`. Removes the `parseServerAddress` + `smp_servers_invalid_address` block.
- [x] **C1.6 — MigrateToDevice** (`MigrateToDevice.kt:205`). `MigrationLink::class`, `close = {}`
  (no action button can appear on this onboarding screen). `onMatch` reuses `checkUserLink(qr.text)`
  **unchanged** — it does `MigrationFileLinkData.readFromLink` + netCfg + advances `MigrationToState`,
  handles `readFromLink == null`, and its inner `strHasSimplexFileLink` recheck is kept (harmless
  redundancy now that classification gates it). **No-user screen**: wrong-type alert shows generic
  guidance + no button (gate handles this).
- [x] **C1.7 — ConnectDesktopView** (`ConnectDesktopView.kt:359`). `DesktopAddress::class`;
  `onMatch` keeps `sessionAddress.value = qr.text` then `connectDesktopAddress(sessionAddress,
  qr.text)`. Removes raw-string-to-core path's reliance on core for the wrong-type case.
- [x] **C1.8 — ScanCodeView** (`ScanCodeView.kt:19`). `QRCodeType.SecurityCode::class` (the
  verify screen scans a contact's security code); `onMatch` KEEPS close-on-success and the
  `incorrect_code` alert on failure (handleScan adds neither).

## Phase 1 — Kotlin cleanup (subtraction)

- [x] **C1.9 — remove dead helpers.** `verifyOnly` / `verifyAndConnect` are removed **with the
  ConnectView migration** (C1.4 — same file; see As-built delta #4); this step additionally removes
  **`strIsSimplexLink`** (`NewChatView.kt`), also dead — the classifier inlines `parseToMarkdown` to
  read the link type, so nothing calls it. `connect()` stays (the paste path uses it). Pure
  deletion; build + tests green.

---

## Phase 2 — iOS (mirror, after Phase 1 lands)

Same shape, with the documented divergences (design doc → "iOS verification"): `Result<…> -> Void`
callbacks (keep `switch resp`, route `.success(r).string`, keep `.failure`); `planAndConnect(_ link,
theme:, dismiss:)` (no rhId); alerts via global `showAlert(_ Alert)` — `mkAlert` for button-less,
a full `Alert(…primaryButton…)` for Connect; `ScanCodeView.verify` returns `(Bool, String)?`;
`ScanProtocolServer` gains validation (was silent).

- [x] **C2.0a (iOS prep)** — expose the iOS file-link parser: `strHasSimplexFileLink` is `private`
  in `Shared/Views/Migration/MigrateToDevice.swift:643` (same as Kotlin) → make it accessible.
  (No `close`-threading needed on iOS — each view gets `@Environment(\.dismiss)`, unlike Kotlin's
  explicit `close`.)
- [x] **C2.0 — type + classifier in the APP layer**, NOT the `SimpleXChat` package. Place in
  `Shared/Views/NewChat/` (e.g. `QRCodeType.swift`): the enum (mirror) + `parseQRCode(raw) ->
  QRCodeType`. Reason: the connection-link parser (`strConnectTarget` on stable; **`strHasSingleSimplexLink`
  on master**, `NewChatView.swift:873`) and `strHasSimplexFileLink`
  (`MigrateToDevice.swift:643`) are **app-layer**; only `parseServerAddress` is in the package, and
  the package can't see app symbols. (The design doc's `SimpleXChat/QRCodeType.swift` note is
  superseded — same layering reason as Kotlin finding #6.)
- [ ] **C2.1** — XCTest for `parseQRCode` (same fixtures as C1.1).
- [x] **C2.2** — `handleScan` equivalent (acceptance via closure) + `showWrongQRCodeAlert`
  (global `showAlert`; Connect builds full `Alert`; no-user gate on `ChatModel.shared.currentUser`).
  Thread `theme: AppTheme` (from `@Environment`) so the Connect button can call
  `planAndConnect(qr.text, theme: theme, dismiss: true)` — iOS has no `rhId`.
- [ ] **C2.3** — strings: add to `Localizable.xcstrings`/`NSLocalizedString`; **derive the iOS
  breadcrumb wording** for `qr_where_*` from the iOS navigation (its Settings/desktop entry differ
  from Android — do NOT copy Android labels).
- [x] **C2.4–C2.8** — migrate the 5 iOS call sites (`NewChatView.swift:698`, `ScanProtocolServer.swift:35`,
  `MigrateToDevice.swift:204`, `ConnectDesktopView.swift:406`, `ScanCodeView.swift:34`), preserving
  each screen's success/failure handling (esp. ScanCodeView: `connectionVerified` + `dismiss()` +
  `showCodeError`; server: `addServer(…, dismiss)`).
- [x] **C2.9** — remove dead iOS helpers: `strIsSimplexLink` (its only caller, the connect
  screen's `processQRCode`, was migrated). `connect()` stays.

---

## Appendix A — new string keys (English; both platforms)

```
wrong_qr_code              = "Wrong QR code"
qr_type_connection         = "This is a %s."   # %s = SimplexLinkType.description (e.g. "SimpleX group link")
qr_type_simplex_address    = "This is a SimpleX address."
qr_type_server_address     = "This is a SimpleX server address."
qr_type_migration_link     = "This is a link to migrate to another device."
qr_type_desktop_address    = "This is an address for linking a mobile to a SimpleX desktop app."
qr_type_security_code      = "This is a contact's security code."
qr_where_connection        = "Tap New chat, then Paste link / Scan."
qr_where_server            = "Tap your profile image, then Settings, Network & servers, your servers, Scan server QR code."
qr_where_desktop           = "Tap your profile image, then Use from desktop, Scan QR code from desktop."
qr_where_migration         = "On the new device, when first setting up the app, choose Migrate from another device."
qr_where_security_code     = "Open the chat, tap the contact's name, then Verify security code."
qr_where_no_user           = "Finish setting up SimpleX first, then scan this in the app."

# The `qr_type_*` lines are complete "This is a …" sentences (period included) so they don't run
# into the `qr_where_*` instruction. ConnectionLink uses `qr_type_connection` formatted with
# `SimplexLinkType.description` → e.g. "This is a SimpleX group link."
qr_action_connect          = "Connect"
```
Reused (do not re-add): `SimplexLinkType.description` (`simplex_link_*`), `relay_address_alert_title`,
`relay_address_alert_message`, `invalid_qr_code`, `code_you_scanned_is_not_simplex_link_qr_code`,
`smp_servers_check_address`, `incorrect_code`. (The `qr_where_*` breadcrumbs are self-contained full
sentences — they do NOT compose `network_and_servers` / `settings_section_title_use_from_desktop`.)
iOS `qr_where_*` values are re-derived from iOS navigation (C2.3), not copied.

## Appendix B — acceptance / definition of done

Per design doc → "Testing". Must all pass before considering done:
- `parseQRCode` test: as built, the Kotlin `ParseQRCodeTest` covers the **FFI-free branches**
  (migration links, desktop addresses, ordering, whitespace) and passes. The connection-link /
  server / unknown + **short-link** fixtures need the core native lib (Android instrumented suite —
  **not yet added**, see C1.1), and the iOS XCTest (C2.1) is **not yet written**. These remain
  open before "done".
- Cross-scan matrix on each screen: wrong kind → one alert naming the exact kind + the real
  button sequence; relay → "cannot be used to connect"; accepted kind → unchanged flow.
- Regression guards: verify screen closes on success / shows `incorrect_code` on wrong code;
  wrong-type held in frame shows the alert once (not ~1×/s); server screen says "check the
  address"; desktop sets `sessionAddress` first. (The stable-only "address-name now accepted"
  guard is N/A on master.)
- **No-user / onboarding**: non-migration scan on the MigrateToDevice scanner → kind named, no
  action button, generic "finish setup first" guidance (NOT "Tap New chat…").
- Camera components (`QRCodeScanner.*`, `CodeScannerView`/`ScannerInView`) untouched; core
  (`apiConnectPlan`, `connectRemoteCtrl`, `ConnectionPlan` dispatch) untouched.

## Risks / rollback

- Each commit is independent and build-green → revert any single screen migration without
  affecting others. The shared additions (C1.0–C1.3) are inert until a call site uses them.
- Highest-risk piece is `parseQRCode` classification (short links, ordering) — gated by C1.1's
  test before any migration commit relies on it.
- iOS breadcrumb wording (C2.3) is the only item requiring fresh iOS-navigation derivation; all
  structural verdicts are already verified (design doc → "iOS verification").
