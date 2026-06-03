# Plan: unify QR code scanning (one type, one alert)

This is the **design** — *what & why*: the problem, the chosen shape, the rationale, and the
verified constraints. Exact code, signatures, string keys/values, and the commit order are the
**single source of truth in `plan-unify-qr-scanning-impl.md`** (referenced inline as "impl plan
Cx / Appendix A"). When the two disagree, the impl plan wins on code/strings; this doc wins on
intent.

> **Branch note (stable vs master).** This design was verified against `stable`; the
> implementation branch is off `master`, which is **behind** stable. On master there is **no**
> `strConnectTarget` / `ConnectTarget` / SimplexName, and short-link classification goes through
> `parseToMarkdown` (size==1 → `Format.SimplexLink`), iOS through `strHasSingleSimplexLink`. So
> wherever the sections below name `strConnectTarget`/`ConnectTarget`/`simplexName` or cite a
> `file:line` (source map, type model, navigation/adversarial reviews), read them as the *stable*
> reference that motivated the design; the **authoritative master mapping is the impl plan's
> "As-built deltas vs this plan"** section. Name-link cases are **N/A on master**.

## Goal

Every QR scanner in the app should recognise **every** kind of QR code the app
uses — not just the one kind its screen happens to want. When the scanned code is
of the wrong kind for the current screen, show **one** alert that names exactly what
was scanned and the exact place to scan it (and offers the action where it is safe —
only ConnectionLink → Connect). One sealed type classifies every scan; one alert
function serves every screen.

This is a structural fix, not new behaviour: today each scanner re-implements its
own "is this my kind of link?" check and its own error message. We replace N
ad-hoc checks + N messages with **one classifier + one alert**, and each call site
collapses to a declaration of *which kind it accepts* plus its success handler.

Scope (confirmed): **multiplatform (Kotlin/Android) and iOS (Swift)** — the two
have near-identical structure, so the same type/classifier/alert is mirrored in
both. Desktop has no camera scanner (stub) and is unaffected.

---

## Problem today

Each scanner accepts one content kind and rejects everything else with a generic
"invalid" message that does not tell the user what they actually scanned or where
it belongs.

| Screen | Accepts | Validates with | Wrong-input message today |
|---|---|---|---|
| Connect to contact/group (`NewChatView` → `ConnectView`) | connection link | `strIsSimplexLink` / `strConnectTarget` | "Invalid QR code — not a SimpleX link QR code" |
| Verify security code (`ScanCodeView`) | numeric security code | caller `verifyCode` | "Incorrect (security) code" |
| Add server (`ScanProtocolServer`) | SMP/XFTP server address | `parseServerAddress` | Android: "Invalid server address"; iOS: *silent* |
| Migrate to this device (`MigrateToDevice`) | migration file link | `strHasSimplexFileLink` | "Invalid link" |
| Link a desktop (`ConnectDesktopView`) | `xrcp` desktop address | core `connectRemoteCtrl` | "Bad desktop address" |

Consequence: scan a contact link on the "Link a desktop" screen and you are told
"Bad desktop address" — true but useless. Scan a desktop address on the connect
screen and you are told "not a SimpleX link" — wrong and useless. The user is left
guessing which screen the QR belongs to.

### Source map (verified)

**Kotlin** (`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`)
- scanner component: `views/newchat/QRCodeScanner.kt` — `onBarcode: suspend (String) -> Boolean` (Android impl real; desktop stub)
- call sites: `views/newchat/NewChatView.kt:654`, `views/chat/ScanCodeView.kt:19`, `views/usersettings/networkAndServers/ScanProtocolServer.kt:19`, `views/migration/MigrateToDevice.kt:205`, `views/remote/ConnectDesktopView.kt:359`
- classifiers reused: `strConnectTarget`/`strIsSimplexLink` (`NewChatView.kt:828`,`:774`), `parseServerAddress` (`SimpleXAPI.kt:4844`), `strHasSimplexFileLink` (`MigrateToDevice.kt:727` → `simplex:/file` or `https://simplex.chat/file`)
- dispatch reused: `planAndConnect` (`views/newchat/ConnectPlan.kt:24`)

**Swift** (`apps/ios/`)
- scanner components: `ScannerInView` and `CodeScannerView` (`Shared/Views/NewChat/NewChatView.swift:750`); callback `(_ resp: Result<ScanResult, ScanError>) -> Void`
- call sites: `Shared/Views/NewChat/NewChatView.swift:698` (`processQRCode`; `ScannerInView` at :653), `Shared/Views/Chat/ScanCodeView.swift:34`, `Shared/Views/UserSettings/NetworkAndServers/ScanProtocolServer.swift:35`, `Shared/Views/Migration/MigrateToDevice.swift:204`, `Shared/Views/RemoteAccess/ConnectDesktopView.swift:406`
- classifiers reused: `strConnectTarget`/`strIsSimplexLink` (`Shared/Views/NewChat/NewChatView.swift:873`,`:858`), `parseServerAddress` (`SimpleXChat/API.swift:170`), `strHasSimplexFileLink`
- dispatch reused: `planAndConnect` (`Shared/Views/NewChat/NewChatView.swift:1308`)

---

## Design

Four pieces. Pieces 1–3 are new and shared; piece 4 is a one-line change at each
call site. The camera components are **not touched**.

### 1. One type — `QRCodeType`

A closed sum of every kind of code the app can scan. `Unknown` is the
genuinely-unrecognised case (a non-SimpleX QR, or garbage).

Six variants, each carrying the scanned `text`:
- **ConnectionLink** — also carries the link sub-type (contact / invitation / group / channel /
  relay), or none for a SimpleX address-*name*.
- **ServerAddress** — `text` only; the `UserServer` is built later in `onMatch`, so the
  classifier needs no `rhId`.
- **MigrationLink**, **DesktopAddress** — `text` only.
- **SecurityCode** — a contact's verification code. The QR is the raw code with no scheme; the
  core formats it as decimal groups of 5 separated by spaces (`verificationCode`), so it is
  recognised **by shape** — ≥32 digits once the grouping whitespace is stripped (`isSecurityCode`),
  checked last. The verify-code screen accepts this kind; the scanned text keeps its spaces so
  `verifyCode` still matches.
- **Unknown** — anything not recognised (a non-SimpleX QR, or garbage); no screen accepts it.

The type and classifier live in the **app/views layer** (`views/newchat/` on Kotlin,
`Shared/Views/NewChat/` on iOS) — **not** `model/` or the `SimpleXChat` package — because the
classifier calls view-layer `strConnectTarget`/`strHasSimplexFileLink` (on master: `parseToMarkdown`/`strHasSimplexFileLink`).

> Exact code: **impl plan C1.0** (Kotlin, canonical); the iOS enum mirrors it (**C2.0**, with
> the Phase-2 divergences).

### 2. One classifier — `parseQRCode`

`parseQRCode(raw)` composes the *existing* local parsers in a fixed priority and returns the
first match, else `Unknown`. No new parsing, no `rhId`, no side effects beyond the FFI parse:

1. `strHasSimplexFileLink` → **MigrationLink** (`simplex:/file` | `https://simplex.chat/file`)
2. `startsWith("xrcp:/")` → **DesktopAddress** (`DESKTOP_ADDRESS_SCHEME`; verified below)
3. the SimpleX-link parser → **ConnectionLink**, carrying its sub-type. On stable this is
   `strConnectTarget` (which also gives the address-**name** case → `linkType = null`); on
   `master` there is no name case, so the build uses `parseToMarkdown` (`size == 1`) +
   `Format.SimplexLink.linkType` and `linkType` is always non-null.
4. `parseServerAddress` → **ServerAddress**
5. `isSecurityCode` (≥32 digits once the grouping spaces are stripped — the core formats the
   code as decimal groups of 5 separated by spaces; no scheme) → **SecurityCode**
6. else → **Unknown**

> Exact code: **impl plan C1.0** (and its "As-built deltas vs this plan" note for the master
> classifier — `strConnectTarget` does not exist on master).

Visibility/layering note: `strHasSimplexFileLink` is currently **`private`** in
`MigrateToDevice.kt:727`, and `strConnectTarget` lives in `views/newchat`. The
classifier must be able to call both — either place `parseQRCode` in `views/newchat`
(next to `strConnectTarget`) or expose these parsers. Do not put it under `model/` if
that forces a view→model inversion.

Ordering rationale: the kinds are disjoint URI shapes (`simplex:/file` vs the
`xrcp` scheme vs `simplex:`/`https://simplex.chat` connection links vs `smp:`/`xftp:`
server addresses), so order only matters as a tie-break safety net. Migration and
desktop are checked first because they are the most specific.

`ConnectionLink` is what the connect screen accepts. On stable it also folds in the SimpleX
**address-name** case (`strConnectTarget` → `ConnectTarget.Name`); **on master there is no
address-name case**, so a `ConnectionLink` always carries a real `linkType`. Relay links remain
`ConnectionLink(linkType = relay)` and keep their existing in-flow rejection inside
`planAndConnect` — no behaviour change there.

### 3. One alert + one accept-gate — `handleScan`

`handleScan` is the shared gate each call site routes through. It classifies the scan and:
- if it **is** the accepted kind, runs the screen's `onMatch` (which keeps that screen's own
  success/failure side effects and returns the bool the Android scanner uses for dedup);
- otherwise shows the one wrong-type alert and returns **true** — so the Android scanner caches
  the code and the alert shows **once**, not ~1×/s (it only dedups on `true`,
  `QRCodeScanner.android.kt:102-109`; returning `false` re-fires the alert).

`handleScan` ONLY adds the wrong-*type* alert; it never replaces a screen's handling of an
accepted scan. Acceptance is by sealed sub-type — no parallel "kind" enum (the verify screen
accepts `SecurityCode`). `rhId` is passed through solely for the ConnectionLink "Connect" button.

> Exact signature/body (`handleScan(rhId, raw, expected, close, onMatch)`): **impl plan C1.3**.

`showWrongQRCodeAlert` is the **one** alert, driven by per-kind metadata defined
**once** (not per call site):

The message MUST name the **exact kind** scanned and the **exact screen** where it
should be scanned — derived once from the scanned kind, independent of which screen
the user is on:

- **Recognised wrong kind** — title *Wrong QR code*; message is the kind's `description`, a
  blank line, then `whereToScan` (the two are separate lines — descriptions are article-less
  noun phrases like "SimpleX group link", so "This is X." would be ungrammatical/unlocalisable).
  Button: ConnectionLink (non-relay) → *Connect* (`planAndConnect`, launched in a coroutine);
  ServerAddress / DesktopAddress → optional no-payload *Open <screen>*; MigrationLink / relay →
  none.
- **No active user** (`currentUser == null`, e.g. the migration scanner during onboarding) — no
  action button (Connect is a no-op without a user, and the other screens don't exist yet) and
  `whereToScan` is replaced by generic "finish setting up SimpleX first" guidance.
- **Relay link** — the existing relay alert, **title + message** (`relay_address_alert_title`
  "Relay address" + `relay_address_alert_message` "cannot be used to connect"); no
  description prefix, no `whereToScan`, no button.
- **Unrecognised** (`Unknown`, where a code wasn't the expected kind) — title *Invalid QR code*
  with an expected-kind-aware message (e.g. "check the address" on the server screen; the
  not-a-SimpleX-link message on the connect screen), no button.

> Exact title/string keys: **impl plan Appendix A**.

Per-kind metadata, defined once (one row per kind; ConnectionLink split by sub-type for the
description). The Button column is what is **verified feasible** (see the navigation review
below — the first draft overpromised navigation the code does not support). `description` reuses
existing labels; `whereToScan` is the navigation target, expressed as a **real button sequence
from the chat list** (verified, Android). The exact localized sentences are the impl plan's
single source of truth (**Appendix A**, `qr_where_*`):

| Scanned kind | `description` (exact kind) | `whereToScan` target | Button (verified) |
|---|---|---|---|
| ConnectionLink (contact/invitation/group/channel) | "This is a " + `SimplexLinkType.description` + "." (e.g. "This is a SimpleX group link.") | New chat ▸ Paste link / Scan | ✅ "Connect" (`planAndConnect`) |
| ConnectionLink (name) | "This is a SimpleX address." | New chat ▸ Paste link / Scan | ✅ "Connect" |
| ConnectionLink (**relay**) | "SimpleX relay address" | — (relay message: "cannot be used to connect") | (none) |
| ServerAddress | "This is a SimpleX server address." | profile image ▸ Settings ▸ Network & servers ▸ Scan server QR code | ⚠️ optional "Open Network & servers" (no payload) |
| MigrationLink | "This is a link to migrate to another device." | onboarding ▸ Migrate from another device (not reachable from a running app) | ❌ none (onboarding/destructive) |
| DesktopAddress | "This is an address for linking a mobile to a SimpleX desktop app." | profile image ▸ Use from desktop ▸ Scan QR code from desktop | ⚠️ optional "Open Use from desktop" |
| SecurityCode | "This is a contact's security code." | chat ▸ contact's name ▸ Verify security code | (none) |
| Unknown | (cannot name it) | expected-kind aware message (see above) | (none) |

(The `description` cells are complete "This is a …" sentences — period included — so they don't
run into the `whereToScan` instruction; ConnectionLink rows format `qr_type_connection` with
`SimplexLinkType.description`.)

Entry step (the bug this review fixed): **Settings** and **Use from desktop** are not
top-level — both are reached by tapping the **profile image** on the chat list, which
opens the user menu (`ChatListView.kt:584` → `UserPicker`). **New chat** is a separate
compose button. Any breadcrumb that starts at "Network & servers" or "Use from desktop"
without the profile-image step is unfollowable.

`description` and `whereToScan` are the **only** per-kind data; the message is always
`"<description>\n\n<whereToScan>"` regardless of which screen scanned it (relay is the one
exception — `relay_address_alert_title` + `relay_address_alert_message`). Only one action path (ConnectionLink →
`planAndConnect`) is wired; the rest rely on the exact-place text (optionally a no-payload
"Open <screen>" button). This satisfies the requirement — every wrong scan tells the user
exactly what it is and where to scan it.

### 4. Call sites collapse to one line each

Every scanner callback becomes: declare accepted kind + success handler. The camera
component signature is unchanged.

Each `onMatch` keeps the screen's existing success/failure handling; `handleScan`
only adds the wrong-type alert.

Per screen — accepted kind, `rhId`, and the side effects `onMatch` must preserve:

| Screen | accepts | `rhId` | `onMatch` preserves |
|---|---|---|---|
| ConnectView | ConnectionLink | `rhId` | `planAndConnect` (on **stable** also accepts address-names; **master** has no name case) |
| ScanProtocolServer | ServerAddress | `rhId` | build the `UserServer` here, then existing `onNext`/`addServer` (which closes) |
| MigrateToDevice | MigrationLink | `null` | `checkUserLink`'s success branch (read link data, net config, advance migration state) |
| ConnectDesktopView | DesktopAddress | `null` | set `sessionAddress` (display) **before** `connectDesktopAddress` |
| ScanCodeView | SecurityCode | `null` | keep close-on-success **and** the `incorrect_code` alert on a wrong code |

> Exact one-liners per call site: **impl plan C1.4–C1.8**.

Wiring note: `close` in the examples must dismiss the current scanner so the Connect/Open
button can leave it. `ConnectView`/`ScanCodeView` already receive `close`; `ScanProtocolServer`,
`ScanDesktopAddressView`, and the MigrateToDevice scanner do not — thread it in (small change),
or pass `{}` where no action button can appear.

`rhId` note: only `ConnectView` and `ScanProtocolServer` have `rhId` in scope; the other three
pass `null` (they aren't host-scoped). `rhId` is used solely for the ConnectionLink "Connect"
button → `planAndConnect`. **iOS has no `rhId` at all** (`planAndConnect(_ link, theme:, dismiss:)`),
so the iOS helper threads `theme: AppTheme` instead, not `rhId`.

iOS differs in shape (do NOT assume parity): the callback is
`(_ resp: Result<ScanResult, ScanError>) -> Void`, returns no Bool, and controls
re-scanning via `scanMode`/`scannerPaused`. Each iOS site keeps its `switch resp`,
routes only `.success(r).string` into the handleScan-equivalent, and keeps its
`.failure(e)` (camera error) branch. Acceptance is a predicate/closure on iOS, since
Swift enum cases with associated values can't be referenced as `KClass`-style values.

Net code change: **replace** each screen's bespoke validate+alert logic with one
`handleScan` call — some checks are deleted (e.g. the connect-screen `strIsSimplexLink`
check), some are preserved inside `onMatch` (e.g. `ScanCodeView`'s `incorrect_code`), and
the iOS server screen had none and gains validation; **add** one type + one classifier +
one alert per platform. The per-site logic shrinks; the shared logic lives in one place.

---

## iOS verification (checked against Swift sources)

Confirmed mirrors: `SimplexLinkType.description` (same strings, `ChatTypes.swift`),
`ConnectTarget` = `.link(text, linkType, linkText)` / `.name(SimplexNameInfo)`
(`NewChatView.swift:868`), and `strConnectTarget` / `strIsSimplexLink` / `parseServerAddress`
exist with the same roles — so the classifier mirrors cleanly. Real differences the iOS
implementation MUST handle (do **not** copy Kotlin verbatim):

1. **`planAndConnect` signature differs** — iOS is `planAndConnect(_ link, theme: AppTheme,
   dismiss: Bool, …)` (NO `rhId`; needs `theme` from `@Environment`). The Connect button →
   `planAndConnect(qr.text, theme: theme, dismiss: true)`.
2. **Alert mechanism differs** — per-screen iOS scanners surface alerts via a state binding
   (`alert = .newChatSomeAlert(…mkAlert…)`), not an imperative call. The unified
   `showWrongQRCodeAlert` should use the **global** `showAlert(_ alert: Alert)`
   (`ContentView.swift:494`) — the same path the existing relay alert uses. Note `mkAlert`
   (`ContentView.swift:507`) builds a **title+message only** alert (no buttons), so it covers
   the button-less cases; the ConnectionLink "Connect" case must build a full
   `Alert(title:message:primaryButton:secondaryButton:)` instead.
3. **`ScanCodeView.verify` differs** — iOS `verify: (String?) async -> (Bool, String)?`
   (async, optional tuple); on success it sets the `connectionVerified` binding + `dismiss()`,
   on failure sets a `showCodeError` `@State` (not `AlertManager`). The iOS onMatch replicates
   that, not the Kotlin `Bool` + `incorrect_code` form.
4. **`ScanProtocolServer` has no pre-validation today** — iOS builds `UserServer.empty`,
   sets `.server = r.string`, calls `addServer(...)` (which validates), and shows **no** alert.
   Under the plan the classifier's `parseServerAddress != null` gate means an invalid server QR
   becomes `Unknown` → now gets the "check the address" alert. iOS behaviour **improvement**
   (no longer silent), but a behaviour change to call out. `addServer` takes `dismiss` (map
   `close` → `dismiss`).
5. Scanner callback is `Result<…> -> Void` with `scanMode`/`scannerPaused` (already noted):
   keep the `switch resp`, route `.success(r).string` through the helper, keep `.failure`.

`QRCodeType.serverAddress` now carries only `text` on **both** platforms (Kotlin dropped the
embedded `UserServer`), so the two type definitions match and `parseQRCode` is `rhId`-free.

Onboarding / no-user (finding #13) verified on iOS: `apiConnectPlan` guards
`ChatModel.shared.currentUser?.userId` and returns a no-op without a user
(`SimpleXAPI.swift`), and the iOS `MigrateToDevice` scanner runs pre-account (above) — so the
`currentUser == null` gate (suppress action button + generic guidance) is required on iOS too.

---

## Adversarial review: navigation feasibility (verified against code)

The "navigate + act with the scanned payload" idea holds for only **one** of the four
non-Unknown kinds. Verified by reading the targets directly:

- **ConnectionLink → "Connect" — FEASIBLE.** `planAndConnect(rhId, link, close, cleanup)`
  (`views/newchat/ConnectPlan.kt`) is already called from 7 independent contexts —
  `FramedItemView.kt:354`, `ChatListView.kt:741/828`, `NewChatSheet.kt:579`,
  `NewChatView.kt:786`, `GroupMemberInfoView.kt:978`, `ChatPreviewView.kt:344` — with
  `close = null`. It depends only on its args + global `ChatModel`/`ModalManager` and drives
  its own sheets. An alert button can safely call it from any scanner. This is also the most
  common real confusion (a contact/group link scanned on the wrong screen), so the one
  feasible action covers the main case.

- **ServerAddress → "Add server" — NOT actionable.** `ScanProtocolServer`'s success calls
  `onNext = addServer(scope, server, userServers, serverErrors, serverWarnings, rhId, close)`
  (`ProtocolServersView.kt:285-296`). Those are in-memory editor lists owned by the operator's
  server editor; the scan appends to a list the user then **Saves** — it does not persist on
  its own. None of that state exists in a connect-scanner context, and the path is Android-only
  and several levels deep in Settings. A button can at best open Network & servers WITHOUT the
  payload. → text guidance; optional no-payload "open" button.

- **MigrationLink → "Migrate" — UNSAFE, do not navigate.** Migrate-to-this-device is an
  onboarding-only flow (entry: `WelcomeView.kt:207`, `SimpleXInfo.kt:98/169`, on
  `ModalManager.fullscreen`). It initializes the chat controller/database from the downloaded
  archive and ends by setting `OnboardingStage.OnboardingComplete`
  (`MigrateToDevice.kt:676,707,717`). Entering it from a logged-in connect scanner is
  destructive and nonsensical. → text guidance only; no navigation button.

- **DesktopAddress → "Link a desktop" — fragile, navigation-only at best.** `ConnectDesktopView`
  (opened from `UserPicker.kt:362`) is a lifecycle-heavy modal: it starts/stops the Android
  service (`DisposableEffect`), runs multicast/known-desktop discovery on launch, and drives a
  `remoteCtrlSession` state machine with verification; the scanned address is consumed only in
  the `session == null` sub-state (`ConnectDesktopView.kt:103,140`). "Open + prefill" is
  possible; "auto-connect with the address" is not a single safe action. Android-only.
  → optional open (with prefill); never claim auto-connect.

**Resolution:** wire the action button **only** for ConnectionLink. For the other kinds the
single alert still does its core job — name the exact kind and the **real button sequence**
to scan it (see the per-kind table for the verified breadcrumbs, which start at the chat-list
profile image / New chat button). An optional no-payload "Open <screen>" button is reasonable
for ServerAddress and DesktopAddress; never for MigrationLink. This satisfies the requirement
and keeps the code small — only one action path is wired.

**iOS — all four verdicts now verified in Swift** (no longer by symmetry):
`planAndConnect` is a global function (ConnectionLink actionable); `addServer` is bound to the
server-editor bindings `$userServers`/… (ServerAddress not actionable); `MigrateToDevice` is
entered only from onboarding (`CreateProfile.swift:269`, `SimpleXInfo.swift:99`), pre-account;
`ConnectDesktopView` (`ChatListView.swift:103`) drives `m.remoteCtrlSession`/`sessionState`
(searching → connecting → pendingConfirmation → verify) and consumes the scanned address only
when there is no session — stateful, no single safe auto-connect.

## Second adversarial review — bugs found & corrected

Traced against runtime behaviour; the design above now reflects the fixes.

| # | Severity | Finding | Fix (applied above) |
|---|---|---|---|
| 1 | high | `ScanCodeView` one-liner dropped `close()` on success AND the `incorrect_code` alert on a wrong-but-accepted code — `handleScan` only adds the wrong-*type* alert. | `onMatch` keeps the screen's own success/failure side effects; example corrected. |
| 2 | high | `Unknown` fallback reused "…not a SimpleX link" everywhere — wrong/unhelpful on server/migration/desktop screens (loses "check the address"). | Unknown message is expected-kind aware; keep `invalid_qr_code` title. |
| 3 | high | `handleScan` returned `false` on wrong type → Android re-fires the alert ~1×/s (scanner dedups only on `true`, `QRCodeScanner.android.kt:102-109`). | Return `true` on the wrong-type path (show once). |
| 4 | med | Parallel `QRCodeKind{…Code}` vs `QRCodeType{…Unknown}` — two enums to keep in sync, names mismatched. | Use `expected: KClass<out QRCodeType>`; no second enum. |
| 5 | med | "iOS is the same shape" false — iOS returns Void, uses `Result`/`scanMode`, keeps `.failure`. | iOS asymmetry documented; predicate-based acceptance on Swift. |
| 6 | med | Classifier in `model/` can't reach `strConnectTarget` (views) and `strHasSimplexFileLink` (private). | Visibility/layering note added; place `parseQRCode` in `views/newchat` or expose parsers. |
| 7 | med | Name (`simplexName`) QRs change from rejected (today's scan path) to accepted. | Documented as an intended improvement (parity with paste). **N/A on master** — no name link type (see Branch note). |
| 8 | low | `strConnectTarget` called twice (FFI). | Bound once in the `when`. |
| 9 | low | `sessionAddress.value = text` dropped for desktop. | Preserved in the example. |
| 10 | low | Migration `onMatch` ≠ literal `startDownload`; must replicate `checkUserLink`. | Noted; redundant inner `strHasSimplexFileLink` recheck dropped. |
| 11 | scope | Paste paths keep old per-screen alerts → scan/paste UX diverges. | Open scope decision: optionally route paste through `parseQRCode`. |
| 12 | high | "Where to scan it" breadcrumbs started inside Settings/Use-from-desktop — skipped the entry step, so they were unfollowable. | Sequences now start at the chat-list **profile image** (opens user menu → Settings / Use from desktop) and the **New chat** button; verified against `ChatListView.kt:584`/`UserPicker`/`NewChatSheet`. iOS sequences to be re-derived. |
| 13 | high | Alert message/action are screen-independent, but the **MigrateToDevice scanner runs during onboarding, pre-account** (`WelcomeView.kt:206-210`; `currentUser` null). Breadcrumbs point to non-existent UI and the "Connect" button is a no-op (`apiConnectPlan` returns null without a user). | Gate on `chatModel.currentUser.value != null`: no-user → no Connect button + generic "finish setup first" guidance instead of in-app breadcrumbs. Single condition, both platforms. |
| 14 | low | "Connect" button pseudocode called `planAndConnect` without a coroutine; it is `suspend`/`async`. | Wrap in `withBGApi { … }` (Kotlin) / `Task { … }` (iOS), as every existing caller does. |
| 15 | low | "delete five validate+alert blocks per platform" imprecise — iOS server screen has none (silent), and `ScanCodeView`'s `incorrect_code` is **preserved** (moved into `onMatch`), not deleted. | Wording corrected below. |
| 16 | med | The wrong-type alert's Connect/Open button needs a `close` to dismiss the scanner, but `ScanProtocolServer(rhId, onNext)` and `ScanDesktopAddressView(sessionAddress)` don't expose `close`, and the MigrateToDevice scanner never closes. The one-line examples used `close` as if always in scope. | Thread the modal `close` into those 3 call sites (small per-site change); where no button can appear (and on the migration/no-user screen), `close` may be a no-op. |
| 17 | low | iOS `mkAlert` has no buttons, so the ConnectionLink "Connect" alert can't use it. | iOS builds a full `Alert(…primaryButton…)` for the Connect case; `mkAlert` only for button-less cases (see iOS verification). |

## New strings — design intent

The **exact keys and English values are the impl plan's single source of truth
(Appendix A)**; this section states only the design intent so the wording can be reviewed.

- A title for a recognised wrong kind, one description per kind, one where-to-scan breadcrumb
  per navigation target, an action label, and the no-user guidance string.
- Reuse existing strings wherever possible: `SimplexLinkType.description` (`simplex_link_*`) for
  connection-link kinds, `relay_address_alert_title` + `relay_address_alert_message` for relay, `invalid_qr_code` /
  `code_you_scanned_is_not_simplex_link_qr_code` / `smp_servers_check_address` for the
  unrecognised cases.
- The message is assembled as `"<description>\n\n<whereToScan>"` — kind as a noun phrase, place
  as its own sentence — to avoid per-language article problems.
- The where-to-scan breadcrumbs MUST follow the **real button sequence from the chat list**,
  including the entry step (Settings and Use from desktop live in the user menu opened by the
  **profile image** — `ChatListView.kt:584` `UserProfileButton` → `UserPicker`; New chat is its
  own compose button). The verified Android sequences are in Appendix A; the **iOS sequences
  must be re-derived** from iOS navigation (its Settings entry differs), not copied.

---

## Desktop scheme — VERIFIED

`DESKTOP_ADDRESS_SCHEME = "xrcp:/"` (single slash — **not** `xrcp://`).

Verified against the source of truth: the `StrEncoding RCSignedInvitation` instance in
`simplexmq/src/Simplex/RemoteControl/Invitation.hs` — the QR-encoded form passed to
`connectRemoteCtrl`. The signed invitation encodes as:

    strEncode invitation <> "&ssig=" <> ssig <> "&idsig=" <> idsig

and `strEncode invitation` (same file, `StrEncoding RCInvitation`) begins with the
literal `"xrcp:/"`, followed by `<ca>@<host>:<port>#/?v=...`. The parser (`strP`)
likewise opens with `A.string "xrcp:/"`, and the instance carries the comment
*"URL-encoded and signed for showing in QR code."* So a scanned desktop address is
always:

    xrcp:/<ca>@<host>:<port>#/?v=...&app=...&ts=...&skey=...&idkey=...&dh=...&ssig=...&idsig=...

`startsWith("xrcp:/")` is therefore the correct, tightest local detector — no core call
is needed for classification (core still does full validation on connect, unchanged).
Matching `"xrcp:/"` also covers any hypothetical future `xrcp://` form, since that
string starts with `xrcp:/`; there is no `://` form today.

No open details remain before coding.

---

## Coverage check — link/QR inventory (swept, both platforms)

Verified the five scanners are the complete set (no 6th `QRCodeScanner` /
`CodeScannerView` / `ScannerInView`), and every QR the app *generates*
(`SimpleX(Created)LinkQRCode`, `MutableQRCode`, `QRCode`) maps to a kind we handle:
contact/group/invitation/address links → ConnectionLink; server-editor QR → ServerAddress;
verify code → SecurityCode (recognised by shape); migrate-from-device link → MigrationLink;
desktop "link a mobile" QR → DesktopAddress. Findings:

- **Short links — must be tested.** A connection link has two encodings: the full
  `simplex:/…` / `https://simplex.chat/…` form **and** a server-hosted **short link**
  `https://<smp-server>.../a#…` (contact/group/invitation variants; the modern 6.x default,
  toggled by `SimpleXCreatedLinkQRCode(link, short)`). Both must classify as
  `ConnectionLink`. The design already does, because classification goes through the **core
  markdown parser** (`strConnectTarget` on stable; **`parseToMarkdown` on master** — see impl
  C1.0 / As-built deltas), which recognises short links — the connect screen accepts them
  today. **Caution:** never shortcut ConnectionLink detection
  with a prefix test on `simplex.chat` — short links use the **SMP server's** domain, so a
  prefix test would drop every short link to `Unknown`. Add short-link fixtures to the
  `parseQRCode` test.
- **Channel web link** (`privacy_chat_list_open_web_link`, commit #7039) — a shareable
  **web URL** (v6.5 "Safe web links"), not a scan-to-connect link; the channel *join* link
  is `ConnectionLink(channel)`. Out of scope; a web-link QR → `Unknown` (acceptable).
- **WebRTC ICE servers** (`stun:`/`turn:`, RTC Servers) — **text-only config, no scanner**.
  Not missed. Such a QR scanned anywhere → `Unknown` (no screen accepts ICE servers).
- **Notification servers** (`ntf:`) — no scanner, no address scan path. Not missed.
- **SimplexName** (`contact.simplex`) and **business addresses** — on stable, folded into
  `ConnectionLink` (`strConnectTarget` → `ConnectTarget.Name` / contact link). **N/A on master**:
  there is no name link type, so this case does not arise (see As-built deltas).

## Commit / diff plan

The build order — add the shared mechanism (inert) → migrate one screen per commit →
remove dead helpers — is the **impl plan's** job, with exact commits and acceptance. The
design constraint it must honour: structure separated from behaviour, each commit builds, the
camera components are never modified.

---

## Testing

- Unit-test `parseQRCode` with one fixture per kind + garbage → asserts the right
  variant. This is the load-bearing test: classification is the whole design.
  Include **both** connection-link encodings: full (`simplex:/contact#…`,
  `https://simplex.chat/group#…`) **and** short (`https://<smp-server>/a#…`,
  contact/group/invitation) → all must be `ConnectionLink`; a `stun:`/`turn:` and a
  channel web URL → `Unknown`. (Harness note: those fixtures call the core parsers (FFI) and
  need the instrumented suite — see impl C1.1. As built, only the migration/desktop branches are
  covered by the JVM `ParseQRCodeTest`; the rest is still to do.)
- Cross-scan matrix (manual, Android + iOS): for each scanner screen, scan each
  other kind of QR and confirm the one alert (a) names the **exact** kind — e.g. a
  group link → "SimpleX group link", a server QR → "Chat server address" — and (b)
  states the **real button sequence** to scan it, starting from the chat list — e.g.
  "Tap New chat ▸ Paste link / Scan"; "Tap your profile image ▸ Settings ▸ Network &
  servers ▸ … ▸ Scan server QR code"; "Tap your profile image ▸ Use from desktop ▸ Scan
  QR code from desktop"; migration → "Migrate from another device" on new-device setup.
  Verify each breadcrumb is actually followable end to end (the entry step must be the
  profile image or New chat button, not a mid-tree screen). Relay link → the "cannot be
  used to connect" message. Scan the accepted kind → normal flow.
- Regression guards (from 2nd review):
  - verify screen: correct code → view **closes**; wrong code → **`incorrect_code`** shown;
    a contact link → wrong-type alert (not "incorrect code").
  - wrong-type scan held in frame → alert shows **once**, does not repeat ~1×/s.
  - server screen: malformed server QR → message still says to check the **server address**,
    not "not a SimpleX link".
  - (stable only) name (`simplexName`) QR on connect scanner → routed to `planAndConnect`;
    **N/A on master** (no name case).
  - desktop scan still sets `sessionAddress` before connecting.
  - **onboarding / no active user**: scan a non-migration QR on the MigrateToDevice scanner
    (pre-account) → alert names the kind but shows **no** "Connect" button and the generic
    "finish setup first" guidance (NOT "Tap New chat…"), since that UI doesn't exist yet.

---

## Non-goals / risks

- **Not** changing the camera/scanner components or the `onBarcode`/`processQRCode`
  signatures — keeps the diff small and platform code untouched.
- **Not** changing core (`apiConnectPlan`, `connectRemoteCtrl`) or the
  `ConnectionPlan` dispatch — `planAndConnect` is reused as-is.
- Navigation is now **resolved, not a risk**: verified that only ConnectionLink →
  `planAndConnect` is a context-free action, so it is the only wired button (see the
  Adversarial review section). Server/desktop get optional no-payload "open" buttons;
  migration gets text only. The type and classifier are unaffected by this choice — only
  `showWrongQRCodeAlert`'s per-kind action metadata.
- Desktop `xrcp:/` scheme is verified against simplexmq source (see above) — not a risk.
