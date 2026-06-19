# iOS: open SimpleX links in chat messages via in-app connect flow

## Problem

On iOS, tapping a **SimpleX connection/invitation link inside message text** does nothing — it never reaches the connection flow. Reproduced on iPhone 17 (v6.5.2 and v6.5.5). On the same screens, tapping a web link (opens browser), a `mailto:`/`tel:` link, and the connection-link **card** all work. Notably it was **device-specific**: dead on an iPhone 17 but working on an iPhone 12 running the **same iOS version**, with only **one** SimpleX app installed.

## Root cause

Inline links are dispatched in `MsgContentView.handleTextTaps` (`apps/ios/Shared/Views/Chat/ChatItem/MsgContentView.swift`):

- web links (`webLinkAttrKey`) → `openBrowserAlert` → `UIApplication.shared.open` (Safari)
- everything else → `UIApplication.shared.open(url)`

SimpleX links fell into the second branch. Two facts make this the bug:

1. **The URI is always the `simplex:` custom scheme.** The core markdown parser normalizes every connection link to the `simplex:` scheme via `simplexConnReqUri` / `simplexShortLink` (`src/Simplex/Chat/Markdown.hs:344,353`), regardless of whether the message contained `https://simplex.chat/…` or `simplex:/…` (see `tests/MarkdownTests.hs`). So the tap always calls `UIApplication.shared.open("simplex:/contact#…")`.

2. **`simplex:` is registered to this app, and the app is in the foreground.** `UIApplication.shared.open` is an OS app-launch API: it asks iOS (LaunchServices) to resolve the scheme to its registered app and activate it. Here the registered app is SimpleX itself, already foregrounded. **Re-entering the same foreground app through `open()` is not a supported operation** — `open()` exists to hand a URL to a *different* app or the system. When the resolved target is the calling foreground app, the outcome is undefined: on some devices iOS still delivers the URL to `onOpenURL`, on others it is a silent no-op (`open` returns `false`, no error, no UI).

That undefined outcome is decided by device-local OS state (scheme resolution / launch services), which is why identical code + identical OS + identical single app behaved differently on the iPhone 12 (delivered → connected) and the iPhone 17 (no-op → dead). It is **not** an OS-version rule and **not** a multiple-handler conflict — both were ruled out (same OS; single install).

This also explains the full symptom matrix — only the path that re-enters the same app via `open()` is affected:

| Tapped | Dispatch | Target | Result |
|---|---|---|---|
| Web link | `openBrowserAlert` → `open()` | Safari (other app) | works |
| `mailto:` / `tel:` | `open()` | Mail / Phone (other apps) | works |
| Invite card | `planAndConnect` in-process | this app, no `open()` | works |
| Inline SimpleX link | `open("simplex:…")` | this app (self), foreground | undefined → dead |

The underlying cause is using the **wrong mechanism**: an OS hand-off API to perform an **in-app** action. Every other connect path handles the connection in-process and never leaves the app:

- the card: `planAndConnect` directly (`FramedItemView.swift`)
- the share extension: `ShareSheet.openExternalLink` sets `ChatModel.appOpenUrl`
- multiplatform: `openVerifiedSimplexUri` → `connectIfOpenedViaUri` → `planAndConnect`

Inline links were the lone exception delegating to the OS, making them hostage to undefined self-open behavior.

## Fix

Restore the three-way dispatch the multiplatform clients use (`WEB_URL` / `OTHER_URL` / `SIMPLEX_URL`):

- web → `openBrowserAlert` (unchanged)
- `mailto:` / `tel:` → `UIApplication.shared.open` (unchanged — these target other apps)
- **SimpleX → `ChatModel.appOpenUrl`** — the same sink `onOpenURL` feeds, leading to `connectViaUrl` → `planAndConnect`, entirely **in-process** with no OS round-trip

SimpleX links are identified by a dedicated attribute key (`simplexLinkAttrKey`) set on the `.simplexLink` format, mirroring the multiplatform `SIMPLEX_URL` annotation tag, rather than sniffing the URL string — so all link types (contact, invitation, group, channel, relay) are covered.

This is correct regardless of the exact device-local trigger, because it removes the dependency on iOS re-delivering a self-owned URL. The invite card already proves the in-process path works on the affected device.

Also fixes the same issue for the **"Send questions and ideas"** (Settings) and **"connect to SimpleX Chat developers"** (chat help) buttons, which opened `simplexTeamURL` (a `simplex:` link) the same broken way.

## Scope

- `apps/ios/Shared/Views/Chat/ChatItem/MsgContentView.swift` — three-way tap dispatch + `simplexLinkAttrKey`
- `apps/ios/Shared/Views/UserSettings/SettingsView.swift`, `apps/ios/Shared/Views/ChatList/ChatHelp.swift` — route `simplexTeamURL` in-process

No behavior change for web / `mailto:` / `tel:` links.

## Verification

- Tap an inline SimpleX invitation/contact link in a received message → the connection sheet opens (on iPhone 17, where it was previously dead).
- The two developer-contact buttons open the connect flow.
- Web links still open the browser; `mailto:`/`tel:` still open Mail/Phone.
- Optional, to confirm the device-local nature: open a `simplex:/contact#…` link from another app (e.g. Notes) on the affected device — if that is also dead there but works on a second device, it confirms the difference is device-local scheme resolution rather than app code.
