# WebRTC device link and native calls: plan

Status: proposal for review. Nothing here is final.

## Goal

1. Desktop and phone link once with a QR code, then connect from either device on any network: local network first, then direct over the internet, then a SimpleX relay. Main case: the phone is next to the desktop, but the desktop firewall or the Wi-Fi router blocks today's LAN link.
2. The iPhone keeps the desktop session alive in the background.
3. Calls use one native WebRTC (Google M156) on every platform, with no WebView and no browser, so end-to-end encryption can become mandatory once old clients are dropped.

Limit: the phone still holds the data. The desktop works only while the phone is on and reachable.

Delivery: build the core of both link and calls first, to prove they work. Then ship two betas, link and calls, within one stable release.

## Direction

| Layer | Device link | Calls |
|---|---|---|
| Apps (Swift, Kotlin) | Pairing and session screens, path indicator, iOS background session, Android foreground service | Call screen, camera, audio, through WebRTC's platform API |
| simplex-chat (Haskell) | Device entity, device connection over SMP, `x.rtc.*` signaling, network policy, opens the WebRTC data channel and passes it to simplexmq as a stream | Signaling (`x.call.*`) and frame encryption, unchanged |
| simplexmq (Haskell) | Remote-control protocol (TLS, handshake, HTTP/2) over a TCP socket or a given stream; one-time invitation link with a device type | none |
| Google WebRTC M156 | Data channel, path finding (ICE), relay (TURN) | Media, codecs, path finding, relay |

Packaging (proposed): on Android, Linux and macOS, WebRTC and its JNI glue go into the existing JNI library `app-lib` (`AppCommon.android.kt:69`, desktop `Main.kt:65`), which exports only JNI symbols. Windows needs `app-lib` built with clang-cl. On iOS the app already ships the M156 framework (#7878); the C wrapper lives in the app, never in `SimpleXChat.framework`, because the notification extension links it. Haskell reaches the wrapper through a function table the app registers at start (`chat_set_webrtc_api`); without it, device link is unavailable.

## Decisions so far (from product review, all proposals)

| Area | Proposal |
|---|---|
| Use case | Phone next to desktop when the direct LAN link is blocked; other networks also work |
| Flow | QR once. Either device starts a session. Desktop request reaches the phone over the device connection |
| Approval | Phone accepts silently only if the app is open and "Connect without asking" is on for that desktop (off by default); otherwise one tap on a sheet or notification |
| Device connection | Dedicated SMP connection per pairing, owned by a hidden agent user on each side, represented by a new Device entity |
| Pairing QR | One-time invitation link with a new device type in the URI (`/d`); link data carries the first WebRTC offer |
| Signaling | `x.rtc.inv` / `offer` / `answer` / `extra` / `end`, reusing call types and send functions |
| Network | Local and direct first, relay fallback; no separate local mechanism (host candidates connect with no ICE servers, about 10 ms in a one-machine test) |
| Proxy on | Local network allowed, no STUN, relay over TCP through the proxy, no DNS outside the proxy |
| Old LAN links | Deprecated: keep working with a notice; upgrade by a new QR scan after a threat-model confirmation on the phone; no new LAN pairings |
| iOS background | Silent audio in background mode while a desktop session is active, with a Live Activity (iOS 16.1+) or a notification; App Store guideline 2.5.4 risk accepted |
| WebRTC | Upstream Google M156, no patches |
| Audio | iOS built-in with CallKit; Android upstream Java audio module (no Oboe); desktop cubeb |
| Codecs | VP8 and Opus only, as today |
| Calls | Platforms drive WebRTC; Haskell keeps signaling and frame encryption, wire-compatible with today |
| Relay credentials | Static for now |

Path icons: Local network uses the wifi icon (`ic_wifi`, SF Symbol `wifi`). Direct uses a solid line with two chevrons, Via relay a dotted line with two chevrons.

The detailed UX (screens, exact copy, timers, state diagrams, edge cases) is in [2026-11-09-webrtc-device-link-ux.md](./2026-11-09-webrtc-device-link-ux.md).

## Steps in implementation order

Steps 1 and 2 run in parallel. Step 3 is the gate: if a test fails there, the design is revised before steps 4 to 7. Steps 4–6 (link) and step 7 (calls) then run in parallel.

### 1. iOS background session (ships first)

Works with today's LAN link, so it ships on its own.

- Branch `ios-desktop-background` (3 commits on c16200e57), reworks #7234 by lukasa1993, with credit:
  - Silent audio, mixed with other apps, only while a verified desktop session is active.
  - Live Activity on iOS 16.1+ (Disconnect button on 17+); notification on iOS 15–16.0, when Live Activities are off, or after the system ends one at 8 hours.
  - Interruptions: playback resumes after a call or route change; if it cannot, the desktop is disconnected cleanly.
  - Fix missed by #7234: when the chat is suspended (for example by the share extension), the session ends in the app too.
- Done when: iOS build passes in CI (passed on the fork), the simulator smoke test passes, and the real-device checklist passes (locked screen, music playing, incoming call, iOS 15 / 16.1 / 17 / 26, killed app, Low Power Mode, SimpleX Lock, share extension).

### 2. Native WebRTC M156 on Android and desktop

- CI builds Google `branch-heads/8078` (M156) for Android arm64 and armv7, Linux x64 and arm64, macOS x64 and arm64, Windows x64. Artifacts pinned by hash; rebuilt only when the version changes.
- Link into `app-lib`. Toolchain: NDK r28+ on Android, lld 19+ on Linux, clang-cl on Windows. libsimplex gains `chat_free`, because `simplex-api.c` frees its strings with `free()`.
- Crypto inside WebRTC (open): keep BoringSSL hidden inside `app-lib` (proposed), or build WebRTC against our OpenSSL (product preference). The OpenSSL build compiles (16 files, OpenSSL 3.0.13), but it also needs libssl, which we don't ship, and it loses DTLS 1.3 and post-quantum key exchange. Decide after measuring both.
- C wrapper with a small API (create peer connection, offer, answer, candidates, data channel, close) and the function table for Haskell.
- Done when: all 7 targets build; a CI check shows `app-lib` exports only JNI symbols (`nm -D`); a CI test shows libsimplex's OpenSSL calls still reach OpenSSL; builds are reproducible.

### 3. Prove both cores (gate)

**3a. simplexmq: remote control over a stream**
- `connectTLS` (`Messaging/Transport.hs:353`) takes any `HasBackend`, not only `Socket`.
- Transport parameter in `connectRCHost` (`RemoteControl/Client.hs:107`, TLS server at :138) and `connectRCCtrl_` (:272, dial at :288), passed through `rcConnectHost` / `rcConnectCtrl` (`Messaging/Agent.hs:863`, `:868`). Certificate pinning, the post-quantum handshake and HTTP/2 (`Transport/HTTP2.hs:41-52`) run unchanged.
- Done when: existing remote-control tests pass over both TCP and an in-memory stream.

**3b. simplex-chat: link prototype**
- `Simplex.Chat.WebRTC`: FFI to the wrapper; an open data channel exposed as a TLS backend.
- Manual signaling between two devices; data channel passed to 3a.
- Done when, on real phones and routers:
  - same network with no ICE servers connects;
  - different networks, including strict routers, connect with relay fallback in 10 s or less;
  - Wi-Fi to mobile data switch recovers in 15 s or less (ICE restart);
  - file transfer reaches 5 MB/s or more on the local network;
  - with a SOCKS proxy on, a packet capture shows no UDP and no DNS outside the proxy.

**3c. Calls core**
- Android: native calls on the WebRTC Java SDK from `app-lib`, replacing the WebView.
- Desktop: native calls on webrtc-java with its JNI in `app-lib` (continues branch `sh/desktop-native-calls`), replacing the browser.
- Signaling and frame encryption unchanged (Haskell, as iOS).
- Done when: Android and desktop call each other and current iOS, Android and desktop apps, with frame encryption both ways.

### 4. Device connection and signaling (core)

- simplexmq: device type in one-time invitation URIs (`/d`), recognised before link data is fetched (as relay links, `Agent/Protocol.hs:1804`). Link data (about 13.7 KB) carries the first offer; it is read once (`LKEY`).
- simplex-chat:
  - Device entity and tables, linked to `remote_hosts` / `remote_controllers`; hidden agent user per side, created at first pairing (works on a desktop with no chat profile).
  - Pairing: QR with the first offer, phone answers in the join message, session code verified once.
  - `x.rtc.*` messages over the device connection. A request carries a fresh signed invitation and expires after 2 minutes.
  - Per-desktop "Connect without asking" (off by default) and local name.
  - Session: phone always makes the offer; path events (local, direct, relay); reconnect by ICE restart for up to 2 minutes; intentional disconnect never retries.
  - Unlink from either side with an "unlinked" message; removes both records.
  - Network policy: ICE servers (ours plus user-configured), proxy rules from the decisions table.
  - Chat commands and events for all of the above; API docs regenerated.
- Notifications: a request must reach a closed app. iOS notification extension and Android instant notifications must show the request (N1 in the UX spec). Not yet investigated; may need changes in push handling for the device connection.
- Done when: core tests cover pairing, sessions started from each side, approval rules, expiry, reconnect, unlink, proxy policy, and a desktop with no profile.

### 5. Device link in the apps

Per the UX spec, on iOS, Android and desktop:
- Desktop: "Link a mobile" with the one-time QR, "Linked mobiles" list with status, connection request, path icon on the phone's pill, reconnect banner.
- Phone: "Use from desktop" list of linked desktops, scanner, verify, request sheet and notification, session screen with path, desktop details (name, "Connect without asking", unlink).
- iOS: step 1 background session for the new link; local network explanation before the system prompt.
- Android: foreground service and ongoing notification for the whole session.
- Settings: phone "Verify connections", "Discover via local network", "Connect automatically" and desktop multicast, address and port settings removed from the new UI.
- FAQ rewritten around internet access, proxy and notifications.
- Done when: the UX spec's flows pass on real devices, and macOS local network permission is checked on a real Mac.

### 6. LAN deprecation

- Old links listed separately with the deprecation notice; old flow keeps working for them.
- "Upgrade link": desktop shows a new QR; the phone shows the threat-model confirmation; on success the old link is removed on both and the desktop keeps received files.
- No new LAN pairings. Old phone with new QR, and new phone with old desktop QR, show the update messages from the spec.
- Done when: all four old/new version combinations behave as in the spec.

### 7. Native calls in the apps

- Android: native call screen; WebView call code removed.
- Desktop: native call screen; browser call page and local server (`localhost:50395`) removed.
- iOS: unchanged apart from sharing the same M156 build.
- Done when: calls work across all platform pairs and against current app versions.

### 8. Release

- Two betas, device link and calls, within one stable release. Which ships first is decided after step 3.
- Beta testers on real networks: same Wi-Fi with client isolation, desktop firewall on, different networks, mobile data, proxy and Tor.

### 9. Later

- Use the phone app while a desktop session is active. New work: no PR does this, and issues #4182 and #5041 were closed as not planned. Needs both UIs on one core, events to both, and handling the same chat open on both.
- Remove LAN discovery code, its settings and tests after the transition (release to be named).
- Make call end-to-end encryption mandatory once old clients are dropped.
- Expiring relay credentials that don't identify the user.
- Desktop that works with the phone off: separate project.

## Requirements

| ID | Requirement | Checked in |
|---|---|---|
| L1 | Desktop and phone connect on different networks, including strict routers, with relay when needed, in 10 s or less | 3b, 5 |
| L2 | Same network connects locally with no ICE server reachable | 3b |
| L3 | Reconnect after Wi-Fi to mobile data in 15 s or less, no user action | 3b, 4 |
| L4 | Later sessions start without QR: one tap, or none with "Connect without asking" | 4, 5 |
| L5 | Security unchanged: pinned certificates, post-quantum handshake, session code at pairing | 3a |
| L6 | File transfer 5 MB/s or more on the local network | 3b |
| L7 | Remote-control tests pass over TCP and over a stream | 3a |
| P1 | A photo of the QR does not reveal the desktop's IP address | 4 |
| P2 | With a proxy on, no UDP and no DNS outside the proxy | 3b |
| B1 | Builds and runs on all 7 non-iOS targets plus iOS | 2 |
| B2 | libsimplex's OpenSSL calls reach OpenSSL, not WebRTC's crypto | 2 |
| B3 | `app-lib` exports only JNI symbols | 2 |
| C1 | New apps call old apps; call messages and frame encryption unchanged both ways | 3c, 7 |
| C2 | Android WebView and desktop browser call paths removed | 7 |

## Open questions

1. Internet required on the same network: pairing and starting a session need SMP signaling, so two devices on an offline LAN can no longer link. Proposed: accept and document in the FAQ.
2. Notification tap: proposed that tapping the request notification connects; alternative is a second tap in the app.
3. Desktop accepts phone-started sessions without asking. Proposed: yes; alternative is a per-mobile "Ask before connecting".
4. Network route setting (local only / any / relay only): proposed none in v1; the proxy rule covers the privacy case.
5. Android in background with instant notifications: connect silently with "Connect without asking" on, or only with the app on screen? Proposed: only on screen, as on iOS.
6. Transition length: which release removes old LAN links.
7. Crypto library inside WebRTC: BoringSSL hidden (proposed) or our OpenSSL (step 2).
8. Waking a closed app with a request: what push and notification extension changes it needs (step 4).
