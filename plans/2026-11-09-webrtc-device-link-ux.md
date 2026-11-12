# Linking a desktop and a phone: UX spec

Scope: pairing and sessions between SimpleX Chat desktop and the phone apps (iOS, Android) over the new WebRTC device link. Calls are out of scope.

Conventions
- **Today** statements cite simplex-chat master `c16200e57` and simplexmq `a0eec710`. Path abbreviations:
  `Remote.hs` = `src/Simplex/Chat/Remote.hs`; `Controller.hs` = `src/Simplex/Chat/Controller.hs`;
  `iCDV` = `apps/ios/Shared/Views/RemoteAccess/ConnectDesktopView.swift`; `iAPI` = `apps/ios/Shared/Model/SimpleXAPI.swift`;
  `kCDV` / `kCMV` = `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/remote/ConnectDesktopView.kt` / `ConnectMobileView.kt`;
  `kAPI` = `.../common/model/SimpleXAPI.kt`; `kUP` = `.../common/views/chatlist/UserPicker.kt`;
  `str` = `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`; `RC/` = simplexmq `src/Simplex/RemoteControl/`.
- Everything in sections 2–4 is **proposal** unless it carries a citation. Text in "quotes" is exact proposed English copy. `<desktop>` and `<phone>` are the stored device names.
- Named values used below (proposal): `QR_LIFETIME` = 10 min, `REQUEST_TTL` = 2 min, `NO_RESPONSE_HINT` = 20 s, `RECONNECT_WINDOW` = 2 min, `LIVE_ACTIVITY_MAX` = 8 h (Apple's limit for one Live Activity).

## 1. Today (only what this spec changes)

1. Desktop "Link a mobile" (`kUP:392-400`; onboarding button when there is no profile, `apps/multiplatform/common/src/desktopMain/.../onboarding/SimpleXInfo.desktop.kt:16-19`) shows an `xrcp:/` QR that encodes the desktop's LAN address and port (`RC/Invitation.hs:58-78`, `Remote.hs:142-164`), with an address/port picker and an "Open port in firewall" hint (`kCMV:455-520`) and a "Discoverable via local network" toggle (`kCMV:98-100`).
2. Phone "Use from desktop" (`apps/ios/Shared/Views/ChatList/UserPicker.swift:73`; Android `kUP:378-390`) opens "Connect to desktop" with a device name field and a QR scanner (`iCDV:151-165`). The phone opens TCP+TLS to the desktop. Both show a session code derived from the TLS session (`Remote.hs:191`, `Remote.hs:487`); the phone taps "Confirm" (`iCDV:244-266`), the desktop shows "Verify code on mobile" (`kCMV:230-240`).
3. Timeouts: desktop waits 60 s for the phone (`Remote.hs:190`); phone network steps 15 s (`Remote.hs:93-94`); discovery 60 s (`Remote.hs:96-97`).
4. Later sessions need action on both devices: the desktop user clicks the phone, which shows a fresh QR "Scan from mobile" and announces over multicast for 60 s (`kCMV:350-428`, `RC/Client.hs:117-121`); the phone, when its screen opens with known desktops and "Discover via local network" on, listens (`iCDV:89-95`, `Remote.hs:408-432`), connects without a tap if "Connect automatically" is on (`iCDV:220-238`) and skips the code if "Verify connections" is off (`iCDV:69-76`).
5. Defaults: iOS discover ON, auto ON, verify OFF (`apps/ios/Shared/Views/UserSettings/SettingsView.swift:144-146`); Android discover OFF, auto ON, verify OFF (`kAPI:265-267`); desktop discoverable ON (`kAPI:268`).
6. The phone runs one session at a time (`Remote.hs:452-461`); the desktop can hold several phones (`Controller.hs:330`).
7. The phone screen must stay open. iOS footer: "Keep the app open to use it from desktop" (`iCDV:302`); leaving the screen disconnects (`iCDV:105-110`); going to background suspends the chat core (`apps/ios/Shared/SimpleXApp.swift:71-83`). Android runs its foreground service only while the screen is open (`kCDV:61-68`); leaving asks "Disconnect desktop?" / "Only one device can work at the same time" (`kCDV:48-54`, `kCDV:581-589`).
8. Failure copy assumes LAN: "Please check that mobile and desktop are connected to the same local network, and that desktop firewall allows the connection." (`iAPI:3099-3105`, `str:2778`). FAQ lists firewall, VPN and router isolation as causes (`docs/FAQ.md:309-320`).
9. Unlink is one-sided: phone `deleteRemoteCtrl` only with no session (`Remote.hs:710-718`); desktop "Delete" only when not active (`kCMV:126-137`), which also deletes the files folder (`Remote.hs:335-344`). The other device keeps its record.
10. Peer names are overwritten with the name the peer sends each session (`Remote.hs:230`, `Remote.hs:656-659`).
11. Old app parsing: the short link parser rejects unknown link type letters (simplexmq `src/Simplex/Messaging/Agent/Protocol.hs:1763-1768`); the phone's desktop scanner maps parse errors to "Bad desktop address" (`iCDV:459-461`, `iCDV:135-136`); the New chat scanner shows "Invalid QR code" (`apps/ios/Shared/Views/NewChat/NewChatView.swift:692`).
12. iOS local network prompt text: "SimpleX uses local network access to allow using user chat profile via desktop app on the same network." (`apps/ios/SimpleX Localizations/en.xcloc/Source Contents/en.lproj/SimpleX--iOS--InfoPlist.strings:11`).

## 2. New flows (proposal)

Names: "linked desktop" / "linked mobile" = new device link. "Old link" = an existing LAN pairing.

Entry points stay where they are: phone "Use from desktop" (item 2 above), desktop "Link a mobile" / "Linked mobiles" (item 1). The phone screen title changes from "Connect to desktop" (`iCDV:163`) to "Use from desktop".

### 2a. First pairing

**Desktop D1, "Link a mobile"** (from the user picker or onboarding)
- Title: "Link a mobile".
- Left column (as today `apps/multiplatform/.../onboarding/LinkAMobileView.kt:63-72`): section "This device name", field placeholder "Enter this device name…", footer "The device name will be shared with the linked mobile." The "Discoverable via local network" toggle is removed.
- QR code (one-time device link, `/d` path, link data carries the first WebRTC offer). No address or port picker.
- Under the QR: "On your phone, open <b>Use from desktop</b>, tap <b>Link a desktop</b> and scan this code."
- Second line: "The code works once and expires in 10 minutes. Your phone needs SimpleX Chat %s or later." (`%s` = minimum app version).
- After `QR_LIFETIME`: QR dimmed with "This code expired" and a "Refresh" button (reuse overlay `kCMV:196-199`, string `str:2810`).
- If signaling cannot reach the SMP server: in place of the QR, "Can't create a link" / "Check your internet connection. Linking needs internet access on both devices." / button "Try again".

**Phone P1, "Use from desktop" with no linked desktops**
- Section "This device name": field (as today `iCDV:153-155`).
- Row "Link a desktop" (icon qrcode) → P2.
- Footer: "Link a desktop to use your chats from it. The desktop can be on any network."

**Phone P0, local network explanation (iOS, before the first scan; macOS desktop has its own, see 4)**
- Shown once, before the scanner opens, if the app has never triggered the system prompt.
- Title: "Allow local network access". Text: "When your phone and desktop are on the same network, SimpleX Chat connects them directly. iOS will ask for permission. If you don't allow it, the devices connect through the internet or a SimpleX relay." Button: "Continue".
- Info.plist string replacement: "SimpleX Chat uses the local network to connect directly to your linked desktop when both are on the same network."

**Phone P2, scanner**
- Title: "Link a desktop". Section header "Scan QR code from desktop" (existing string, `iCDV:328`).
- A `/d` link scanned or pasted anywhere else (New chat scanner, paste, opened from a browser) shows alert "Link a desktop?" / "This QR code links a desktop to this phone." / buttons "Link desktop" (goes to P3) and "Cancel".
- Errors:
  - Expired or used link: "This code can't be used" / "It was already used or has expired. Show a new code on the desktop and scan again." / "OK".
  - Old desktop QR (`xrcp:/`) from a desktop not yet linked: "Update the desktop app" / "This QR code is from an older version of SimpleX Chat for desktop. Update the desktop app to %s or later to link it." / "OK".
  - No internet: "No internet connection" / "Linking needs internet access on both devices." / "OK".

**Phone P3, connecting**
- Title: "Linking desktop". Body: desktop name from link data, with "(new)" (existing, `iCDV:271`), version line (existing, `iCDV:276-283`), spinner, "Cancel".
- If this desktop is already linked: alert before connecting: "Already linked" / "This desktop is already linked as <desktop>. Linking again replaces the existing link." / "Link again", "Cancel".

**Phone P4 and desktop D2, verify**
- Phone: title "Verify connection", section "Desktop" with name, section "Verify code with desktop" with the code (first 23 chars, as `iCDV:310-312`), buttons "Confirm" and "Cancel". Verification is always required at first pairing. The code is derived from the keys of this WebRTC session (assumption for engineering).
- Desktop: title "Verify connection", section "Connected mobile" with phone name, section "Verify code on mobile" (existing `kCMV:217-240`), footer "Check that your phone shows the same code, then tap Confirm on the phone."
- Phone "Cancel" → desktop alert "Linking cancelled" / "<phone> did not confirm the code. Show a new code to try again." / "OK".

**After Confirm**
- Both save the link. The session is now connected (2c).
- Desktop: closes the modal, switches to the phone (as today `kAPI:3496-3500`), toast "Linked <phone>".
- Phone: session screen (2c) with a one-time banner: "Desktop linked. Next time, connect from either device without a QR code."
- "Connect without asking" is OFF for the new desktop.

**Naming**
- Each device sends its "This device name" as today. New: each side can rename the peer locally in its details screen (field "Name", footer "Only you see this name."). A local name is not overwritten by the name the peer sends (today it is, item 10).

### 2b. Later sessions

**Device lists (both sides)**
- Desktop D0 "Linked mobiles" (today `kCMV:81-149`): section "This device name"; section "Devices" with "This device", then each linked mobile: phone icon, name, subtitle "Not connected" / "Waiting for response…" / "Connected · <path label>" / "Reconnecting…"; row "Link a mobile". Section "Old links" only while old links exist.
- Phone P1 with links: section "This device name"; section "Linked desktops" with each desktop: desktop icon, name, same subtitles, trailing "Connect" when not connected; row "Link a desktop"; section "Old links" only while old links exist.
- Opening or closing these screens never starts or stops a session (today the phone screen disconnects on open and close, `iCDV:91-92`, `iCDV:105-110`).

**Started from desktop**
1. Desktop "Linked mobiles": click a phone that is not connected, or its menu "Connect".
2. Desktop modal D3: title "Connecting to <phone>", spinner, text "Waiting for <phone> to accept…", button "Cancel".
   After `NO_RESPONSE_HINT`: add "Open SimpleX Chat on your phone. If it is closed, tap the notification."
   After `REQUEST_TTL`: title "No response from <phone>", text "Your phone may be offline, or the app may be closed with notifications off.", buttons "Try again", "Close".
3. Desktop sends the session request over the device connection. Phone behaviour:

| Phone app | "Connect without asking" ON | OFF |
|---|---|---|
| Open on screen (iOS scene active; Android activity visible), unlocked | Connects silently, opens session screen | Request sheet P5 |
| In background, closed, or SimpleX Lock showing | Notification N1 | Notification N1 |

- **P5 request sheet**: title "<desktop> wants to connect". Text: "Your chats will be available on <desktop> until you disconnect. You can't use the app on this phone at the same time." Buttons: "Connect", "Not now".
- **N1 notification**: title "<desktop> wants to connect". Body: "Tap to use SimpleX Chat from this desktop." Action (long press on iOS, button on Android): "Decline". Android channel name: "Desktop connection requests".
- Tapping N1 counts as the one tap: the app opens (after SimpleX Lock, if enabled) and connects without a second confirmation.
- "Not now" or "Decline" → desktop: title "<phone> did not accept", buttons "Try again", "Close".
- Request after `REQUEST_TTL` (phone was offline): no notification is shown. If N1 is tapped after expiry: alert "Request expired" / "<desktop> asked to connect more than 2 minutes ago. Connect now?" / "Connect" (starts a phone-initiated session), "Cancel". The app removes expired N1 notifications when it runs.
4. The phone makes the WebRTC offer; desktop answers. On success desktop switches to the phone (as today) with toast "Connected to <phone>".

**Started from phone**
1. Phone "Use from desktop" → row of the desktop → trailing button "Connect" (or details screen P6 → "Connect").
2. Phone screen: title "Connecting to <desktop>", spinner, "Cancel". After `NO_RESPONSE_HINT`: "Make sure SimpleX Chat is running on <desktop>." After `REQUEST_TTL`: alert "No response from <desktop>" / "The desktop may be offline or the app may be closed." / "Try again", "OK".
3. The desktop accepts without asking if the app is running, switches to the phone and shows toast "Connected to <phone>".

### 2c. While connected

**Path labels** (both devices, updated live when the path changes):

| Path | Icon | Label | Explanation (footer / tooltip) |
|---|---|---|---|
| Host candidates on same LAN | wifi (`ic_wifi`, SF Symbol `wifi`) | "Local network" | "Your devices are connected directly over your local network." |
| Direct over the internet | solid line with two chevrons | "Direct" | "Your devices are connected directly over the internet. Each device sees the other's IP address." |
| TURN relay | dotted line with two chevrons | "Via relay" | "Your devices are connected through a SimpleX relay. The relay sees the IP addresses of both devices (or of your proxy) and the amount of traffic, not its content." |

**Phone session screen P7** (replaces today's active session view, `iCDV:285-308`, `kCDV:315-337`)
- Title: "Connected to desktop". Section "Connected desktop": name, version line.
- Row "Connection": path label; tapping shows the explanation.
- Section "Session code": shown only for the first session after pairing or an upgrade (today shown always, `iCDV:292-296`).
- Button "Disconnect" (no confirmation; today none on iOS, `iCDV:490-496`).
- iOS footer (replaces "Keep the app open to use it from desktop"): "SimpleX Chat keeps running in the background while the desktop is connected. Disconnect to stop."
- Android footer: "SimpleX Chat keeps running while the desktop is connected. Disconnect to stop."
- The screen still can't be dismissed while connected (iOS `iCDV:148`; Android back keeps the "Disconnect desktop?" alert, `kCDV:48-54`).

**iOS background**
- First session on this phone, when the app first goes to background: no alert can show then, so show this alert on P7 when the session starts, once per install: "The app keeps running" / "While a desktop is connected, SimpleX Chat stays active in the background, so the desktop keeps working when you lock the phone or switch apps. This uses more battery. It stops when you disconnect." / "OK".
- iOS 16.1+ with Live Activities allowed: Live Activity from session start to end.
  - Lock screen: line 1 "Connected to <desktop>", line 2 "<path label> · <elapsed time>". Reconnecting: line 1 "Reconnecting to <desktop>…".
  - Dynamic Island compact: desktop icon (leading), elapsed time (trailing). Expanded: name, path label, elapsed time.
  - iOS 17+: button "Disconnect" in the Live Activity. iOS 16.1–16.x: tapping opens the app on P7.
  - At `LIVE_ACTIVITY_MAX` iOS ends the activity; the app then posts notification N2 so the indicator never disappears while the session runs.
- iOS 15–16.0, or Live Activities turned off: notification N2 posted when the app goes to background, removed when it returns or the session ends. Title "Connected to <desktop>". Body "SimpleX Chat keeps running in the background. Open the app to disconnect."
- Background audio and the indicator stop when the session ends for any reason, including a failed reconnect.
- Prior work: open PR #7234 (external contributor) implements looping background audio plus a Live Activity gated on iOS 16.1 and Live Activities being enabled; its review flagged that the audio asset must be silent, the App Store background-audio guideline, and an interaction with CallKit audio sessions. Its Live Activity shows the desktop name and time since connect, which matches this spec. Open draft PR #7338 adds reconnect after transport loss over LAN with a "reconnecting" state on iOS and a blocking reconnect view on macOS; this spec uses a non-blocking banner instead and reconnects over the device connection, not a saved LAN invitation.

**Android**: the foreground service runs for the whole session (today only while the screen is open, `kCDV:61-68`). Ongoing notification: title "Connected to <desktop>", text "<path label>. Tap to open.", action "Disconnect". In Instant notifications mode the existing service notification shows this content during the session.

**Desktop**
- Device pill of the phone (`kUP:529-570`): small path icon (wifi / solid line with two chevrons / dotted line with two chevrons) with tooltip "<path label>".
- "Linked mobiles" row subtitle: "Connected · <path label>".
- "Connected to mobile" modal (`kCMV:430-452`): add row "Connection" with path label and explanation, keep "Disconnect".

**Disconnect from either side**
- Phone "Disconnect", Live Activity / notification "Disconnect", desktop pill "×" or menu "Disconnect": ends immediately, no confirmation, no reconnect. The other side is told it was intentional.
- Peer notice: desktop toast "Mobile <b><phone></b> was disconnected" (existing `str:2773`); phone transient notice "<desktop> disconnected".

### 2d. Reconnect and failures

**Reconnect** after transport loss (ICE disconnected or failed, network change), not after an intentional disconnect:
- The phone sends a new offer (ICE restart) over the device connection and retries for up to `RECONNECT_WINDOW`. No user action.
- Phone P7: "Connection" row shows "Reconnecting…"; Live Activity / N2 updated as above.
- Desktop: banner over the chat area "Reconnecting to <phone>…" with link "Disconnect"; the composer is disabled with placeholder "Reconnecting to <phone>…". Lists stay visible.
- Success: banner disappears, label shows the new path. No toast.
- Give up: desktop alert "Disconnected from <phone>" / "The connection was lost and could not be restored. Check that both devices have internet access." / "Connect again", "OK". Phone closes P7 and shows the same alert with <desktop>; if in background, notification "Disconnected from <desktop>" / "Open SimpleX Chat to connect again."

**Failure copy**

| Situation | Shown on | Title / text / buttons |
|---|---|---|
| Phone offline, app closed, notifications off | Desktop | 2b step 2 copy |
| Desktop app not running | Phone | 2b "Started from phone" copy |
| Request declined | Desktop | "<phone> did not accept" / — / "Try again", "Close" |
| Signaling server unreachable | Starting side | "Can't reach <name>" / "Check your internet connection. Connecting needs internet access on both devices, even on the same network." / "Try again", "OK" |
| ICE failed (no direct path, relay failed) | Both | "Could not connect to <name>" / "The devices could not connect directly or through a relay. Check your network and proxy settings, then try again." / "Try again", "Copy error", "OK" ("Copy error" as today `iAPI:3104`) |
| Incompatible versions | Both | "Incompatible version" / "Desktop app version %s is not compatible with this app." (existing `str:2786-2787`) — phone side; desktop side uses existing `str:2820` |
| Phone in a session with another desktop | Requesting desktop, after phone declines or switches | see 2f |
| Link removed on the other device | Both | see 2f |
| Wrong or unconfirmed code at pairing | Desktop | "Linking cancelled" (2a) |

### 2e. Upgrading an old LAN link

**Where the notice appears**
- Phone P1: old links listed in a separate section "Old links", each row subtitle "Local network only". Section footer: "Local network links are deprecated and will be removed in a future version. Upgrade each link to keep using that desktop." Row menu / details: "Upgrade link".
- Phone P7 during an old-link session: footer "This desktop uses an old local network link, which will stop working in a future version." with button "Upgrade link".
- Desktop "Linked mobiles": old rows subtitle "Old link · local network only", menu items "Connect" (today's flow), "Upgrade link", "Unlink". Section footer same as phone with "mobile".
- Desktop "Connected to mobile" modal during an old-link session: same footer and "Upgrade link".

**Flow** (QR shown by desktop, threat-model confirmation on the phone, because the phone's data becomes reachable from anywhere)
1. Desktop "Upgrade link" on the old row of <phone> → dialog: title "Upgrade link to <phone>?" Text: "You will scan a new QR code with <phone>. After the upgrade, the devices can connect from any network, not only your local network." Buttons "Show QR code", "Cancel".
2. Desktop D1 variant: title "Upgrade link", same QR rules; link data identifies the old link it replaces. Under the QR: "On <phone>, open <b>Use from desktop</b>, tap <b>Upgrade link</b> next to this desktop, and scan this code."
3. Phone "Upgrade link" (or any scan of this QR) → phone confirmation, required, shown before any connection:
   - Title: "Upgrade link to <desktop>?"
   - Text: "Today <desktop> can connect to this phone only when both are on the same local network. After the upgrade, <desktop> can ask to connect from any network, directly or through a SimpleX relay. Anyone who uses <desktop> could then reach your chats from anywhere. You will be asked each time, unless you turn on Connect without asking. The old link will be removed."
   - Buttons: "Upgrade", "Cancel". "Cancel" → desktop alert "Upgrade cancelled" / "<phone> did not confirm the upgrade." / "OK".
4. Scanner (P2), then verify (P4/D2) exactly as first pairing.
5. On success: the old link is removed on both devices; the desktop moves the old link's files folder (today `Remote.hs:340-342`) to the new link so received files stay. Banner on phone: "Link upgraded."
6. If the phone that scans has no matching old link, it is a normal first pairing and the desktop keeps the old row.

**Version mismatches**
- Old phone scans the new QR: in "Use from desktop" it shows "Bad desktop address" (`iCDV:135-136`, `str:2785`); in New chat it shows "Invalid QR code" (item 11). The version line under D1 covers this. When the desktop's "Upgrade link" targets an old link whose phone reported an older version in its last session, the dialog in step 1 instead says: "Update SimpleX Chat on <phone> to %s or later, then upgrade the link." / "OK".
- New phone, old desktop: scanning `xrcp:/` from a desktop with an old link continues the old flow, with the deprecation footer. Scanning `xrcp:/` from an unknown desktop shows "Update the desktop app" (2a P2). Multicast discovery keeps working for old links only.
- New desktop, old phone with an old link: old flow works unchanged, with the desktop notice.
- At the end of the transition, old rows read "Old link no longer works" with only "Unlink"; old-link settings are deleted (2g).

### 2f. Per-desktop settings, unlink, several desktops, phone not reachable

**Phone desktop details P6** (tap a linked desktop)
- Title: <desktop>. Field "Name" (2a Naming). Row "Status": "Not connected" / "Connected · <path label>" / "Reconnecting…".
- Button "Connect" or "Disconnect".
- Toggle "Connect without asking", default OFF. Footer: "When SimpleX Chat is open on this phone, <desktop> connects without asking you. When the app is closed or in the background, you get a notification to tap."
- Footer (iOS) when notifications are off, or (Android) when the notification mode is not Instant (`kCDV:61`): "To receive requests from <desktop> when the app is closed, turn on instant notifications." with link "Notification settings".
- Button "Unlink desktop" (red).

**Desktop mobile details** (row click when connected opens today's modal; menu adds "Details"): Name field, Status row, "Connect"/"Disconnect", "Unlink mobile".

**Unlink, from either side; removes on both**
- Phone alert: "Unlink <desktop>?" / "<desktop> will no longer be able to connect to this phone. The link is removed on both devices. To use it again, scan a new QR code." / "Unlink", "Cancel". Desktop: "Unlink <phone>?" / "This desktop will no longer be able to connect to <phone>. The link is removed on both devices. Files received from <phone> on this desktop are deleted." / "Unlink", "Cancel". Rename the desktop menu item "Delete" (`kCMV:133`) to "Unlink".
- Allowed during a session: disconnects first (today blocked, `Remote.hs:712`, `kCMV:127-137`).
- The unlinking device sends "unlinked" over the device connection, then deletes its side. If the other device is offline it removes the link when the message arrives.
- The other device: row removed and alert "<name> unlinked this device" / "To use it again, link it with a new QR code." / "OK" (desktop shows it at once; phone shows it next time "Use from desktop" or the app opens).

**Several desktops**
- Phone lists all linked desktops; each has its own "Connect without asking". One session at a time stays (item 6).
- Request from desktop B while connected to A, or while A's request is shown: P5 variant for B, never silent, even with the toggle on: title "<B> wants to connect", text "You are connected to <A>. Connecting to <B> disconnects <A>." Buttons "Switch to <B>", "Not now".
- After a switch, A's alert: "Disconnected from <phone>" / "<phone> connected to another desktop." / "OK". No reconnect.
- Two requests arriving within one second while idle: the first received is handled, the second waits and is shown after; if it expires meanwhile, it is dropped silently and the desktop shows "No response".

**Phone offline or app closed**
- Desktop D3 timeline in 2b. A request is never acted on after `REQUEST_TTL`, so a phone coming online later does not connect to a desktop the user has left.

### 2g. Settings removed and added

Removed from the new UI (kept, under "Old links", only while an old link exists; deleted at the end of the transition):

| Device | Setting | Where today |
|---|---|---|
| Phone | "Verify connections" | `iCDV:374`, `kCDV:437-439`; keys `SettingsView.swift:81,144`, `kAPI:265` |
| Phone | "Discover via local network" | `iCDV:375`, `kCDV:440-442`; `SettingsView.swift:82,145`, `kAPI:266` |
| Phone | "Connect automatically" | `iCDV:376-378`, `kCDV:443-447`; `SettingsView.swift:83,146`, `kAPI:267` |
| Desktop | "Discoverable via local network" | `kCMV:98-100`, `LinkAMobileView.kt:69-71`; `kAPI:268` |
| Desktop | Address and port picker, "Open port in firewall" | `kCMV:455-520` |

The three phone keys are also exported in app settings (`apps/ios/Shared/Views/UserSettings/AppSettings.swift:50-52,91-93`, `apps/ios/Shared/Model/AppAPITypes.swift:2313-2315`, `kAPI:8772-8774`) and go with them.

Kept: "This device name" on both; developer tool "Desktop address" paste on the phone (`iCDV:333-358`), which now also accepts `/d` links; desktop developer "Share link" (`kCMV:207-213`).

Added:
- Phone, per desktop: "Connect without asking" (default OFF), "Name".
- Desktop, per mobile: "Name".
- No global network setting in v1 (see question 4).

## 3. State diagrams (proposal)

**Linked device (per peer, each side)**
```
                 scan /d QR + code confirmed
 (none) ───────────────────────────────────────▶ LINKED{askEachTime=ON}
   ▲                                             │  toggle ▲▼ (phone only)
   │                                             │ LINKED{askEachTime=OFF}
   │ local unlink / "unlinked" received          ▼
   └──────────────────────────────────────── UNLINKING (send "unlinked", delete)
 OLD_LAN ──"Upgrade link" + phone confirm + scan + code──▶ LINKED (OLD_LAN deleted)
 OLD_LAN ──transition ends──▶ OLD_LAN_DEAD (only "Unlink")
 OLD_LAN ──unlink──▶ (none)   (one-sided, as today)
```

**Session (one per phone; one per phone on the desktop)**
```
IDLE ──desktop "Connect"──▶ REQUESTED ──phone accepts (silent or tap)──▶ CONNECTING
IDLE ──phone "Connect"────────────────────────────────────────────────▶ CONNECTING
REQUESTED ──declined──▶ IDLE ("did not accept")
REQUESTED ──REQUEST_TTL──▶ IDLE ("No response")
CONNECTING ──ICE ok, first pairing──▶ VERIFYING ──phone Confirm──▶ CONNECTED(path)
CONNECTING ──ICE ok, later session──▶ CONNECTED(path)
CONNECTING / VERIFYING ──fail, timeout, cancel──▶ IDLE (failure copy)
CONNECTED(path) ──path change──▶ CONNECTED(path')
CONNECTED ──transport lost──▶ RECONNECTING ──ok──▶ CONNECTED(path)
RECONNECTING ──RECONNECT_WINDOW──▶ IDLE ("Disconnected from …")
CONNECTED / RECONNECTING ──Disconnect (either side) or switch──▶ IDLE (no retry)
Phone background keep-alive and indicator: on in CONNECTED and RECONNECTING only.
```

## 4. Edge cases (proposal)

- **SOCKS/Tor proxy on** (either device): candidates are local network and relay over TCP through the proxy; no direct internet path. Path label is "Local network" or "Via relay". P6/P7 footer: "Your proxy is on. SimpleX Chat connects over your local network or through a relay via your proxy." Relay through Tor is slow; the failure alert keeps "Check your network and proxy settings".
- **No internet, same LAN**: pairing and starting a session fail with "Can't reach <name>" because signaling needs SMP. An already connected local-network session continues; reconnect fails after `RECONNECT_WINDOW`. FAQ entry "My mobile app does not connect to desktop app" (`docs/FAQ.md:309-320`) is rewritten around internet access, proxy and notifications.
- **iOS local network permission**: P0 before the first prompt. If denied, sessions still work (direct or relay). P7 "Connection" explanation adds, when the path is not local and permission is denied: "Local network access is off for SimpleX Chat. Turn it on in Settings to connect directly on the same network." with "Open Settings".
- **macOS local network permission** (macOS 15+; assumed to trigger for the desktop's WebRTC stack, to verify): before the first QR, D1 shows "Allow local network access" / "When your phone and desktop are on the same network, SimpleX Chat connects them directly. macOS may ask for permission. If you don't allow it, the devices connect through the internet or a SimpleX relay." / "Continue". If the macOS application firewall asks to accept incoming connections, the same fallback applies; no port setting is needed.
- **Desktop with no chat profile**: "Link a mobile" stays in onboarding (`SimpleXInfo.desktop.kt:16-19`). The hidden agent user is created at that moment. Phone-initiated sessions work while the desktop app runs. "This device" stays hidden in the device list while there is no local profile (as today `kCMV:104`).
- **Phone chat profile deleted**: the link is owned by the hidden agent user and survives. The desktop refreshes the profile list; if the deleted profile was active on the desktop, it switches to the phone's active profile. The hidden user never appears in profile lists, unread counts or "Use from desktop" profile choices.
- **Phone database migrated to another phone** ("Migrate to another device"): the links move with the database; the new phone receives requests. The desktop sees no change.
- **Two desktops request at once / while connected**: 2f.
- **SimpleX Lock on phone**: silent connect only when the app is unlocked on screen; otherwise N1, and connection happens after unlock.
- **Phone call during a session**: out of scope; the session is not ended by this spec.
- **Desktop asleep or app quit during a session**: phone goes to RECONNECTING, then "Disconnected from <desktop>"; background keep-alive stops.
- **Desktop has several phones connected**: unchanged from today (item 6); each has its own pill and path icon.

## 5. Open questions for the product owner

1. **Internet required on the same LAN.** With SMP signaling, two devices on an offline LAN can no longer link or start a session (today they can). Accept and document in the FAQ?
2. **Notification tap = accept.** This spec treats tapping N1 as the single tap that connects. Alternative: the tap opens P5 and a second tap connects. Which?
3. **Desktop accepts phone-initiated sessions without asking.** The phone owner starts it and the data is the phone's. Confirm, or add a per-mobile "Ask before connecting" on the desktop?
4. **Network route setting.** Ship v1 without a "Local network only / Any / Relay only" choice (recommended: the proxy rule covers the privacy case and keeps settings minimal), or add it on the phone?
5. **Android in background with Instant notifications.** The app process is alive; should "Connect without asking" connect silently there too, or only when the app is on screen as decided for iOS?
6. **Transition length.** Which release removes old links, so the notice can name it ("will stop working in v7.x")?
