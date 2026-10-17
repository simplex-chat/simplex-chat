# Windows: clicking a notification opens the chat

Status: draft rev 10 (after review round 7). Toast history model decided 2026-10-17: Android-parity clearing plus a 1-hour expiry. All decisions taken. Implementation is gated by G1. Base: `origin/master` a932c8298. Paths are relative to `apps/multiplatform` unless they start with `src/` (the Haskell core).
Related plans: [sound](2026-10-17-desktop-notification-sound.md), [turn off](2026-10-17-desktop-notifications-turn-off.md). Research notes: `~/ntf-research/windows-findings.md`, `twoslices-findings.md`.

## Problem

On Windows, clicking a SimpleX notification does nothing:
- The chat doesn't open.
- The window isn't raised.
- From Action Center, nothing happens at all.

Related: #4201 (SimpleX is not a stable app in Windows notification settings; its sound switch resets), #3301, closed PR #7182. No issue states the click bug directly; the reports come from users.

## Cause

- INFERENCE [`twoslices-findings.md:48`]: on Windows, two-slices selects `AWTToaster` (GNTP only if Snarl or Growl listens on port 23053).
- FACT [`AWTToaster.java:103, 173-175`, md5-identical in 0.9.1–0.9.7]:
  - It registers itself as the `TrayIcon` ActionListener, but `actionPerformed` is empty.
  - `builder.defaultAction()` and `builder.actions()` are never read.
  - So the click and the Accept/Reject buttons can never reach the app. Upgrading the library doesn't help.
- FACT [`AWTToaster.java:92-128`]: it adds a second tray icon for toasts and removes it 10 s after the last toast. INFERENCE: after that, an Action Center click has no target.
- FACT [`NtfManager.kt:70-83`]: `openChatAction` never raises the window. It is effectively unused on desktop today, since no toaster in use invokes it, and it hard-codes the local host (`changeActiveUser(null, …)` `:76`, `openChat(rhId = null)` `:81`, TODO at `:74`).

## Existing structure to reuse

- FACT [`AppLinkScheme.desktop.kt:67-71, 97-106, 168+`]:
  - On each start, a background thread registers `simplexchat:` and `simplex:` per user under `HKCU\Software\Classes` for any jpackage launcher on Windows, with the command `"<SimpleX.exe>" "%1"`.
  - A scheme held by another program is left alone. `registered` reflects `simplexchat:` (`:180`).
  - The app already uses `chat.simplex.app` as its Windows capabilities name (`:26`) and the display name `APP_DISPLAY_NAME = "SimpleX Chat"` (`:43`).
- FACT [`SingleInstance.kt:49-66, 139-157, 205-214, 227-245`; `Main.kt:28-32`]:
  - A second launch writes its link to `simplex.show`, allows the running instance the foreground (`AllowSetForegroundWindow`), and exits.
  - The running instance takes the signal on the Swing thread and calls `openDesktopLink(link)` (`AppLinks.kt:19-22`), which stores `appOpenUrl` and calls `showWindow()` (`DesktopApp.kt:259-273`).
  - The watcher starts before `initApp` (`Main.kt:32` vs `:41`). On a cold start, `Main.kt:30` stores the link from args.
- FACT [`AppLinks.kt:12-13`, `ChatListView.kt:797-798`]: an accepted link is a case-insensitive `simplexchat:` prefix (or a connection link), at most 8192 bytes. Signal files are re-checked in `takeSignal`.
- FACT [`App.kt:240-249`, `ChatListView.kt:775-789, 804-814`]:
  - `appOpenUrl` is processed only when authorized, onboarding is complete and chat is running.
  - `simplexchat:` links go to `openAppLink` unless a badge redemption is in flight (`isBadgeLinkIssuing()`), in which case the link is dropped.
  - `openAppLink` (commonMain) shows "not supported" for unknown paths.
- FACT [`SimpleXAPI.kt:2937`]: `rhId` of every event comes from the desktop's own core.
- FACT [`src/Simplex/Chat/Library/Commands.hs:561, 574, 4456`]: hiding a profile sets `showNtfs = False`, unhiding sets it back, and muting is refused while hidden.
- FACT [`SimpleXAPI.kt:787-789`, `HiddenProfileView.kt:38-39`, `UserProfilesView.kt:371`, `LocalAuthView.kt:73`, `DatabaseView.kt:598`]: these are the clearing calls, the same set Android has:
  - `cancelNotificationsForUser` when leaving a hidden active profile, when hiding a non-active profile, and on user deletion;
  - `cancelAllNotifications` on self-destruct and database deletion.
- FACT [`AppLock.kt:23-24, 271-284`; `DesktopApp.kt:184-205`; `PrivacySettings.kt:487`]:
  - `userAuthorized` / `enteredBackground` are the lock state.
  - With lock delay 0, re-locking happens only in the focus handler.
  - `clearOverlays` closes the passcode prompt (`App.kt:111-115`), and ChatView stays composed under the lock screen (`App.kt:157-172`), where it marks visible items read (`ChatView.kt:3090-3094`).
- FACT [`desktop/build.gradle.kts:70-78`]: the MSI makes a desktop shortcut only (no `menu`) and sets no AppUserModelID.

## Ideal final result

Clicking a SimpleX toast from the current run, on screen or in Action Center, behaves like opening a `simplexchat:` link: Windows starts SimpleX with the link, and the existing single-instance path raises the running window, which then opens the chat. A toast from an earlier run starts or raises the app.

Microsoft documents a "stub" CLSID with protocol-activation toasts for unpackaged apps, in the form where the AUMID and CLSID sit on a Start-menu shortcut. The registry-only form (`HKCU\Software\Classes\AppUserModelId\<id>` with `CustomActivator`, as the Windows App SDK and CommunityToolkit write it, but without their `LocalServer32`) is **INFERENCE**. Hence G1.

## Options

1. **WinRT toasts with protocol activation** (chosen, D1). The click works from the popup and from Action Center, and SimpleX becomes a stable app in Windows notification settings (#4201, INFERENCE). Cost: about 300–400 lines of Windows-only JNA/COM code, verifiable only on Windows. Each click starts a second JVM to forward the link (about 1–3 s, INFERENCE).
2. **Code-only tray-icon stopgap.** It works only while the balloon is on screen and only for the last notification. Rejected.
3. **A third-party WinRT library.** A new native dependency in trusted code. Rejected.
4. **PowerShell per toast.** Slow, blockable, and message content would reach command lines and logs. Rejected.

## Design (option 1)

Narrative: on Windows, each notification becomes a Windows toast whose click opens `simplexchat:/notification/<token>`. When the desktop app receives such a link, it raises the window and, if the app is unlocked, opens the chat the token stands for, on the host and in the profile it came from.

Commits, in this order:
- (a) add the `rhId` parameter and pass it through: structure only, no behaviour change;
- (b) `openChatAction` routes by `rhId`;
- (c) the AUMID registration;
- (d) `WindowsToast`, tokens and the link handler;
- (e) docs.

1. **AUMID registration** in `AppLinkScheme.desktop.kt`, on the same thread and through the same `WindowsShell`/`RegistryValue` abstraction as `registerWindowsSchemes`. It runs only after `simplexchat:` registered successfully, because without the scheme a click can't come back.
   - Key: `Software\Classes\AppUserModelId\chat.simplex.app`.
   - `DisplayName = APP_DISPLAY_NAME`.
   - `IconUri` = a PNG written to the data dir when missing (not `tmpDir`, which is wiped at start, `Main.kt:43`).
   - `CustomActivator = {fixed GUID}`. No `LocalServer32` (stub).
   - The outcome (ready or failed) is published to `WindowsToast`.
2. **`WindowsToast`** (new `platform/WindowsToast.desktop.kt`, Windows only).
   - **Thread and commands.**
     - One daemon thread takes commands from one **unbounded** `LinkedBlockingDeque`.
       - Clear commands and the registration outcome go to the front (`offerFirst`); toast commands go to the back.
       - A separate `AtomicInteger` counts toasts waiting, including the held list below. Offering a toast, in order:
         1. Reserve a slot: `incrementAndGet() <= 32`. Otherwise `decrementAndGet()` and drop the toast without touching the token map.
         2. Put its token in the map.
         3. `offerLast`.
         - Accepted toasts are processed in order and at most 32 wait, so a waiting toast's token is never evicted by the 256-entry map.
         - The count is decremented in a `finally` around routing each toast, whatever the exit: dropped, `DisabledFor*`, shown, or fallback. No exit leaks a slot.
       - Clears are never dropped.
       - The thread blocks on the deque, so a clear runs as soon as it is queued, even with no toast after it. Callers never block.
     - **Registration wait**: one deadline, 10 s after the thread starts.
       - Until the outcome arrives, toasts taken from the deque are moved to a local held list (they still count toward the 32), and the thread waits with `pollFirst(deadline - now)`, so clears keep running.
       - On "ready", held toasts are shown in order. On "failed" or at the deadline, they go to the fallback, and the thread returns to a blocking `takeFirst`.
       - The deadline is not an outcome. Toasts arriving after it, while no outcome has arrived, go straight to the fallback. A late "ready" switches every later toast to WinRT, so a slow first-run registration costs only the toasts before it. "Failed" is final.
       - The first outcome wins. The registration code publishes "ready" after writing the key. Its `finally` publishes "failed" only if nothing was published yet, which covers `Unpackaged`, a scheme held by another program, and exceptions.
   - **Thread start**: `RoInitialize(RO_INIT_MULTITHREADED)`. Accept `S_OK` and `S_FALSE`; `RPC_E_CHANGED_MODE` or any other error disables the toaster, and every toast goes to the fallback.
   - **Once**, with each interface's IID and the vtable slot of each call written in the code next to the call:
     - `RoGetActivationFactory("Windows.UI.Notifications.ToastNotificationManager", IToastNotificationManagerStatics {50AC103F-D235-4598-BBEF-98FE4D1A3AD4})` → `CreateToastNotifierWithId(aumid)` (slot 7) → `IToastNotifier`.
     - QI `IToastNotificationManagerStatics2` → `get_History` → `IToastNotificationHistory`: `RemoveGroupWithId(group, aumid)` and `ClearWithId(aumid)`. The `…WithId` forms are needed without package identity.
     - `RoGetActivationFactory("Windows.UI.Notifications.ToastNotification", IToastNotificationFactory)`.
     - `RoGetActivationFactory("Windows.Foundation.PropertyValue", IPropertyValueStatics)`.
   - **Before routing** each toast, whether to WinRT or to the fallback (held toasts, a failed registration, a disabled toaster): if it has a token and that token is no longer in the map, drop it. Its user, or everything, was cleared after it was queued.
   - **Per WinRT toast**:
     1. Read `IToastNotifier.Setting` (slot 8).
        - `Enabled`: continue.
        - `DisabledForApplication` / `DisabledForUser` / `DisabledByGroupPolicy`: drop the toast, and no sound. The user turned SimpleX off in Windows.
        - `DisabledByManifest` or an error: fallback.
     2. `RoActivateInstance("Windows.Data.Xml.Dom.XmlDocument")` → QI `IXmlDocumentIO` → `LoadXml`.
     3. QI `IXmlDocument` {F7F3A506-1E87-42D6-BCFB-B8C809FA5494} → `IToastNotificationFactory::CreateToastNotification` (slot 6).
     4. On the new toast:
        - `put_ExpirationTime` (`IToastNotification`, slot 7) = now + 1 h. The value is `IPropertyValueStatics::CreateDateTime` → QI `IReference<DateTime>` {5541D8A7-497C-5AA4-86FC-7713ADBF2A2C}, with `DateTime.UniversalTime` in 100 ns ticks since 1601-01-01 UTC.
        - `put_Group` (`IToastNotification2`, slot 8) = `"<rhId or local>-<userId>"`.
        - `put_ExpiresOnReboot(true)` through QI `IToastNotification6` {43EBFE53-89AE-5C1E-A279-3AECFE9B6F54}. That interface exists only on Windows 10 1903+; `E_NOINTERFACE` just skips the step, since the 1 h expiry still applies.
        - No Tag.
     5. `IToastNotifier::Show` (slot 6), then run the toast's `onShown`.
     - A failure in steps 2–5 sends that toast to the fallback. Only factory or apartment failures disable the toaster for the session.
   - **Fallback** = `displayViaTwoSlices(…)`: today's two-slices code, renamed in place, which never routes back to `WindowsToast`. `WindowsToast` passes it `onShown.takeIf { BALLOON_SILENT }`, where `BALLOON_SILENT` is a constant set from V1 (sound plan). The balloon's own sound and the app's sound never both play.
   - **XML**, one function, in this order:
     1. Truncate the title to 128 and the body to 512 code points.
     2. Remove characters XML 1.0 forbids (U+0000–0008, 000B, 000C, 000E–001F, U+FFFE/FFFF, lone surrogates).
     3. Escape `& < > " '`.
     - All values go through it: title, body, the `launch` attribute, and the image URI (`file.toPath().toUri()`, i.e. `file:///C:/…`).
     - Elements: `ToastGeneric`, two `<text>`, optional `appLogoOverride` from the existing `prepareIconPath`, and `activationType="protocol" launch="…"` on `<toast>`.
     - **Audio** is decided once, at the `displayNotification` call site, and passed in the request as `ToastAudio.Silent` or `ToastAudio.Default`. Every toast from `displayNotification` (messages, contact requests, contact-connected, missed calls) gets `Silent` (`<audio silent="true"/>`) when either is true:
       - the app plays its own sound: the toggle is on and `NotificationSound` is not disabled for the session;
       - recording or a call is in progress: the sound plan's `recordingOrCall`, so the microphone doesn't pick up a WinRT toast sound. Fallback balloons can't be silenced (known limits).
       - Otherwise there is no `<audio>` element, and Windows plays its default sound under SimpleX's per-app switch.
     - Call toasts (`notifyCallInvitation`) and error toasts (`showMessage`) get `Silent` when a call is active or a voice message is being recorded (`activeCall` or `RecorderInterface.stopRecording`), so the microphone doesn't pick up the toast sound. Otherwise they get `Default`. The in-app ring itself is muted while the call view is shown (`IncomingCallAlertView.kt:31-32`). `activeCallInvitation` is not checked here, because it is set only after `notifyCallInvitation` returns (`CallManager.kt:17-18`).
3. **Remote host ID in notifications** (D2). Commit (a) is the parameter only; commit (b) is the routing.
   - Common `NtfManager`: `rhId: Long?` is added as the **last** parameter, and callers pass it by name (`notifyContactRequestReceived` mixes named and positional arguments, `NtfManager.kt:33-36`). It goes on:
     - the abstract `displayNotification` (`:114`);
     - `notifyContactConnected` (`:21-26`) and `notifyContactRequestReceived` (`:28-37`), which pass it on;
     - `notifyMessageReceived`, which already has it and now passes it at `:48`.
   - Callers, all with `rhId` in scope:
     - `SimpleXAPI.kt:2965, 3012` (inside `processReceivedMsg`, `val rhId = msg.rhId`, `:2937`);
     - `:3122` and `:3763`, which are dead on desktop and live on Android;
     - `CallManager.kt:21` (`invitation.remoteHostId`).
     - `:3073` needs no change.
   - Overrides: `AppCommon.desktop.kt:27` and Android `SimplexApp.kt:201` gain the parameter. Android ignores it.
   - Commit (b): `openChatAction(userId, chatId, rhId: Long? = null, canOpen: () -> Boolean = { true })`. Android's callers are unchanged.
     - `canOpen()` is checked before each step that changes what is shown, and the function stops if it no longer holds:
       1. If `rhId != chatModel.remoteHostId()`: `switchUIRemoteHost(rhId)` (`SimpleXAPI.kt:3819-3845`).
       2. The existing guard `userId != currentUser.userId` → `changeActiveUser(rhId, userId, null)`.
       3. `clearOverlays` and `openChat(rhId = rhId, …)`.
     - Each switch can take seconds, so a lock that happens in between stops the next step.
     - `null` becomes `rhId` in every branch, and the TODO at `:74` is removed.
     - On Android, `remoteHostId()` is always null, so behaviour is unchanged.
4. **Tokens, clearing and sound** (`NtfManager.desktop.kt`).
   - On Windows, `displayNotificationViaLib` hands the toast to `WindowsToast`. Elsewhere it calls `displayViaTwoSlices`, so non-Windows is unchanged.
   - It gains:
     - `target: NotificationTarget?`, a data class `(rhId, userId, chatId)`; `null` for `showMessage`;
     - `audio: ToastAudio`;
     - `onShown: (() -> Unit)?`.
   - The token and the launch link are built inside the Windows branch, in §2's offering steps 1–3. Other OSes ignore `target` and `audio`.
   - The existing `userId`/`chatId` parameters, used for `prevNtfs`, are replaced by `target`: `target?.userId ?: -1` and `target?.chatId ?: "MESSAGE"`, as `showMessage` passes today.
   - `displayNotification` passes `NotificationSound::play` as `onShown`, under the sound plan's conditions, which stay at the call site.
     - The call moves into `onShown`: on Linux and macOS the sound now starts right after `toast()`, inside the existing `withBGApi` block (`:136-139`), where `play()` only queues.
     - `notifyCallInvitation` and `showMessage` pass `null`.
   - **Tokens**:
     - Created only for Windows toasts from `displayNotification` and `notifyCallInvitation`. Each is put in the map only after its toast reserved a slot (§2 offering steps 1–3). A toast without a slot never touches the map, so a flood never evicts live tokens.
     - `showMessage` uses `simplexchat:/notification` (no token).
     - A token is 16 `SecureRandom` bytes as lowercase hex. The map token → (rhId, userId, chatId) is a `synchronized` LinkedHashMap of the last 256 entries.
   - **Clearing**, at Android's call sites only (FACT above):
     - `cancelNotificationsForUser(userId)` drops that userId's tokens and queues `RemoveGroupWithId` for `"local-<userId>"` and for `"<id>-<userId>"` for each id in `chatModel.remoteHosts`. It has no rhId, so it over-clears.
     - `cancelAllNotifications` empties the map and queues `ClearWithId`.
     - Nothing is cleared at start, exit, host switch or host stop. The 1 h expiry bounds those cases (see the known limits).
5. **The link handler**, desktop only. Common `openAppLink` and Android are untouched.
   - `AppLinks.kt` `isNotificationLink(uri)`: a case-insensitive `simplexchat:` scheme, and a path that is exactly `/notification` or `/notification/<32 lowercase hex>`. Anything else, including `simplexchat://notification…` and `/notificationsX`, takes today's path.
   - `openDesktopLink`: for a notification link, call `chat.simplex.common.model.NtfManager.openNotificationLink(token)`, a function on the desktop object, not the common abstract. Otherwise, today's code.
   - `openNotificationLink`, in order:
     1. Look up the token. If it is unknown or missing: `showWindow()` and return. Nothing that needs `initApp` is touched.
     2. Evaluate `locked()` **before** raising the window, while `windowFocused` still has its value from before the click.
     3. `showWindow()`, as every link does today.
     4. Return without opening anything if any of the conditions below holds.
   - `locked()` is a side-effect-free check that conservatively mirrors when `AppLock.recheckAuthState` (`AppLock.kt:271-284`) would lock, given how desktop calls it (`DesktopApp.kt:184-205`). It leaves out `recheckAuthState`'s call-view exemption, so during a call a click may only raise the window:
     - `appPrefs.performLA.get() && (AppLock.userAuthorized.value != true || (!simplexWindowState.windowFocused.value && (AppLock.enteredBackground.value == null || elapsedRealtime() - enteredBackground >= laLockDelay * 1000)))`.
     - A focused, unlocked window never re-locks. With lock delay 0, the focus event that would re-lock runs after this Swing task.
     - It does not call `recheckAuthState`, so the focus handler doesn't show the prompt twice.
     - The same `locked()` is passed as `canOpen = { !locked() }` to `openChatAction`.
   - The conditions for step 4 are:
     - `locked()`;
     - `chatModel.chatRunning.value != true`, or onboarding not complete: the same readiness as ordinary links (`App.kt:243-246`). This covers stopped chat, database export and import;
     - `isBadgeLinkIssuing()` (as `ChatListView.kt:781-782`);
     - `rhId != null` and that host's `sessionState` is not `RemoteHostSessionState.Connected`.
   - Otherwise it calls `openChatAction(userId, chatId, rhId, canOpen = { !locked() })`. Chat is already running here (step 4), so `awaitChatStartedIfNeeded` returns at once. By the time `canOpen` runs, the window is focused, so the focus term is false. A lock that happened meanwhile shows in `userAuthorized`, which the focus handler's `recheckAuthState` has updated.
   - `Main.kt:30` (cold start): a notification link is not stored as `appOpenUrl`. The app just starts.
   - Visible elsewhere: on Linux and macOS, a forged `simplexchat:/notification/<32 hex>` no longer shows the "not supported" alert. The window is raised, as for any link today.
6. **Docs**:
   - `spec/services/notifications.md`:
     - §2: the abstract signatures with `rhId`.
     - §4: the Windows path, tokens, AUMID, `Setting` handling, routing, the clearing model and known limits.
   - `spec/client/navigation.md`: the `simplexchat:/notification/` path next to the badge link.

Not "feature off = unchanged": every Windows user gets the new toaster and one more HKCU key on each start, with today's two-slices path as the fallback. On Linux and macOS, behaviour changes only for forged notification links.

## Known limits (documented in spec §4)

These cases follow Android, where toasts are not cleared. Each is bounded by the 1 h expiry, and from Windows 10 1903 on, by reboot.
- Hiding the **active** profile does not clear its earlier toasts until the user leaves it. This matches Android (`HiddenProfileView.kt:38`).
- Switching host, quitting or crashing while a hidden profile is active.
- `/hide user` typed in the in-app terminal; hiding or deleting a profile on the connected phone.
- Fallback balloons (registration failed, `DisabledByManifest`, a toast-level failure) follow today's balloon behaviour, not the expiry.
- An event serialised just before a hide commits can still show one toast for that profile.
- If registration takes longer than 10 s, toasts before it finishes are fallback balloons: not clickable, and outside the Windows per-app switch.
- Two fast clicks on toasts from different hosts run two host switches at once (the existing TODO at `SimpleXAPI.kt:3820`).
- A click on a toast from an earlier run, or one whose token left the 256-entry map (more than 256 toasts within its hour), only starts or raises the app.
- With `BALLOON_SILENT = false`, fallback balloons play the Windows sound even while recording or in a call.

## Go/no-go tests (on Windows, before code review)

On the oldest supported Windows 10 build (1809 LTSC, for the missing `IToastNotification6`) and on Windows 11.
- **G1**, with only the registry AUMID (no shortcut property):
  - A toast with an icon shows "SimpleX Chat" and persists in Action Center.
  - A click starts `SimpleX.exe` with the link.
  - SimpleX is listed in Settings → Notifications.
  - `RemoveGroupWithId` and `ClearWithId` remove toasts.
  - A toast leaves Action Center after its expiry: test with a short value, then set it to 1 h.
  - On 1903+, toasts are gone after a reboot.
  - With the AUMID key deleted while running, record what `Setting` and `Show` report.
  - Under Focus Assist, the toast lands in Action Center and the click works from there.
- If G1 fails: the fallback is a Start-menu shortcut carrying `System.AppUserModel.ID` and `ToastActivatorCLSID`, created at runtime via `IShellLink`/`IPropertyStore` or by the installer. That becomes a new decision for you.

## Threat model

- **XML injection.** The display name, group name and message text are peer-controlled.
  - Unescaped, they could add `<action>`, change `launch`, add a remote `<image src>` (an IP leak) or `<audio src>`.
  - Invalid characters, or a truncation splitting a surrogate pair, would make `LoadXml` fail.
  - Mitigation: the single truncate → sanitise → escape function. It is unit-tested with `</text><action …>`, `&`, quotes, `]]>`, `{title}`, NUL and each control character, lone surrogates, an emoji at the cut point, bidi overrides (U+202E, U+2066–2069) and 100 KB strings.
  - Bidi can't cross between the separate `<text>` elements.
- **Link forgery** (web pages, local programs, or a process writing `simplex.show`, which is re-checked by `isAcceptedLink`).
  - Tokens are unguessable, so a forged link at most raises the window.
  - Launch strings also sit in the Windows notification database, readable by the same user. Such a process can already do more.
- **Remote host.** userIds collide across hosts; the token carries the core-assigned rhId. A disconnected host's token only raises the window. A malicious mobile controls only its own users, chats and text, and escaping covers the text.
- **Hidden profiles.** Clearing follows Android, plus a 1 h expiry and expiry on reboot. The known limits above are the residual risk, and the preview mode ("hide content") applies to every toast.
- **App lock.** A click never opens a chat, switches profile or closes the passcode prompt while the app is or would be locked. The lock is checked before raising the window and again right before `clearOverlays`.
- **The Windows OS switch.** Respected: no toast and no sound when SimpleX is disabled in Windows. Startup toasts wait for registration (bounded) instead of going around it through the balloon.
- **Floods.** At most 32 toasts wait; the rest are dropped. Clears are never dropped, and tokens are kept only for accepted toasts.
- **Registry.** HKCU, the same trust level as the existing scheme keys.
- **Native code.** A wrong vtable slot crashes the process. Mitigations: ABI slots documented per call, one isolated thread, and G1 plus the test plan on two Windows versions before merge. No `Native.setProtected`: it is process-wide and unreliable with the app's other JNA users.

## Test plan

MSI from fork CI (a one-job workflow on a fork branch, as for PR #7581). Run by you on Windows 10 and 11:
- Running app, window behind another app: click the popup → the window comes forward and the chat opens. The same from Action Center. Note the delay.
- Minimized, and hidden in the tray: the same.
- App closed: an Action Center click starts the app on the chat list.
- Locked, with lock delay 0 and with 30 s: a click only raises the window. The lock screen stays, the passcode prompt is not closed, and no items are marked read.
- Lock on (delay 30 s), app focused and unlocked: a popup for another chat opens it, if clicking the popup doesn't take focus from the app (observe and record). With delay 0, a click that takes focus re-locks, as any focus loss does today.
- Chat stopped (Settings → Database → stop): a click on an earlier toast only raises the window.
- During a call: an incoming call toast and message toasts are silent.
- Clearing races: self-destruct at the startup lock screen during a startup flood. Afterwards, Action Center is empty and no queued chat toast appears. A queued error toast (`showMessage`, no token) may still appear; it carries no chat content.
- A background profile's chat: the profile switches.
- A remote host (mobile) connected:
  - A mobile notification while local is shown: switches to the mobile and opens the chat.
  - A local notification while the mobile is shown: switches back.
  - The mobile shown, and a click for another profile on the mobile: that mobile profile opens, and the local profile is unchanged.
  - After a disconnect, the mobile's old toasts only raise the window.
- Clearing:
  - Leave a hidden active profile, hide a non-active profile, delete a profile: their toasts disappear right away, with no new toast arriving.
  - Self-destruct and database delete: Action Center is empty.
- A bot sends hostile names and texts: shown literally, with no extra button or image, and later toasts still work.
- SimpleX turned off in Windows Settings: no toast, no balloon, no sound. SimpleX is listed there, and its switches survive a restart (#4201).
- Sound:
  - App sound off (default): the Windows default sound per toast, under SimpleX's switch, except while recording or in a call. If V1 found today's balloon silent, this is a new sound; say so in the PR.
  - App sound on: exactly one sound, the app's, at most once per 3 s.
  - Fallback toasts never add a second sound.
- Startup flood (100 messages while registering): toasts appear after registration; the app stays responsive; clicks on them work.
- Linux AppImage regression: notifications unchanged. A forged notification link raises the window without the alert.

## Decisions

- **D1. Approach.** DECIDED: option 1, gated by G1.
- **D2. Remote host.** DECIDED: pass `rhId` through common `displayNotification`, so remote clicks open the right host's chat (§3).
- **D3. Action buttons.** DECIDED: not in this change; body click only.
- **D4. Clearing or replacing a chat's toasts when it is opened.** DECIDED: a follow-up.
- **D5. macOS and Linux clicks.** DECIDED: a separate plan.
- **D6. Toast history.** DECIDED: Android-parity clearing plus a 1 h expiry (and on reboot), with the known limits documented.
