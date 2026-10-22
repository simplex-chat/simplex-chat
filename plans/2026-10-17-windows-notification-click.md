# Windows: clicking a notification opens the chat

Status: draft rev 13 (after the rev 12 review). Redesigned 2026-10-17 so that no injection is possible by construction and the diff is smaller:
- Toasts are built only through Windows' XML DOM. Our code never builds, escapes or parses XML.
- The two-slices fallback uses only its plain-text AWT backend, constructed directly.
- Remote-host clicks open the clicked chat on its host.

All decisions are taken. Implementation is gated by G1. Base: `origin/master` a932c8298. Paths are relative to `apps/multiplatform` unless they start with `src/` (the Haskell core).

Related plans: [sound](2026-10-17-desktop-notification-sound.md), [turn off](2026-10-17-desktop-notifications-turn-off.md).

## Problem

On Windows, clicking a SimpleX notification does nothing:
- The chat doesn't open.
- The window isn't raised.
- From Action Center, nothing happens at all.

Related: #4201 (SimpleX is not a stable app in Windows notification settings; its sound switch resets), #3301, closed PR #7182.

## Cause

- On Windows, two-slices selects `AWTToaster`, or `GNTPToaster` if Snarl or Growl listens on port 23053 at the host's own address (FACT [two-slices `ToasterFactory.java:50-76`, the service order, the app's classpath]).
- FACT [two-slices `AWTToaster.java:103, 173-175`, identical in 0.9.1–0.9.7]:
  - It registers itself as the `TrayIcon` ActionListener, but `actionPerformed` is empty.
  - `builder.defaultAction()` and `builder.actions()` are never read.
  - So clicks never reach the app, and upgrading the library doesn't help.
- FACT [`AWTToaster.java:92-128`]: it adds a second tray icon for toasts and removes it 10 s after the last toast. INFERENCE: after that, an Action Center click has no target.
- FACT [`NtfManager.kt:70-83`]: `openChatAction` never raises the window. It also hard-codes the local host: `changeActiveUser(null, …)` at `:76`, `openChat(rhId = null)` at `:81`, and a TODO at `:74`. It is effectively unused on desktop today.
- FACT [two-slices `GNTPToaster.java:77-78`]: the GNTP backend writes `Notification-Title: %s\r\n` and `Notification-Text: %s\r\n` without escaping. Today, with Snarl or Growl listening on the host's own address (port 23053), a CR/LF in peer text injects GNTP headers. §2 removes this on Windows.
- FACT [two-slices `ToasterFactory.java:56-74`]: a preferred toaster class is only a preference. The factory still constructs every service in order, and when the preferred one fails, it falls back to the first that was constructed, which can be GNTP.

## Existing structure to reuse

- FACT [`AppLinkScheme.desktop.kt:67-71, 97-106, 168+`]:
  - On each start, a background thread registers `simplexchat:` and `simplex:` per user under `HKCU\Software\Classes` for any jpackage launcher on Windows, with the command `"<SimpleX.exe>" "%1"`.
  - A scheme held by another program is left alone. `registered` reflects `simplexchat:` (`:180`).
  - The app already uses `chat.simplex.app` (`:26`) and `APP_DISPLAY_NAME = "SimpleX Chat"` (`:43`).
- FACT [`SingleInstance.kt:49-66, 139-157, 205-214, 227-245`; `Main.kt:28-32`]:
  - A second launch writes its link to `simplex.show`, allows the running instance the foreground, and exits.
  - The running instance takes the signal on the Swing thread and calls `openDesktopLink(link)` (`AppLinks.kt:19-22`), which raises the window (`DesktopApp.kt:259-273`).
  - On a cold start, `Main.kt:30` stores the link from args.
- FACT [`AppLinks.kt:12-13`, `ChatListView.kt:797-798`]: an accepted link has a case-insensitive `simplexchat:` prefix (or is a connection link) and is at most 8192 bytes. Signal files are re-checked in `takeSignal`.
- FACT [`App.kt:240-249`, `ChatListView.kt:775-789, 804-814`]:
  - Ordinary links are processed only when the app is authorized, onboarding is complete and chat is running.
  - Links are dropped during a badge redemption.
  - Unknown `simplexchat:` paths show "not supported".
- FACT [`SimpleXAPI.kt:2937`]: every event's `rhId` comes from the desktop's own core.
- FACT [`SimpleXAPI.kt:787-789`, `HiddenProfileView.kt:38-39`, `UserProfilesView.kt:371`, `LocalAuthView.kt:73`, `DatabaseView.kt:598`]: these are the clearing calls, the same set Android has.
  - `cancelNotificationsForUser`: leaving a hidden active profile, hiding a non-active profile, user deletion.
  - `cancelAllNotifications`: self-destruct, database deletion.
- FACT [`AppLock.kt:23-24, 271-284`; `DesktopApp.kt:184-205`; `PrivacySettings.kt:487`; `App.kt:111-115, 157-172`; `ChatView.kt:3090-3094`]:
  - The lock state is `userAuthorized` / `enteredBackground`.
  - A focused window never re-locks. With lock delay 0, re-locking happens only in the focus handler.
  - `clearOverlays` closes the passcode prompt.
  - ChatView stays composed under the lock screen and marks visible items read.
- FACT [`IncomingCallAlertView.kt:31`]: the in-app call ring always plays on desktop.
- FACT [`desktop/build.gradle.kts:70-78`, `shortcut = true` at `:75`]: the MSI makes a desktop shortcut only. It has no Start-menu group and no AppUserModelID.

## Ideal final result

Clicking a SimpleX toast from the current run, on screen or in Action Center, behaves like opening a `simplexchat:` link. Windows starts SimpleX with the link. The existing single-instance path raises the running window, which then opens the chat on the host it came from: this computer or a connected mobile.

Peer-controlled text reaches Windows only as DOM text-node values. No code of ours turns it into markup or protocol text.

The registry-only AUMID form (`HKCU\Software\Classes\AppUserModelId\<id>` with a stub `CustomActivator`) is INFERENCE; Microsoft documents the stub CLSID on a Start-menu shortcut. Hence G1.

## Options

1. **WinRT toasts built through the XML DOM, with protocol activation** (chosen).
   - Injection-free by construction. The click works from the popup and from Action Center, and #4201 is addressed (INFERENCE).
   - About 250 lines of Windows-only JNA/COM code in one new file, plus small edits in three desktop files. In common code: the `rhId` parameter and the `openChatAction` routing.
2. **AWT balloon with an in-app click listener**, reviving the dead `windowsDisplayNotification`.
   - A plain-text API in about 25 lines.
   - No Action Center clicks and no fix for #4201. Not chosen.
3. **Hand-built XML with an escaping function** (rev 10). It relies on our escaping being right. Replaced by option 1.
4. **A third-party WinRT library, or PowerShell per toast.** Rejected: a new native dependency, or content exposed on the command line.

## Design (option 1)

Narrative: on Windows, each notification becomes a Windows toast, built from Windows' own template with peer text inserted as text nodes. Its click opens `simplexchat:/notification/<token>`. When the desktop app receives that link, it raises the window. If the app is unlocked and ready, it opens the chat the token stands for, on that chat's host and in its profile.

Commits:
- (a) add the `rhId` parameter and pass it through (structure only, no behaviour change);
- (b) `openChatAction` routes by `rhId` and gains the lock guard;
- (c) on Windows, two-slices uses only a directly constructed AWT toaster;
- (d) the AUMID registration;
- (e) `WindowsToast` and tokens; this also removes the sound plan's Windows toggle hiding, if V1 added it;
- (f) the link handler;
- (g) docs.

1. **AUMID registration**, in `AppLinkScheme.desktop.kt`, on the same thread and through the same `WindowsShell`/`RegistryValue` abstraction as `registerWindowsSchemes`. It runs only after `simplexchat:` registered successfully.
   - Key: `Software\Classes\AppUserModelId\chat.simplex.app`.
   - `DisplayName = APP_DISPLAY_NAME`.
   - `IconUri`: only if G1 shows the toast or Settings entry needs it. If so, it is a PNG written to the data dir when missing; otherwise it is left out with its file.
   - `CustomActivator = {fixed GUID}`. No `LocalServer32`, so this is a stub.
   - When done, it sets `@Volatile windowsToastsReady = true`. Until then, and if registration never succeeds, toasts use the fallback (a known limit).
2. **Fallback: AWT only, constructed directly.** On Windows, `initApp` installs `ToasterFactory.setFactory(object : ToasterFactory() { override fun toaster() = awtToaster })` (`ToasterFactory.java:107`).
   - `awtToaster` is a lazily constructed `AWTToaster(ToasterFactory.getSettings())`. No other backend is ever constructed, so GNTP never opens a socket or sends its REGISTER.
   - If `AWTToaster` can't be constructed (no system tray), `toaster()` throws. The existing catch logs it, and the fallback toast is not shown. It never reaches another backend.
   - Behaviour change: Snarl/Growl users on Windows get the balloon instead of GNTP.
3. **`WindowsToast`** (new `platform/WindowsToast.desktop.kt`, Windows only).
   - **Thread.**
     - One single-thread executor, started on Windows at `initApp` whatever the registration outcome, so clears always run.
     - Toasts and clears are submitted in order (FIFO). Only toast tasks reserve a slot; clear tasks don't.
     - An `AtomicInteger` limits waiting toasts to 32. Offering a toast:
       1. Reserve a slot: `incrementAndGet() <= 32`; otherwise decrement and drop.
       2. Put its token in the map.
       3. Submit.
     - Each toast task decrements the count in `finally`. Every task catches `Throwable`.
     - **Wedge check**: each task sets `@Volatile runningSince` when it starts and clears it in `finally`.
       - At offer time, `runningSince != 0 && now - runningSince > 10 s` means the toaster is wedged (for example the notification service is stuck). The new toast then goes to the fallback instead of the queue.
       - This is a live condition, not a latch. Once the stuck task returns, later toasts use WinRT again.
       - Toast tasks that waited longer than 30 s are dropped when they start, so a wedge doesn't end in a burst of stale toasts and sounds.
       - Clears queued behind a wedged task run when it returns (known limit).
   - **Thread start**: `RoInitialize(RO_INIT_MULTITHREADED)`. `S_OK` and `S_FALSE` continue. Anything else disables the toaster: toasts go to the fallback, and clears are no-ops.
   - **COM helper**: one function calls vtable slot *n* of a COM pointer and checks the HRESULT.
     - Every interface pointer and HSTRING is released in the `finally` of the scope that owns it: `Release`, `WindowsDeleteString`. That is the per-toast scope for the document, nodes, toast and boxed `DateTime`, and the process for the factories and the notifier.
     - Logging: `WindowsToast` logs only the HRESULT, the slot and the step, never a title or body.
     - HSTRINGs are made with `WindowsCreateString` from a `char[]` with an explicit length, so embedded NULs are kept as data.
     - Each IID, QI and slot is written next to its call.
   - **Once**: activation factories for:
     - `ToastNotificationManager`: `IToastNotificationManagerStatics` for `CreateToastNotifierWithId` and `GetTemplateContent`; QI to `…Statics2` for `get_History`;
     - `ToastNotification`: `IToastNotificationFactory`;
     - `Windows.Foundation.PropertyValue`: `IPropertyValueStatics`.
   - **Per toast**:
     1. If it has a token that is no longer in the map, drop it: it was cleared after it was queued.
     2. Read `IToastNotifier.Setting`.
        - `Enabled`: continue.
        - `DisabledForApplication`, `DisabledForUser`, `DisabledByGroupPolicy`: drop it, with no sound.
        - `DisabledByManifest`, or an error: fallback.
     3. **Build the content with Windows' DOM only**:
        - `GetTemplateContent(ToastTemplateType_ToastText02)` → `IXmlDocument`. This is Windows' own template, with a title and a body text element.
        - `GetElementsByTagName("text")` → `IXmlNodeList`. Items 0 and 1 are `IXmlNode`.
        - For each, `CreateTextNode(value)` → `IXmlText`, QI to `IXmlNode` {1C741D59-2122-47D5-A856-83F3D4214875}, then `AppendChild`.
        - Values are cut at a code-point boundary to fixed lengths. This is a length cut only, with no escaping or filtering, to stay under the toast payload limit (INFERENCE: about 5 KB serialised). G1 measures the real limit, using 4-byte code points and `&`/`<`-heavy strings, which grow when serialised. The cut is then sized so that a maximal value of either kind fits; the starting values are 128 and 512.
        - `get_DocumentElement` → `IXmlElement`: `SetAttribute("activationType", "protocol")` and `SetAttribute("launch", link)`. The link is `simplexchat:/notification/<32 hex>` or `simplexchat:/notification`, built only from app-generated hex.
        - For `ToastAudio.Silent`: `CreateElement("audio")`, then `SetAttribute("silent", "true")`, then QI both elements to `IXmlNode` and `AppendChild`.
        - There is no image element: toasts carry no avatar.
     4. `IToastNotificationFactory::CreateToastNotification(doc)`. Then:
        - `put_ExpirationTime` = now + 1 h, via `IPropertyValueStatics::CreateDateTime` (100 ns ticks since 1601-01-01 UTC), QI to `IReference<DateTime>`;
        - `put_Group` through a QI to `IToastNotification2`: `"<rhId or local>-<userId>"`, made only of digits and `-`;
        - `put_ExpiresOnReboot(true)` through a QI to `IToastNotification6`, skipped on `E_NOINTERFACE` (before Windows 10 1903).
     5. `IToastNotifier::Show`, then `onShown`.
     6. If steps 3–5 fail with the real text, for example because Windows rejects a character, retry once with the toast's **constant text**, keeping the same token, group and expiry.
        - The caller supplies the constant text as app strings only: `notification_preview_somebody` as the title, and as the body `notification_preview_new_message` for chat notifications, or the call-type text for call toasts.
        - Only if the retry also fails does the toast go to the fallback. A peer therefore can't force the balloon: the retry contains no peer data.
   - **Clearing** tasks:
     - `RemoveGroupWithId(group, aumid)`, and `ClearWithId(aumid)` on `IToastNotificationHistory`.
     - They need only the AUMID string, so they run even if registration never succeeded. Toasts from an earlier run are cleared too.
   - **Audio** is decided at each call site, and passed as `ToastAudio.Silent` or `ToastAudio.Default`:
     - `displayNotification`: `Silent` when the app will play its own sound (the sound toggle is on and `NotificationSound` is not disabled), or during recording, a call or a ringing call (the sound plan's `recordingOrCall`). Otherwise `Default`, which is Windows' sound under SimpleX's per-app switch.
     - `notifyCallInvitation`: always `Silent`, because the in-app ring always plays on desktop (`IncomingCallAlertView.kt:31`).
     - `showMessage`: `Silent` during recording or a call, otherwise `Default`.
4. **Remote host ID in notifications**. Commit (a) adds the parameter only; commit (b) adds the routing.
   - In common `NtfManager`, `rhId: Long?` is added as the **last** parameter and passed by name at every call site (`notifyContactRequestReceived` mixes named and positional arguments, `NtfManager.kt:33-36`). It is added to:
     - the abstract `displayNotification` (`:114`);
     - `notifyContactConnected` (`:21-26`) and `notifyContactRequestReceived` (`:28-37`);
     - `notifyMessageReceived`, which already has it and now passes it on (`:48`).
   - Callers, all with `rhId` in scope:
     - `SimpleXAPI.kt:2965, 3012` (`processReceivedMsg`, `val rhId = msg.rhId`, `:2937`);
     - `:3122` and `:3763`;
     - `CallManager.kt:21` (`invitation.remoteHostId`).
   - Overrides `AppCommon.desktop.kt:27` and Android `SimplexApp.kt:201` gain the parameter. Android ignores it.
   - Commit (b): `openChatAction(userId, chatId, rhId: Long? = null, canOpen: () -> Boolean = { true })`. Android's caller (`MainActivity.kt:128`) is unchanged.
     - Each `canOpen()` check runs in `withContext(Dispatchers.Main)`, the Swing thread on desktop, so it is ordered with the focus handler's `recheckAuthState`. `openChatAction` itself runs on `Dispatchers.Default`. It is checked before each step that changes what is shown, and the function stops if it no longer holds:
       1. if `rhId != chatModel.remoteHostId()`: `switchUIRemoteHost(rhId)` (`SimpleXAPI.kt:3819-3845`);
       2. the existing guard `userId != currentUser.userId` → `changeActiveUser(rhId, userId, null)`;
       3. `clearOverlays` and `openChat(rhId = rhId, …)`.
     - `null` becomes `rhId` in every branch, and the TODO at `:74` is removed. On Android, `remoteHostId()` is always null, so behaviour is unchanged.
5. **`NtfManager.desktop.kt`**.
   - `displayNotificationViaLib` gains `rhId`, `audio: ToastAudio`, `constantText: Pair<String, String>` and `onShown: (() -> Unit)?`. Its existing `userId` and `chatId` stay.
   - On the WinRT path, `prepareIconPath` is not called: no avatar is decoded and no PNG is written. The icon is prepared only for the fallback.
   - On Windows:
     - Once `windowsToastsReady`, and while the toaster is not disabled, it hands the toast to `WindowsToast`. Otherwise it calls `displayViaTwoSlices` (today's two-slices code, renamed in place).
     - Both Windows paths apply `onShown.takeIf { BALLOON_SILENT }` before any two-slices call. `BALLOON_SILENT` comes from the sound plan's V1, so the balloon's sound and the app's sound never both play.
     - Other OSes are unchanged.
   - `displayNotification` passes `NotificationSound::play` as `onShown`, under the sound plan's conditions, which stay at the call site. On Linux and macOS, the sound then starts right after `toast()` inside the existing `withBGApi` block.
   - **Tokens** (Windows only, for toasts from `displayNotification` and `notifyCallInvitation`):
     - 16 `SecureRandom` bytes as lowercase hex, mapped to `(rhId, userId, chatId)` in a `synchronized` LinkedHashMap of the last 256 entries.
     - `showMessage` uses `simplexchat:/notification` (no token).
   - **Clearing**, only where Android clears:
     - `cancelNotificationsForUser(userId)` has no rhId, so it over-clears. It drops that userId's tokens and queues `RemoveGroupWithId` for `"local-<userId>"` and for `"<id>-<userId>"` for each id in `chatModel.remoteHosts`.
     - `cancelAllNotifications` empties the map and queues `ClearWithId`.
6. **The link handler**, desktop and **Windows only**. Linux, macOS, common `openAppLink` and Android are untouched.
   - `AppLinks.kt` `isNotificationLink(uri)` matches the whole string against `simplexchat:/notification` optionally followed by `/<32 lowercase hex>`, with the scheme case-insensitive. A query, a fragment or a different number of slashes doesn't match. It is checked only when `desktopPlatform.isWindows()`.
   - Cold start (`Main.kt:30`): a notification link is not stored. The app just starts; no token from an earlier run exists.
   - `openDesktopLink` sends notification links to `chat.simplex.common.model.NtfManager.openNotificationLink(token)`, a desktop-object function. It runs these steps in order:
     1. If the token is unknown or missing: `showWindow()`, then return.
     2. Evaluate `locked()`:
        - `appPrefs.performLA.get() && (AppLock.userAuthorized.value != true || (!simplexWindowState.windowFocused.value && (AppLock.enteredBackground.value == null || elapsedRealtime() - enteredBackground >= laLockDelay * 1000)))`.
        - It conservatively mirrors when `recheckAuthState` would lock, without calling it.
        - For an Action Center click, focus has already left the window, so the elapsed-time term applies.
     3. `showWindow()`.
     4. Return without opening anything if any of these holds:
        - `locked()`;
        - chat is not running, or onboarding is not complete;
        - `isBadgeLinkIssuing()`;
        - `rhId != null` and that host's `sessionState` is not `RemoteHostSessionState.Connected`.
     5. `ntfManager.openChatAction(userId, chatId, rhId, canOpen = { !locked() })`.
7. **Docs**:
   - `spec/services/notifications.md` §2: the signatures with `rhId`.
   - `spec/services/notifications.md` §4: the Windows path, DOM-only content, the retry, the AWT pin, tokens, AUMID, `Setting`, routing, clearing, and the known limits.
   - `spec/client/navigation.md`: the notification link path on Windows.

Not "feature off = unchanged":
- Every Windows user gets the new toaster and one more HKCU key on each start. The fallback is the two-slices AWT balloon, which today's users already see.
- Snarl/Growl users on Windows get the balloon instead of GNTP.
- Linux and macOS are unchanged.

## Known limits (documented in spec §4)

These are bounded by the 1 h expiry and, from Windows 10 1903, by reboot:
- Toast history follows Android:
  - Hiding the active profile doesn't clear its earlier toasts until the user leaves it.
  - Switching host, quitting or crashing while a hidden profile is active leaves its toasts.
  - So do `/hide` in the in-app terminal, and hiding or deleting a profile on the connected phone.
- An event serialised just before a hide commits can still show one toast.

These only raise the window instead of opening the chat:
- clicks on a toast from a disconnected host, or from an earlier run;
- clicks on a toast whose token left the 256-entry map.

Also:
- Two fast clicks on toasts from different hosts run two host switches at once (the existing TODO at `SimpleXAPI.kt:3820`).
- The lock timer can fire while `openChat` is loading, after the last `canOpen` check. The chat then opens under the lock screen, as an Android notification tap does.
- Removing or unpairing a remote host (`deleteRemoteHost`, `SimpleXAPI.kt:2709`) doesn't clear its toasts. Its message previews stay in Action Center for up to 1 h, and clicks on them only raise the window: host ids are never reused (AUTOINCREMENT).
- While the toaster is wedged, clears wait behind the stuck task, so toasts may stay in Action Center until it returns.
- Fallback balloons are not bounded by the expiry. They appear before registration completes, when registration fails, and when even the constant-text retry fails or the toaster is disabled. They aren't clickable, aren't cleared and aren't under the Windows switch. With `BALLOON_SILENT = false`, they may sound during recording, a call or a ringing call.
- If Windows rejects a toast asynchronously (the `Failed` event, which is not subscribed to), that toast is lost after its sound played. G1 records how rejections arrive.
- Toasts show no avatar. Text longer than the cut is truncated.

## Go/no-go tests (on Windows, before code review)

Run on the oldest supported Windows 10 build (1809 LTSC) and on Windows 11.
- **G1**, with only the registry AUMID, on a fresh Windows user:
  - Read `IToastNotifier.Setting` before any toast, exactly as the app will. An error here would send every toast to the fallback.
  - Check whether the toast and the Settings entry work with `DisplayName` alone (decides `IconUri`).
  - A `ToastText02` toast built through the DOM shows "SimpleX Chat" with its title and body, and persists in Action Center.
  - A click starts `SimpleX.exe` with the link.
  - SimpleX is listed in Settings → Notifications.
  - `RemoveGroupWithId` and `ClearWithId` work.
  - Expiry works (test with a short value). On 1903+, toasts are gone after a reboot.
  - Under Focus Assist, the Action Center click works.
  - With the AUMID key deleted while running, record what `Setting` and `Show` report.
  - Record whether a rejected toast fails at `Show` (HRESULT) or later (`Failed` event).
  - Measure the payload limit with maximal values of 4-byte code points, and of `&`/`<`. Size the cut from it.
  - Text nodes containing each of `</text><action …>`, `&`, `{title}`, NUL and control characters, lone surrogates, bidi overrides, and 100 KB strings: either shown literally (cut), or the constant-text retry is shown. Never an extra element, a changed link, an image or a balloon.
- If G1 fails, the fallback is a Start-menu shortcut carrying `System.AppUserModel.ID` and `ToastActivatorCLSID`. That becomes a new decision for you.

## Threat model

- **Injection.**
  - Peer-controlled text (names, message text) is passed only as the value of `CreateTextNode`, after a length cut. A DOM text node is character data: our code never parses it as markup and has no escaping function to get wrong.
  - Every attribute value and element is either a constant, app-generated hex, or digits.
  - The constant-text retry means peer text can't force a fallback.
  - The fallback is AWT's plain-text balloon, constructed directly. No other two-slices backend is constructed on Windows, so the unescaped GNTP backend can't be reached.
  - Logs never contain peer text from `WindowsToast`. On the fallback path, the existing catch logs the exception class, not its message: `ToasterException` messages include the title (`AWTToaster.java:136, 138`).
  - What remains is trust in Windows' own XML DOM, the same component every WinRT toast app relies on.
- **Link forgery** (web pages, local programs, writing `simplex.show`).
  - Tokens are unguessable and map only to chats the user was notified about, so a forged link at most raises the window.
  - Launch strings sit in the Windows notification database, readable by the same user, who can already do more.
- **Remote host.**
  - The token carries the core-assigned rhId. A click switches to that host and its profile.
  - A disconnected host's token only raises the window.
  - Routing is always by `(rhId, userId)`, because userIds collide across hosts.
  - A malicious mobile controls only its own users, chats and text, and its text only ever becomes text nodes.
- **App lock.** A click doesn't switch host or profile, close the passcode prompt or open a chat while the app is, or would be, locked. The lock is checked before the window is raised, and before each step that changes what is shown. The one residual gap is in the known limits.
- **Hidden profiles.** Android-parity clearing, a 1 h expiry, and expiry on reboot. The residual risks are in the known limits. The preview mode ("hide content") applies to every toast.
- **The Windows OS switch.** Respected for WinRT toasts. Fallback balloons are outside it (known limit).
- **Floods.** At most 32 toasts wait. Clears are queued in order and never dropped. A wedged toaster is detected and bypassed. Tokens also stay for toasts that are dropped by `Setting` or sent to the fallback; that is harmless, and they age out.
- **Registry.** HKCU, the same trust level as the existing scheme keys.
- **Native code.** A wrong vtable slot crashes the process. Mitigations: slots and QIs documented per call in one helper, one isolated thread, and G1 on two Windows versions. No `Native.setProtected`.

## Test plan

An MSI from fork CI, as for PR #7581 (a one-job workflow on a fork branch). You run these on Windows 10 and 11:
- Running app, window behind another app: clicking the popup brings the window forward and opens the chat. The same from Action Center. Note the delay.
- Minimized, and hidden in the tray: the same.
- App closed: an Action Center click starts the app.
- Locked:
  - With delay 0 and with 30 s, a click only raises the window. The lock screen stays, the prompt is not closed, and no items are marked read.
  - Focused and unlocked (delay 30 s): a popup click for another chat opens it, if the click doesn't take focus from the app (observe).
- Chat stopped: a click only raises the window.
- A background profile's chat: the profile switches.
- Mobile connected:
  - A mobile notification while local is shown switches to the mobile and opens the chat.
  - A local notification while the mobile is shown switches back.
  - With the mobile shown, a click for another profile on the mobile opens that profile there; the local profile is unchanged.
  - After disconnect, old mobile toasts only raise the window.
- Clearing:
  - Leaving a hidden active profile, hiding a non-active profile, or deleting a profile makes its toasts disappear at once.
  - Self-destruct and database delete leave Action Center empty, also in a run where registration failed.
- A bot sends hostile names and texts (the G1 list): shown literally (cut) or as the constant-text toast. Never altered, never a balloon.
- Snarl running (GNTP port 23053), with registration forced to fail: the fallback is a balloon. Snarl receives no connection.
- SimpleX turned off in Windows Settings: no toast, no balloon, no sound. SimpleX is listed there, and its switches survive a restart (#4201).
- Sound:
  - App sound off: Windows' sound per message toast, except during recording or a call.
  - Incoming call: only the in-app ring.
  - App sound on: exactly one sound, the app's, at most once per 3 s.
  - Fallback toasts never add a second sound.
- 100 messages in 1 s: the app stays responsive.
- Linux AppImage regression: notifications and links are unchanged.

## Decisions

- **D1. Approach.** DECIDED: WinRT toasts built through the DOM (option 1), gated by G1.
- **D2. Remote host.** DECIDED: a click switches to the toast's host and opens the chat there. `rhId` is passed through common `displayNotification`.
- **D3. Action buttons.** DECIDED: not in this change.
- **D4. Clearing or replacing a chat's toasts when it is opened.** DECIDED: a follow-up.
- **D5. macOS and Linux clicks.** DECIDED: a separate plan.
- **D6. Toast history.** DECIDED: Android-parity clearing, plus a 1 h expiry and expiry on reboot.
