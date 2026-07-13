# iOS: keep a remote desktop session alive in background

Upstream PR: [simplex-chat/simplex-chat#7234](https://github.com/simplex-chat/simplex-chat/pull/7234)

## Problem

An iOS remote desktop session depends on the mobile chat controller remaining
active. When the app enters the background, its normal lifecycle suspends the
controller and closes the shared store so notification and share extensions can
use it. That also stops an active desktop session, so the user has to keep the
iOS app open to continue using SimpleX from desktop.

iOS 26 adds `BGContinuedProcessingTask`, which can continue a foreground,
user-initiated activity after the app is backgrounded. Earlier iOS versions only
provide the short, system-granted `UIApplication` background-task window.

Apple's [continued-processing documentation](https://developer.apple.com/documentation/backgroundtasks/performing-long-running-tasks-on-ios-and-ipados)
requires foreground, user-initiated submission and permits `.fail` when the work
is only useful if it can start immediately. The
[WWDC25 background-tasks session](https://developer.apple.com/videos/play/wwdc2025/227/)
also confirms that these handlers may be registered dynamically when the user
expresses intent, and that the system monitors progress and may expire the task.

## Goals

- Start continued processing only after the user successfully verifies a
  desktop session in the foreground.
- On iOS 26, let the system control how long the session may continue; do not
  invent a countdown or fixed application limit.
- On pre-iOS 26, and when iOS 26 rejects immediate continued processing, use
  only the legacy background time granted by the system.
- End the remote session cleanly when background execution expires, then restore
  the existing chat suspension and background-refresh lifecycle.
- Preserve the share extension's database-ownership handshake while a desktop
  session is connected.

## Platform behavior

| State | Keepalive | User-facing behavior |
|---|---|---|
| iOS 26, continued-processing submission accepted | `BGContinuedProcessingTask` starts from successful verification and remains active across backgrounding | The system shows the continued activity; the in-app "Keep the app open" footer is hidden |
| iOS 26, registration or submission rejected | `UIApplication.beginBackgroundTask` starts when the app backgrounds | The session lasts only for the system-granted window; the footer remains visible |
| iOS 25 and earlier | `UIApplication.beginBackgroundTask` starts when the app backgrounds | The session lasts only for the system-granted window; the footer remains visible |
| No active remote session | Existing lifecycle is unchanged | The chat suspends and schedules background refresh normally |

The iOS 26 request uses the `.fail` submission strategy. A task that cannot run
immediately must fall back to the legacy path rather than being queued after the
desktop session is no longer useful. Progress is indeterminate because a remote
control session has no honest completion percentage or predetermined duration.

## Design

### Keepalive owner

Add a `@MainActor` `RemoteCtrlBGKeepAlive` singleton alongside the existing chat
suspension lifecycle. It owns all system-task state:

- one lazily registered iOS 26 continued-processing handler;
- the active `BGContinuedProcessingTask`, if the system launched it;
- one legacy `UIBackgroundTaskIdentifier`;
- whether continued processing was successfully submitted, used to prevent the
  legacy and continued mechanisms from overlapping.

The iOS 26 task identifier is declared under
`BGTaskSchedulerPermittedIdentifiers`, and the iOS target declares the
`processing` background mode. Dynamic registration is performed during the
foreground verification action, as permitted for continued-processing tasks.

### Start and scene lifecycle

Successful `verifyRemoteCtrlSession` first changes the model session to
`.connected`, then submits the iOS 26 continued-processing request. The
redundant `remoteCtrlConnected` event does not submit a request because it is not
an independent foreground user action.

When the app enters the background with an active remote session:

1. Start the legacy task only if continued processing is not in use.
2. Keep the main chat controller active for the remote session.
3. Skip the normal `suspendChat()` and `BGManager.schedule()` path while this
   keepalive owns execution.

When the app becomes active, end any legacy task. A successfully submitted iOS
26 continued-processing activity remains associated with the desktop session
until the session or task ends.

### Expiration and teardown

Both system expiration handlers converge on one cleanup path:

1. Ask the core to stop remote control.
2. Switch back to the local session when the UI still represents a connected
   remote session, or clear incomplete session state.
3. Complete/cancel the continued task and end the legacy task idempotently.
4. If the app is still in the background, suspend chat and schedule the existing
   background refresh.

Explicit user disconnect and the core's `remoteCtrlStopped` event also finish
the keepalive so no system task outlives its remote session.

### Share and notification extensions

Do not add a global `activeRemoteCtrl` guard to `suspendChat()`. The share
extension deliberately sends `.sendingMessage` to make the main app release the
shared database before the extension writes. Scoping the keepalive exception to
the app's background scene transition preserves that handshake; the extension
can suspend the controller temporarily and the existing `.inactive` response
reactivates it afterward.

While keepalive is active, the main app remains the chat-controller owner and
processes incoming activity. After expiration, normal suspension closes the
store so notification/background processing can resume under the existing
coordination rules.

## Threat model

This change adds no pairing method, remote-control command, or new network
input. It extends the time during which an already verified desktop retains its
existing authority after the phone is backgrounded or locked.

- A malicious or compromised paired desktop can continue issuing every command
  already allowed by that remote session during the system-granted background
  window. It gains no authority beyond the verified session, but the usable
  window is longer than before.
- Continued processing begins only from successful foreground verification, so
  a background event or unsolicited core event cannot create that extended
  authority by itself.
- The user can disconnect in the app or cancel the activity through the system
  UI. Remote-stop events and system expiration also revoke the keepalive and
  restore local/suspended state.
- Keeping the main store open longer must not permit concurrent extension
  access. The share-extension suspension handshake remains authoritative; task
  expiration restores the notification-extension-compatible suspended state.

## Scope and non-goals

- No guarantee of indefinite execution: iOS can reject, expire, throttle, or
  terminate either background mechanism.
- No background creation, reconnection, or recovery of a desktop session.
- No new remote-control protocol or core behavior.
- No countdown, artificial timeout, or fabricated progress percentage.
- No broad refactor of the existing remote-session lifecycle. Consolidating the
  repeated disconnect-and-restore logic can be a separate change if another
  lifecycle path needs it.

## Risks and trade-offs

- Continued-processing tasks are designed for user-initiated work that makes
  progress. A live remote session is intentionally indeterminate, so a real
  iOS 26 device must confirm that the system grants useful runtime and presents
  the activity appropriately.
- Expiration offers little cleanup time. Remote teardown currently completes
  before the system task is ended so the database and UI state are not left
  inconsistent, but a slow teardown increases termination risk.
- The legacy fallback is inherently short and unpredictable. Its purpose is a
  graceful limited window, not parity with iOS 26.
- Keepalive transitions are invoked from verification, scene lifecycle,
  explicit disconnect, and remote-stop handling. Each new remote-session end
  path must also finish the keepalive until lifecycle ownership is centralized.

## Required before merge

- Handle `UIApplication.beginBackgroundTask` returning `.invalid`. The current
  scene path skips normal suspension whenever a remote session is active, even
  if iOS grants no legacy background time and therefore supplies no expiration
  callback. `startLegacyTask()` should report whether it acquired a valid task;
  denial must immediately stop/restore the remote session or fall through to the
  normal suspension and background-refresh path.
- Confirm with the maintainer that using the legacy mechanism on iOS 26 after a
  continued-processing rejection is intentional. It is a sensible immediate
  fallback and is documented here, but the PR body currently mentions legacy
  execution only for pre-iOS 26.
- Smoke-test expiration on a real device. The handler awaits remote teardown
  before ending the system task; if that work can exceed the expiration budget,
  cleanup needs a bounded or synchronous cancellation path.

Rollback is limited to removing `RemoteCtrlBGKeepAlive` and its call sites,
removing the new background identifier/mode, and restoring the prior scene-phase
suspension and footer behavior.

## Verification

### iOS 26 real device

1. Verify a desktop session in the foreground and confirm the system continued
   activity appears with the localized title.
2. Background and lock the phone; confirm desktop commands continue without an
   app-defined timeout or countdown.
3. Return to the app; confirm the session remains connected and no legacy task
   is left active.
4. Disconnect from the phone, disconnect from desktop, and exercise a network
   stop; in each case confirm the system activity ends exactly once and the app
   returns to local state.
5. Force or wait for expiration; confirm remote control stops, the app suspends,
   and background refresh/notification processing resumes.
6. Exercise registration/submission rejection; confirm the footer remains and
   backgrounding uses the legacy task instead of queuing the continued request.

### iOS 25 or earlier

1. Connect and background the app; confirm the desktop remains usable only for
   the system-granted legacy window.
2. Foreground before expiration; confirm the legacy task ends without ending the
   remote session.
3. Allow expiration; confirm remote control is stopped and normal suspension is
   restored.

### Extension and regression coverage

- While connected to desktop and backgrounded, send text and an attachment from
  the share extension; confirm it does not hit the two-second database wait and
  the remote session recovers after the suspension handshake.
- After keepalive expiration, receive a notification and confirm the notification
  service extension can open the shared store.
- Confirm calls still use the existing CallKit suspension exception when no
  remote session is active.
- Confirm ordinary background/foreground transitions are unchanged with no
  remote session.
- Build the iOS app against an iOS 26 SDK and an iOS 15+ deployment target.

## Implementation surface

- `apps/ios/Shared/Model/SuspendChat.swift` — keepalive owner and expiration
  cleanup.
- `apps/ios/Shared/SimpleXApp.swift` — scene-phase integration.
- `apps/ios/Shared/Views/RemoteAccess/ConnectDesktopView.swift` — foreground
  submission, explicit teardown, and conditional footer.
- `apps/ios/Shared/Model/SimpleXAPI.swift` — remote-stop teardown.
- `apps/ios/SimpleX--iOS--Info.plist` — permitted identifier and processing mode.
- `apps/ios/product/` and `apps/ios/spec/` — product, architecture, and impact
  documentation.
