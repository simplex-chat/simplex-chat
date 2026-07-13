# iOS: keep remote desktop session alive in background

## Problem

An iOS remote desktop session depends on the mobile chat controller remaining
active. The normal background lifecycle suspends the controller and closes the
shared store, which also stops the desktop session. Users therefore have to keep
the iOS app open while controlling it from desktop.

iOS 26 provides `BGContinuedProcessingTask` for foreground, user-initiated work
that should continue after the app is backgrounded. Earlier versions only offer
the short window granted by `UIApplication.beginBackgroundTask`.

## Behavior

- **iOS 26:** submit a continued-processing request after successful desktop
  verification. The system controls its duration; the app shows no countdown or
  fixed limit.
- **iOS 26 rejection:** use the legacy background task when the app backgrounds
  rather than queueing a request that may start after the session is useful.
- **Pre-iOS 26:** use only the system-granted legacy background window.
- **No remote session:** preserve the existing suspension and background-refresh
  lifecycle.

The "Keep the app open to use it from desktop" footer remains visible when only
the legacy mechanism is available and is hidden when continued processing was
accepted.

## Implementation

Add a `@MainActor` `RemoteCtrlBGKeepAlive` singleton in `SuspendChat.swift`. It
owns the continued-processing task, the legacy task identifier, and all
expiration cleanup.

After `verifyRemoteCtrlSession` changes the session to `.connected`, register and
submit `BGContinuedProcessingTaskRequest` with `.fail`. Registration happens at
this foreground user action, which is supported for continued-processing tasks.
The request uses indeterminate progress because a desktop session has no honest
completion percentage or predetermined duration.

On background transition with an active remote session:

1. Start a legacy task only when continued processing is not active.
2. Keep the chat controller running instead of calling `suspendChat()`.
3. Skip `BGManager.schedule()` while the keepalive owns background execution.

On expiration, stop remote control, restore the local session, finish the system
task, and then run the normal suspension/background-refresh path if the app is
still backgrounded. Explicit disconnect and `remoteCtrlStopped` also finish the
keepalive. Foregrounding ends only the legacy task so the connected session can
continue.

Add the continued-processing identifier and `processing` mode to the iOS
Info.plist, and update the iOS product/spec documentation.

### Extension coordination

Do not add a global `activeRemoteCtrl` guard to `suspendChat()`. The share
extension uses `.sendingMessage` to make the main app release the shared database
before it writes. Keeping the exception in the scene-background path preserves
that handshake; the existing `.inactive` response reactivates chat afterward.

## Threat model

The change adds no pairing flow, command, or network input. It extends how long
an already verified desktop keeps its existing authority while the phone is
backgrounded or locked. A compromised paired desktop can therefore act for
longer, but gains no new permissions. Continued processing starts only after
foreground verification, and user disconnect, remote stop, system cancellation,
or expiration ends the keepalive.

## Risks and required fixes

- `beginBackgroundTask` may return `.invalid`. `startLegacyTask()` must report
  whether time was granted; otherwise the scene path can skip suspension without
  receiving an expiration callback. Denial should stop/restore the remote session
  or fall through to normal suspension.
- Expiration handlers have little cleanup time. Verify on a real device that
  awaiting remote teardown does not prevent task completion and store suspension.
- Confirm that the iOS 26 legacy fallback is intentional. It matches the current
  implementation, while the PR summary mentions legacy behavior only for
  pre-iOS 26.
- Continued processing may be rejected or expired at any time and does not
  guarantee an indefinite desktop session.

## Scope

- No background creation or reconnection of a desktop session.
- No remote-control protocol or core changes.
- No broad lifecycle refactor; repeated disconnect cleanup can be consolidated
  separately if another path needs it.

## Verification

- On an iOS 26 device, verify a session, background/lock the phone, and confirm
  desktop commands continue while the system activity is active.
- Exercise submission rejection and confirm the legacy fallback and footer.
- On pre-iOS 26, confirm the session lasts only for the granted legacy window.
- Confirm foregrounding, user disconnect, remote stop, and expiration each end
  the appropriate task exactly once and restore local state.
- Send text and an attachment from the share extension while connected; confirm
  database coordination and remote-session recovery.
- After expiration, confirm notification processing and ordinary background
  suspension still work.
