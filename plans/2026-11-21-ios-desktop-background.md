# iOS: desktop session stays connected in background

## Problem

A desktop session with the iOS app ends when the app moves to background or the phone locks. The phone must stay unlocked with the app open while the desktop is used.

## Change

While a verified desktop session is connected:

- the app plays silence in a mixable audio session, so that iOS keeps the app running in background;
- the chat is not suspended in background;
- an indicator shows the session and offers Disconnect.

When the session ends for any reason, playback stops, the saved audio session settings are restored, the indicator is removed, and the chat is suspended as usual in background.

### Session states

```mermaid
stateDiagram-v2
  [*] --> Idle
  Idle --> KeptAlive: session connected, no active call
  Idle --> ScreenOn: session connected, active call
  KeptAlive --> Idle: session stopped, suspended or disconnected
  ScreenOn --> Idle: session stopped
  KeptAlive --> Idle: playback restart failed or media services reset
```

| State | Audio | Indicator | Screen | Background |
|---|---|---|---|---|
| Idle | session of the base | none | as on the base | chat suspended |
| KeptAlive | silence played | Live Activity or notification | locks normally | chat active |
| ScreenOn | the call's | none | kept on | as on the base |

### Keepalive

`RemoteCtrlKeepAlive` subscribes to `ChatModel.remoteCtrlSession` at launch and alone starts and stops the keepalive.

Audio session while KeptAlive:

| Setting | Value |
|---|---|
| category | `.playback` |
| mode | `.default` |
| options | `.mixWithOthers` |
| prefersNoInterruptionsFromSystemAlerts | `true` |

- Silence: a zero-filled 4,096-frame mono buffer at 44,100 Hz, looped by `AVAudioPlayerNode` with `.loops`.
- The previous category, mode, options and `prefersNoInterruptionsFromSystemAlerts` are saved at start and restored at stop.

Interruptions:

| Notification | Action |
|---|---|
| interruption `.ended` | playback restarted |
| interruption `.began`, reason `.appWasSuspended` | playback restarted |
| `AVAudioEngineConfigurationChange` | engine rebuilt on the main queue |
| media services reset | desktop disconnected |
| playback restart failed | desktop disconnected |

### Indicator

| iOS | Live Activities | Indicator | Disconnect |
|---|---|---|---|
| 15.0–16.0 | — | local notification | notification action |
| 16.1–16.x | enabled | Live Activity | tap opens the desktop screen |
| 17+ | enabled | Live Activity | `DisconnectDesktopIntent` button |
| 16.1+ | disabled, failed, ended by the system after 8 h, or dismissed | local notification | notification action |

- Extension target `SimpleX Live Activity`, bundle id `chat.simplex.app.SimpleX-Live-Activity`, deployment target 16.1.
- `RemoteCtrlActivityAttributes`:
  - `desktopName: String?`, `nil` when notification previews are hidden
  - `connectedAt: Date`
- `ActivityKit` and `AppIntents` are linked weakly by the app.
- At launch, activities and the notification left by a terminated process are ended.

### Disconnect

- `stopRemoteCtrlSession()` and `clearRemoteCtrlSession()` in SimpleXAPI.swift are called by the desktop screen, the Live Activity intent, the notification action and the keepalive.
- `_suspendChat` calls `clearRemoteCtrlSession()`, because `APISuspendChat` stops the remote session in the core (`cancelActiveRemoteCtrl Nothing`) without the event `remoteCtrlStopped`.

### Desktop screen

- The sheet stays undismissable while connected (`interactiveDismissDisabled`, as on the base).
- `ChatListView.onAppear` presents the desktop sheet while connected.
- `ConnectDesktopView.onDisappear` stops the session only while the scene is active.
- Footer while KeptAlive: "The app keeps running in the background until you disconnect."

## Tests

App fixtures (docs/contributing/TESTS.md, "App fixtures"):

- A remote session test records the events `remoteCtrlConnected` and `remoteCtrlStopped` of the mobile side with `eventsFixture` in `tests/fixtures/api/remote/`.
- `RemoteCtrlKeepAliveTests` in SimpleX Unit Tests passes the recorded events to `processReceivedMsg`, with the audio session, playback and indicator behind a protocol and replaced by a recorder:

| Case | Expected |
|---|---|
| connected event | keepalive started, indicator shown with the desktop name |
| connected event, previews hidden | indicator shown without the name |
| connected event, active call | keepalive not started, screen kept on |
| stopped event | playback stopped, settings restored, indicator ended |
| chat suspended while connected | session cleared, keepalive stopped |
| playback restart failure | `stopRemoteCtrlSession` requested |
| launch with an indicator left | indicator ended |

Checked on a device only:

- iOS keeps the app running while silence plays with the screen locked.
- Music from another app keeps playing and stays the Now Playing app.
- The Live Activity on iOS 16.1, 17 and 26, and the notification on iOS 15.

## Release

- An App ID and a provisioning profile for `chat.simplex.app.SimpleX-Live-Activity`.
- `scripts/ios/update-version.sh` updates 12 version lines, the new target included.
