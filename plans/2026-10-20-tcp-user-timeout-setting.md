# TCP_USER_TIMEOUT setting

## Problem

A connection with unacknowledged data gets no TCP keep-alive probes, so the OS fails it only when it gives up retransmitting (~15 minutes on Linux). simplexmq #1926 adds an opt-in `unackedDataTimeout` to `KeepAliveOpts`: when set, the sockets that use keep-alive also set `TCP_USER_TIMEOUT` (`TCP_RXT_CONNDROPTIME` on Darwin, nothing on Windows, never via a SOCKS proxy) to the keep-alive detection time, 30 + 15 x 4 = 90 s with the default settings. This PR is the app setting for it.

## Change

- `KeepAliveOpts.unackedDataTimeout`, nullable on both platforms; off is `null`, so the network config sent with the setting off is the same as before (the field is omitted).
- A "TCP_USER_TIMEOUT" switch in Advanced network settings, under the other keep-alive options, greyed out when keep-alive is off.
- Stored next to the other keep-alive values: Android and desktop `NetworkTCPUnackedDataTimeout`, iOS app group defaults (also read by the notification service and share extensions).

Fixed during review: Android and desktop did not store the setting (it was lost on restart and when other settings resent the config), and with keep-alive on and the switch off the Android and desktop settings screen looked unsaved whenever it was opened, because it built `false` while the saved config had `null`; "Reset to defaults" was enabled even on an untouched screen with default settings, on all three platforms, because the screen (Kotlin) or the stored config (iOS) had `false` while the defaults had `null`. On iOS, `getNetCfg()` built `false` with the switch off, so the config sent also differed from master (from the code; iOS is not built here).

## Alternatives

- `false` as the off value, with a non-null Kotlin default: the screen would compare equal too, but the config sent with the setting off would differ from master.
- Hiding the switch on Windows or with a SOCKS proxy, where it has no effect: more UI conditions on three platforms; the other raw `TCP_*` rows are not hidden either.
- Behind developer tools: not done; it is off by default, and the neighbouring `TCP_*` rows are not behind developer tools either.

## Testing

Desktop and Android, master vs this change: the switch is present only with this change; an untouched screen has Save and Reset disabled on both; the switch is stored and survives a restart and a SOCKS proxy on/off; the network config has `"unackedDataTimeout":true` only when it is on; keep-alive settings, Reset to defaults and messaging after "Save and reconnect" are the same on both.

Effect (desktop, a test build with simplexmq #1926, a local SMP server in a network namespace whose replies were dropped from just before a message was sent): with the switch on, the app's connection failed ~90 s after its data went unacknowledged and the app reconnected once the replies were let through; with it off, the connection was still open 337 s after the message was sent.

## Open questions

- The setting has no effect until the simplexmq pin includes #1926.
- iOS is not built here: the Swift needs an Xcode build.
- On both master and this change, turning keep-alive off, saving, restarting and turning it on again shows the default keep-alive values instead of the saved ones (and, with this change, the TCP_USER_TIMEOUT switch off).
