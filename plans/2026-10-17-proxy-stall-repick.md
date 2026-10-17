# Private routing: "connecting…" stuck until restart when the chosen proxy fails

## Problem

With private routing, a contact can stay at "connecting…", or joining a link can fail on every "Retry", until the app is restarted. After the restart the connection completes in seconds.

Seen on 2026-10-14 with public servers: forwarding from smp4 to smp10 timed out for ~10 minutes, and connections whose agent had chosen smp4 as the proxy for smp10 stalled for the whole time.

## Cause

In simplexmq, the agent chooses one proxy for each destination relay and keeps it (`smpProxiedRelays`). It forgot the choice only when the proxy could not create a session to the relay, when the proxy connection closed, or when the agent stopped. Two failures kept it, so every retry went through the same proxy:

1. Connecting to the proxy fails (proxy offline or unreachable).
2. A forwarded command fails with `PROXY (BROKER e)` other than a host error, e.g. `PROXY (BROKER TIMEOUT)`: the proxy has a session to the relay but can't get an answer from it.

## Fix

simplexmq: forget the chosen proxy in both cases, so the next attempt chooses again. Details, timing and trade-offs: simplexmq `plans/2026-10-15-proxy-repick-on-failure.md` (simplexmq PR linked in this PR).

This repository only bumps simplexmq to that commit (`cabal.project`, `scripts/nix/sha256map.nix`). No app code changes; the app's network settings are unchanged.

Recovery with the default timeouts:
- Background delivery (e.g. sending the confirmation after the contact joins the app's link): the proxy reports the failure before the client's own timeout, and the next automatic retry chooses a proxy again.
- Joining a link (interactive): client timeouts ("Connection timeout") keep the proxy. The first or second "Retry" gets the proxy's report ("Private routing timeout"), and the Retry after it chooses a proxy again.

## Verification

Desktop AppImage built from this branch vs master, local servers: destination relay R, proxy P1 that reaches R through a SOCKS relay passing the TLS and SMP handshakes and then dropping all bytes (so session creation succeeds and every forwarded command times out at P1), good proxy P2; the app's message servers are only P1 and P2. Trials where the first pick was P1:
- The contact joins the app's 1-time link ("connecting…" in the app): connected with no user action after 33 s and 95 s (2/2). Master: not connected after 10 minutes (2/2).
- The app joins a link, tapping "Retry" after each error: connected after 2 to 4 Retries, 52 to 114 s (6/6). Master: not connected after 6 Retries (2/2).

The failed proxy is not excluded from the next pick, so with two proxies the next pick is P1 again about half of the time; that is what made the longer trials longer.

Android (Waydroid, core library built from this PR), fixed-outcome checks. With P1 as the only server, wait until P1 reports the failure. Then switch the servers to P2 only, without restarting the app, and try again on the same relay:
- the app joins a second link: connected in 3 s through P2;
- the app's background delivery after the contact joined its link: connected 5 s after the switch.

The master core keeps P1 for the relay in the same checks: the second join fails through P1, and the background delivery is still not connected 240 s after the switch.

Random-pick trials (P1 and P2 both configured) recover after a few Retries, because each re-pick has a 50% chance of choosing P1 again. In 2 early Android trials all 8 re-picks landed on P1 by chance and the joins stalled after 6 Retries. Later runs gave 5 of 9 on Android, and 7 of 13 in a desktop build that logged every pick.
