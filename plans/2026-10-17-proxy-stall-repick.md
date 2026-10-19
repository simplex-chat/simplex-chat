# Private routing: "connecting…" stuck until restart when the chosen proxy fails

## Problem

With private routing, a contact can stay at "connecting…", or joining a link can fail on every "Retry", until the app is restarted. After the restart the connection completes in seconds.

Seen on 2026-10-14 with public servers: forwarding from smp4 to smp10 timed out for ~10 minutes, and connections whose agent had chosen smp4 as the proxy for smp10 stalled for the whole time.

## Cause

In simplexmq, the agent chooses one proxy for each destination relay and keeps it (`smpProxiedRelays`). It forgot the choice only when the proxy could not create a session to the relay, when the proxy connection closed, or when the agent stopped. Two failures kept it, so every retry went through the same proxy:

1. Connecting to the proxy fails (proxy offline or unreachable).
2. A forwarded command fails with `PROXY (BROKER e)` other than a host error, e.g. `PROXY (BROKER TIMEOUT)`: the proxy has a session to the relay but can't get an answer from it.

## Fix

simplexmq: forget the chosen proxy when it keeps failing, so the next attempt chooses again. Details, timing and trade-offs: simplexmq `plans/2026-10-15-proxy-repick-on-failure.md` (simplexmq PR linked in this PR).

- Case 1: at once. Without a live connection the proxy normally has not received the destination, so choosing again adds no proxy that learns it.
- Case 2: when it keeps failing for 30 s. The first failure starts a failure streak; a forward that reaches the relay clears it; a failure at least 30 s after the start of the streak forgets the proxy. A single error, or a burst of errors (e.g. when the relay restarts), keeps the proxy.

This repository only bumps simplexmq to that commit (`cabal.project`, `scripts/nix/sha256map.nix`). No app code changes; the app's network settings are unchanged.

Recovery in case 2 with the default timeouts:
- Background delivery (e.g. sending the confirmation after the contact joins the app's link): the proxy reports the failure about 30 s after the send, and again about 32 s later; the second report forgets the proxy, and the next automatic retry chooses again, about a minute after the first send.
- Joining a link (interactive): client timeouts ("Connection timeout") don't count. The first or second "Retry" gets the proxy's report ("Private routing timeout"), the next Retry's report forgets the proxy, and the Retry after it chooses again.

The failed proxy is not excluded from the next pick, so with two proxies the next pick is the failed one again about half of the time, and recovery then takes another 30 s of failures.

## Verification

Desktop AppImage built from this branch vs master, local servers: destination relay R, proxy P1 that reaches R through a SOCKS relay passing the TLS and SMP handshakes and then dropping all bytes (so session creation succeeds and every forwarded command times out at P1), good proxy P2; the app's message servers are only P1 and P2. Trials where the first pick was P1:
- The contact joins the app's 1-time link ("connecting…" in the app): connected with no user action after 64, 64, 129 and 192 s (4/4; the longer ones picked P1 again after forgetting it). Master: not connected after 10 minutes (2/2).
- The app joins a link, tapping "Retry" after each error: connected after 3, 4 and 6 Retries (86, 122 and 177 s), and once after 61 s with no Retry, when a background send picked again (4/4). Master: not connected after 6 Retries (2/2).

Android: needs testing with the core library built from this PR. (The previous version of this PR, which forgot the proxy on the first failure, passed fixed-outcome checks on Android with a library built from it.)
