# Report a network change when the network identity changes

## Problem

With simplexmq #1927 the core closes and reopens its server connections on every online network event. The observers deduplicated on `UserNetworkInfo(networkType, online)` only, so a change of network that keeps the type and has no offline or lost event in between (an address replaced on the same network, or a switch to another Wi-Fi network or a cellular reattach without the previous network going offline first) was not reported. Connections that are dead then fail only by the socket timeout (measured on Linux: ~95 s with the opt-in timeout of simplexmq #1926 enabled through #7655, except via a SOCKS proxy, ~15 min otherwise). Android's netd closes the sockets of an address once it is on no interface (AOSP `NetlinkHandler` / `SockDiag::destroySockets`, not checked on a device), so there a removed address fails its connections at once and the report mainly lets the core reconnect without waiting out its retry intervals (with simplexmq #1929).

Base defects fixed on the way, observable without #1927:

1. After a re-init in the same process (database deleted, imported, migrated, self-destruct), the new chat controller was not told the network if it did not change (Android told it only when there was no active network), so on cellular the core used the fast timeouts. Both observers now reset what was reported when the chat is initialized.
2. A failed report was not repeated until the network info changed: it is now repeated on a later network event.
3. Android held a 180 s wake lock for every reported change or loss, also when nothing was sent: it is now released when the report finishes, unless an online report was sent.

## Change

- Android: tracks the `Network` handle and its addresses (`onLinkPropertiesChanged`); removed addresses are counted rather than compared, as an address removed and assigned again also leaves the connections via it dead. Reports are serialized by a mutex and deduplicated against what the core accepted; reports for a replaced controller are not recorded (generation counter). The loss of a network other than the last one reported online is not reported, in case a device reports the loss of the replaced network after the switch (not observed; Android documents that the default network callback does not).
- iOS: compares the interface the path uses, its gateways and the interface's addresses (`getifaddrs`), as `NWPath` has no network identity.
- Both: the device's link-local addresses are ignored (not used for connections to servers on the internet; iOS still compares the gateways, which for IPv6 are usually link-local); unique local IPv6 addresses are counted, as a VPN tunnel can use them.
- Android: an online event for the same network, with no address removed, within 3 s of an offline one that was not sent is not sent, as the core never saw offline; with #1927 it would cause a reconnect (see Accepted limitations).
- Android: `apiSetNetworkInfo` sends without the error alert, as failed reports are repeated.

## Alternatives

- The core decides per socket (local address no longer on any interface): most precise, but a redesign of the core stack (Android's netd already does this for removed addresses).
- An opaque network identity in `UserNetworkInfo`: an API change, and blind to an address removed and re-assigned between events.
- A snapshot comparison instead of the removal counter: smaller, same blind spot, bounded by #1926 when its timeout is enabled (#7655).
- Sending synchronously on the Android callback thread: simpler, but blocks it on the core.
- iOS without `getifaddrs`: misses an address replaced on the same interface and gateway.

## Accepted limitations

- An online report after a loss of validation shorter than 3 s is not sent, as the core never saw offline (e.g. a router WAN re-dial with a new public IP waits for the socket timeouts).
- One extra report (two if an address is replaced) when Android's bundled link-properties snapshot is older than the addresses read when the network became current.
- A reconnect when an IPv6 temporary address expires (about daily) or a unique local address is removed.
- iOS cannot tell apart two Wi-Fi networks with the same gateway and IPv4 address and no IPv6, without an update in between; whether `NWPathMonitor` reports address-only changes needs a device check.
- When a network that was never validated is lost while another one is the last reported online, the loss is not reported; the network type shown can stay that network's instead of none.
- With no chat controller (after "Delete database"), once a network change was reported and failed, Android attempts the report again on each capabilities event until the chat is started.
- The temporary controller of migration to a device (`chatInitControllerRemovingDatabases`) gets no report when it is created, as before (later changes are reported to it).

## Testing

Android (Waydroid), #7650 vs this change: a re-init sends `/_network info` to the new controller only with this change; removing an IP address from the network sends it only with this change (once also when it is added and removed within a second); offline for more than 3 s then online is reported on both; offline and online within 3 s re-sends online only on #7650. On the final code (link-local addresses ignored) the re-init check and an address removal (added and removed at least 3 s apart) were repeated, and removing a link-local address sends nothing. Regression on both: network status in "Network & servers", console `cmdOk`, messaging; desktop launch and messaging. The wake lock check was inconclusive. The reconnect itself needs a core with simplexmq #1927.
