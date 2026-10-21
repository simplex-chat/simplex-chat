# Desktop: report network changes to the core

## Problem

Desktop never reported the network to the core (no desktop caller of `apiSetNetworkInfo`). With simplexmq #1927 the core reconnects on an online network event, so without reports a desktop whose network changed keeps a dead connection that has data in flight until the OS gives up on it (~15 minutes on Linux, ~95 s with the opt-in timeout of simplexmq #1926 enabled through #7655, except via a SOCKS proxy).

## Change

JVM has no network change notifications, so a daemon thread polls:

- Every 10 s (every 2 s while the core was last told offline) it connects a UDP socket to a documentation address for IPv4 and for IPv6, which sends nothing and gives the local address the routing table would use. IPv4 addresses are compared whole, IPv6 by the /64 prefix (temporary addresses rotate within the network). Link-local addresses are ignored.
- Without a route, the addresses of the interfaces that are up are compared instead, with a "no route" marker that is removed when the route returns (a network used via a proxy or a server in the local network is not offline). While the core has the network offline, the interfaces are listed every 10 s.
- It reports when the online state changes, or when an address that was in use at the last report or was added since is no longer in use (adding an address is not reported by itself); when the IPv6 address the connections were made from is deleted (looked up every 6th poll once it is no longer the one in use, as listing interfaces can be slow on Windows); and after a sleep that took much longer than it should (the computer was suspended). Offline is reported after the addresses are gone for a second, and never before the first online report was accepted, so a host that is never online (e.g. a network-less VM using a forwarded proxy) behaves as before.
- What was reported is reset when the chat controller is created; reports are made under the same lock as the restart, so a report of the observer that is stopping is either delivered and cleared, or not delivered.
- The platform hook `androidRestartNetworkObserver` is renamed `restartNetworkObserver`.

With the core pinned in the apps, an offline report makes the core hold its network work (sending, file transfers, pending commands and resubscribing) until the online report (at most 30 minutes per wait) and the status icon turns grey; an online report only ends that hold, and closing the existing connections on an online report needs simplexmq #1927.

## Alternatives

- Native notifications per OS (netlink, `NotifyIpInterfaceChange`, SystemConfiguration via JNA): immediate, but three native implementations.
- Polling all interface addresses: changes with every unrelated interface (VPN, docker, virtual adapters).
- Comparing full IPv6 addresses: a reconnect on every temporary-address rotation.
- Reporting nothing offline: no held sends, but no faster recovery after an outage either.

## Accepted limitations

- Changes are seen up to 10 s late (offline up to 11 s, including the 1 s confirmation; a deleted IPv6 address up to about a minute).
- Not seen: a switch between polls between two IPv4-only networks that give the same private address, a public IP change behind NAT, and an IPv6-only switch between interfaces in the same /64 while the old interface keeps its address.
- On a multipath default route whose source address alternates between polls, each alternation is reported as a change.
- A machine with an always-up virtual adapter is never reported offline (as before this change, the core keeps trying).
- Servers reachable only via loopback or link-local addresses are held while all other addresses are gone.
- A stall of the process or a forward wall-clock jump that makes a sleep end more than 25 s after it began counts as a suspend (one extra report).
- Not checked: the cost of listing interfaces on Windows, whether the UDP socket triggers a firewall prompt on Windows or macOS, and App Nap on macOS.

## Testing

Desktop AppImages in a network namespace with a dummy interface, master vs this change: one report at start; offline shown 3-6 s after the link goes down (grey status), online ~2 s after it returns; a message sent while offline is held and delivered ~2.5 s after the link returns (master, which is not told, delivers it at once because the test server is on loopback in the same namespace); a fresh start without a network does not report offline; an address change, a lost IPv6 prefix, a deleted IPv6 address that was in use (28 s after the deletion in one run), a returned route and a resume from a stopped process are each reported once; nothing for an added address or a rotated one while the old one is kept (seen for 23 s); one observer thread after a re-init; 6 polls a minute with no packets sent. The offline/online timings, the held message, the IPv6 deletion and rotation and the effect runs below were run on the final code; the other checks, Android included, on an earlier revision, before the later observer changes (the IPv6 deletion check, the 2 s polling and 10 s interface listing while offline, no offline confirmation while offline, skipping an interface that disappears). Android: the renamed hook still reports.

Effect, two network namespaces joined by a veth pair with a local SMP server, the app address changed: with the pinned core neither message was delivered within 300 s; with a test build of simplexmq #1926-#1929 both were delivered 10 s after the change.
