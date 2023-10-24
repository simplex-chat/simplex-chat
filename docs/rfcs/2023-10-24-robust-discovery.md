# Robust discovery

## Problem

Remote session protocol has the "discovery" phase where mobile and desktop try to find each other.

Given how easy it is to spoof UDP datagrams extra care should be taken to avoid unauthenticated data.
In the tech spike for remote sessions, a discovery datagram contains only a TLS key fingerprint.
While this is enough to operate in a safe environment, the datagram itself should be authenticated.

Using link-local broadcast address of `255.255.255.255` is problematic on MacOS.

The initial implementation effort shown that discovery process better be running as a stand-alone service.
Additionally, it is desirable to run multiple service announcers in parallel from a single process.
Each announced service may be a remote controller assigned to a different remote host device, or some other site-local service entirely.

We still want to avoid system interface enumeration due to guesswork involved in filtering them and extra permissions/entitlements required on mobile devices.

## Solution

* An OOB data is extended with a public key to authenticate datagrams.
* A datagram is extended with MAC envelope, service address and its tag.
* A site-local multicast group is used for announcement.
* Additional site-local multicast group is used by announcer to find its own public LAN address.

### Datagram

- `[8]` Nonce (*TBD* timestamp or random). Used to prevent replay attacks (together with the counter).
- `[2]` Announce counter.
- `[6]` Service address (host and port).
- `[8]` Service tag. Used to filter announcements by service type and/or version. *TBD*: something like `REMPROF1` may be used to mark the remote profile connections.
- `[32]` SHA256 fingerprint of CA key used to issue per-session TLS certificates. *TBD*: other site-local services may use something else.
- `[32]` Datagram authentication tag.

That gives 64+24 bytes that is well under typical MTU of ~1500.

A site-local multicast group `224.0.0.251` is used to announce services on port `5226`.

> The same group is used in mDNS (AKA bonjour/avahi/dns-sd/zeroconf) so we expect it to run with most home/SOHO access points without further configuration.
> The port is different though, not to interfere with the real mDNS activities, so this may thwart the effort.
> A future versions may include configuration options to use different group and/or port.

### OOB data

Announcer MUST include the public key used to sign announce datagrams in its OOB link/QR code.

Announcer SHOULD include its device name for better initial contact UX.

Announcer MAY include the same key fingerprint that would be announced to pre-register remote controller.

### Discovery announcer

> TBD: announcer may function as a singleton service or be spawned per-service and function independently.
>
> A singleton service will maintain a list of active services, with a registry-like API.
> It automatically starts to send datagrams when there's anything to announce.
>
> An independent announcer will be started and stopped directly by a service that wants to be discovered.
> Multiple announcers can send to the same group/port simultaneously.

A typical announce interval is 1 second to balance UI responsiveness with network traffic.

Announcer MUST first discover its own address.

Announcer MUST NOT announce a service for a different host.

### Service (TCP server)

A service submits its port/tag/payload to announcer and cancels it when a client connection is established or the service is shut down.

A service SHOULD use system-provided dynamic port (i.e. bind to port `0`) to avoid getting "address in use" errors due to multiple service instances running or another/system service running on a designated port.

### Discovery listener

> TBD: A listener is most certainly a singleton service. But what would its lifetime be?
> We can run it continously for a snappier discovery and no-brainer client API.
> Or we can run it on-demand, registering there requests for discovery.

An active listener service receives datagrams and maintains a discovery table mapping service tags and keys to source addresses.
A service key is derived from the payload, which MAY be used as-is.
Source address contains both host and port.

Listener MUST verify datagram signature against the key it got OOB.

Listener MUST check that source address matches the announce.

### Service (TCP client)

> TBD: This assumes always-on listener.

A TCP client will use STM to wait for expected service tag and key to appear in discovery table to get its address.

E.g. a remote host on a mobile will wait for the remote profile service with a key fingerprint from OOB.

### Finding own address with multicast

An host with a multicast entitlement may use it to find its own address.
Receiving your own datagram would reveal source address just as it is used in (unauthenticated) discovery tests.

A different site-local multicast group `224.0.0.151` is used to send "mirror" datagrams on port `6226`.
This is to prevent changing discovery group membership of the process.
