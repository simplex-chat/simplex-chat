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

- `[4]` Version range encoding
- `[1]` 'A' (Announce)
- `[8]` systemSeconds of SystemTime encoding - does not change within session.
- `[2]` Announce counter.
- `[6]` Service address (host and port).
- `[1 + 32]` SHA256 fingerprint of CA certificate used to issue per-session TLS certificates.
- `[1 + ?]` X25519 DH key encoding to agree for per-session encryption inside TLS (hello received in response to hello from controller will contain host DH key).
- `[1 + ?]` Ed25519 public key used to sign datagram (the host also will receive it in the QR code, it should match this one).
- `[1 + ?]` Ed25519 signature signing the previous bytes.

"Encoding" here means Encoding class.

That gives ~250 bytes (TBC) that is well under typical MTU of ~1500.

A site-local multicast group `224.0.0.251` is used to announce services on port `5227`.

> The same group and port are used in mDNS (AKA bonjour/avahi/dns-sd/zeroconf) so we expect it to run with most home/SOHO access points without further configuration, although using a different port can make it ineffective.

### OOB data

Announcer MUST include:
- ED25519 public key used to sign announce datagrams in its OOB link/QR code (also included in datagram, so they can be validated before scanning QR code).
- the CA certificate fingerprint (also included in datagram).
- device name for better initial contact UX.

### Discovery announcer

> announcer is run before the controller service.
>
> Multiple announcers can send to the same group/port simultaneously.

A typical announce interval is 1 second to balance UI responsiveness with network traffic.

Announcer MUST first discover its own address and validate with the list of local network interfaces.

To discover it's address it will send a datagram with this format:

- `[4]` Version range encoding
- `[1]` 'I' (Identify)
- `[1 + 32]` Random number.

Announcer MUST NOT announce a service for a different host.

### Implementation

```
ChatController {
    ...
    multicastSubscribers :: TMVar Int
    ...
}
```

Controller/host connection steps:

1. take multicastSubscribers, if 0 subscribe to multicast group
2. increase counter and put it back.
3. send SXC0 datagrams to discover own address.
4. when received, match address to network interfaces, fail if no match after a few datagrams.
5. get free port for TCP session.
6. generate DH key for session.
7. prepare and start sending signed SXC1 datagrams.
8. when host connects to the address in the announcement, stop sending datagrams.
9. take multicastSubscribers, if 1 unsubscribe from multicast group
10. put back min(0, cnt - 1).
11. send Hello to host.
12. get Hello from host with DH key, compute shared secret to be used for remaining  commands and responses.

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

Listener MUST verify datagram signature against the key it got in datagram.

Listener MUST verify that the address in the announcement matches the source address of the datagram.

During the first connection to the new controller:

OOB must have the same:
- Ed25519 key used to sign datagrams.
- CA cert fingerprint.

During the subsequent connections, these keys and CA cert fingerprint in the datagram mush match the record.

### Service (TCP client)

> TBD: This assumes always-on listener.

A TCP client will use STM to wait for expected service tag and key to appear in discovery table to get its address.

E.g. a remote host on a mobile will wait for the remote profile service with a key fingerprint from OOB.

### Finding own address with multicast

An host with a multicast entitlement may use it to find its own address.
Receiving your own datagram would reveal source address just as it is used in (unauthenticated) discovery tests.

The same multicast group `224.0.0.251` is used to send "mirror" datagrams on port `5227`.
