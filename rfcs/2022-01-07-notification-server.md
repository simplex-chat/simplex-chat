# SimpleX Notification Server for SimpleX Chat

## Background and motivation

SimpleX Chat clients should receive message notifications when not being online and/or subscribed to SMP servers.

To avoid revealing identities of clients directly to SMP servers via any kind of push notification tokens, a new party called SimpleX Notification Server is introduced to act as a service for subscribing to SMP server queue notifications on behalf of clients and sending push notifications to them.

## Proposal

Communication between SimpleX Chat clients and SimpleX Notification Servers is carried out using SMP protocols (via SMP agents), which provides stronger security guarantees out of the box compared to HTTPS. Communication between SimpleX Notification Servers and SMP servers is also carried out using SMP protocol, with SimpleX Notification Server using an SMP client internally.

Before establishing message notifications:

1. SimpleX Notification Server creates a permanent address via SMP protocol.

2. This address is supplied to SimpleX Chat client via configuration or options, or baked in.

3. If SimpleX Chat client enables message notifications (for all or given chat connections) it establishes connection(s) to SimpleX Notification Server(s).

   TBC:

   - establish connection(s) to Notification Server(s) preemptively or on-demand;
   - one connection per Notification Server or per chat connection (probably the latter doesn't make sense);
   - to one or many Notification Servers.

Order of communication to establish message notifications goes as follows:

1. SimpleX Chat client requests SMP server to establish notifications for a queue using NKEY command containing a public key for authentication of NSUB command.
2. SMP server replies to SimpleX Chat client with NID response containing the queue's ID to be used for NSUB command.
3. SimpleX Chat client requests SimpleX Notification Server to subscribe to message notifications for the queue by sending `s.post` message (see `2022-01-07-simplex-services.md` rfc) to the [previously ?] established connection with Notification Server. The message contains SMP server address, notifier ID from SMP server's NID response, notifier private key (public key was provided to SMP server in NKEY command) and some kind of push notification token.
4. SimpleX Notification Server sends NSUB command to the SMP server containing notifier ID and signed with notifier key.
5. SMP server responds to SimpleX Notification Server with OK or NMSG if messages are available. After that SMP server sends NMSG notifications to SimpleX Notification Server for new available message. TBC - for all messages? some heuristics?
6. SimpleX Notification Server sends `s.ok` (`s.resp.ok`?) message to SimpleX Chat client signaling it has subscribed to notifications.
7. SimpleX Notification Server sends push notifications to SimpleX Chat client on new NMSG notifications from SMP server via provided push notification token.

## Implementation plan

https://github.com/simplex-chat/simplexmq/pull/314

Make agent work with postgres:
- fix issue with writing binary data
- make all tests pass with postgres
- revise instances, error handling, transaction settings
- class abstracting `execute`, `query`, `Only`, error handling, probably more
- separate migration logic
  - Q duplicate? (as now)
  - Q or reuse store methods + store method for `exec`
- parameterize agent to run either on sqlite or postgres
- \* move postgres instances to separate package to avoid compiling for mobile app
- make tests run for both sqlite and postgres

Protocol design:
- service protocol
- notifications sub-protocol

Push notifications:
- investigate sending push notifications to ios, (*) android
- notification server code running with postgres agent
  - communication with smp servers
  - communication with clients
  - notification specific store
    - Q same database? same server different database?
    - Q how to reuse database code? notification specific methods? -> should be unavailable to sqlite agent
  - Q other areas?
- notification server deployed
  - Q deployed where and how (probably a script similar to linode using systemd)
    - \* should be built on github to be downloaded as binary, can be shortcutted
  - Q spinning up postgres - postgres server, db, settings (password?)
  - Q transport - will self signed certificates work for push notifications? or will CA signed certificates be required?
- notifications at client
  - client to request notifications from notification server, (*) parameterized
  - store
  - client to wake up and selectively subscribe to smp server, then notify
  - api for wake up, api for response
    - Q will separarate c binding be needed?
  - logic in swift, (*) kotlin
- test & fix
