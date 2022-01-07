# SimpleX announces SimpleX chat v1 release, the world's most private and secure chat and application platform

We are building the most secure and private platform for building Internet applications. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat) is a chat application built on the SimpleX platform that also serves as an example and reference application.

## What's new in v1?

### Stable protocol implementation
All releases from v1 onwards will be forwards and backwards compatible. SimpleX Chat itself is still backwards compatible by supporting "legacy" contacts that should be migrated once both connected users upgrade the chat.

### Message encryption has been completely re-engineered to provide forward secrecy and break-in recover

SimpleX Chat v1 uses:

- double-ratchet E2E encryption using AES-256-GCM cipher with X3DH key exchange using 2 ephemeral Curve448 keys to derive secrets for ratchet initialization. These keys and secrets are separate for each contact, group membership and file transfer.
- in addition to double ratchet, there is a separate E2E encryption in each message queue with DH key exchange using Curve25519 and NaCl crypto-box - seperate E2E encryption has been added to avoid having any cipher-text in common between message queues withing a single contact (to prevent traffic correlation).
- additional encryption of messages delivered from servers to recipients, also using Curve25519 DH exchange and NaCl crypto-box - to avoid shared cipher-text in sent and received traffic (also to prevent traffic correlation).

### Improved user and server authentication and transport
SimpleX now uses ephemeral Ed448 keys to sign and verify client commands to the servers. As before, these keys are different per message queue and do not represent a user's identity.

Instead of ad-hoc encrypted transport we now use TLS 1.2+ limited to the most performant and secure cipher with forward secrecy (ECDHE-ECDSA-CHACHA20POLY1305-SHA256), Curve448 groups and Ed448 keys.

Server identity is validated as part of TLS handshake - the fingerprint of offline server certificate is used as a permanent server identity which is included in server address, preventing the possibility of MITM attack between clients and servers.

SimpleX also uses tls-unique channel binding in each signed client command to the server preventing the possibility of replay attacks.


### Changes in protocol encoding

We switched from inefficient text-based low level protocol encodings, that simplified early development, to space and performance efficient binary encoding.



## Team changes

We are delighted to announce that ... is joining SimpleX in a ... capacity. ... brings X years of experience in ... and will help SimpleX to grow/achieve/solve xyz problem.



## What is SimpleX?

We recognised that there is currently no messaging application which respects user privacy and guarantees metadata privacy -- _elevator pitch to a journalist on why SimpleX was developed._

...

Further detail on the design motivations and objectives is available [here](https://github.com/simplex-chat/simplex-chat/blob/blog/simplex.md).

SimpleX Chat client can be used in the terminal on all major desktop platforms (Windows/Mac/Linux) and also on Android devices with [Termux](https://github.com/termux).

SimpleX also allows people to host their own servers and own their own chat data. SimpleX servers are exceptionally lightweight and require a single process with the initial memory footprint of under 20 Mb, which grows grows as the server adds in-memory queues (even with 10,000 queues it uses less than 50Mb, not accounting for messages).



## We need your help!

Please download and try the chat...




