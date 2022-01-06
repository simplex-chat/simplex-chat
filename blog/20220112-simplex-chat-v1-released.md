# SimpleX Chat v1 released!

## What is SimpleX?

SimpleX as a whole is designed to be the most secure and private platform for building Internet applications. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat) is one such application that also serves as an example and reference application.

You can read the design motivation and objectives in SimpleX overview document.

Anybody can host SimpleX servers using our software - they are exceptionally lightweight and require a single process with the initial memory footprint of under 20 Mb. It grows as the server adds in-memory queues and depending on how many messages are not delivered, but even with 10,000 queues it uses less than 50Mb (not accounting for messages).

The chat client we developed can be used in the terminal on all major desktop platforms (Windows/Mac/Linux) and also on Android devices with termux.

With all the major changes in v1, the chat client still supports your existing contacts and groups, but they have to be migrated semi-automatically, once you and your contact upgrade the chat, to get the benefit of all the changes - better encryption, more performant message delivery, shorter invitation links and other improvements and fixes.

## Team changes

Since we have started working on SimpleX full time, we had some great people joining us...

...

## We need your help!

Please download and try the chat...

## Changes in SimpleX Chat terminal app v1.

After more than a year of development, we've reached stable protocol implementation that will allow our chat applications to be backward- and forward- compatible for all releases starting from v1.

SimpleX Chat itself is still backwards compatible by supporting "legacy" contacts that should be migrated once both connected users upgrade the chat.

### Changes in encryption.

Message encryption was completely re-engineered to provide forward secrecy and break-in recovery. SimpleX Chat v1 uses:
  - double-ratchet E2E encryption using AES-256-GCM cipher with X3DH key exchange using 2 ephemeral Curve448 keys to derive secrets for ratchet initialization. These keys and secrets are separate for each contact, group membership and file transfer.
  - in addition to double ratchet, there is a separate E2E encryption in each message queue with DH key exchange using Curve25519 and NaCl crypto-box - it is added to avoid having any cipher-text in common between message queues withing a single contact (to prevent traffic correlation).
  - additional encryption of messages delivered from servers to recipients, also using Curve25519 DH exchange and NaCl crypto-box - to avoid shared cipher-text in sent and received traffic (also to prevent traffic correlation).

### Changes in user and server authentication and transport

We now use ephemeral Ed448 keys to sign and verify client commands to the servers. As before, these keys are different per each message queue and do not represent a user's identity.

Instead of ad-hoc encrypted transport we now use TLS 1.2+ limited to the most performant and secure cipher with forward secrecy (ECDHE-ECDSA-CHACHA20POLY1305-SHA256), Curve448 groups and Ed448 keys.

Server identity is validated as part of TLS handshake - the fingerprint of offline server certificate is used as a permanent server identity which is included in server address, preventing the possibility of MITM attack between clients and servers.

We also use tls-unique channel binding in each signed client command to the server preventing the possibility of replay attacks.

### Changes in protocol encoding

We switched from inefficient text-based low level protocol encodings, that simplified the initial development, to space and performance efficient binary encoding.
