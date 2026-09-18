Revision 1, 2026-09-18

# SimpleX Supporter Badges

## Table of contents

- [Introduction](#introduction)
  - [What is a badge](#what-is-a-badge)
  - [What a badge grants](#what-a-badge-grants)
  - [What a badge discloses](#what-a-badge-discloses)
  - [Comparison with Signal badges](#comparison-with-signal-badges)
  - [Non-goals](#non-goals)
- [Architecture](#architecture)
  - [Participants](#participants)
  - [Service requests](#service-requests)
  - [Buying and redeeming](#buying-and-redeeming)
  - [Monthly issuance](#monthly-issuance)
  - [Presentation](#presentation)
  - [Servers](#servers)
- [Cryptographic primitives](#cryptographic-primitives)
- [Security](#security)
  - [Design objectives](#design-objectives)
  - [Threat model](#threat-model)
  - [Current gaps](#current-gaps)
- [Future work](#future-work)
- [Conclusion](#conclusion)


## Introduction

The goal is for SimpleX Chat to be partly funded by its users, who buy supporter badges. A badge is shown on the profile of the person who bought it, and it raises the limits that other users and servers apply to that person's files. This document describes what a badge is, what it grants, how it is issued and presented, and what it discloses about the holder.

In an ordinary messenger a paid feature is attached to an account, and the operator sees in one record who paid and what they did with the feature. The SimpleX network has no accounts, so there is nothing to attach a purchase to, and introducing an account for this purpose would undo what the rest of the design achieves. A badge is therefore designed so that the purchase, which the badge service necessarily records, cannot be connected with the profile, the conversations or the servers of the person who made it.

### What is a badge

A badge is a credential issued by the badge service, a bot operated by SimpleX Chat. The credential is a [BBS signature](https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/) over four values: a secret master key, which the app generates and keeps; the expiry date; the badge type, which is either `supporter` or `legend`; and a reserved field that is currently empty.

The holder of a BBS signature can prove that the issuer signed certain values without revealing the signature itself, and can choose which of the signed values to disclose. Every such proof is different from every other, so two proofs cannot be recognised as coming from the same signature. The credential is therefore never sent to anyone. When the badge is to be shown or used, the app generates a proof that discloses the badge type and the expiry date and nothing else. Proofs from one credential cannot be linked to each other or to the credential, by the issuer or by anyone else, and each proof is generated for one context, whether a conversation, a file or a session with a server, and is not accepted in any other.

The transport protocols of the network call the same credential an entitlement: a name, an expiry and an extra string, disclosed in a proof. The chat application puts the badge type into the name, so a server that verifies an entitlement and a contact who sees a badge are looking at different zero-knowledge proofs of the same credential.

### What a badge grants

A badge is shown on the profile to contacts, to the members of groups, and to the subscribers of channels.

A badge allows larger files. The default limit is 1GB; a supporter can send files up to 2GB, and a legend up to 5GB. The limit is applied by the recipient, whose app verifies the sender's proof before it accepts a file above the default size.

A badge extends the storage of files on servers. An XFTP server stores a file for 48 hours by default. It may be configured to store files for longer when they are uploaded by the holder of a badge of a given type, and in the reference configuration the files of supporters are stored for 7 days and the files of legends for 21 days.

Two other uses are planned. A backup will be a link whose content the app replaces each time it makes a new backup, so that the same link always restores the latest state; a badge will raise the size of the backup and the time it is stored, and the storage time is then the period the app can remain offline before the backup is lost. Servers that verify the proof will apply higher rate limits when creating messaging queues, file chunks and notification tokens. Both are described under [Future work](#future-work).

### What a badge discloses

The badge service holds the purchase key, the number of months bought, and the payment record of the purchase. A card payment is linked to the purchase, as in any other service; a user who wants the payment itself to be private may pay with XMR. In either case the record is held by the service alone, and the service does not learn where the badge is later shown or used, because a zero-knowledge proof contains nothing that refers back to the credential or the purchase.

A party that verifies a proof learns the badge type and the expiry date. Since all credentials issued in the same week have the same expiry date, these two values place the holder among the supporters of that week. Two presentations of the same badge cannot be linked: a server that has verified a proof in one session has no way to tell whether a later session belongs to the same person, and a contact who sees a badge in one conversation has no way to tell whether a badge in another belongs to the same person.

### Comparison with Signal badges

Signal sells donation badges using [receipt credentials](https://eprint.iacr.org/2019/1416.pdf). The client obtains a credential against a payment without revealing its account, and then presents the credential once to the account server, which records its serial number, level and expiry against the account and attaches the badge to the account. The payment is not linked to the account cryptographically. The same mechanism is used for Signal's paid backups.

The differences follow from Signal having accounts and SimpleX not having them. Signal's server holds the list of accounts that have badges; a SimpleX server verifies a proof for one session and cannot link that session to any other. Signal's credential can be verified only by the server that issued it, whereas a SimpleX credential can be verified against the issuer's public key by contacts, channel relays and independently operated servers. Signal's credential is presented once, after which the badge is a property of the account; a SimpleX credential is presented every time the badge is shown or used, with a new proof each time. Signal's client requests the credential and redeems it in a single sequence of jobs without delay, so the server can match the request made under the payment with the redemption made under the account by their times and by network address, and the expiry of a Signal credential is rounded up to the next day; in SimpleX the request for a renewed credential and the profile update that presents it are made on different days, and the expiry is shared by all credentials issued in a week.

| Property | Signal badges | SimpleX badges |
|---|---|---|
| Account required | Yes | No |
| Operator holds the list of badge holders | Yes | No |
| Verified by | The issuing server | Any party with the issuer's public key |
| Presentations per credential | One | Unlimited |
| Proof bound to the context of presentation | No | Yes |
| Credentials with the same expiry | Issued in one day | Issued in one week |
| Issuance and presentation on different days | No | Yes |
| Usable on independently operated servers | No | Yes |

### Non-goals

A badge does not restrict anything that is available today: the defaults are unchanged, and a badge only raises them. It does not create an identity: it has no persistent identifier, it is not linked across conversations, and an incognito profile does not show it. It cannot be transferred: a code can be redeemed once, and the credential obtained with it is usable only with the master key it was issued for. It does not exempt its holder from the limits a server applies: since a credential can be presented in any number of unlinkable sessions, a badge lowers the cost of a resource without removing the limit on it. And it cannot be revoked; instead, credentials are issued for one month at a time.

## Architecture

```
   ┌─────────────────┐   code    ┌──────────────┐  credential   ┌──────────┐
   │  Payment (web)  │ ────────> │   Holder     │ <──────────── │  Issuer  │
   └─────────────────┘           │  (the app)   │  every month  └──────────┘
                                 └──────┬───────┘
                                        │  a new proof each time
              ┌─────────────────────────┼───────────────────────┐
              │                         │                       │
         ┌────▼─────┐             ┌─────▼──────┐          ┌─────▼──────┐
         │ Contacts,│             │ File       │          │ The user's │
         │ groups,  │             │ recipients │          │ own servers│
         │ channels │             └────────────┘          └────────────┘
         └──────────┘
```

### Participants

The issuer is the badge service, a bot on the SimpleX network with a contact address. It holds the secret key with which credentials are signed. Apps and servers hold a list of eight issuer public keys, and every credential includes the index of the key that signed it, so the service can move to the next key without a release of apps or servers.

The holder is the app. It keeps the credential in the user's profile, generates proofs on the device, and renews the credential every month by means of a background task.

The verifiers are contacts, group members, channel relays, the recipients of files, and the user's own servers. A verifier holds the issuer public keys and nothing else about badges.

### Service requests

The app and the badge service communicate through one-off service requests, a primitive of the SimpleX agent described in [One-off requests to service addresses](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2026-07-11-service-rpc.md). Although the service has a contact address, it is not a contact of the app: no connection is established, no conversation exists, and nothing is kept on either side once a request has been answered.

A request is a single message to the service's address. The app establishes a double ratchet, with post-quantum key agreement, from the keys published in the service's address link, encrypts the request with double ratchet, and sends it to the address queue on the service's server in the same way as any message to a server chosen by another party, that is, through a proxy server when private routing is enabled (which is the default). With the request the app sends the address of a reply queue that it created for this request on one of its own servers. The service decrypts the request with a receiving ratchet initialised from its private keys, sends the reply to the reply queue under the same ratchet, and deletes its state; the app deletes the reply queue and the ratchet when it has the reply or when the request times out.

Compared with a chat connection, this has the following advantages. The app never connects to the service, nor the service to the app; each communicates only with SMP servers, and the service's server does not see the app's network address. Two requests from the same app cannot be linked by the service or by servers, since each uses fresh keys and a fresh reply queue and leaves nothing behind. A reply that decrypts proves that it came from the holder of the keys committed by the service's link, so a server cannot substitute a reply. And the badge service answers a repeated request with the same result and executes nothing twice, so the app can repeat a request whose reply was lost.

A purchase is identified by an Ed25519 key pair that the app generates for it and uses for nothing else. Every request concerning the purchase, the redemption and each monthly renewal, is signed with this key, and the signature covers the request together with a value derived from the ratchet of that exchange, so it is valid for that exchange only and cannot be replayed. The agent verifies the signature and delivers the verified public key to the service with the request, and the service accepts a request about a purchase only when the verified key is the purchase key.

### Buying and redeeming

A badge is bought as a code on the web, for one or more months, and the code is redeemed in the app. The code is the only thing that passes between the two: the web site does not learn which app redeems a code, and the app does not see the payment.

A code consists of the letters `SB` followed by twenty characters, shown in four groups of five, from an alphabet that omits the letters most easily confused with digits. The last character is a check character, so that a single mistyped character is detected before anything is sent; codes may be typed in either case, and the letters `I`, `L` and `O` are read as the digits `1` and `0`. The service keeps a hash of each code rather than the code itself.

To redeem a code, the app generates the master key and the purchase key pair, and sends the code and the master key to the service, signed with the purchase key. The service verifies the signature, signs the credential, and only then records the code as spent; if signing fails, the code remains unspent and can be tried again. An unknown code and a malformed code receive the same error. A code redeemed a second time with the same purchase key returns the same credential; a code redeemed with a different purchase key is refused. A badge issued without a sale, for example in compensation for a problem, is a code minted by the operator and redeemed in the same way.

### Monthly issuance

A credential is issued for one month at a time, however many months were bought. The service keeps a count of the months remaining for each purchase key, and issues the next credential when the app asks for it. The user sees a single date: the day on which the support ends.

Credentials are issued monthly to limit what the expiry date discloses. If credentials were issued for the whole term, a user who bought a year would hold a credential expiring on a day a year ahead, when few other credentials expire, and this date would be disclosed in every proof for a year. Instead, every credential is issued for one month, and its expiry is rounded to the end of the Monday following the end of the paid month (UTC). For example, if the paid month ends on Wednesday 14 October 2026, the credential expires at the end of Monday 19 October, and so does every credential whose month ends between Monday 12 and Sunday 18 October. All credentials issued in a given week thus expire at the same instant, and a proof discloses only that its holder is one of the supporters of that week.

The renewal is split over two days for the same reason. The app requests the next credential on the day before the current one expires, and when the current one expires, it switches to the new one and sends its updated profile to its contacts. In the example above, the app asks the service for the new credential on Monday 19 October and starts showing it on Tuesday 20 October, so a party in a position to observe both the request and the profile update cannot match them by time.

The renewal runs in the background and requires nothing of the user. If the app is offline when a renewal is due, it renews at the next start; recipients accept a badge for seven days after its expiry, and servers for one day, so a renewal that is a few days late is not noticed by contacts.

### Presentation

A BBS proof is generated over a string, called the presentation header, and is verified only against the same string. The string binds the proof to the context in which it is presented. Without it, a proof received in one conversation could be copied and shown as one's own in another. Between apps the string is sent with the proof, and the recipient checks that it is the string it expects; servers know the string already, so it is not sent to them.

The context of a conversation is the same string over which message signatures are computed. In a direct chat it is a hash derived from the state of the end-to-end encryption, which only the two sides hold; in a group it is the member's identifier together with the member's signing key; in a channel it is the channel's identifier together with the member's identifier. A session with a server is identified by the TLS session identifier, which both sides derive from the TLS handshake and which differs on every connection.

Each time the app sends its profile, whether to a new contact, to a group it joins, or to everyone when the profile is updated, it generates a new proof and includes it. An incognito profile never includes a badge. At present, profile proofs are generated over a random value rather than over the context of the conversation, so a proof copied from a received profile would be accepted if it were placed in another profile. The next step is to bind profile proofs to the conversation, so that a group member's badge is accepted only from a message signed with that member's key, a channel member's badge is verified against the key established by the channel's roster, and a contact's badge is bound to the connection.

A file larger than the default limit is sent with two proofs. The first is placed in the file invitation, the message that announces the file, and is bound to the conversation and to the size of the file; the recipient's app verifies it when the invitation arrives and records whether the file may be received. The second is placed in the file description, the record of where the chunks of the file are stored and how they are decrypted, and is bound in addition to a hash of the description and to the storage time of the file; the app verifies it before the download begins. A proof taken from a profile is therefore not accepted for a file, and a proof made for one file is not accepted for another. When a file is forwarded to a member who has just joined a group, the forwarding app includes the proofs it stored with the file. The sender's app applies the same limit to itself, and stops applying the badge one day after the badge expires, whereas recipients continue to accept the badge for seven days, so that a file sent on the last day of the badge is still accepted.

A received proof is verified with the issuer key it names, and the result is stored with the profile or with the file. The badge is shown as active until seven days after its expiry, as expired between seven and 38 days, and is hidden after that. If the proof names an issuer key the app does not know, the badge is stored as unverified and verified again after the app is updated. A badge type the app does not know is accepted and stored under its name.

### Servers

The client presents the entitlement proof in the transport handshake, bound to the TLS session identifier. The server verifies it once, when the session is established, and applies the result to every command in the session. If the proof names a badge type for which the server has no configuration, it is ignored without verification.

The client presents the proof only to its own servers, that is, to those configured for the profile, which are identified by the certificate fingerprint pinned in TLS. It does not present the proof to a server whose address it received from a contact or found in a file description, because a file description names the servers on which the chunks are stored, and these are chosen by the sender. If the app presented its proof to every server it connected to, a sender could store a file on a server of their own and learn, when the file is downloaded, that the person downloading it holds a badge.

On XFTP servers the proof extends the storage time of files. When the client creates a file it may ask for a storage time. The server grants the smaller of the requested time and the maximum configured for the badge type, or the maximum when no time is requested, and returns the resulting expiry, which the client passes on to the recipients. A server refuses to start if the maximum for any badge type is below its default, so a proof cannot shorten storage. SMP servers and notification servers accept the same proof in their handshakes, since version 22 of the SMP protocol and version 4 of the notifications protocol, but do not yet make use of it.

The exchange between the app and the badge service, that is, the commands, responses and errors, is described in the [badge service protocol](./badges-rpc.md).


## Cryptographic primitives

Badges are built on [BBS signatures](https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/), in the BLS12-381-SHA-256 suite, implemented by [libbbs](https://github.com/Fraunhofer-AISEC/libbbs) over [blst](https://github.com/supranational/blst). A credential is a signature over four values under the header `SimpleX badges v1`; a proof discloses three of them and is 304 bytes long. BBS was chosen for three properties that the design needs together: a proof can be verified by anyone who holds the issuer's public key, so contacts and independently operated servers verify badges without consulting the issuer; a proof discloses only the values selected, which keeps the master key hidden while the type and expiry are shown; and a proof cannot be linked to any other proof of the same credential, so the same badge can be presented many times without the presentations being connected. Credentials of the kind Signal uses have the second and third properties but not the first, since they can be verified only by their issuer. Single-use tokens, such as blind signatures and the tokens of Privacy Pass, have the third but not the second, and are spent when presented.

Service requests are protected by the double ratchet of the [SimpleX agent](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md), with X448 key agreement and the sntrup761 KEM, established from keys published in the service's link. The purchase keys with which requests are signed, and the member keys that form part of the conversation context in groups, are Ed25519 keys. Codes are hashed with SHA-256, and the file description in the context of a file proof with SHA-512. The TLS session identifier binds a proof to a single server connection.


## Security

### Design objectives

1. A proof discloses no value that links it to another proof or to the purchase.
2. A proof is accepted only in the session, conversation or file for which it was generated.
3. The timing of presentations does not identify the holder: all credentials issued in a week share an expiry, and the renewal request and the profile update are made on different days.
4. Requests to the badge service cannot be linked to each other, to a profile or to a network address, and a request about a purchase can be made only by the holder of the purchase key.
5. A credential cannot be forged: the issuer keys are fixed in apps and servers, and the app verifies a credential before storing it.
6. A missing or failed proof leaves the default limit in place, and a server cannot be configured to grant a badge type less than the default.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and addresses the threats specific to badges.

**The badge service.** The service sees the purchase key of every badge, the master key generated for it, the number of months bought, and any payment record; and since it holds the issuer key, it can issue any credential and can refuse to issue. It cannot connect a purchase with a profile, a contact, a group or a session with a server, because nothing in a proof refers back to the purchase, and it does not learn where a badge is shown or used, or the network address of the app, since requests reach it through SMP servers on connections created for the purpose.

**A contact, a group member or a channel relay.** Such a party sees the badge type and the expiry date, and, until profile proofs are bound to the conversation, it can copy a profile proof into another profile ([gap 1](#current-gaps)). It cannot tell whether a badge seen in another conversation belongs to the same person, reuse a file proof, or distinguish the holder from the other supporters of the same week by anything in the proof.

**A server operator.** The operator sees the badge type and the expiry once in each session, and can group together the sessions of the supporters of one week. The operator cannot reuse the proof on another connection, link a session to the purchase, a profile or sessions on other servers except through the week of expiry, or obtain a proof from a client that does not use the server. The server that hosts the badge service's address sees that requests and replies pass through it, but not their content, nor the address of the app when private routing is used.

**One badge on many machines.** Since proofs are unlinkable, a server cannot count the sessions of a single credential, so the benefit of a badge must be limited without counting. For this reason a badge lowers the cost of a resource without removing the limit on it, and limits per session remain in force.

**Compromise of the user's device or backup.** An attacker who obtains the credential and the purchase key can use the badge until it expires and renew it while months remain. There is no revocation, so the loss is bounded only by the months bought.

**Interception of a code.** A code is a bearer secret until it is redeemed; once redeemed, it is refused to any other purchase key.

**A passive network observer.** The observer sees SimpleX traffic between clients and servers, but does not see a proof, and cannot distinguish a session in which a proof was presented from one in which it was not: proofs to servers are inside TLS, in a handshake block of fixed size, and proofs to other users and requests to the badge service are inside end-to-end encrypted messages.

### Current gaps

1. Profile proofs are not bound to the conversation, so a proof copied from a received profile would be accepted in another profile, as described under [Presentation](#presentation).
2. SMP servers and notification servers do not verify the proof; the field in the handshake exists, but the servers ignore it.
3. While few badges have been sold, the supporters of any one week form a small group.
4. All apps whose credentials share an expiry request their renewal at the same time.
5. There is no revocation, so a stolen credential remains usable until it expires.


## Future work

Backups will be stored on XFTP servers, and a badge will raise their size and their storage time, as described under [What a badge grants](#what-a-badge-grants).

The next step on the server side is for SMP servers and notification servers to verify the proof, which closes the second gap. A server that verifies it can charge the holder of a badge less for creating queues, file chunks and notification tokens (under the planned proof-of-work scheme, the required effort is divided by a factor configured for each badge type), and can apply higher rate limits and larger quotas.

Profile proofs will be bound to the conversation, and once released apps present bound proofs, unbound ones will be rejected; this closes the first gap. Renewal requests will be spread over the day before the expiry rather than made all at once, which closes the fourth.

Further ways to pay are planned: invoices with card and crypto payment in the app, purchases through the app stores, subscriptions, the transfer of remaining months to a new device, and the pausing of a prepaid badge.


## Conclusion

A supporter badge is a credential that is presented as a series of unlinkable proofs. The badge service records the purchase and is reached through one-off requests that leave no connection behind; contacts, relays and servers verify proofs that disclose the badge type and an expiry date shared by all credentials of the same week; and no proof can be linked to the purchase, to another proof or to a profile. The same credential is shown on the profile and presented to servers as an entitlement, and it serves as the mechanism for larger files, longer storage, backups and lower resource costs on servers.
