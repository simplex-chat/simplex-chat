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
  - [Buying and redeeming](#buying-and-redeeming)
  - [Monthly issuance](#monthly-issuance)
  - [Presentation contexts](#presentation-contexts)
  - [Profiles](#profiles)
  - [Files](#files)
  - [Servers](#servers)
  - [Verification](#verification)
- [Cryptographic primitives](#cryptographic-primitives)
- [Security](#security)
  - [Design objectives](#design-objectives)
  - [Threat model](#threat-model)
  - [Current gaps](#current-gaps)
- [Future work](#future-work)
- [Conclusion](#conclusion)


## Introduction

SimpleX Chat is funded in part by its users, who buy supporter badges. A badge is shown on the profile of the person who bought it, and it raises the limits that other users and servers apply to that person's files. This document describes what a badge is, what it grants, how it is issued and presented, and what it discloses about the person who holds it.

Consider what happens when you pay for extra storage in an ordinary messenger. The operator's database gains a row: this account, paid on this date, with this card. Every file the account uploads is then checked against that row, and the operator can see, in one place, who paid and what they did with the storage. The payment, the account and the use of the service are all linked, and they are linked because there is an account to link them to.

The SimpleX network has no user accounts, and this is the difficulty in selling anything to its users. There is nothing to attach a purchase to, and introducing something for the purpose would undo what the rest of the design achieves. A badge is therefore designed so that the purchase, which the badge service necessarily records, cannot be connected with the profile, the conversations or the servers of the person who made it.

### What is a badge

A badge is a credential issued by the badge service, a bot operated by SimpleX Chat. The credential is a [BBS signature](https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/) over four values: a secret master key, which the app generates and keeps; the expiry date; the badge type, which is either `supporter` or `legend`; and a reserved field that is currently empty.

A BBS signature has a property that an ordinary signature lacks. Its holder can demonstrate that the issuer signed certain values without showing the signature itself, and can choose which of the signed values to disclose in the demonstration. Each demonstration, which is called a proof, is different from every other, so that two proofs from the same signature cannot be recognised as coming from the same signature. This is the property the badge is built on.

The credential itself is never shown to anyone. When the badge is to be shown or used, the app generates a proof from the credential, and it is the proof that is sent. A proof discloses the badge type and the expiry date, and it demonstrates that the issuer signed these values, but it discloses neither the master key nor the signature. Two proofs generated from the same credential cannot be linked to each other, nor to the credential, and this holds for the issuer as much as for anyone else. A proof is also generated for a particular context, whether a conversation, a file or a session with a server, and is not accepted in any other.

The transport protocols of the network, which know nothing about badges, call the same credential an entitlement: a name, an expiry and an extra string, disclosed in a proof. The chat application puts the badge type into the name, so a server that verifies an entitlement and a contact who sees a badge are looking at the same credential.

A badge does not identify the person who holds it, cannot be passed to another person, and has no monetary value. A user who has no badge is not affected by the existence of badges.

### What a badge grants

The first thing a badge grants is visible: it is shown on the profile to contacts, to the members of groups, and to the subscribers of channels.

The second is larger files. Files sent through the network are limited to 1GB. A supporter can send files up to 2GB, and a legend up to 5GB. The limit is applied by the recipient, whose app verifies the sender's proof before it accepts a file above the default size.

The third is longer storage of files on the servers. An XFTP server stores a file for 48 hours by default, after which the file is deleted whether or not it was downloaded. A server may be configured to store files for longer when they are uploaded by the holder of a badge of a given type, and it applies the longer time to every file created in a session in which a proof was presented. In the reference configuration, the files of supporters are stored for 7 days and the files of legends for 21 days.

Two further uses are planned. The first is backups. A backup will be a link whose content the app replaces each time it makes a new backup, so that the same link always restores the latest state. A badge will raise the size of the backup and the time it is stored on the servers, and the storage time is then the period the app can remain offline before the backup is lost. The second is better terms on servers: a server that verifies the proof will charge less for the creation of messaging queues, file chunks and notification tokens, and will apply higher rate limits. Both are described under [Future work](#future-work).

### What a badge discloses

It is easiest to describe what a badge discloses by following the credential from purchase to use.

The badge service holds the purchase key, the number of months bought, and whatever payment record accompanies the purchase. A card payment is linked to the purchase, as it would be anywhere; a user who wants the payment itself to be private may pay in Monero. In either case the record stops at the service. The service does not learn where the badge is later shown or used, because the proofs contain nothing that refers back to the credential.

A party that verifies a proof learns the badge type and the expiry date. Since all credentials issued in the same week have the same expiry date, these two values place the holder among the supporters of that week and say nothing further.

Two presentations of the same badge cannot be linked to each other. A server that has verified a proof in one session has no way to tell whether a later session belongs to the same person, and a contact who sees a badge in one conversation has no way to tell whether a badge in another conversation belongs to the same person.

The design follows the [commercial model proposed in 2024](../rfcs/2024-04-26-commercial-model.md), in which the software vendor issues credentials that users present to infrastructure operators.

### Comparison with Signal badges

Signal sells donation badges, and its design is the natural point of comparison. Signal uses [receipt credentials](https://eprint.iacr.org/2019/1416.pdf). The client obtains a credential against a payment without revealing its account, and then presents the credential once to the account server, which records its serial number, level and expiry against the account and attaches the badge to the account. The payment is not linked to the account cryptographically. The same mechanism is used for Signal's paid backups.

The differences between the two designs follow from Signal having accounts and SimpleX having none. Signal's server holds the list of accounts that have badges, because a badge is an attribute of an account. A SimpleX server holds no such list: it verifies a proof for the duration of one session, and cannot link that session to any other. Signal's credential can be verified only by the server that issued it, and so can only ever be presented to Signal. A SimpleX credential is verified against the issuer's public key, which contacts, group members, channel relays and independently operated servers all hold. Signal's credential is presented once, after which the badge is simply a property of the account; a SimpleX credential is presented every time the badge is shown or used, with a new proof each time.

The two systems also differ in timing. Signal's client requests the credential and redeems it in a single sequence of jobs, without any delay between the two steps, so that the server can match the request made under the payment with the redemption made under the account by their times and by the network address from which they came. In SimpleX the request for a renewed credential and the profile update that presents it to contacts are made on different days. The expiry of a Signal credential is rounded up to the next day, so that all credentials issued in a day have the same expiry; in SimpleX the expiry is shared by all credentials issued in a week.

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

There are several things a badge does not attempt to do. It does not restrict anything that is available today: the defaults are unchanged, and a badge only raises them. It does not create an identity: it has no persistent identifier, it is not linked across conversations, and an incognito profile does not show it. It cannot be transferred: a code can be redeemed once, and the credential obtained with it is usable only with the master key it was issued for. It does not exempt its holder from the limits a server applies: since a credential can be presented in any number of sessions that cannot be linked, a badge can lower the cost of a resource but cannot remove the limit on it. And it cannot be revoked; instead, credentials are issued for one month at a time.

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

Three parties take part. The issuer is the badge service, a bot on the SimpleX network with a contact address like any other service. It holds the secret key with which credentials are signed. Apps and servers hold a list of eight issuer public keys, and every credential includes the index of the key that signed it, so the service can move to the next key without a release of apps or servers, and a new list is released before the current one is used up.

The holder is the app. It keeps the credential in the user's profile, generates proofs on the device, and renews the credential every month by means of a background task.

The verifiers are contacts, group members, channel relays, the recipients of files, and the user's own servers. A verifier holds the issuer public keys and nothing else about badges.

### Buying and redeeming

A badge is bought as a code on the web, for one or more months, and the code is redeemed in the app. The code is the only thing that passes between the two: the web site does not learn which app redeems a code, and the app does not see the payment.

A code consists of the letters `SB` followed by twenty characters, shown in four groups of five, from an alphabet that omits the letters most easily confused with digits. The last character is a check character, so that any single mistyped character is detected before anything is sent. Codes may be typed in either case, and the letters `I`, `L` and `O` are read as the digits `1` and `0`. The service keeps a hash of each code rather than the code itself, so a code cannot be recovered from the service.

To redeem a code, the app generates a master key and a purchase key pair, and sends the code and the master key to the service, signed with the purchase key. The request is sent over the SimpleX network to the service's contact address, on a connection created for this one request and deleted after the reply, so the service does not learn the client's network address. The service verifies the signature, signs the credential, and only then records the code as spent; if signing fails, the code remains unspent and can be tried again. An unknown code and a malformed code receive the same error. A code redeemed a second time with the same purchase key returns the same credential, which is what allows the app to repeat a request whose reply was lost; a code redeemed with a different purchase key is refused.

A badge issued without a sale, for example in compensation for a problem, is a code minted by the operator and redeemed in the same way as any other.

### Monthly issuance

A credential is issued for one month at a time, however many months were bought. The service keeps a count of the months remaining for each purchase key, and issues the next credential when the app asks for it. The user sees a single date: the day on which the support ends.

The reason for issuing monthly is what the expiry date would otherwise disclose. Suppose a credential were issued for the whole term at once. A user who bought a year would hold a credential expiring on some day a year ahead, and that day would be disclosed in every proof for a year. Few other credentials would expire on the same day, so the date alone would come close to identifying the holder. Instead, every credential is issued for one month, and its expiry is rounded to the end of the Monday following the end of the paid month (UTC). Take a month that ends on Wednesday 14 October 2026: the credential expires at the end of Monday 19 October, and so does every credential whose month ends anywhere between Monday 12 and Sunday 18 October. All credentials issued in a given week thus expire at the same instant, and what a proof discloses about its holder is that they are one of the supporters of that week.

The renewal is split over two days for the same reason. The app requests the next credential on the day before the current one expires, while the current one is still valid, and when the current one expires, it switches to the new one and sends its updated profile to its contacts. In the example above, the app asks the service for the new credential on Monday 19 October and starts showing it on Tuesday 20 October. The request to the service and the profile update to contacts thus happen on different days, and a party in a position to observe both cannot match them by time.

The renewal runs in the background and requires nothing of the user. If the app is offline when a renewal is due, it renews at the next start. Recipients accept a badge for seven days after its expiry, and servers for one day, so a renewal that is somewhat late is not noticed by contacts.

### Presentation contexts

A BBS proof is generated over a string, called the presentation header, and is verified only against the same string. This is what ties a proof to the context in which it is presented. Without it, a proof received in one conversation could be copied and shown as one's own in another; with it, a proof is accepted only where it was made for. The string is not transmitted to servers, since both sides know it already; between apps it is transmitted with the proof, and the recipient checks that it is the string it expects before verifying anything.

The context of a conversation is the same string over which message signatures are computed. In a direct chat it is a hash derived from the state of the end-to-end encryption, which only the two sides hold. In a group it is the member's identifier together with the member's signing key, and in a channel it is the channel's identifier together with the member's identifier.

A file invitation is presented under the context of the conversation together with the size of the file. A file description adds to these a hash of the description, that is, of the file's size, digest and encryption key and of the digests of its chunks, and the time until which the file is stored on the servers.

A session with a server is identified by the TLS session identifier, which both sides derive from the TLS handshake and which differs on every connection.

### Profiles

Each time the app sends its profile, whether to a new contact, to a group it joins, or to everyone when the profile is updated, it generates a new proof and includes it. An incognito profile never includes a badge.

At present, profile proofs are generated over a random value rather than over the context of the conversation. A proof copied from a received profile would therefore be accepted if it were placed in another profile. The next step is to bind profile proofs to the conversation, so that a group member's badge is accepted only from a message signed with that member's key, a channel member's badge is verified against the key established by the channel's roster, and a contact's badge is bound to the connection.

### Files

A file larger than the default limit is sent with two proofs. The first is placed in the file invitation, the message that announces the file, and is bound to the conversation and to the size of the file. The recipient's app verifies it when the invitation arrives and records whether the file may be received. The second is placed in the file description, the record of where the chunks of the file are stored and how they are decrypted, and is bound in addition to the hash of the description and to the storage time of the file. The app verifies it before the download begins. A proof taken from a profile is therefore not accepted for a file, and a proof made for one file is not accepted for another.

When a file is forwarded to a member who has just joined a group, the forwarding app includes the proofs it stored with the file. For a file of its own it generates fresh proofs if the stored ones have expired.

The sender's app applies the same limit to itself. It refuses to send a file larger than its badge allows, and it stops applying the badge one day after the badge expires, whereas recipients continue to accept the badge for seven days, so that a file sent on the last day of the badge is still accepted.

### Servers

The client presents the entitlement proof in the transport handshake, bound to the TLS session identifier. The server verifies it once, when the session is established, and applies the result to every command in the session. If the proof names a badge type for which the server has no configuration, it is ignored without verification.

The client presents the proof only to its own servers, that is, to those configured for the profile, which are identified by the certificate fingerprint pinned in TLS. It does not present the proof to a server whose address it received from a contact or found in a file description. The reason is that a file description names the servers on which the chunks of the file are stored, and these are chosen by the sender. If the app presented its proof to every server it connected to, a sender could store a file on a server of their own and learn, when the file is downloaded, that the person downloading it holds a badge.

On XFTP servers the proof extends the storage time of files. When the client creates a file it may ask for a storage time. The server grants the smaller of the requested time and the maximum configured for the badge type, or the maximum when no time is requested, and returns the resulting expiry, which the client passes on to the recipients. A server refuses to start if the maximum for any badge type is below its default, so a proof cannot shorten storage.

SMP servers and notification servers accept the same proof in their handshakes, since version 22 of the SMP protocol and version 4 of the notifications protocol, but do not yet make use of it.

### Verification

A received proof is verified with the issuer key it names, and the result is stored with the profile or with the file. The badge is shown as active until seven days after its expiry, as expired between seven and 38 days, and is hidden after that. If the proof names an issuer key the app does not know, the badge is stored as unverified and verified again after the app is updated; a proof that fails verification is stored as failed. A badge type the app does not know is accepted and stored under its name.

The exchange between the app and the badge service, that is, the commands, responses and errors, is described in the [badge service protocol](./badges-rpc.md).


## Cryptographic primitives

Badges are built on [BBS signatures](https://datatracker.ietf.org/doc/draft-irtf-cfrg-bbs-signatures/), in the BLS12-381-SHA-256 suite, implemented by [libbbs](https://github.com/Fraunhofer-AISEC/libbbs) over [blst](https://github.com/supranational/blst). A credential is a signature over four values under the header `SimpleX badges v1`; a proof discloses three of them and is 304 bytes long.

BBS was chosen for three properties that the design needs together. A proof can be verified by anyone who holds the issuer's public key, so contacts and independently operated servers verify badges without consulting the issuer. A proof discloses only the values selected, which keeps the master key hidden while the type and expiry are shown. And a proof cannot be linked to any other proof of the same credential, and therefore the same badge can be presented many times without the presentations being connected. Credentials of the kind Signal uses have the second and third properties but not the first, since they can be verified only by their issuer. Single-use tokens, such as blind signatures and the tokens of Privacy Pass, have the third but not the second, and are spent when presented.

The purchase keys with which requests to the badge service are signed, and the member keys that form part of the conversation context in groups, are Ed25519 keys. Codes are hashed with SHA-256, and the file description in the context of a file proof with SHA-512. The TLS session identifier binds a proof to a single server connection. The context of a direct conversation is a hash of the associated data of the double ratchet, which is inherited from the [SimpleX agent layer](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/agent-protocol.md), and the encryption of the ratchet protects the proofs exchanged between apps.


## Security

### Design objectives

The design is intended to achieve the following.

1. A proof discloses no value that links it to another proof or to the purchase.
2. A proof is accepted only in the session, conversation or file for which it was generated.
3. The timing of presentations does not identify the holder: all credentials issued in a week share an expiry, and the renewal request and the profile update are made on different days.
4. A credential cannot be forged: the issuer keys are fixed in apps and servers, and the app verifies a credential before storing it.
5. A badge adds no value that persists across conversations, and incognito profiles are unaffected.
6. A missing or failed proof leaves the default limit in place, and a server cannot be configured to grant a badge type less than the default.
7. A credential is renewed before it expires and accepted for a period after, and the background task recovers from any interruption.

### Threat model

This threat model assumes the [SimpleX network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md) and addresses the threats specific to badges.

**The badge service.** The service sees the purchase key of every badge, the master key generated for it, the number of months bought, and any payment record; and since it holds the issuer key, it can issue any credential and can refuse to issue. What it cannot do is connect a purchase with a profile, a contact, a group or a session with a server, because nothing in a proof refers back to the purchase. Nor does it learn where a badge is shown or used, or the network address of the client, since requests reach it over the SimpleX network on connections created for the purpose.

**A contact, a group member or a channel relay.** Such a party sees the badge type and the expiry date, and, until profile proofs are bound to the conversation, it can copy a profile proof into another profile ([gap 1](#current-gaps)). It cannot tell whether a badge seen in another conversation belongs to the same person, reuse a file proof, or distinguish the holder from the other supporters of the same week by anything in the proof.

**A server operator.** The operator sees the badge type and the expiry once in each session, and can group together the sessions of the supporters of one week. The operator cannot reuse the proof on another connection, link a session to the purchase, a profile or sessions on other servers except through the week of expiry, or obtain a proof from a client that does not use the server, since the client presents proofs only to the servers it has configured.

**One badge on many machines.** Since proofs cannot be linked, a server cannot count the sessions of a single credential, and what a badge grants must be bounded in a way that does not depend on counting. For this reason a badge lowers the cost of a resource without removing the limit on it, and limits per session remain in force.

**Compromise of the user's device or backup.** An attacker who obtains the credential and the purchase key can use the badge until it expires and renew it while months remain. There is no revocation, so the loss is bounded only by the months bought.

**Interception of a code.** A code is a bearer secret until it is redeemed; once redeemed, it is refused to any other purchase key.

**A passive network observer.** The observer sees SimpleX traffic between clients and servers, but does not see a proof, and cannot distinguish a session in which a proof was presented from one in which it was not: proofs to servers are inside TLS, in a handshake block of fixed size, and proofs to other users are inside end-to-end encrypted messages.

### Current gaps

1. Profile proofs are not bound to the conversation, so a proof copied from a received profile would be accepted in another profile, as described under [Profiles](#profiles).
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

A supporter badge is a credential that is presented as a series of unlinkable proofs. The badge service records the purchase; contacts, relays and servers verify proofs that disclose the badge type and an expiry date shared by all credentials of the same week; and no proof can be linked to the purchase, to another proof or to a profile. The same credential is shown on the profile and presented to servers as an entitlement, and it serves as the mechanism for larger files, longer storage, backups and lower resource costs on servers.
