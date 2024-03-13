---
layout: layouts/article.html
title: "SimpleX Chat v5.6 (beta) adds quantum resistantce to Signal double ratchet algorithm."
date: 2024-03-14
preview: v5.6 (Beta) already allows using it in direct chats via experimental toggle, both for the new and existing contacts.
image: images/20230301-xftp.jpg
imageWide: true
permalink: "/blog/20230301-simplex-file-transfer-protocol.html"
---

# SimpleX Chat v5.6 (beta) adds quantum resistantce to Signal double ratchet algorithm.

This is a major upgrade for SimpleX Chat messaging protocol stack, so for the Pi day (which is also the International day of Mathematics) we are really proud to present this work.

This post is about various aspects of end-to-end encryption and about how quantum-resistant encryption is added to SimpleX Chat:

- [why do we need end-to-end encryption](#why-do-we-need-end-to-end-encryption)?
- [why encryption is even allowed](#why-encryption-is-even-allowed)?
- [End-to-end encryption security: attacks and defence](#end-to-end-encryption-security-attacks-and-defence).
- [How secure is encryption in different messengers](#how-secure-is-end-to-end-encryption-in-different-messengers)?
- [Adding quantum resistantce to Signal double ratchet algorithm](#adding-quantum-resistantce-to-signal-double-ratchet-algorithm).
- [When can you have quantum resistance in SimpleX Chat](#when-can-you-have-quantum-resistance-in-simplex-chat)?
- [Next for PQ crypto - all direct chats, small groups and security audit](#next-for-pq-crypto---all-direct-chats-small-groups-and-security-audit).

## Why do we need end-to-end encryption?

The objective of end-to-end encryption is to make any potential attackers, such as traffic observers or communication providers who pass the messages between senders and recipients, unable to recover *any* message content or meaningful information about the messages, even if these attackets possess very advanced computing or mathematical capabilities.

While human eyes would not be able to see any difference between the messages that were simply scrambled, so they can't be read, and messages that were encrypted so they can't be deciphered, the difference between unreadable scrambling and unbreakable encryption can be as huge as just a few seconds to unscramble a message on an average laptop or a smartphone and more time than the Universe existed required to break the encryption on the most powerful computer in the world.

Achieving the latter requires a lot of mathematical precision in both the cryptographic algorithms and in how they are used, and effectively makes encrypted messages indistinguishable from absolutely random noise, without any discoverable patterns or statistical irregularities that a computer could use to decrypt the message without having the keys.

End-to-end encryption is an important component of our security and privacy. Having our private communications protected from any observers is both the natural condition and our inaliable human right.

It's very sad to see the same people who keep their financial affairs private to protect from financial crimes, lock their doors and cover their windows to protect from thieves and from the prying eyes, when it comes to protecting their personal life from data criminals say "we don't care, as we have nothing to hide". Everybody's personal and family safety depends on keeping their affairs and relations private, not visible to a vast and ruthless data gathering machines, that abuse this data for commercial gain, without any regard to our interests or even basic safety.

## Why encryption is even allowed?

If encryption is such a powerful tool to protect our lives, it also can be used to conceal crimes, so why the governments don't consider it similar to arms, and don't heavily regulate its use?

Prior to 1996 the cryptography was considered muntion, and its export from the United States was controlled under this category, [alongside flamethrowers and B-1 bombers](https://cr.yp.to/export/1995/0303-eff.txt). When [Daniel J. Bernstein](https://en.wikipedia.org/wiki/Daniel_J._Bernstein) (DJB), then a student of Mathematics at University of California, Berkeley, wanted to publish the paper and the source code of his Snuffle encryption system, the Office of Defense Trade Controls of the Department of State (DOS) after more than a year of correspondence requested that DJB registers as the exporter of munitions.

In 1995 DJB represented by the Electronic Frontier Foundation brought a case against the DOS to overturn cryptography restrictions. The ruling in the case declared that the export control over cryptographic software and related technical data constitute [an impermissible infringement on speech in violation of the First Amendment](https://cr.yp.to/export/1996/1206-order.txt). This decision resulted in regulatory changes, reducing controls on encryption exports, perticularly for open-source software. The case continued until 2003, when it was put on hold after the commitment from the US government not to enforce any remaining regulations.

This case is very important for the whole industry, as to this day we can freely create and use open-source cryptography without export control restrictions. It also shows the importance of engaging with the system and challenging its views in an open dialogue, rather than either blindly complying or violating regulations.

DJB role for cryptogrpaphy and open-source goes beyond this case – many cryptographic algorithms that are considered to be the most advanced, and many of which we use in SimpleX Chat, were designed and developed by him:

- Ed25519 cryptographic signature algorithms we use to authorise commands to the servers (and will only be used to authorise recipients' commands, while senders' commands will switch to use authenticators based on NaCL cryptobox to provide repudiation in all protocol layers, not only in end-to-end encryption).
- NaCL library with cryptobox and secretbox constructions that combine X25519 Diffie-Helman key agreement with Salsa20 encryption and Poly1305 authentication. We use cryptobox to encrypt messages in two of three encryption layers and secretbox to encrypt files.
- Streamlined NTRU Prime algorithm for quantum resistant key agreement that we used in the protocol for linking mobile app with desktop, and now added to Signal (double ratchet) algorithm, as explained below.

Without DJB's work the world would have been in a much worse place privacy- and security-wise - thank you and congratulations on the International Mathematics Day.

TODO: insert DJB photo?

## End-to-end encryption security: attacks and defence

End-to-end encryption is offered by many messaging apps and protocols, but the security of different implementations are not the same. While many users know about the importance of forward secrecy - the quality of end-to-end encryption that preserves security of the encryption of the past and future messages, even if the keys used to encrypt some of the messages were compromised - there are many other qualities that protect from different attacks. Here I provide the overview of these attacks and the properties of end-to-end encryption schemes that mitigate these attacks.

### Attacks based on message size - mitigated by padding messages to a fixed size

While the content encryption is the most important, concealing the actual message size is almost as important for several reasons:

- attacker able to observe even approximate message sizes can use these sizes as an additional signal for machine learning to de-anonymise the users and to categorize the relationships between the users.
- if a messenger conceals the routing of the messages to hide the transport identities (IP addresses) of senders and recipients, message sizes can be used by traffic observers to confirm the fact of communication with a high degree of probability.

The only effective mitigation to these attacks is to pad all messages to a fixed size. Using space-efficient schemes like Padme, or even padding to encryption block size is ineffective for mitigating these attacks, as they still allow differentiating message sizes.

To the best of our knowledge the only messenger other than SimpleX Chat that padded all messages to a fixed packet size was Pond by AGL - we see SimpleX design as an evolution of it.

### Compromised confidential messages - mitigated by repudiation

Many users are very interested in having ability to irreversibly delete sent messages from the recipients devices. But not only would this ability violate data sovereignty of device owners, it is also completely ineffective, as the recipients could simply put the device offline or use a modified client app to ignore message deletion request. While SimpleX Chat provides features like disappearing messages and ability to delete sent messages provided both parties agree to that, these are convenience features, and they cannot be seen as security measures.

The solution to that is very well known to cryptographers - it is a quality of encryption algorithms called "repudiation", sometimes also called "deniability". This is the ability of the senders to plausibly deny having sent any messages because cryptographic algorithms used to encrypt allow recipients forging these messages on their devices, so while the encryption proves authenticity of the message to the recipient, it cannot be used as a proof to any third party.

Putting it all in a simpler language - a sender can claim that the recipient forged messages on their device, and deny every having sent them. The recipient will not be able to provide any proof. This quality makes digital conversation having the same qualities as private off-the-record conversation - that's why the family of algorithms that provide these qualities are called off-the-record (OTR) encryption.

Repudiation is still a rather new concept - the first off-the-record algorithms were proposed in 2004 and only offered to mass market users via Signal messenger. This concept is still quite badly understood by users and society, and yet to have been used as the defense in any public court cases. Which does not mean it cannot be used as an effective defense, and legal systems evolve much slower than technology.

Repudiation in messaging systems can be easily undermined by adding cryptographic signature to the protocol, and most messengers that use OTR encryption algorithms do exactly that. SimpleX Chat does not use signature in any part of client-client protocol, but the signature is currently used when authorising sender's messages to the relays. v5.7 will  enable a different authorization scheme that will provide full-stack repudiation in all protocol layers - it is already implemented.

### Compromised message keys - mitigated by forward secrecy

The attacker who obtained or broke the keys used to encrypt individual messages, may try to use these keys to decrypt past messages or future messages. This attack is highly unlikely via simple message interception, and it is likely to require breaking into the device storage. But in any case, if the key was broken or obtained in some other way it's important that this key cannot be used to decrypt other messages - this is achieved by forward secrecy (also called perfect forward secrecy, to highlight the fact that each message is encrypted by the same key).

This property is well understood by the users, and most messengers that focus on privacy and security (with the notable exception of Session) provide forward secrecy as part of their encryption schemes design.

### Compromised long-term or session keys - mitigated by break-in recovery

This attack is much less understood by the users, and forward secrecy does not protect from it. Arguably, it's almost impossible to compromise individual message keys without compromising long-term or session keys. So the ability of the encryption scheme to recover from break-in (attacker making a copy of the device data without retaining the ongoing access) is both very important and pragmatic - break-ins are the most common attacks on mobile devices, and they are much simpler to execute during short-term device access than long-term ongoing compromise.

Out of all encryption algorithms known to us only Signal (double ratchet) algorithm provides the ability to recover encryption security from break-in. This recovery happens automatically and transparently to the users, without them doing anything special or even knowing about break-in, by simply sending messages. Every time one of the communication parties replies to another party message, new random keys are generated and previously stolen keys become useless.

Signal (double ratchet) algorithm is used in Signal, Cwtch and SimpleX Chat. This is why you cannot use SimpleX Chat profile on more than one device at the same time - the encryption scheme rotates the long term keys, randomly, and keys on another device become useless, as they would become useless for the attacker who stole them. Security always has some costs to the convenience.

### Man-in-the-middle attack - mitigated by two-factor key exchange

End-to-end encryption is as secure as key exchange. While any intermediary passing the keys between senders and recipients cannot determine private keys by observing public keys, they can simply replace the passed keys with their own and then proxy all communication between the users having full access to decrypted messages. So instead of having one end-to-end encrypted channel, users would have to half-way encrypted channgels - with their communication intermediary.

TODO: insert picture of MITM attack from the old site

The knowns solutions to mitigate this attack is to either pass each party key via a different channel, which is much more secure, or at least to verify the security of key exchange after it is completed, usually by comparing security codes. One of the effective approaches is to make user's key (or key fingerprint) part of the user address, thus making two-factor key exchange non-optional. This approach is used in Session, Cwtch and SimpleX Chat.

A less secure approach is to provide users an optional way to compare security codes - this is what is done by Signal, Element, Briar, and many other messengers. The problem with this post-exchange validation is that it is optional, and can be skipped by the users, and that this security can change because the user changed the device or as a result of the attack via the operator. So when you see in the client app the notification that the security code changed it's meaningles to ask in the same messenger whether device was changed, as if it was an attack, the attacker would of course confirm it. Instead the security code needs to be re-validated again via another channel. Alternatively, the users should warn their communication partners about the intention to switch the device before switching.

### "Record now, decrypt later", when quantum computers become available

This is the idea based on the assumption that commercially viable quantum computers will become viable during our life-times, and then they can use time-efficient [Shor's algorithm](https://en.wikipedia.org/wiki/Shor%27s_algorithm) developed in 1994 to break asymmetric encryption (symmetric encryption is not vulnerable to this algorithm).

Post-quantum cryptography, or encryption algorithms that are resistant to quantum computers, has been the area of ongoing research for several decades, and there are some algorithms that _might_ protect from quantum computers. It's important to account for these limitations:

- none of the post-quantum algorithms are proven to be secure against quantum computers. They are usually referred to as "believed to be secure". There is continuous research to break post-quantum algorithms, or to prove their security, and many of them are broken every year, often with conventional computers.
- because of the lack of proofs or guarantees that post-quantum algorithms deliver on their promise, they can only be used in hybrid schemes to augment conventional cryptography, never to replace it, contrary to many expert recommendations, as DJB explains in a [blog post](https://blog.cr.yp.to/20240102-hybrid.html).
- they are much more computationally expensive and less space efficient, and the encryption schemes have to balance their usability and security, often opimising for usability.
- many of post-quantum algorithms have known patent claims, so any system deploying them accepts the risks of patent litigation.
- the silver lining to this is that the risk of appearance of commercially viable quantum computers in the next decade may be strongly exaggerated in order to justify current R&D funding for both computer and algorithm development.

So, to put it bluntly, post-quantum cryptography is a remedy against the decease nobody has without any guarantee that it will work. The closest anology in the field of medicine is _snake oil_, although, in fairness to snake oil, it was usually sold to cure the real deceases people already had, not the future ones.

## How secure is end-to-end encryption in different messengers?

TODO Insert table.

TODO write some commentary.

## Adding quantum resistantce to Signal double ratchet algorithm

Does it mean that post-quantum cryptography is competely useless and should be ignored? Absolutely not. The risks of "record now, decrypt later" attacks are absolutely real, particularly for all high profile targets, that includes millions of people - all journalists, whitleblowers, freedom-fighters in oppressive regimes, and even some ordinary people may become targets of information crimes. Large scale collection of encrypted communication data is ongoing, and this data may be used in the future. So having the solution that _might_ protect you (PQ cryptography), as long as it doesn't remove the solution that is _proven_ to protect you (conventional cryptography), is highly beneficial in any communication solution, and has already been deployed in many tools and in some messengers.

TODO insert photo of some really large datacenter?

We have been exploring this subject since early 2022, when SimpleX Chat was first released, and we did not want to be pioneers here - cryptography is critically important to make it right.

We hoped to adopt the algorithm that will be standardized by NIST, but the standardisation process turned out to be hugely disappointing and the ML-KEM algorithm that was accepted as a standard (called Crystals Kyber during NIST competition) was modified to remove a very important hashing step, that mitigates the attacks via a compromised random numbers generator, ignoring strong criticism from many expert cryptographers, including DJB.

We also analysed the encryption schemes proposed in Tutanota in 2021 and another scheme adopted by Signal last year, and published a design that we believe provides much better security than either of these schemes:

- unlike Tutanota design, it augments rather than replaces conventional cryptography, and also avoids using signatures when new keys are agreed (ratchet steps).
- unlike other messengers, we used Streamlined NTRU Prime algorithm (strnup761) that has no problems of ML-KEM, no known patent claims, and seems less likely to be compromised than other algorithms - exactly the same algorithm that is used in SSH.
- unlike Signal design, that only added quantum resistance to the initial key exchange, replacing X3DH key agreement scheme with PQXDH, but did not change the actual Signal algorithm, our design added quantum-resistant key agreement in double ratchet algorithm, making its break-in recovery property also quantum resistant.

The reason we could make break-in recovery property of Signal algorithm, and why, probably, Signal didn't, is because irrespective of the message size SimpleX Chat uses a fixed block size of 16kb to provide security and privacy against any traffic observers and against messaging relays. So we had an extra space to accomodate additional ~2.2kb worth of keys in each message without any additional traffic costs.

In case when the message is larger than the remaining size, e.g. when the message contains image or link preview, or a long text, we used zstd compression to provide additional space for the requred keys without reducing image preview quality or creating additional traffic - our previously inefficient JSON encoding of messages was helpful in this case.

TODO insert picture of double KEM key agreement

The additional challenge in adding sntrup761 is that unlike Diffie-Hellman key exchange which is a non-interactive and symmetric (that is, the parties can share their public keys in any order and the shared secret can be computed from two public keys), sntrup761 is interactive key-encapsulation mechanism (KEM) that requires that one party shares its public key, and another party uses it to encrypt a random shared secret, and send it back - making it somewhat similar to RSA cryptography. But this asymmetric design does not fit the symmetric operation of Signal double ratchet algorithm, where both sides generate random public keys and both sides need to compute new shared secrets every time messaging direction changes for them. So to add symmetry back we had to use two KEM key agreements running in parallel, in a lock-step fashion, when both parties generate random public keys independently and also use the public key of another party to encapsulate the random shared secret. Effectively, it adds a double post-quantum key agreement to double ratchet algorithm!

## When can you have quantum resistance in SimpleX Chat?

TODO insert photos of the conversation and settings pages

Quantum resistant double ratchet algorithm is already available in v5.6 (beta) of SimpleX Chat as an optional feature that can be enabled for the new and, separately, for the existing direct conversations.

The reason it is released as opt-in is because once the conversation is upgraded to be quantum resistant, it will no longer work in the previous version of the app, and we see this ability to downgrade the app if something is not working as it should as very important for the users who use the app for critical communications.

To enable quantum resistance for the new conversation:
- open the app settings (tap user avatar in the top left corner).
- scroll down to _Developer tools_ and open them.
- enable _Show developer options_ toggle.
- now you will see _Post-quantum E2EE_ toggle - enable it as well, it was well-hidden.

Now all new contacts you add to the app will use quantum resistant Signal algorithm.

Once you have enabled it for the new contacts, you can also enable it for some of the existing contacts:
- open the chat with the contact you want to upgrade.
- tap contact name above the chat.
- tap Allow PQ encryption
- exchange several messages back and forth with that contact - the quantum resistant double ratchet will kick in after 3-6 messages (depending on how many messages you send in each direction), you will see the notice in the chat once it enables.

## Next for PQ crypto - all direct chats, small groups and security audit

We will be making quantum resistance default for all direct chats in v5.7, and they will be upgraded for all users without any action.

We will also be adding quantum resistance to small groups up to 10-20 members. Computing cryptographic keys is much slower, in comparison, and it would be very inefficient (and completely unnecessary) for large public groups.

We have also arranged a cryptographic review of our protocol and encryption schemes design for June/July 2024 - it will cover all the additions to SimpleX protocol since the last review in November 2022, including XFTP protocol we use for file transfers and quantum resistant Signal double ratchet algorithm we just released in this beta version.

In November 2024 we will be conducting further implementation audit - with double the scope of our 2022 audit.

Security audits are very expensive, as they require employing exceptionally competent engineers and cryptographers, and it does stretch our budgets - so any donations would be hugely helpful - see below. 

That's it for now!

Thank you for helping us improve the app, and look forward to your feedback.

## SimpleX platform

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[How SimpleX is different from Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/README.md#frequently-asked-questions).

Please also see our [website](https://simplex.chat).

## Help us with donations

Huge thank you to everybody who donated to SimpleX Chat!

As I wrote, we are planning a 3rd party security audit for the protocols and cryptography design, and also for an app implementation, and it would hugely help us if some part of this $50,000+ expense would be covered with donations.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, so anybody can build the future implementations for the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/tree/master#help-us-with-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
