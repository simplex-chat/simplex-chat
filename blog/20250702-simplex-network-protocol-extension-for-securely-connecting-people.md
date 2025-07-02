---
layout: layouts/article.html
title: "SimpleX network: new experience of connecting with people &mdash; available in SimpleX Chat v6.4-beta.4"
date: 2025-07-02
preview: Now you can start talking to your contacts much faster, as soon as you scan the link. This technical post covers the technology that enabled this new user experience - short links and associated data of messaging queues.
# image: images/20250308-captcha.png
# imageBottom: true
permalink: "/blog/20250702-simplex-network-protocol-extension-for-securely-connecting-people.html"
---

# SimpleX network: new experience of connecting with people &mdash; available in SimpleX Chat v6.4-beta.4

**Published:** Jul 2, 2025

The mission of communication network is connecting people [1]. And while the process of connecting in SimpleX network is really secure, being protected from MITM attacks by server thanks to out-of-band key exchange, it was a really bad experience for the users prior to this beta version.

## What was the problem?

How did it work before:

1. Your contact created a large link (1-time invitation or contact address [2]) and shared with you via another messenger, email, social profile or website. While sharing the link "out-of-band" (not via the server) is necessary for security, it was "broken" in several ways:
  - the link was incorrectly changed by some applications, e.g. Telegram, because of link complexity, preventing people from connecting.
  - some people were worried that the link is "malware", because of how long and complex it looked, and refused to use them.
  - the QR code for the link was large, and some devices had problems scanning them.
  - the link did not fit the size limit in social media profiles.
2. Once you received the link, you used it in the app. You did not learn anything new at this point - you could not see who you were connecting to. The only choice you had is to either share your current profile or incognito profile. And as people didn't know which profile will be shared with them, many people were choosing to share incognito profile, that made management of contacts more complex - you don't know who is who unless you attach aliases to incognito contacts.
3. Once you tap Connect, all you see is the line in the list of chats that said "connecting via link". The process of connection required your contact to be online, and in some cases to approve the request, so you may see "connecting" for a really long time.

So it is not surprising that a large number of people failed connecting – either they refused to engage because of scary long links, or their application made the link unusable, or they abandoned the process at step 3 deciding that the app is just broken.

## Why can't we just use usernames or very short links?

Many people asked - why don't you just use usernames or a link shortener for some really short links, as other networks and apps do.

The problem is that usernames or very short links make e2e encryption security of your chats dependent on the servers that would have to provide public keys in exchange for a username, and unless the link you share contains enough randomness in it and is cryptographically linked to the returned keys, the servers can substitute the e2e encryption keys and read all your communication without you knowing it.

Mitigation against this "man-in-the-middle" attack by the server [3] is offered by Signal and other apps via security code verification [4], when you compare the code in your app with your contact's app, but:
- most people do not verify security codes, and even if they do, they do not re-verify them every time security code changes, so their security is dependent on the server not being compromised, which is not a great security,
- the servers can still compromise the initial messages, where profile names are exchanged, before you had the chance to verify the security codes.

When we design communication protocols for SimpleX network we always aim to protect you from the attack by your communication operators – this is what sets SimpleX network design apart from many other communication networks and platforms.

Even though you choose the servers that you trust, and they are bound by privacy policy, and we follow best security practices to protect servers from any 3rd party attacks, there is still a possibility that servers are compromised by some attackers, and unless your communications are not protected from the servers, they are not protected from whoever can compromise the servers [5].

## How the new tech works from the users' point of view

Before diving into the details of technology, let's walk though the new process of connecting people introduced in [v6.4-beta.4](https://github.com/simplex-chat/simplex-chat/releases/tag/v6.4.0-beta.4).

1. As before, to connect you or your contact need to create a 1-time invitation or a contact address link. But all the past problems of the long links are now solved:
- this link is correctly processed by all applications, as it has a simple structure,
- it does not look as malware anymore, e.g. the short link of SimpleX Chat team we use for support is: https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw
- the QR code is now much smaller, fits a standard business card, and is easy to scan from all devices,
- it fits in most social media profiles.

While the link is short, it still contains 256 bits of key material with additional 192 bits of server-generated link ID for one-time invitation links, so the connection remained as secure, and in case of one-time invitations it became more secure (see below).

2. As before, you have to use this link in the app, either by pasting the link or scanning QR code. But now you instantly see the name of your contact or group you are connecting to, and from v6.4.1 you will also see a profile image (currently disabled for backward compatibility).

3. Once you tap "Open new chat", the app will instantly open a conversation with your contact where you can choose which profile to use to connect. As you now can see which profile is shared with you via the link, you can decide which profile to use, or if you should connect incognito. If your contact shared an incognito pseudonym, then you may also choose to connect incognito. But if your contact shared a real name with you, you may also want to share your real name, making it easier for your contact to recognize you - the law of reciprocity in action!

If you are connecting via a contact address you can also add a message to your request, making it more likely to be accepted. And from v6.4.1 contact addresses can include a welcome message that you would see before connecting, right in the chat. This way, you become connected to your contact and start a secure conversation even before you tap "Connect" button.

If you are connecting via a one-time invitation link, all you need to do is to tap "Connect", and then you can send messages straight away, without waiting for your contact to be online - they will be securely received.

This new experience of connecting is very similar to commonly used messengers, but it protects your security, so we hope it will be much easier for the new users to connect to their friends.

## What about security?

We took a great care to design the protocol extension for the new experience of connecting in a way that not only preserves security at the same level as before, but also increases security of connecting via one-time invitation links.

First, because all the keys are now included in encrypted link data on the server, and not in the link itself as before, we can include the keys for post-quantum (PQ) key exchange and make the first message send via one-time link - your profile - encrypted with PQ e2e encryption. Previously, PQ encryption started from the 2nd message, after your profile was sent.

Second, whoever can observe the link is not able to determine which public keys are used in key exchange and what messaging queue address is used, and this data is removed from the server once the connection is established. Previously, the invitation link contained public keys and the actual queue address that could have been used for a long time, unless you rotated it.

Third, if somebody tried to retrieve the associated data of one-time invitation link they observed in transit, this link would become unusable for the intended recipient, so the recipient would know that the connection security was compromised, and would alert the contact that sent the link.

## How does it work?

In short, a new short link references a container with the encrypted data on the server that contains:
- the original full link that now include quantum-resistant that previously were not included in the link because of their size,
- contact's or group's profile, including conversation preferences, from v6.4.1 it will include profile image,
- from v6.4.1 it will include an optional welcome message.

Making user profile and welcome included in the encrypted link data allows to start conversation as soon as you scan the link, as described in the previous section.

### Design objectives and cryptographic primitives that allow achieving them

This section is not a formal specification of the protocol, but an informal technical explanation of objectives we had for this design and how they were achieved. The technical details are available in [this RFC document](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2025-03-16-smp-queues.md).

When designing the protocol extension for this new experience we had the following objectives:

1. **Associated link data cannot be accessed by the server, unless it has the link itself**.

It means that while the client apps should use the link to derive both the link ID for the server and decryption key for the associated link data, the server should not be able to derive the link and decryption key from the link ID that it knows.

This objective is achieved by using the encryption (`secret_box`) of link data with the symmetric key derived from link URI, which is different from link ID known to the server. As it is a symmetric encryption, it is secure against quantum computers.

2. **Allow changing the user-defined part of link data without changing the link itself**.

This is necessary to allow changes in user profile, chat preferences and welcome messages.

This is possible via a specific server request that allows to change user-defined part of the link data to the link owner. Because the link is derived from fixed part of the link data, the link itself remains the same.

3. **Prevent MITM attack on the link data by the server, even if the server somehow obtained the link**.

It means that the server should not be able to replace the associated link data even if it obtained the link and can decrypt the data.

This objective is achieved by deriving (using cryptographic `HKDF` function) encryption key from the hash (`SHA3-256`) of the fixed (immutable) part of the link data - if server changes the link data, it would be rejected by the client, as its hash won't match the link. Server also cannot replace the user-defined (mutable) part of the link data, because it is signed and will be verified with the key (`ED25519`) included in the fixed part of link data.

4. **For one-time links, prevent undetectably accessing link data by link observers who did not compromise the server**.

This is explained in the previous section - if link observers retrieve the link data, the link will be unusable for the intended recipient.

This objective is achieved because the link data of 1-time invitation link data can only be accessed with the server request that locks queue on the first access. Any subsequent access to the queue must uses the same authorization key (`ED25519`).

5. **The link owner cannot include address of another queue in the link**.

It means that the link cannot redirect the connecting party to another server or to another queue on the same server - the apps would reject the links that attempt to do it. While allowing redirects may be seen as higher security from the server, it would open the possibility of resource exhaustion attacks, as the server would not know if the links were actually used to connect or not, and when the data can be removed. So we decided that preventing redirects is a better tradeoff. This cryptographically enforced association between link and queue allows to remove link data from the server once the connection is established, or once some time passes (e.g., 3 weeks for one-time links).

This objective is achieved by including queue ID and link data into the same server response.

6. **Prevent link owner from being able to change the queue address referenced by the link and also encryption keys for key exchange**.

This quality prevents the MITM attack on e2e encryption via break-in attack on the client of the link owner.

This objective is achieved because the server does not provide any API to change the fixed part of the link data. Also, changing fixed data would require changing the link, as otherwise the hash of the data won't match the link.

7. **Retain the quality that it is impossible to check the existence of a messaging queue for one-time invitation links from having any of its IDs visible to the message sender (sender ID and link ID in 1-time invitations)**.

It means that any 3rd party that observed 1-time invitation link (e.g., by reading the message where it was sent) must not be able to undetectably confirm whether this link was connected to, and whether the messaging queue still exists, by attempting to create another queue with the same link ID and the same link data.

This objective is achieved by servers requiring that sender ID is derived (using `SHA3-384`) from request correlation ID, so an arbitrary sender ID cannot be used, and by generating link ID on the server - for 1-time invitation link, the link ID is included in the link in addition to link key, and is not derived from the link data.

The detailed [threat model](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2025-03-16-smp-queues.md#threat-model) for the new user experience of making connections is also included in the RFC document.

## Let us know what you think!

We worked really hard to deliver this big change - this simplicity of user experience required to hide a lot of complexity under the hood. We really hope that it will help you to bring more of your friends to SimpleX network and to benefit from using secure communications.

The stable versions v6.4 and v6.4.1 will be released this July, but you can already use the beta version available via Play Store (Android), Test Flight (iOS) and GitHub (Android and desktop).

Big thank you to all hundreds of thousands of people who use SimpleX network, even though the experience of connecting to people was so bad.

With your help, SimpleX network can get over the million active users now!

## SimpleX network

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-and-security-technical-details-and-limitations).

[Frequently asked questions](../docs/FAQ.md).

Please also see our [website](https://simplex.chat).

## Please support us with your donations

Huge *thank you* to everybody who donated to SimpleX Chat!

Prioritizing users privacy and security, and also raising the investment, would have been impossible without your support and donations.

Also, funding the work to transition the protocols to non-profit governance model would not have been possible without the donations we received from the users.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds &mdash; any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/#please-support-us-with-your-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder

[1]: An interesting case study would be rise and fall of Nokia as the dominant supplier of mobile phones. The slogan "Connecting people" was created in 1992 by Ove Strandberg, an intern at Nokia, and as it was adopted as the core mission of the company, we saw it rise to dominance as a mobile phone supplier. The fall of Nokia is usually attributed on iPhone success. But it may also be attributed to internal cultural changes, with Nokia's communications chief leaving in early 2000s. Nokia failed to understand how the definition of "Connecting people" should evolve with time, and Nokia's failure to capitalize on people needs that were emerging in early 2000s, long before iPhone - a more interactive device with a larger screen. So it's not iPhone success resulted in Nokia's failure, but Nokia's failure to stick to its core ethos of "Connecting people" allowed iPhone success.

[2]: 1-time invitation vs address.

[3]: Link to encryption post re mitm attack

[4]: SimpleX apps also offer security code verification, but it protects against the link being substituted by the channel you use to pass the link, not from the attacks by the servers - SimpleX servers cannot compromise e2e encryption.

[5]: That is also why "securely scanning users' communications", also known as "Chat Control" is impossible - what communication operator can access, cyber-criminals will also access at some point, and instead of reducing crime it would expose users to more crime.
