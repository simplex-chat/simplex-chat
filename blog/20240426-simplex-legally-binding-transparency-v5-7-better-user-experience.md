---
layout: layouts/article.html
title: "SimpleX network: legally binding transparency, v5.7 released with better user experience for calls and messages"
date: 2024-04-26
preview: blog_previews/20240323.html
draft: true
permalink: "/blog/20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.html"
---

# SimpleX network: legally binding transparency, v5.7 released with better user experience for calls and messages

## Legally binding transparency 

We think it’s important for users not to settle for whatever companies deem to be "acceptable" in terms of legal policies and transparency, and to make commitments to open-source, privacy and security legally binding. 

In line with this commitment, here are recent changes we made:

- We now have a [Transparency Reports](https://simplex.chat/transparency/) page.
- We updated our [Privacy Policy](https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md) to remove undefined terms "impermissible" and "acceptable", which would allow us to remove anything we don't like, without any clarity on what that is. You can see the edits [here](https://github.com/simplex-chat/simplex-chat/pull/4076/files).
- We published a new page with [Frequently Asked Questions](https://simplex.chat/faq/), thanks to the guidance from users.
- We also have a new [Security Policy](https://simplex.chat/security/), which we welcome your feedback on.

What do we mean by “legally binding transparency?”. It includes the following principles:
- Accountability: An empty promise or commitment to transparency that is not legally binding is pure marketing, and can provide opportunities for companies or organizations to be misleading or not disclose important information that can affect their users privacy and security.
- Genuine transparency: Often, there's a disconnect between marketing claims and legally binding policies. Our approach is to ensure that promises made in marketing materials or any external communications align with legally binding documents, so that users can rely on our promises, and know exactly what to expect from us. 

For example:
- we use open-source code, and we made a legally binding commitment to use the published code in all released apps and deployed preset servers.
- we use precise and technical language in the Privacy Policy defining what data and metadata can be accessed via the preset relays.

If you see any inconsistency between technical parameters our SimpleX Network and what is promised in our Privacy Policy please raise it with us.

## What's new in v5.7

This release focus is on improving the app usability and preparing the foundation for v5.8 that will provide an in-built protection of user IP addresses, reducting the need to use Tor (which would still remain supported via SOCKS proxy, for additional privacy).

### Quantum resistant encryption

TODO picture

We [wrote before](./20240314-simplex-chat-v5-6-quantum-resistance-signal-double-ratchet-algorithm.md) about how quantum resistant encryption is added to SimpleX Chat and also about other properties of end-to-end encryption, possible attacks on its security and known mitigations.

Quantum resistant encryption will now be enabled by default in all direct chats. For the new conversations it will be enabled from the beginning, and for the existing conversations it will be agreed after you exchange several messages with your contacts - you will see a notice in the conversation when it happens.

You can still safely downgrade the app to earlier version if needed to v5.6.1 or v5.6, but trying to downgrade to the earlier version will irreversibly disrupt the conversations with quantum resistant encryption.

With the users who didn't install the new version yet, the app will continue work, using the conventional encryption, which is still very secure. It's important that we did not replace conventional encryption with post-quantum cryptographic algorithm, but augmented it, using a hybrid construction, as recommended by the cryptography experts.

Please note that groups currently do not support quantum resistant encryption yet - we plan to add it to small groups.

### Forward and save messages

TODO picture

You can now save received messages to private notes and forward them to your contacts and groups. This is both more convenient than copy-pasting the messages, but also more private - you no longer need to save the received file outside of the app to be able to forward it, and you could preserve the file inside a disappearing message in your private notes instead of saving it to the device files.

You can see the original source of the message via the message information, but the recipient of the message can only see that it was forwarded, but not from which conversation - in this way you can signal that the message was quoted from another source without revealing the source ([Chatham House Rule](https://en.wikipedia.org/wiki/Chatham_House_rule)).

### In-call sounds

This was one of the most frequent complaint of the users who use SimpleX Chat for audio and video calls - there is no sound indication of the call connection progress, as we got used to in all other apps. This release added two sounds - initially there is "connecting" sound when the call invitation is sent, but it is not known yet if the recipient is aware (e.g., because the recipient's device is offline or delivery receipts are disabled), and then a long beeping sound when the recipient is likely to have incoming call rings. Once the call is connected or disconnected there will be a vibration, to notify you when you are heard.

### Customize shapes of profile images in the app

TODO picture

### Network management

TODO

## SimpleX network

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[Frequently asked questions](../docs/FAQ.md).

Please also see our [website](https://simplex.chat).

## Help us with donations

Huge thank you to everybody who donates to SimpleX Chat!

We are planning a 3rd party security audit for the protocols and cryptography design in July 2024, and also the security audit for an implementation in December 2024/January 2025, and it would hugely help us if some part of this $50,000+ expense is covered with donations.

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX network based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/tree/master#help-us-with-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
