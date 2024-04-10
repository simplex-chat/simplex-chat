---
title: Frequently Asked Questions
permalink: /faq/index.html
revision: 10.04.2024
---

# Frequently Asked Questions

[Funding and business model](#funding-and-business-model)
- [How are you funded?](#how-are-you-funded)
- [Why VCs?](#why-vcs)
- [What will be the business model?](#what-will-be-the-business-model)

[How to use it](#how-to-use-it)
- [I have nobody to chat with! Where can I find any groups?](#i-have-nobody-to-chat-with-where-can-i-find-any-groups)
- [What is database? What can I do with it?](#what-is-database-what-can-i-do-with-it)
- [Can I send files over SimpleX? ](#can-i-send-files-over-simplex)
- [What’s incognito profile?](#whats-incognito-profile)
- [How do invitations work?](#how-do-invitations-work)
- [How to configure and delete groups?](#how-to-configure-and-delete-groups)
- [Are there any reactions to messages? Can I answer specific messages directly?](#are-there-any-reactions-to-messages-can-i-answer-specific-messages-directly)
- [What do checkmarks mean?](#what-do-checkmarks-mean)
- [Can I use the same profile on desktop? Do messages sync cross-platform?](#can-i-use-the-same-profile-on-desktop-do-messages-sync-cross-platform)

[Privacy and security](#privacy-and-security)
- [Does SimpleX support post quantum cryptography?](#does-simplex-support-post-quantum-cryptography)
- [What user data can be provided on request?](#what-user-data-can-be-provided-on-request)
- [Does SimpleX protect my IP address?](#does-simplex-protect-my-ip-address)
- [Can I host my own relays?](#can-i-host-my-own-relays)

## Funding and business model

### How are you funded?

SimpleX Chat Ltd is funded by private investors and venture capital. As an open-source project, it is also being generously supported by donations as well. There are more details here on the VCs involved and amounts raised: 
https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html#how-is-it-funded-and-what-is-the-business-model

### Why VCs?

Here are some reflections on VC funding being a necessity for a project at this scale, as well as sustainability and profitability for longtime operations: https://www.poberezkin.com/posts/2023-10-31-why-privacy-impossible-without-venture-funding.html

And another perspective from a team member on the delicate balance of venture-backed and nonprofit structures, and the plans for the SimpleX network protocols to evolve under the stewardship of nonprofit entities in various jurisdictions, so that its continued evolution aligns more closely with the vision of community-driven, independent and transparent governance:
https://simplex.chat/blog/20240404-why-i-joined-simplex-chat-esraa-al-shafei.html

### What will be the business model?

We are focusing on product-market fit, and as such the business model is still a work in progress. However, the app will have a freemium model with extra features or capabilities for paid users (taking into consideration a potential formula like 5% paying $5/month is $3/user/year - ~90% gross profit margin).

The other income stream would be via business services, for entities needing direct and customized support to integrate with the SimpleX protocol or related resources. There will also be a revenue-sharing model from customers to network operators, to provide an incentive for them to continue running nodes, which will increase decentralization and reliability of the network.

Non-exploitative commercial models with fully open source code are not easy to achieve, and we’re committed to finding the best possible fit for our context. Everything will be fully communicated as this plan progresses.

## How to use it

### I have nobody to chat with! Where can I find any groups?

Please check our [Groups Directory](./DIRECTORY.md) in the first place. You might find some interesting groups and meet even more interesting people.

### What is database? What can I do with it?

Database is essential for SimpleX Chat to function properly. In comparison to centralized messaging providers, it is _the user_ who is responsible for taking care of their data. On the other hand, user is sure that _nobody but them_ has access to it. Please read more about it: [Database](./guide/managing-data.md).

### Can I send files over SimpleX? 

Of course! While doing so, you are using a _state-of-the-art_ protocol that greatly reduces metadata leaks. Please read more about it: [XFTP Protocol](../blog/20230301-simplex-file-transfer-protocol.md).

### What’s incognito profile?

This feature is unique to SimpleX Chat – it is independent from chat profiles. 

When "Incognito Mode” is turned on, your currently chosen profile name and image are hidden from your new contacts. It allows anonymous connections with other people without any shared data – when you make new connections or join groups via a link a new random profile name will be generated for each connection. 

### How do invitations work?

It is quite a complex process, but fortunately all of this happens in the background, so it's simply to use.

Whenever somebody connects to you via your address, they basically ask your client whether they want to establish connection. After that, you can either agree or disagree.
If interested, please read more: [Addresses and invitations](./guide/making-connections.md).

### How to configure and delete groups?

Please check: [Users guide](./guide/secret-groups.md).

### Are there any reactions to messages? Can I answer specific messages directly?

Yes! Currently, there are six emojis available. What's more, you can respond to specific message by holding it and selecting _Reply_.

### What do checkmarks mean?

It's quite simple:
- one checkmark - message is delivered to the relay (the server).
- two checkmarks - message is delivered to the recipient's device.
"sent" means accepted by the relay for delivery, "delivered" - stored on the recipient device.

### Can I use the same profile on desktop? Do messages sync cross-platform?

You can use your profile from mobile device on desktop. However, to do so you need to be on the same network, both on your mobile and desktop. More about it: [Release info](../blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.md#link-mobile-and-desktop-apps-via-secure-quantum-resistant-protocol).

## Privacy and security

### Does SimpleX support post quantum cryptography?

Yes! Please read more about quantum resistant encryption is added to SimpleX Chat and about various properties of end-to-end encryption in [this post](../blog/20240314-simplex-chat-v5-6-quantum-resistance-signal-double-ratchet-algorithm.md).

### What user data can be provided on request?

Our objective is to consistently ensure that no user data and absolute minimum of the metadata required for the network to function is available for disclosure by any infrastructure operators, under any circumstances.

Please see our [Privacy Policy](../PRIVACY.md) and [Transparency Reports](./TRANSPARENCY.md).

### Does SimpleX protect my IP address?

Not fully yet, it is a work in progress. While your device does not connect to your contacts' devices directly, as it happens in p2p networks, your contacts can self-host their relays, and you will connect to them when sending messages. A modified relay can record IP addresses connecting devices, as is the case with any other server, including Tor entry nodes, VPN providers, etc. - IP address is fundamental to Internet functioning, and there will always be some server that can observe your IP address.

We are currently working on the next version of message routing protocol that will protect your IP address from the relays chosen by your contacts, so it will only be visible to the relays chosen by you. Read about technical details here: [RFC](https://github.com/simplex-chat/simplexmq/blob/stable/rfcs/2023-09-12-second-relays.md). 

### Can I host my own relays?

Of course! Please check this tutorials: [SMP server](./SERVER.md) and [XFTP server](./XFTP-SERVER.md).
