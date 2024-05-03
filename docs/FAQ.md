---
title: Frequently Asked Questions
permalink: /faq/index.html
revision: 23.04.2024
---

# Frequently Asked Questions

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

[Troubleshooting](#troubleshooting)
- [I do not receive messages or message notifications](#i-do-not-receive-messages-or-message-notifications)
- [I do not see the second tick on the messages I sent](#i-do-not-see-the-second-tick-on-the-messages-i-sent)
- [I see image preview but cannot open the image](#i-see-image-preview-but-cannot-open-the-image)
- [I cannot play a voice message](#i-cannot-play-a-voice-message)
- [Audio or video calls do not connect](#audio-or-video-calls-do-not-connect)
- [I clicked the link to connect, but could not connect](#i-clicked-the-link-to-connect-but-could-not-connect)

[Privacy and security](#privacy-and-security)
- [Does SimpleX support post quantum cryptography?](#does-simplex-support-post-quantum-cryptography)
- [What user data can be provided on request?](#what-user-data-can-be-provided-on-request)
- [Does SimpleX protect my IP address?](#does-simplex-protect-my-ip-address)
- [Can I host my own relays?](#can-i-host-my-own-relays)

[Funding and business model](#funding-and-business-model)
- [How are you funded?](#how-are-you-funded)
- [Why VCs?](#why-vcs)
- [What will be the business model?](#what-will-be-the-business-model)

## How to use it

### I have nobody to chat with! Where can I find any groups?

Please check our [Groups Directory](/docs/DIRECTORY.md) in the first place. You might find some interesting groups and meet even more interesting people.

### What is database? What can I do with it?

Database is essential for SimpleX Chat to function properly. In comparison to centralized messaging providers, it is _the user_ who is responsible for taking care of their data. On the other hand, user is sure that _nobody but them_ has access to it. Please read more about it: [Database](/docs/guide/managing-data.md).

### Can I send files over SimpleX? 

Of course! While doing so, you are using a _state-of-the-art_ protocol that greatly reduces metadata leaks. Please read more about it: [XFTP Protocol](/blog/20230301-simplex-file-transfer-protocol.md).

### What’s incognito profile?

This feature is unique to SimpleX Chat – it is independent from chat profiles. 

When "Incognito Mode” is turned on, your currently chosen profile name and image are hidden from your new contacts. It allows anonymous connections with other people without any shared data – when you make new connections or join groups via a link a new random profile name will be generated for each connection. 

### How do invitations work?

It is quite a complex process, but fortunately all of this happens in the background, so it's simply to use.

Whenever somebody connects to you via your address, they basically ask your client whether they want to establish connection. After that, you can either agree or disagree.
If interested, please read more: [Addresses and invitations](/docs/guide/making-connections.md).

### How to configure and delete groups?

Please check: [Users guide](/docs/guide/secret-groups.md).

### Are there any reactions to messages? Can I answer specific messages directly?

Yes! Currently, there are six emojis available. What's more, you can respond to specific message by holding it and selecting _Reply_.

### What do checkmarks mean?

It's quite simple:
- one checkmark - message is delivered to the relay (the server).
- two checkmarks - message is delivered to the recipient's device.
"sent" means accepted by the relay for delivery, "delivered" - stored on the recipient device.

Also see [ ](#i-do-not-see-the-second-tick-on-the-messages-i-sent)

### Can I use the same profile on desktop? Do messages sync cross-platform?

You can use your profile from mobile device on desktop. However, to do so you need to be on the same network, both on your mobile and desktop. More about it: [Release info](/blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.md#link-mobile-and-desktop-apps-via-secure-quantum-resistant-protocol).

## Troubleshooting

### I do not receive messages or message notifications

There may be several reasons messages are not delivered to you from your contact:

**You or your contact cannot connect to the server that you use to receive messages from your contact.**

You can check which server is used to receive messages by tapping the contact name above the conversation.

You can also run tests for this server from the app Network settings.

Please ask your contact if they have a single tick on the message to determine if the message failed to send or if you fail to receive it.

**Message delivery got stuck because of some unresolved bug.**

Fully restarting the app is the workaround to resume message delivery.

To do it on iOS, simply close the app (swipe up from the opened apps) and open it again.

To do it on Android - choose Restart from the app settings, simply closing and re-opening the app will not restart the messaging service.

**Your Android operating system kills the app while it is in background.**

Check battery settings for the app - it should be set to Unrestricted.

For some devices, there may be additional options to prevent the app from being killed - e.g., on Xiaomi you need to enable Auto Start setting for the app. Please consult https://dontkillmyapp.com site for any additional settings for your device.

**iOS notifications failed to initialize correctly**

Check the color of the bolt icon next to Notifications in app settings - it should be green.

If it's not, please open notifications, disable them (choose Off / Local), and then enable again - you should do it when you have Internet connection.

If the above didn't help, the reason could be that iOS failed to issue notification token - we have seen this issue several times. In this case, restarting the whole device should help.

**Messaging server or notification server is under maintenance**

Please check the current status of preset servers at [https://status.simplex.chat](https://status.simplex.chat). You can also connect to status bot via QR code on that page - it will send the updates when the server is offline for maintenance, and also when the new versions of the app are released.

### I do not see the second tick on the messages I sent

You may not have the second tick on your sent messages for these reasons:

- your contact is not online, and did not receive your message.
- possibly, message delivery to your contact or to you is disrupted - see [I do not receive messages](#i-do-not-receive-messages-or-message-notifications) - please check with your contact via some other channel if they received your message. If the message was delivered, then it means your device could fail to receive the delivery notification.
- possibly, your contact disabled sending delivery receipts - it can be disabled for specific or for all contacts - please check with your contact.

### I see image preview but cannot open the image

It can be for these reasons:
- your contact did not finish uploading the image file, possibly closing the app too quickly. When the image file is fully uploaded there will be a tick in the _top right corner_ or the image
- your device fails to receive it. Please check server connectivity and run server tests, and also try increasing network timeouts in Advanced network settings. File reception was substantially improved in v5.7 - please make sure you are using the latest version.
- file expired and can no longer be received. Files can be received only for 2 days after they were sent, after that they won't be available and will show X in the top right corner.

### I cannot play a voice message

This can happen for similar reasons as for [images](#i-see-image-preview-but-cannot-open-the-image).

Please check your network settings and make sure you use the latest version of the app.

Please report such issues if you use v5.7 or newer.

### Audio or video calls do not connect

App uses WebRTC for calls. Please check that you can connect to the servers configured in the app: stun.simplex.im and turn.simplex.im - see [troubleshooting WebRTC](/docs/WEBRTC.md#troubleshoot).

If you can connect to the server, please report this issue to us privately, including the following information:

- how you connect to the network: WiFi, mobile network, VPN provider - the more information you can provide the better.

- app version and platform. For mobile apps, it would help if you can make a screen recording from both devices during unsuccessful calls and share with us.

- if the issue is on desktop app, which browser is used for calls. In this case also please check browser console during the call and send us the log, ideally from both sides of the unsuccessful calls.

Thank you for helping us debug and improve calls.

### I clicked the link to connect, but could not connect

If you confirmed the connection in the app, pending connection will be shown in the list of chats - you can assign the name to it, so you know who it was when your contact is connected (e.g., if they choose some name you don't recognize).

For connection to complete, your contact has to be online and have the app running - please ask them to open the app, and try to have the app open at the same time - it will help to complete the connection faster.

Once the connection is established you don't need to be online at the same time to send messages.

## Privacy and security

### Does SimpleX support post quantum cryptography?

Yes! Please read more about quantum resistant encryption is added to SimpleX Chat and about various properties of end-to-end encryption in [this post](/blog/20240314-simplex-chat-v5-6-quantum-resistance-signal-double-ratchet-algorithm.md).

### What user data can be provided on request?

Our objective is to consistently ensure that no user data and absolute minimum of the metadata required for the network to function is available for disclosure by any infrastructure operators, under any circumstances.

Please see our [Privacy Policy](/privacy) and [Transparency Reports](/transparency).

### Does SimpleX protect my IP address?

Not fully yet, it is a work in progress. While your device does not connect to your contacts' devices directly, as it happens in p2p networks, your contacts can self-host their relays, and you will connect to them when sending messages. A modified relay can record IP addresses connecting devices, as is the case with any other server, including Tor entry nodes, VPN providers, etc. - IP address is fundamental to Internet functioning, and there will always be some server that can observe your IP address.

We are currently working on the next version of message routing protocol that will protect your IP address from the relays chosen by your contacts, so it will only be visible to the relays chosen by you. Read about technical details here: [RFC](https://github.com/simplex-chat/simplexmq/blob/stable/rfcs/2023-09-12-second-relays.md). 

### Can I host my own relays?

Of course! Please check these tutorials: [SMP server](/docs/SERVER.md) and [XFTP server](/docs/XFTP-SERVER.md).

## Funding and business model

### How are you funded?

SimpleX Chat Ltd is funded by private investors and venture capital. As an open-source project, it is also being generously supported by donations as well. Read [more details](/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.md#how-is-it-funded-and-what-is-the-business-model).

### Why VCs?

Here are some reflections on VC funding being a necessity for a project at this scale, as well as sustainability and profitability for longtime operations: https://www.poberezkin.com/posts/2023-10-31-why-privacy-impossible-without-venture-funding.html

And another perspective from a team member on the delicate balance of venture-backed and nonprofit structures, and the plans for the SimpleX network protocols to evolve under the stewardship of nonprofit entities in various jurisdictions, so that its continued evolution aligns more closely with the vision of community-driven, independent and transparent governance:
[https://simplex.chat/blog/20240404-why-i-joined-simplex-chat-esraa-al-shafei.html](/blog/20240404-why-i-joined-simplex-chat-esraa-al-shafei.md).

### What will be the business model?

We are focusing on product-market fit, and as such the business model is still a work in progress. However, the app will have a freemium model with extra features or capabilities for paid users (taking into consideration a potential formula like 5% paying $5/month is $3/user/year - ~90% gross profit margin).

The other income stream would be via business services, for entities needing direct and customized support to integrate with the SimpleX protocol or related resources. There will also be a revenue-sharing model from customers to network operators, to provide an incentive for them to continue running nodes, which will increase decentralization and reliability of the network.

Non-exploitative commercial models with fully open source code are not easy to achieve, and we’re committed to finding the best possible fit for our context. Everything will be fully communicated as this plan progresses.
