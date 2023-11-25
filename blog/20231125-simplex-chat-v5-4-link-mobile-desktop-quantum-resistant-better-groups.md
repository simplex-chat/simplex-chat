---
layout: layouts/article.html
title: "SimpleX Chat v5.4 - link mobile and desktop apps via quantum resistant protocol, and much better groups."
date: 2023-11-25
preview: SimpleX Chat v5.4 - link mobile and desktop apps via quantum resistant protocol, and much better groups.
# image: images/20231125-remote-desktop.jpg
draft: true
imageWide: true
permalink: "/blog/20231125-simplex-chat-v5-4-quantum-resistant-mobile-from-desktop-better-groups.html"
---

# SimpleX Chat v5.4 - link mobile and desktop apps via quantum resistant protocol, and much better groups.

**Published:** Nov 25, 2023

**What's new in v5.4:**
- [Link mobile and desktop apps via secure quantum-resistant protocol](#link-mobile-and-desktop-apps-via-secure-quantum-resistant-protocol).
  - Quick start - how to use it
  - How does it work?
- [Better groups](#better-groups)
  - [Faster to join and more reliable](#faster-to-join-with-more-reliable-message-delivery).
  - [Create groups with incognito profile](#create-groups-with-incognito-profile).
  - [Block group members to reduce spam](#block-group-members-to-reduce-spam).
  - [Optionally, prohibit files and media in a group](#prohibit-files-and-media-in-a-group).
- [Better calls](#better-calls)
  - Improved stability and reduced connection time.
  - Screen sharing in video calls in desktop app.
- [Other improvements]
  - profile names now allow spaces
  - when you delete contacts, they are optionally notified.
  - previously used and your own SimpleX links are recognized by the app.
  - many fixes and improvements.

## Link mobile and desktop apps via secure quantum-resistant protocol

This release allows to use chat profiles you have in mobile app from desktop app.

This is only possible when both devices are connected to the same local network. To send and receive messages mobile app has to be connected to the Internet.

### Quick start - how to use it

TODO screenshots

If you don't have desktop app installed yet, [download it](https://simplex.chat/downloads/) and create any chat profile - you don't need to use it, and when you create it there are no server requests and no accounts are created. Think about it as about user profile on your computer.

Then in desktop app settings choose *Link a mobile* - it will show a QR code.

In mobile app settings choose *Use from desktop*, scan the QR code and verify session code when it appears on both devices - it should be the same. Verifying session code confirms that the devices are connected directly via a secure encrypted connection. There is an option to verify this code on subsequent connections too, but by default it is only required once.

The devices are now paired, and you can continue using all mobile profiles from desktop.

If it is an Android app, you can move the app to background, but iOS app has to remain open. In both cases, while you are using mobile profiles from desktop, you won't be able to use mobile app.

The subsequent connections happen much faster - by default, the desktop app broadcasts its session address to the network, in encrypted form, and mobile app connect to it once you choose *Use from desktop* in mobile app settings.

### How does it work?

The way we designed this solution avoided any security compromises, and the end-to-end encryption remained as secure as it was - it uses [double-ratchet algorithm](../docs/GLOSSARY.md#double-ratchet-algorithm), with [perfect forward secrecy](../docs/GLOSSARY.md#forward-secrecy), [post-compromise security](../docs/GLOSSARY.md#post-compromise-security) and deniability.

This solution is similar to WhatsApp and WeChat. But unlike these apps, no server is involved in the connection between mobile and desktop. The connection itself uses a new SimpleX Remote Control Protocol (XRCP) based on secure TLS 1.3 and additional quantum-resistant encryption inside TLS. You can read XRCP protocol specification and threat model in [this document](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2023-10-25-remote-control.md). We will soon be [augmenting double ratchet](https://github.com/simplex-chat/simplex-chat/pull/3463) to be resistant to quantum computers as well.

The downside of this approach is that mobile device has to be connected to the same local network. But the upside is that the connection is secure, and you do not need to have a copy of all your data on desktop, which usually has lower security than mobile.

Please note, that the files you send, save or play from desktop app, and also images you view are automatically saved on your desktop device - with the exception of videos, they are locally encrypted in case mobile app has local encryption enabled (which is the default). To remove all these files you can unlink the paired mobile device from the desktop app settings – there will be an option soon allowing to remove the files without unlinking the mobile.

## Better groups

### Faster to join, with more reliable message delivery

We improved the protocols for groups, by making joining groups much faster, and also by adding message forwarding. Previously, the problem was that until a new member connects directly with each existing group member, they did not see each other messages in the group. The problem is explained in detail in [this video](https://www.youtube.com/watch?v=7yjQFmhAftE&t=1104s) at 18:23.

With v5.4, the admin who added members to the group forwards messages to and from the new members until they connect to the existing members. So you should no longer miss any messages and be surprised with replies to messages you have never seen once you and new group members upgrade.

### Create groups with incognito profile

Previously, you could only create groups with your main profile. You could, as a workaround, add yourself to this group with an incognito profile, make this new member an owner, and then group creator could leave the group. This version allows creating groups with incognito profile directly. You will not be able to add your contacts, they can only join via group link.

### Block group members to reduce noise.

You now can block messages from group members that send too many messages, or the messages you don't won't to see. It won't affect other group members, and blocked members won't know that you blocked their messages. When they send messages they will appear in the conversation as one line, showing how many messages were blocked. You can reveal them, or delete all sequential blocked messages at once.

### Prohibit files and media in a group

Group owners now have an option to prohibiting sending files and media. This can be useful if you don't won't any images shared, and only want to allow text messages.

## Better calls

Calls in SimpleX Chat still require a lot of work to become stable, but this version improved the speed of connecting calls, and they should work for more users.

We also added screen sharing in video calls to desktop app.

## Other improvements

This version also has many small and large improvements to make the app more usable and reliable.

The new users and group profiles now allow spaces in the names, to make them more readable. To message these contacts in CLI you need to use quotes, for example, `@'John Doe' Hello!`.

When you delete contacts, you can notify them - to let them know they can't message you.

When you try to connect to the same contact or join the same group, or connect via your own link, the app will recognize it and warn you, or simply open the correct conversation.

You can find the full list of fixed bugs and small improvements in the [release notes](https://github.com/simplex-chat/simplex-chat/releases/tag/v5.4.0).

## SimpleX platform

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[How SimpleX is different from Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/README.md#frequently-asked-questions).

Please also see our [website](https://simplex.chat).

## Help us with donations

Huge thank you to everybody who donated to SimpleX Chat!

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/tree/master#help-us-with-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
