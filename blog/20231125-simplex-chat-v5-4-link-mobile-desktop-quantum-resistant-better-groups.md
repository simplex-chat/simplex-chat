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

TODO stub for release announcement

# SimpleX Chat v5.4 - link mobile and desktop apps via quantum resistant protocol, and much better groups.

**Published:** Nov 25, 2023

- [Quick start: control SimpleX Chat mobile app from CLI](#⚡️-quick-start-use-profiles-in-SimpleX-Chat-mobile-from-desktop-app)
- [What's the problem](#whats-the-problem)
- [Why didn't we use some existing solution?](#why-didnt-we-use-some-existing-solution)

## ⚡️ Quick start: use profiles in SimpleX Chat mobile from desktop app

## What's the problem?

Currently you cannot use the same SimpleX Chat profile on mobile and desktop devices. Even though you can use small groups instead of direct conversations as a workaround, it is quite inconvenient – read status and delivery receipts become much less useful.

So, we need a way to use the same profile on desktop as we use on mobile.

If SimpleX Chat profile was stored on the server, the problem would have been simpler - you can just connect to it from another device. But even in this case, accessing the conversation history without compromising the security of double ratchet end-to-end encryption is not really possible.

So we decided to implement the solution that is similar to what WhatsApp and WeChat did in early days - allowing a desktop device access profile on mobile via network. Unlike these big apps, we don't use the server to connect to mobile, but instead use the connection over the local network.

The downside of this approach is that mobile device has to be with you and connected to the same local network (and in case of iOS, the app has to be in the foreground as well). But the upside is that we the connection can be secure, and that you do not have to have a copy of your profiles on the desktop, which usually has lower security.

## Why didn't we use some existing solution?

While there are several existing protocols for remote access, all of them are vulnerable to spoofing and man 

 in many cases support of sending files and images is not very good, and sending videos and large files is simply impossible. There are currently these problems:

- the sender has to be online for file transfer to complete, once it was confirmed by the recipient.
- when the file is sent to the group, the sender will have to transfer it separately to each member, creating a lot of traffic.
- the file transfer is slow, as it is sent in small chunks - approximately 16kb per message.

As a result, we limited the supported size of files in the app to 8mb. Even for supported files, it is quite inefficient for sending any files to large groups.

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
