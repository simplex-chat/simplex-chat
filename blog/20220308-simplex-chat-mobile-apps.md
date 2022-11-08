---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat mobile apps for iOS and Android"
date: 2022-03-08
preview: Brand new mobile apps with battle-tested Haskell core.
permalink: "/blog/20220308-simplex-chat-mobile-apps.html"
---

# SimpleX announces SimpleX Chat mobile apps for iOS and Android

**Published:** March 8, 2022

## SimpleX Chat is the first chat platform that is 100% private by design - it has no access to your connections graph

We have now released iPhone and Android apps to [Apple AppStore](https://apps.apple.com/us/app/simplex-chat/id1605771084) and [Google Play Store](https://play.google.com/store/apps/details?id=chat.simplex.app), [APK for Android](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk) is also available for direct download.

**Please note**: the current version is only supported on iPhone 8+ and on Android 10+ - we are planning to add support for iPad and older devices very soon, and we will announce it on our [Reddit](https://www.reddit.com/r/SimpleXChat/) and [Twitter](https://twitter.com/SimpleXChat) channels - please subscribe to follow our updates there.

## What is SimpleX

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter.

We aim to provide the best possible protection of messages and metadata. Today there is no messaging application that works without global user identities, so we believe we provide better metadata privacy than alternatives. SimpleX is designed to be truly distributed with no central server, and without any global user identities. This allows for high scalability at low cost, and also makes it virtually impossible to snoop on the network graph.

The first application built on the platform is Simplex Chat. The platform can easily support a private social network feed and a multitude of other services, which can be developed by the Simplex team or third party developers.

Further details on platform objectives and technical design are available [in SimpleX platform overview](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

## Why we are building it

Evgeny (SimpleX Chat founder): I have been working on this platform for a long time to provide a place where all people can communicate freely with each other, without fear of persecution because of what they said and who they are connected with. Not sharing information about your connections is very important, particularly for people living in oppressive regimes. Because of the terrible conflict between Russia and Ukraine, people of both countries – I have friends and family there – could be at risk when sharing their opinions or just from being connected to people who were prosecuted. Every messenger app that knows who you are can end up sharing all of your connections with undesirable third parties, either as a result of a court order or as a result of attack - so even Signal, which has strong encryption, cannot protect your connection graph. I hope our messenger can help people living in the oppressive regimes to express their opinions without fear and risk of prosecution.

## Huge thanks to our testers!

Thanks a lot to everybody who helped testing and improving the apps!

If you have a [TestFlight version](https://testflight.apple.com/join/DWuT2LQu) installed you can continue using it.

We plan to keep it as stable as we can, and it will give you access to all new features 1-2 weeks earlier - it's limited to 10,000 users, so you can grab it while it's available. You can still communicate with people who use a public version – we are committed to maintaining backwards compatibility.

You can always migrate from a public App Store version to a TestFlight version. The opposite migration - from TestFlight to public version - is only possible when we have the same app versions released, as there are usually some database migrations that cannot be reversed. To migrate to public version you have to disable automatic updates on TestFlight, wait until public version catches up and then install it from App Store. In any case, it is safe installing the public version, but it might crash if you have a newer version from TestFlight - in this case you just need to re-install the app from TestFlight and install App Store version a bit later - you would not lose any of your data.

## It's not all new - our core code has been used for a long time by a few thousand people in our terminal app.

The apps use the same core code as our terminal app, that was used and stabilized over a long time, and it provides the same level of privacy and security that has been available since the release of v1 earlier this year:

- [double-ratchet](https://www.signal.org/docs/specifications/doubleratchet/) E2E encryption.
- separate keys for each contact.
- additional layer of E2E encryption in each message queue (to prevent traffic correlation when multiple queues are used in a conversation - something we plan later this year).
- additional encryption of messages delivered from servers to recipients (also to prevent traffic correlation).

You can read more technical details in our recent [v1 announcement](./20220112-simplex-chat-v1-released.md).

A big thank you to [@angerman](https://github.com/angerman) for making it possible to compile our Haskell code to mobile platforms and getting it approved on app stores - it has been a non-trivial project, and it is still ongoing.

## Install the apps and make a private connection!

Once you install the app, you can connect to anybody:

1. Create your local chat profile - it is not shared with SimpleX servers, it is local to your devices, and it will be shared with your contacts when you connect.
2. To make a private connection, you need to create a one-time connection link / QR code via "Add contact" button in the app. You can either show the QR code to your contact in person or via a video call - this is the most secure way to create a connection - or you can share the link via any other channel - only one user can connect via this link.
3. Once another user scans the QR code or opens the app via the link (they also should create their profile first) the connection will be created and you can send e2e encrypted messages privately, without anybody knowing you are connected.

## New features and improvements that are coming soon

- push notification server. Currently the apps load messages in the background periodically, that can be quite infrequent on iOS if you don't open the app regularly. With push notifications you would know about the new messages instantly.
- e2e encrypted audio and video calls via WebRTC.
- export and import of the chat database.
- "reply to message" - feature allowing you to quote the message you are replying to.
- localization - we will let you know once you can contribute the translations to your languages.
- configuring your servers in the apps - this will be released this week, both for iOS and Android. By default the apps are using SimpleX Chat servers, but you will be able to configure your own and still be connected to other users who use our app with our servers.
- user profile images.
- sending images and files - image preview will be sent via the servers, so it can be asynchronous, and large files/full resolution images via WebRTC, so both devices will have to be online.

Please let us know what else you think is important and if you find any bugs.
