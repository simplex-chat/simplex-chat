# SimpleX announces SimpleX Chat mobile apps for iOS and Android

**Published:** March 8, 2022

## Simplex Chat is the first chat platform that is 100% private by design - SimpleX no access to your connections graph

We have now released iPhone and Android apps to Apple AppStore and Google Play Store, APK for Android is also available for direct download.

Thanks a lot to everybody who helped testing it!

The apps uses the same same core code as our terminal app, that was used and stabilized over a long time, and it provides the same level of privacy and security that has been available since the release of v1 earlier this year:
- [double-ratchet](https://www.signal.org/docs/specifications/doubleratchet/) E2E encryption.
- separate keys for each contact.
- additional layer of E2E encryption in each message queue (to prevent traffic correlation when multiple queues are used in a conversation - something we plan later this year).
- additional encryption of messages delivered from servers to recipients (also to prevent traffic correlation).

You can read more technical details in our recent [v1 announcement](https://github.com/simplex-chat/simplex-chat/blob/stable/blog/20220112-simplex-chat-v1-released.md).

## Install the apps and make a private connection!

Once you install the app, you can connect to anybody:

1. Create your local chat profile - it is not shared with SimpleX servers, it is local to your devices, and it will be shared with your contacts when you connect.
2. To make a private connection, you need to create a one-time connection link / QR code via "Add contact" button in the app. You can either show the QR code to your contact in person or via a video call - this is the most secure way to create a connection - or you can share the link via any other channel - only one user can connect via this link.
3. Once another user scans the QR code or opens the app via the link (they also should create their profile first) the connection will be created and you can send e2e encrypted messages privately, without anybody knowing you are connected.

## New features and improvements that are coming soon

- push notification server. Currently the apps load messages in the background periodically, that can be quite infrequent on iOS if you don't open the app regularly. With push notifications you would know about the new messages instantly.
- configuring your servers in the apps - this will be released this week, both for iOS and Android. By default the apps are using SimpleX Chat servers, but you will be able to configure your own and still be connected to other users who use our app with our servers.
- user profile images.
- e2e encrypted audio and video calls via WebRTC
- sending images and files - image preview will be sent via the servers, so it can be asynchronous, and large files/full resolution images via WebRTC, so both devices will have to be online.
- "reply to message" - feature allowing you to quote the message you are replying to.

Please let us know what else you think is important and any bugs you find.

## What is SimpleX?

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter.

We aim to provide the best possible protection of messages and metadata. Today there is no messaging application that works without global user identities, so we believe we provide better metadata privacy than alternatives. SimpleX is designed to be truly distributed with no central server, and without any global user identities. This allows for high scalability at low cost, and also makes it virtually impossible to snoop on the network graph.

The first application built on the platform is Simplex Chat, which is available for terminal (command line in Windows/Mac/Linux) and as iOS public beta - with Android app coming in a few weeks. The platform can easily support a private social network feed and a multitude of other services, which can be developed by the Simplex team or third party developers.

SimpleX also allows people to host their own servers to have control of their chat data. SimpleX servers are exceptionally lightweight and require a single process with the initial memory footprint of under 20 Mb, which grows as the server adds in-memory queues (even with 10,000 queues it uses less than 50Mb, not accounting for messages). It should be considered though that while self-hosting the servers provides more control, it may reduce meta-data privacy, as it is easier to correlate the traffic of servers with small number of messages coming through.

Further details on platform objectives and technical design are available [in SimpleX platform overview](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).
