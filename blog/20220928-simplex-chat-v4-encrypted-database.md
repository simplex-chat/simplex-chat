---
layout: layouts/article.html
title: "SimpleX Chat v4.0 with encrypted database is released"
date: 2022-09-28
permalink: "/blog/20220928-simplex-chat-v4-encrypted-database.html"
---

# SimpleX Chat v4.0 with encrypted database is released

**Published:** Sep 28, 2022

## What's new in v4.0

- [encrypted chat database](#chat-database-encrypted-with-passphrase).
- [improved stability of creating new connections: more reliable groups, files and contacts](#improved-stability-of-creating-new-connections).
- [deleting files and media](#deleting-files-and-media)
- [support for self-hosted WebRTC STUN/TURN servers](#self-hosted-webrtc-stunturn-servers)
- [TypeScript SDK for integrating with SimpleX Chat](#typescript-sdk-for-integrating-with-simplex-chat) (e.g., chat bots or chat assistants).
- support animated images in Android app.
- disable notifications per contact / group in terminal app (it is already supported in mobile apps).

Other new features since v3.0:
- secret chat groups (see details in [v3.1 announcement](./20220808-simplex-chat-v3.1-chat-groups.md#secret-chat-groups) - they are fully decentralized, only their members know these groups exist.
- accessing messaging servers via Tor with support for .onion server addresses (see details in  [v3.1](./20220808-simplex-chat-v3.1-chat-groups.md#access-messaging-servers-via-tor) and [v3.2](./20220901-simplex-chat-v3.2-incognito-mode.md#using-onion-server-addresses-with-tor) announcements) - to protect users anonymity on the TCP transport level.
- Incognito mode - sharing a random profile name with each new contact, to completely eliminate any shared data between them (see details in [v3.2](./20220901-simplex-chat-v3.2-incognito-mode.md#incognito-mode) announcement).
- endless scrolling and search in chats.
- reduced Android APK size for direct download and in F-Droid repo from 200 to 50Mb!

[Implementation audit is arranged for October](#we-ask-you-to-help-us-pay-for-3rd-party-security-audit)!

### Chat database encrypted with passphrase

SimpleX Chat focus has always been on protecting message in delivery, not when they are stored on the device. This release changes it - now all the messages that you receive and send are stored on the device encrypted using SQLCipher 4.0.

If you are already using SimpleX Chat, your database will remain unencrypted until you enter the passphrase via the app settings. You have to remember passphrase you choose, as there is no way to recover it if it is lost.

By default the passphrase you enter will be stored securely on the device (in KeyChain on iOS or encrypted with the key stored in TPM, if available, on Android) - it is only accessible to the app, and only on one device. Storing passphrase is required for instant notifications to work. In this case, if you lose the passphrase, the app will continue to work, but you will not be able to change the passphrase and to migrate your user profile to another device.

For additional security of your messages, you also have the option to remove passphrase from the device for additional security. In this case you will need to enter the passphrase every time you start the app. The notifications will continue to work only while the app is in the background. For iOS it means that periodic and local notifications will work, but instant notifications will only show that there is a message available, but not message content or who it is from - you will need to open the app and enter the passphrase to see the messages. In this case, if you lose the passphrase, you will not be able to open the app or decrypt the database - so make sure you store it securely.

### Improved stability of creating new connections

Adding groups to SimpleX Chat made it much more useful, but because SimpleX groups are completely decentralized and for them to work each member should connect to each member, sometimes these connections failed - for example, because of bad network connection, - and the group became fragmented - some users did not receive all messages. That was more common with larger groups, as the number of required member connections is O(n^2) of the group size.

The reason for that problem was that many network operations required for group connections were not retried. This is now improved - any network 

### Deleting files and media

### Self-hosted WebRTC STUN/TURN servers

### TypeScript SDK for integrating with SimpleX Chat

## SimpleX platform

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](./20220723-simplex-chat-v3.1-tor-groups-efficiency.md#privacy-technical-details-and-limitations).

[How SimpleX is different from Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/README.md#frequently-asked-questions).

## We ask you to help us pay for 3rd party security audit

We have already signed the agreement and paid for the security audit!

It is planned for October, and if there are no major issues we will publish this report straight away, otherwise - once we fix them.

This is a major expense for use - over $20,000 - I would really appreciate if you could help us cover some part of this cost with the donations.

Our promise to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We will be establishing a legal framework this year to ensure that it doesn't change if the ownership of SimpleX Chat Ltd changes at any future point.

Please consider making a donation - it will help us to raise more funds. Donating any amount, even the price of the cup of coffee, would make a huge difference for us.

It is possible to donate via:

- [GitHub](https://github.com/sponsors/simplex-chat): it is commission-free for us.
- [OpenCollective](https://opencollective.com/simplex-chat): it also accepts donations in crypto-currencies, but charges a commission.
- Monero wallet: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Bitcoin wallet: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG

Thank you,

Evgeny

SimpleX Chat founder
