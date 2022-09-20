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

### Improved stability of creating new connections

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
