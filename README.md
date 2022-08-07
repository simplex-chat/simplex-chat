<img src="images/simplex-chat-logo.svg" alt="SimpleX logo" width="100%">

# SimpleX - the first messaging platform that has no user identifiers of any kind - 100% private by design!

[![build](https://github.com/simplex-chat/simplex-chat/actions/workflows/build.yml/badge.svg?branch=stable)](https://github.com/simplex-chat/simplex-chat/actions/workflows/build.yml)
[![GitHub downloads](https://img.shields.io/github/downloads/simplex-chat/simplex-chat/total)](https://github.com/simplex-chat/simplex-chat/releases)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)
[![Follow on Twitter](https://img.shields.io/twitter/follow/SimpleXChat?style=social)](https://twitter.com/SimpleXChat)
[![Join on Reddit](https://img.shields.io/reddit/subreddit-subscribers/SimpleXChat?style=social)](https://www.reddit.com/r/SimpleXChat)

[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apple_store.svg" alt="iOS app" height="42">](https://apps.apple.com/us/app/simplex-chat/id1605771084)
&nbsp;
[![Android app](https://github.com/simplex-chat/.github/blob/master/profile/images/google_play.svg)](https://play.google.com/store/apps/details?id=chat.simplex.app)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/f_droid.svg" alt="F-Droid" height="41">](https://app.simplex.chat)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/testflight.png" alt="iOS TestFlight" height="41">](https://testflight.apple.com/join/DWuT2LQu)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apk_icon.png" alt="APK" height="41">](https://github.com/simplex-chat/website/raw/master/simplex.apk)

- 🖲 Protects your messages and metadata - who you talk to and when.
- 🔐 Double ratchet end-to-end encryption, with additional encryption layer.
- 📱 Mobile apps for Android ([Google Play](https://play.google.com/store/apps/details?id=chat.simplex.app), [APK](https://github.com/simplex-chat/website/raw/master/simplex.apk)) and [iOS](https://apps.apple.com/us/app/simplex-chat/id1605771084).
- 🚀 [TestFlight preview for iOS](https://testflight.apple.com/join/DWuT2LQu) with the new features 1-2 weeks earlier - **limited to 10,000 users**!
- 🖥 Available as a terminal (console) app / CLI on Linux, MacOS, Windows.

## Contents

- [Why privacy matters](#why-privacy-matters)
- [SimpleX approach to privacy and security](#simplex-approach-to-privacy-and-security)
  - [Complete privacy](#complete-privacy-of-your-identity-profile-contacts-and-metadata)
  - [Protection against spam and abuse](#the-best-protection-against-spam-and-abuse)
  - [Ownership and security of your data](#complete-ownership-control-and-security-of-your-data)
  - [Users own SimpleX network](#users-own-simplex-network)
- [Frequently asked questions](#frequently-asked-questions)
- [News and updates](#news-and-updates)
- [Make a private connection](#make-a-private-connection)
- [Quick installation of a terminal app](#zap-quick-installation-of-a-terminal-app)
- [SimpleX Platform design](#simplex-platform-design)
- [Privacy: technical details and limitations](#privacy-technical-details-and-limitations)
- [For developers](#for-developers)
- [Roadmap](#roadmap)
- [Help us pay for 3rd party security audit](#help-us-pay-for-3rd-party-security-audit)
- [Disclaimer, License](#disclaimer)

## Why privacy matters

Everyone should care about privacy and security of their communications - innocuous conversations can put you in danger even if there is nothing to hide.

One of the most shocking stories is the experience of [Mohamedou Ould Salahi](https://en.wikipedia.org/wiki/Mohamedou_Ould_Slahi) that he wrote about in his memoir and that is shown in The Mauritanian movie. He was put into Guantanamo camp, without trial, and was tortured there for 15 years after a phone call to his relative in Afghanistan, under suspicion of being involved in 9/11 attacks, even though he lived in Germany for the 10 years prior to the attacks.

It is not enough to use an end-to-end encrypted messenger, we all should use the messengers that protect the privacy of our personal networks - who we are connected with.

## SimpleX approach to privacy and security

### Complete privacy of your identity, profile, contacts and metadata

**Unlike any other existing messaging platform, SimpleX has no identifiers assigned to the users** - not even random numbers. This protects the privacy of who are you communicating with, hiding it from SimpleX platform servers and from any observers. [Read more](./docs/SIMPLEX.md#full-privacy-of-your-identity-profile-contacts-and-metadata).

### The best protection against spam and abuse

As you have no identifier on SimpleX platform, you cannot be contacted unless you share a one-time invitation link or an optional temporary user address. [Read more](./docs/SIMPLEX.md#the-best-protection-against-spam-and-abuse).

### Complete ownership, control and security of your data

SimpleX stores all user data on client devices, the messages are only held temporarily on SimpleX relay servers until they are received. [Read more](./docs/SIMPLEX.md#complete-ownership-control-and-security-of-your-data).

### Users own SimpleX network

You can use SimpleX with your own servers and still communicate with people using the servers that are pre-configured in the apps or any other SimpleX servers. [Read more](./docs/SIMPLEX.md#users-own-simplex-network).

## Frequently asked questions

1. _How SimpleX can deliver messages without any user identifiers?_ See [v2 release annoucement](./blog/20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers) explaining how SimpleX works.

2. _Why should I not just use Signal?_ Signal is a centralised platform that uses phone numbers to identify its users and their contacts. It means that while the content of your messages on Signal is protected with robust end-to-end encryption, there is a large amount of meta-data visible to Signal - who you talk with and when.

3. _How is it different from Matrix, Session, Ricochet, Cwtch, etc., that also don't require user identites?_ Although these platforms do not require a _real identity_, they do rely on anonymous user identities to deliver messages – it can be, for example, an identity key or a random number. Using a persistent user identity, even anonymous, creates a risk that user's connection graph becomes known to the observers and/or service providers, and it can lead to de-anonymizing some users. If the same user profile is used to connect to two different people via any messenger other than SimpleX, these two people can confirm if they are connected to the same person - they would use the same user identifier in the messages. With SimpleX there is no meta-data in common between your conversations with different contacts - the quality that no other messaging platform has.

## News and updates

Selected updates:

[Jul 23, 2022. v3.1-beta: access via Tor SOCKS5 proxy in terminal app, join and leave chat groups in mobile apps, up to 90x reduced battery and traffic usage, docker configurations for self-hosted servers](./blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.md)

[Jul 11, 2022. v3.0: instant push notifications for iOS, e2e encrypted WebRTC audio/video calls, chat database export/import, privacy and performance improvements](./blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md)

[May 11, 2022. v2.0 released - sending images and files in mobile apps](./blog/20220511-simplex-chat-v2-images-files.md)

[Mar 08, 2022 Mobile apps for iOS and Android released](./blog/20220308-simplex-chat-mobile-apps.md)

[Jan 12, 2022. SimpleX v1 released: the only messaging and application platform without user identities](./20220112-simplex-chat-v1-released.md)

[All updates](./blog)

## Make a private connection

You need to share a link or scan a QR code (in person or during a video call) to make a connection and start messaging.

The channel through which you share the link does not have to be secure - it is enough that you can confirm who sent you the message and that your SimpleX connection is established.

<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/conversation.png" alt="Make a private connection" width="594" height="360">

## :zap: Quick installation of a terminal app

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

or to install v3.1.0-beta.0 that supports accessing SimpleX servers via SOCKS5 proxy:

```
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash -s -- v3.1.0-beta.0
```

Once the chat client is installed, simply run `simplex-chat` from your terminal.

![simplex-chat](./images/connection.gif)

Read more about [installing and using the terminal app](./docs/CLI.md).

## SimpleX Platform design

SimpleX is a client-server network with a unique network topology that uses redundant, disposable message relay nodes to asynchronously pass messages via unidirectional (simplex) message queues, providing recipient and sender anonymity.

Unlike P2P networks, all messages are passed through one or several server nodes, that do not even need to have persistence. In fact, the current [SMP server implementation](https://github.com/simplex-chat/simplexmq#smp-server) uses in-memory message storage, persisting only the queue records. SimpleX provides better metadata protection than P2P designs, as no global participant identifiers are used to deliver messages, and avoids [the problems of P2P networks](./docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols).

Unlike federated networks, the server nodes **do not have records of the users**, **do not communicate with each other** and **do not store messages** after they are delivered to the recipients. There is no way to discover the full list of servers participating in SimpleX network. This design avoids the problem of metadata visibility that all federated networks have and better protects from the network-wide attacks.

Only the client devices have information about users, their contacts and groups.

See [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) for more information on platform objectives and technical design.

See [SimpleX Chat Protocol](./docs/protocol/simplex-chat.md) for the format of messages sent between chat clients over [SimpleX Messaging Protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md).

## Privacy: technical details and limitations

SimpleX Chat is a work in progress – we are releasing improvements as they are ready. You have to decide if the current state is good enough for your usage scenario.

What is already implemented:

1. Instead of user profile identifiers used by all other platforms, even the most private ones, SimpleX uses pairwise per-queue identifiers (2 addresses for each unidirectional message queue, with an optional 3rd address for push notificaitons on iOS, 2 queues in each connection between the users). It makes observing the network graph on the application level more difficult, as for `n` users there can be up to `n * (n-1)` message queues.
2. End-to-end encryption in each message queue using [NaCl cryptobox](https://nacl.cr.yp.to/box.html). This is added to allow redundancy in the future (passing each message via several servers), to avoid having the same ciphertext in different queues (that would only be visible to the attacker if TLS is compromised). The encryption keys used for this encryption are not rotated, instead we are planning to rotate the queues. Curve25519 keys are used for key negotiation.
3. [Double ratchet](https://signal.org/docs/specifications/doubleratchet/) end-to-end encryption in each bi-directional conversation between two users (or group members). This is the same algorithm that is used in Signal and many other messaging apps; it provides OTR messaging with forward secrecy (each message is encrypted by its own ephemeral key), break-in recovery (the keys are frequently re-negotiated as part of the message exchange). Two pairs of Curve448 keys are used for the initial key agreement, initiating party passes these keys via the connection link, accepting side - in the header of the confirmation message.
4. Additional layer of encryption using NaCL cryptobox for the messages delivered from the server to the recipient. This layer avoids having any ciphertext in common between sent and received traffic of the server inside TLS (and there are no identifiers in common as well).
5. Several levels of content padding to the fixed size - server blocks inside TLS and message content inside each of 3 encrypted envelopes are padded to a constant size, to prevent any correlation by message size.
6. Starting from v2 of SMP protocol (the current version is v4) all message metadata, including the time message was received by the server (rounded to a second) is sent to the recipients inside an encrypted envelope, so even if TLS is compromised it cannot be observed.
7. Only TLS 1.2/1.3 are allowed for client-server connections, limited to cryptographic algorithms: CHACHA20POLY1305_SHA256, Ed25519/Ed448, Curve25519/Curve448.
8. To protect against replay attacks SimpleX servers require [tlsunique channel binding](https://www.rfc-editor.org/rfc/rfc5929.html) as session ID in each client command signed with per-queue ephemeral key.

We plan to add soon:

1. Access to messaging servers via Tor. Currently it is supported only for [terminal CLI clients](./docs/CLI.md); mobile clients access servers via public Internet, and the servers can observe IP addresses of the clients. Depending which platform you use, you might be able to configure access via Tor independently. The servers provided by SimpleX Chat do not correlate users by or log IP addresses, and you can use your own servers, but in some scenarios that may be not a sufficient level of privacy.
2. Message queue rotation. Currently the queues created between two users are used until the contact is deleted, providing a long-term pairwise identifiers of the conversation. We are planning to add queue rotation to make these identifiers termporary and rotate based on some schedule TBC (e.g., every X messages, or every X hours/days).
3. Local database encryption. Currently the local chat database stored on your device is not encrypted.
4. Message "mixing" - adding latency to message delivery, to protect against traffic correlation by message time.
5. Independent implementation audit.

## For developers

We plan that the SimpleX platform will grow into the platform supporting any distributed Internet application. This will allow you to build any service that people can access via chat, with custom web-based UI widgets that anybody with basic HTML/CSS/JavaScript knowledge can create in a few hours.

You already can:

- use SimpleX Chat library to integrate chat functionality into your apps.
- use SimpleX Chat bot templates in Haskell to build your own chat bot services (TypeScript SDK is coming soon).

If you are considering developing with SimpleX platform please get in touch for any advice and support.

## Roadmap

- ✅ Easy to deploy SimpleX server with in-memory message storage, without any dependencies.
- ✅ Terminal (console) client with groups and files support.
- ✅ One-click SimpleX server deployment on Linode.
- ✅ End-to-end encryption using double-ratchet protocol with additional encryption layer.
- ✅ Mobile apps v1 for Android and iOS.
- ✅ Private instant notifications for Android using background service.
- ✅ Haskell chat bot templates.
- ✅ v2.0 - supporting images and files in mobile apps.
- ✅ Manual chat history deletion.
- ✅ End-to-end encrypted WebRTC audio and video calls via the mobile apps.
- ✅ Privacy preserving instant notifications for iOS using Apple Push Notification service.
- ✅ Chat database export and import
- 🏗 Chat groups in mobile apps (in progress).
- 🏗 Connecting to messaging servers via Tor (in progress).
- 🏗 Chat server and TypeScript client SDK to develop chat interfaces, integrations and chat bots (in progress).
- Chat database encryption.
- Disappearing messages, with mutual agreement.
- Web widgets for custom interactivity in the chats.
- SMP protocol improvements:
  - SMP queue redundancy and rotation.
  - Message delivery confirmation.
  - Supporting the same profile on multiple devices.
- Privacy-preserving identity server for optional DNS-based contact/group addresses to simplify connection and discovery, but not used to deliver messages:
  - keep all your contacts and groups even if you lose the domain.
  - the server doesn't have information about your contacts and groups.
- Channels server for large groups and broadcast channels.
- Media server to optimize sending large files to groups.

## Help us pay for 3rd party security audit

I will get straight to the point: I ask you to support SimpleX Chat with donations.

We are prioritizing users privacy and security - it would be impossible without your support we were lucky to have so far.

We are planning a 3rd party security audit for the app, and it would hugely help us if some part of this $20,000+ expense could be covered with donations.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

If you are already using SimpleX Chat, or plan to use it in the future when it has more features, please consider making a donation - it will help us to raise more funds. Donating any amount, even the price of the cup of coffee, would make a huge difference for us.

It is possible to donate via:

- [GitHub](https://github.com/sponsors/simplex-chat) - it is commission-free for us.
- [OpenCollective](https://opencollective.com/simplex-chat) - it charges a commission, and also accepts donations in crypto-currencies.
- [Monero wallet](monero:8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt): 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt

Thank you,

Evgeny

SimpleX Chat founder

## Disclaimer

[SimpleX protocols and security model](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) was reviewed and had many improvements in v1.0.0; we are currently arranging for the independent implementation audit.

You are likely to discover some bugs - we would really appreciate if you use it and let us know anything that needs to be fixed or improved.

## License

[AGPL v3](./LICENSE)

[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apple_store.svg" alt="iOS app" height="42">](https://apps.apple.com/us/app/simplex-chat/id1605771084)
&nbsp;
[![Android app](https://github.com/simplex-chat/.github/blob/master/profile/images/google_play.svg)](https://play.google.com/store/apps/details?id=chat.simplex.app)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/f_droid.svg" alt="F-Droid" height="41">](https://app.simplex.chat)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/testflight.png" alt="iOS TestFlight" height="41">](https://testflight.apple.com/join/DWuT2LQu)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apk_icon.png" alt="APK" height="41">](https://github.com/simplex-chat/website/raw/master/simplex.apk)
