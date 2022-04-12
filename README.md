<img src="images/simplex-chat-logo.svg" alt="SimpleX logo" width="100%">

# SimpleX - the first messaging platform that has no user identifiers of any kind - 100% private by design!

[![GitHub build](https://github.com/simplex-chat/simplex-chat/workflows/build/badge.svg)](https://github.com/simplex-chat/simplex-chat/actions?query=workflow%3Abuild)
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

## Why privacy of communications matter

Everyone should care about privacy and security of their communications - innocuous conversations can put you in danger even even if there is nothing to hide.

One of the most shocking stories is the experience of [Mohamedou Ould Slahi](https://en.wikipedia.org/wiki/Mohamedou_Ould_Slahi) that he wrote about in his memoir and that is shown in The Mauritanian movie. He was put into Guantanamo camp, without trial, and was tortured there for 15 years after a phone call to his relative in Afghanistan, under suspicion of being involved in 9/11 attacks, even though he lived in Germany for the 10 years prior to the attacks.

It is not enough to use an end-to-end encrypted messenger, we all should use the messengers that protect the privacy of our personal networks - who we are connected with.

## SimpleX unique approach to privacy and security

### Full privacy of your identity, profile, contacts and metadata

**Unlike any other existing messaging platform, SimpleX has no identifiers assigned to the users** - not even even random numbers. This protects the privacy of who are you communicating with, hiding it from SimpleX platform servers and from any observers. [Read more](./docs/SIMPLEX.md#full-privacy-of-your-identity-profile-contacts-and-metadata).

### The best protection against spam and abuse

As you have no identifier on SimpleX platform, you cannot be contacted unless you share a one-time invitation link or an optional temporary user address. [Read more](./docs/SIMPLEX.md#the-best-protection-against-spam-and-abuse).

### Complete ownership, control and security of your data

SimpleX stores all user data on client devices, the messages are only held temporarily on SimpleX relay servers until they are received. [Read more](./docs/SIMPLEX.md#complete-ownership-control-and-security-of-your-data).

### Users own SimpleX network

You can use SimpleX with your own servers and still communicate with people using the servers that are pre-configured in the apps or any other SimpleX servers. [Read more](./docs/SIMPLEX.md#users-own-simplex-network).

## For developers

We plan that the SimpleX platform will grow into the platform supporting any distributed Internet application. This will allow you to build any service that people can access via chat, with custom web-based UI widgets that anybody with basic HTML/CSS/JavaScript knowledge can create in a few hours.

You already can:

- use SimpleX Chat library to integrate chat functionality into your apps.
- use SimpleX Chat bot templates in Haskell to build your own chat bot services (TypeScript SDK is coming soon).

If you are considering developing with SimpleX platform please get in touch for any advice and support.

## News and updates

[Apr 04, 2022. Instant notifications for SimpleX Chat mobile apps](./blog/20220404-simplex-chat-instant-notifications.md). We would really appreciate any feedback on the design we are implementing.

[Mar 08, 2022 Mobile apps for iOS and Android released](./blog/20220308-simplex-chat-mobile-apps.md)

[Feb 14, 2022. SimpleX Chat: join our public beta for iOS](./blog/20220214-simplex-chat-ios-public-beta.md)

[All updates](./blog)

## Make a private connection

You need to share a link or scan a QR code (in person or during a video call) to make a connection and start messaging.

The channel through which you share the link does not have to be secure - it is enough that you can confirm who sent you the message and that your SimpleX connection is established.

<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/conversation.png" alt="Make a private connection" width="594" height="360">

## :zap: Quick installation of a terminal app

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Once the chat client is installed, simply run `simplex-chat` from your terminal.

![simplex-chat](./images/connection.gif)

Read more about [installing and using the terminal app](./docs/CLI.md).

## SimpleX Platform design

SimpleX is a client-server network with a unique network topology that uses redundant, disposable message relay nodes to asynchronously pass messages via unidirectional (simplex) message queues, providing recipient and sender anonymity.

Unlike P2P networks, all messages are passed through one or several server nodes, that do not even need to have persistence. In fact, the current [SMP server implementation](https://github.com/simplex-chat/simplexmq#smp-server) uses in-memory message storage, persisting only the queue records. SimpleX provides better metadata protection than P2P designs, as no global participant identifiers are used to deliver messages, and avoids [the problems of P2P networks](./docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols).

Unlike federated networks, the server nodes **do not have records of the users**, **do not communicate with each other** and **do not store messages** after they are delivered to the recipients. There is no way to discover the full list of servers participating in SimpleX network. This design avoids the problem of metadata visibility that all federated networks have and better protects from the network-wide attacks.

Only the client devices have information about users, their contacts and groups.

See [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more information on platform objectives and technical design.

## Roadmap

- ✅ Easy to deploy SimpleX server with in-memory message storage, without any dependencies.
- ✅ Terminal (console) client with groups and files support.
- ✅ One-click SimpleX server deployment on Linode.
- ✅ End-to-end encryption using double-ratchet protocol with additional encryption layer.
- ✅ Mobile apps v1 for Android and iOS.
- ✅ Private instant notifications for Android using background service.
- ✅ Haskell chat bot templates
- 🏗 Privacy preserving instant notifications for iOS using Apple Push Notification service (in progress).
- 🏗 Mobile app v2 - supporting files, images and groups etc. (in progress).
- 🏗 Chat server and TypeScript client SDK to develop chat interfaces, integrations and chat bots (in progress).
- Chat database portability and encryption.
- End-to-end encrypted audio and video calls via the mobile apps.
- Web widgets for custom interactivity in the chats.
- SMP protocol improvements:
  - SMP queue redundancy and rotation.
  - Message delivery confirmation.
  - Supporting the same profile on multiple devices.
- Privacy-preserving identity server for optional DNS-based contact/group addresses to simplify connection and discovery, but not used to deliver messages:
  - keep all your contacts and groups even if you lose the domain.
  - the server doesn't have information about your contacts and groups.
- Media server to optimize sending large files to groups.
- Channels server for large groups and broadcast channels.

## Disclaimer

[SimpleX protocols and security model](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) was reviewed and had many improvements in v1.0.0; we are currently arranging for the independent implementation audit.

You are likely to discover some bugs - we would really appreciate if you use it and let us know anything that needs to be fixed or improved.

## License

[AGPL v3](./LICENSE)
