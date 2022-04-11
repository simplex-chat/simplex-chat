<img src="images/simplex-chat-logo.svg" alt="SimpleX logo" width="100%">

# SimpleX - the first messaging platform operating without user identifiers of any kind - 100% private by design!

[![GitHub build](https://github.com/simplex-chat/simplex-chat/workflows/build/badge.svg)](https://github.com/simplex-chat/simplex-chat/actions?query=workflow%3Abuild)
[![GitHub downloads](https://img.shields.io/github/downloads/simplex-chat/simplex-chat/total)](https://github.com/simplex-chat/simplex-chat/releases)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)
[![Follow on Twitter](https://img.shields.io/twitter/follow/SimpleXChat?style=social)](https://twitter.com/SimpleXChat)
[![Join on Reddit](https://img.shields.io/reddit/subreddit-subscribers/SimpleXChat?style=social)](https://www.reddit.com/r/SimpleXChat)

## SimpleX unique approach to privacy and security

### Full privacy of your identity, profile, contacts and metadata

**Unlike any other messaging platform, SimpleX, has no identifiers assigned to the users** - we do not use phone numbers (like Signal or WhatsApp), email addresses, usernames (like Telegram), public keys or even random numbers (like all other messengers) to identify our users - in fact, we do not even know how many users we have.

To deliver messages instead of user identifiers used by other platforms SimpleX uses the addresses of ephemeral message queues. Using SimpleX is like having several different email addresses or phone numbers to communicate with each (!) contact you have, never using the same address for more than one contact, but without the hassle of managing all these addresses. This approach protects the privacy of who are you communicating with, hiding it from SimpleX platform servers and any observers. You can further improve your privacy by configuring your network access to connect SimpleX servers via some overlay transport network, e.g. Tor.

### Protection against spam or abuse

As you would have no identifier on SimpleX platform, you cannot be contacted unless you share a one-time invitation link or an optional temporary user address. Even with the user address, while it makes it possible to send spam contact requests, you can change it or completely delete it without losing any of your connections.

### Complete ownership and control of your data

SimpleX stores all user data on client devices, the messages are only held temporarily on SimpleX relay serverss until they are received.

We use portable database format that can be used on all supported devices - we will soon add the ability to export the database from mobile apps so it can be used on another device.

### Nobody owns SimpleX network or hosts user accounts

You can use SimpleX with your own servers, and communicate with people using the servers we pre-configured in the apps or any other SimpleX servers. Unlike federated networks, like email, XMPP or Matrix, SimpleX servers do not store user accounts - they simply relay messages to the recipients, protecting the privacy of both senders and resipients - there are no identifiers or encrypted messages in common between sent and received traffic of the server, thanks to the additional encryption layer for delivered messages, so whoever is observing server traffic cannot see who is communicating with whom (other than by message time).

## Use SimpleX Chat

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
- 🔐 Double ratchet end-to-end encryption + 2 additional encryption layers.
- 📱 Mobile apps for Android ([Google Play](https://play.google.com/store/apps/details?id=chat.simplex.app), [APK](https://github.com/simplex-chat/website/raw/master/simplex.apk)) and [iOS](https://apps.apple.com/us/app/simplex-chat/id1605771084).
- 🚀 [TestFlight preview for iOS](https://testflight.apple.com/join/DWuT2LQu) with the new features 1-2 weeks earlier - **limited to 10,000 users**!
- 🖥 Available as a terminal (console) app / CLI on Linux, MacOS, Windows.

## Make a private connection

You need to share a link or scan a QR code (in person or during a video call) to make a connection and start messaging.

The channel through which you share the link does not have to be secure - it is enough that you can confirm who sent you the message and that your connection is established.

<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/conversation.png" alt="Make a private connection" width="594" height="360">

## What people say

...

## :zap: Quick installation of a terminal app

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Once the chat client is installed, simply run `simplex-chat` from your terminal.

![simplex-chat](./images/connection.gif)

Read more about installing and using the terminal app on [this page](./CLI.md).

## SimpleX Platform design

SimpleX is a client-server network that uses redundant, disposable message relay nodes to asynchronously pass messages via message queues, providing recipient and sender anonymity.

Unlike P2P networks, all messages are passed through one or several (for redundancy) servers, that do not even need to have persistence (in fact, the current [SMP server implementation](https://github.com/simplex-chat/simplexmq#smp-server) uses in-memory message storage, persisting only the queue records) - it provides better metadata protection than P2P designs, as no global participant identifier is required, and avoids many [problems of P2P networks](./simplex.md#comparison-with-p2p-messaging-protocols).

Unlike federated networks, the participating server nodes **do not have records of the users**, **do not communicate with each other** and **do not store messages** after they are delivered to the recipients, and there is no way to discover the full list of participating servers. SimpleX network design avoids the problem of metadata visibility that federated networks have and better protects the network, as servers do not communicate with each other. Each server node provides unidirectional "dumb pipes" to the users, that do access authorization without authenticating the users, on a per-resource basis, having no knowledge of the the users or their contacts. Each message queue is assigned two epheneral Ed448 keys - one for receiver and one for sender - and each queue access is authorized with a signature created using a respective key's private counterpart.

The routing of messages relies on the data stored in client devices about which disposbale queues are used for user's contacts and groups.

See [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more information on platform objectives and technical design.

## News and updates

[Apr 04, 2022. Instant notifications for SimpleX Chat mobile apps](./blog/20220404-simplex-chat-instant-notifications.md). We would really appreciate any feedback on the design we are implementing.

[Mar 08, 2022 Mobile apps for iOS and Android released](./blog/20220308-simplex-chat-mobile-apps.md)

[Feb 14, 2022. SimpleX Chat: join our public beta for iOS](./blog/20220214-simplex-chat-ios-public-beta.md)

[All updates](./blog)

## Roadmap

1. Mobile app v2 - supporting files, images, groups, instant notifications, etc. (in progress).
2. SMP protocol improvements:
   - SMP queue redundancy and rotation.
   - Message delivery confirmation.
   - Support multiple devices.
3. Privacy-preserving identity server for optional DNS-based contact/group addresses to simplify connection and discovery, but not used to deliver messages:
   - keep all your contacts and groups even if you lose the domain.
   - the server doesn't have information about your contacts and groups.
4. Media server to optimize sending large files to groups.
5. Channels server for large groups and broadcast channels.

## Disclaimer

[SimpleX protocols and security model](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) was reviewed and had many improvements in v1.0.0; we are currently arranging for the independent implementation audit.

You are likely to discover some bugs - we would really appreciate if you use it and let us know anything that needs to be fixed or improved.

## License

[AGPL v3](./LICENSE)
