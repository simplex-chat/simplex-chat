<img src="images/simplex-chat-logo.svg" alt="SimpleX logo" width="100%">

# SimpleX - the first messaging platform that has no user identifiers of any kind - 100% private by design!

[![build](https://github.com/simplex-chat/simplex-chat/actions/workflows/build.yml/badge.svg?branch=stable)](https://github.com/simplex-chat/simplex-chat/actions/workflows/build.yml)
[![GitHub downloads](https://img.shields.io/github/downloads/simplex-chat/simplex-chat/total)](https://github.com/simplex-chat/simplex-chat/releases)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)
[![Join on Reddit](https://img.shields.io/reddit/subreddit-subscribers/SimpleXChat?style=social)](https://www.reddit.com/r/SimpleXChat)
[![Follow on Mastodon](https://img.shields.io/mastodon/follow/108619463746856738?domain=https%3A%2F%2Fmastodon.social&style=social)](https://mastodon.social/@simplex)

[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apple_store.svg" alt="iOS app" height="42">](https://apps.apple.com/us/app/simplex-chat/id1605771084)
&nbsp;
[![Android app](https://github.com/simplex-chat/.github/blob/master/profile/images/google_play.svg)](https://play.google.com/store/apps/details?id=chat.simplex.app)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/f_droid.svg" alt="F-Droid" height="41">](https://app.simplex.chat)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/testflight.png" alt="iOS TestFlight" height="41">](https://testflight.apple.com/join/DWuT2LQu)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apk_icon.png" alt="APK" height="41">](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk)

- 🖲 Protects your messages and metadata - who you talk to and when.
- 🔐 Double ratchet end-to-end encryption, with additional encryption layer.
- 📱 Mobile apps for Android ([Google Play](https://play.google.com/store/apps/details?id=chat.simplex.app), [APK](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk)) and [iOS](https://apps.apple.com/us/app/simplex-chat/id1605771084).
- 🚀 [TestFlight preview for iOS](https://testflight.apple.com/join/DWuT2LQu) with the new features 1-2 weeks earlier - **limited to 10,000 users**!
- 🖥 Available as a terminal (console) app / CLI on Linux, MacOS, Windows.

**NEW**: Security audit by [Trail of Bits](https://www.trailofbits.com/about), the [new website](https://simplex.chat) and v4.2 released! [See the announcement](./blog/20221108-simplex-chat-v4.2-security-audit-new-website.md)

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
- [Join a user group](#join-a-user-group)
- [Translate the apps](#translate-the-apps)
- [Contribute](#contribute)
- [Help us with donations](#help-us-with-donations)
- [Disclaimers, Security contact, License](#disclaimers)

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

Recent updates:

[Feb 04, 2023. v4.5 released - with multiple user profiles, message draft, transport isolation and Italian interface](./blog/20230204-simplex-chat-v4-5-user-chat-profiles.md).

[Jan 03, 2023. v4.4 released - with disappearing messages, "live" messages, connection security verifications, GIFs and stickers and with French interface language](./blog/20230103-simplex-chat-v4.4-disappearing-messages.md).

[Dec 06, 2022. November reviews and v4.3 released - with instant voice messages, irreversible deletion of sent messages and improved server configuration](./blog/20221206-simplex-chat-v4.3-voice-messages.md).

[Nov 08, 2022. Security audit by Trail of Bits, the new website and v4.2 released](./blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

[Sep 28, 2022. v4.0: encrypted local chat database and many other changes](./blog/20220928-simplex-chat-v4-encrypted-database.md).

[All updates](./blog)

## Make a private connection

You need to share a link or scan a QR code (in person or during a video call) to make a connection and start messaging.

The channel through which you share the link does not have to be secure - it is enough that you can confirm who sent you the message and that your SimpleX connection is established.

<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/app1.png" alt="Make a private connection" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/arrow.png" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/app2.png" alt="Conversation" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/arrow.png" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/app3.png" alt="Video call" height="360">

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

See [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) for more information on platform objectives and technical design.

See [SimpleX Chat Protocol](./docs/protocol/simplex-chat.md) for the format of messages sent between chat clients over [SimpleX Messaging Protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md).

## Privacy: technical details and limitations

SimpleX Chat is a work in progress – we are releasing improvements as they are ready. You have to decide if the current state is good enough for your usage scenario.

What is already implemented:

1. Instead of user profile identifiers used by all other platforms, even the most private ones, SimpleX uses pairwise per-queue identifiers (2 addresses for each unidirectional message queue, with an optional 3rd address for push notificaitons on iOS, 2 queues in each connection between the users). It makes observing the network graph on the application level more difficult, as for `n` users there can be up to `n * (n-1)` message queues.
2. End-to-end encryption in each message queue using [NaCl cryptobox](https://nacl.cr.yp.to/box.html). This is added to allow redundancy in the future (passing each message via several servers), to avoid having the same ciphertext in different queues (that would only be visible to the attacker if TLS is compromised). The encryption keys used for this encryption are not rotated, instead we are planning to rotate the queues. Curve25519 keys are used for key negotiation.
3. [Double ratchet](https://signal.org/docs/specifications/doubleratchet/) end-to-end encryption in each conversation between two users (or group members). This is the same algorithm that is used in Signal and many other messaging apps; it provides OTR messaging with forward secrecy (each message is encrypted by its own ephemeral key), break-in recovery (the keys are frequently re-negotiated as part of the message exchange). Two pairs of Curve448 keys are used for the initial key agreement, initiating party passes these keys via the connection link, accepting side - in the header of the confirmation message.
4. Additional layer of encryption using NaCL cryptobox for the messages delivered from the server to the recipient. This layer avoids having any ciphertext in common between sent and received traffic of the server inside TLS (and there are no identifiers in common as well).
5. Several levels of content padding to frustrate message size attacks.
6. Starting from v2 of SMP protocol (the current version is v4) all message metadata, including the time when the message was received by the server (rounded to a second) is sent to the recipients inside an encrypted envelope, so even if TLS is compromised it cannot be observed.
7. Only TLS 1.2/1.3 are allowed for client-server connections, limited to cryptographic algorithms: CHACHA20POLY1305_SHA256, Ed25519/Ed448, Curve25519/Curve448.
8. To protect against replay attacks SimpleX servers require [tlsunique channel binding](https://www.rfc-editor.org/rfc/rfc5929.html) as session ID in each client command signed with per-queue ephemeral key.
9. To protect your IP address all SimpleX Chat clients support accessing messaging servers via Tor - see [v3.1 release announcement](./blog/20220808-simplex-chat-v3.1-chat-groups.md) for more details.
10. Local database encryption with passphrase - your contacts, groups and all sent and received messages are stored encrypted. If you used SimpleX Chat before v4.0 you need to enable the encryption via the app settings.
11. Transport isolation - different TCP connections and Tor circuits are used for traffic of different user profiles, optionally - for different contacts and group member connections.

We plan to add soon:

1. Automatic message queue rotation. Currently the queues created between two users are used until the queue is manually changed by the user or contact is deleted. We are planning to add automatic queue rotation to make these identifiers temporary and rotate based on some schedule TBC (e.g., every X messages, or every X hours/days).
2. Local files encryption. Currently the images and files you send and receive are stored in the app unencrypted, you can delete them via `Settings / Database passphrase & export`.
3. Message "mixing" - adding latency to message delivery, to protect against traffic correlation by message time.

## For developers

You can:

- use SimpleX Chat library to integrate chat functionality into your mobile apps.
- create chat bots and services in Haskell - see [simple](./apps/simplex-bot/) and more [advanced chat bot example](./apps/simplex-bot-advanced/).
- create chat bots and services in any language running SimpleX Chat terminal CLI as a local WebSocket server. See [TypeScript SimpleX Chat client](./packages/simplex-chat-client/) and [JavaScipt chat bot example](./packages/simplex-chat-client/typescript/examples/squaring-bot.js).
- run [simplex-chat terminal CLI](./docs/CLI.md) to execute individual chat commands, e.g. to send messages as part of shell script execution.

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
- ✅ Chat database export and import.
- ✅ Chat groups in mobile apps.
- ✅ Connecting to messaging servers via Tor.
- ✅ Dual server addresses to access messaging servers as v3 hidden services.
- ✅ Chat server and TypeScript client SDK to develop chat interfaces, integrations and chat bots (ready for announcement).
- ✅ Incognito mode to share a new random name with each contact.
- ✅ Chat database encryption.
- ✅ Automatic chat history deletion.
- ✅ Links to join groups and improve groups stability.
- ✅ Voice messages (with recipient opt-out per contact).
- ✅ Basic authentication for SMP servers (to authorize creating new queues).
- ✅ View deleted messages, full message deletion by sender (with recipient opt-in per contact).
- ✅ Block screenshots and view in recent apps.
- ✅ Advanced server configuration.
- ✅ Disappearing messages (with recipient opt-in per-contact).
- ✅ "Live" messages.
- ✅ Contact verification via a separate out-of-band channel.
- ✅ Multiple user profiles in the same chat database.
- ✅ Optionally avoid re-using the same TCP session for multiple connections.
- ✅ Preserve message drafts.
- 🏗 File server to optimize for efficient and private sending of large files.
- 🏗 Improved audio & video calls.
- 🏗 SMP queue redundancy and rotation (manual is supported).
- 🏗 Reduced battery and traffic usage in large groups.
- 🏗 Support older Android OS and 32-bit CPUs.
- Ephemeral/disappearing/OTR conversations with the existing contacts.
- Access password/pin (with optional alternative access password).
- Local app files encryption.
- Video messages.
- Improved navigation and search in the conversation (expand and scroll to quoted message, scroll to search results, etc.).
- Message delivery confirmation (with sender opt-in or opt-out per contact, TBC).
- Feeds/broadcasts.
- Web widgets for custom interactivity in the chats.
- Programmable chat automations / rules (automatic replies/forward/deletion/sending, reminders, etc.).
- Supporting the same profile on multiple devices.
- Desktop client.
- Privacy-preserving identity server for optional DNS-based contact/group addresses to simplify connection and discovery, but not used to deliver messages:
  - keep all your contacts and groups even if you lose the domain.
  - the server doesn't have information about your contacts and groups.
- Hosting server for large groups, communities and public channels.
- Message delivery relay for senders (to conceal IP address from the recipients' servers and to reduce the traffic).
- High capacity multi-node SMP relays.

## Join a user group

You can join an English-speaking group if you want to ask any questions: [#SimpleX-Group-2](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FQP8zaGjjmlXV-ix_Er4JgJ0lNPYGS1KX%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEApAgBkRZ3x12ayZ7sHrjHQWNMvqzZpWUgM_fFCUdLXwo%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22xWpPXEZZsQp_F7vwAcAYDw%3D%3D%22%7D)

There are also several groups in languages other than English, that we have the apps interface translated into. These groups are for testing, and asking questions to other SimpleX Chat users. We do not always answer questions there, so please ask them in one of the English-speaking groups.

- [\#SimpleX-DE](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FkIEl7OQzcp-J6aDmjdlQbRJwqkcZE7XR%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAR16PCu02MobRmKAsjzhDWMZcWP9hS8l5AUZi-Gs8z18%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22puYPMCQt11yPUvgmI5jCiw%3D%3D%22%7D) (German-speaking).
- [\#SimpleX-FR](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FvIHQDxTor53nwnWWTy5cHNwQQAdWN5Hw%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAPdgK1eBnETmgiqEQufbUkydKBJafoRx4iRrtrC2NAGc%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%221FyUryBPza-1ZFFE80Ekbg%3D%3D%22%7D) (French-speaking).
- [\#SimpleX-RU](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FXZyt3hJmWsycpN7Dqve_wbrAqb6myk1R%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAMFVIoytozTEa_QXOgoZFq_oe0IwZBYKvW50trSFXzXo%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22xz05ngjA3pNIxLZ32a8Vxg%3D%3D%22%7D) (Russian-speaking).
- [\#SimpleX-IT](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2F0weR-ZgDUl7ruOtI_8TZwEsnJP6UiImA%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAq4PSThO9Fvb5ydF48wB0yNbpzCbuQJCW3vZ9BGUfcxk%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22e-iceLA0SctC62eARgYDWg%3D%3D%22%7D) (Italian-speaking).

You can join these groups either by opening these links in the app or by opening them in a desktop browser and scanning QR code.

Join via the app to share what's going on and ask any questions!

## Translate the apps

Thanks to our users and [Weblate](https://hosted.weblate.org/engage/simplex-chat/), SimpleX Chat apps are translated to many other languages. Join our translators to help SimpleX grow faster!

Current interface languages:

English (development language)
German: [@mlanp](https://github.com/mlanp)
French: link to be added
Italian: [@unbranched](https://github.com/unbranched)
Russian: project team

Languages in progress: Chinese, Hindi, Japanese, Dutch and [many others](https://hosted.weblate.org/projects/simplex-chat/#languages). We will be adding more languages as some of the already added are completed – please suggest new languages and get in touch with us!

## Contribute

We would love to have you join the development! You can contribute to SimpleX Chat with:

- translate website homepage - there is a lot of content we would like to share, it would help to bring the new users.
- writing a tutorial or recipes about hosting servers, chat bot automations, etc.
- developing features - please connect to us via chat so we can help you get started.

## Help us with donations

Huge thank you to everybody who donated to SimpleX Chat!

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, would make a big difference for us.

It is possible to donate via:

- [GitHub](https://github.com/sponsors/simplex-chat) - it is commission-free for us.
- [OpenCollective](https://opencollective.com/simplex-chat) - it charges a commission, and also accepts donations in crypto-currencies.
- Monero address: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Bitcoin address: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- BCH address: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Ethereum address: 0x83fd788f7241a2be61780ea9dc72d2151e6843e2
- Solana address: 43tWFWDczgAcn4Rzwkpqg2mqwnQETSiTwznmCgA2tf1L
- please let us know, via GitHub issue or chat, if you want to create a donation in some other cryptocurrency - we will add the address to the list.

Thank you,

Evgeny

SimpleX Chat founder

## Disclaimers

[SimpleX protocols and security model](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) was reviewed, and had many breaking changes and improvements in v1.0.0.

The security audit was performed in October 2022 by [Trail of Bits](https://www.trailofbits.com/about), and most fixes were released in v4.2.0 – see [the announcement](./blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

SimpleX Chat is still a relatively early stage platform (the mobile apps were released in March 2022), so you may discover some bugs and missing features. We would really appreciate if you let us know anything that needs to be fixed or improved.

The default servers configured in the app are provided on the best effort basis. We are currently not guaranteeing any SLAs, although historically our servers had over 99.9% uptime each.

We have never provided or have been requested access to our servers or any information from our servers by any third parties. If we are ever requested to provide such access or information, we will be following due legal process.

We do not log IP addresses of the users and we do not perform any traffic correlation on our servers. If transport level security is critical you must use Tor or some other similar network to access messaging servers. We will be improving the client applications to reduce the opportunities for traffic correlation.

Please read more in [Terms & privacy policy](./PRIVACY.md).

## Security contact

To report a security vulnerability, please send us email to chat@simplex.chat. We will coordinate the fix and disclosure. Please do NOT report security vulnerabilities via GitHub issues.

Please treat any findings of possible traffic correlation attacks allowing to correlate two different conversations to the same user, other than covered in [the threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md#threat-model), as security vulnerabilities, and follow this disclosure process.

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
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apk_icon.png" alt="APK" height="41">](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk)
