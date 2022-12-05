---
layout: layouts/article.html
title: "SimpleX Chat v4.3 – voice messages, improved server management and conversation preferences"
date: 2022-12-06
preview: Voice messages are released!
permalink: "/blog/20221206-simplex-chat-v4.3-voice-messages.html"
---

# SimpleX Chat reviews and v4.3 released – with voice messages, irreversible deletion of sent messages and improved server configuration.

**Published:** Dec 6, 2022

## What's new in v4.3

- [instant voice messages!](#instant-voice-messages)
- [irreversible deletion of sent messages for all recipients](#irreversible-message-deletion)
- [improved server configuration and support for server passwords](#smp-servers-configuration-and-password)
- [privacy and security improvements](#privacy-and-security-improvements):
  - protect app screen in recent apps and prevent screenshots
  - improved privacy and security of SimpleX invitation links in the app
  - optional app data backup
  - optionally allow direct messages between group members

### Instant voice messages

TODO screenshot

Voice messages, unlike normal files, are sent instantly, in the existing connection with your contact and without acceptance from the recipient. For this reason we limited the size of voice messages to ~92.5kb (an equivalent of 6 messages), that limits the duration to 30 seconds on iOS and to ~42 seconds on Android (the size is different because of different encoders). The voice messages are sent in MP4AAC format that is natively supported both on iOS and on Android, and iOS can play voice message files outside of the app as well.

Users who do not want to receive voice messages can disable them, either globally, for all contacts, or for each contact independently. Groups have a separate policy that allows disabling voice messages (they are allowed by default). This policy can be set when creating a group or later, via Group preferences page.

### Irreversible message deletion

When you receive email, you have full confidence that the sender cannot delete their email from your mailbox after you have received it. And it seems correct – in the end, this is your device, and nobody should be able to delete any data from it.

Most existing messengers made an opposite decision – the senders can irreversibly delete their messages from the recipient device after the recipients received them, whether recipients agree to that or not. And it seems correct too - this is your message, you should be able to delete it, at least for a limited time, the fact that the message is on the recipient device doesn't change your ownership of this message.

While both these statements appear correct, at least to some people, they simply cannot both be correct at the same time, as they contradict each other - eitger one or both of them must be wrong. This appears to be a very polarising subject, and the polls I made yesterday only confirm it - the votes are split evenly.

You may want to be able to delete your messages even after they are received to protect your privacy and security, and you want the communication product you use to enforce it. But you may also have many reason to disagree to the deletion of messages for several different reasons:

- it may be a business context, and either your organisation policy or a compliance requirement that every single message you receive is preserved for some time.
- these messages may incriminate the sender and you want to protect yourself - e.g., the sender could insult or threaten you, and you want to have a proof of it.
- you may have paid for the the message, and you don't want it to suddenly disappear before you had chance to store it outside of the conversation.

Instead of taking any side in this chose, we decided to allow to change this behaviour either globally or separately for each contact or group. That makes SimpleX Chat unique, being suitable both for communication contexts where email is traditionally used, where recipients do not allow the deletion of the received messages, and in informal contexts, that would allow the senders to delete messages irreversibly, provided that the recipients agree to that.

In any case, the senders can never be 100% certain that the message is deleted from the recipient's device - recipient can be running a modified client that does not honour the conversation setting.

If irreversible message deletion is not allowed in the conversation, the senders can still mark their messages as deleted, and it would show "mark deleted" placeholder in the conversation. The recipients can then both reveal the content of the original message and fully delete it on their devices.

### SMP servers configuration and password

When you self-host your own SMP server you may want to make it public so that anybody can use it to receive messages via it. But several users wanted to host their private servers, so that only they and their friends can use them to receive the messages.

v4.0 of SMP server and the new version of the apps adds support for server password. It is chosen randomly when you initialize the new server, and if you already have a server you can change it. Anybody can still message you, it doesn't require knowing the password, and the links you share do not include it, but to be able to receive the messages you need a server address that includes password.

Via the new server configuration you can now test your server before you start using it and you can also share your server address via QR code, so that your friends or your team can use it too, without the need to copy paste the addresses.

You can read how to install and configure SMP server in [this guide](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SERVER.md).

### Privacy and security improvements

#### Protect app screen

It is enabled by default, but you can disable it via settings.

iOS app only hides the app screen in the receint apps, Android app in addition to that also prevents the screenshot.

This is not the security measure for the senders, and we made it optional, as the recipient could circumvent it anyway – this is to protect your app screen when you give your phone to somebody.

#### Privacy and security of SimpleX invitation links

Previously, when you sent somebody an invitation link, a contact address or a group link, they could open in the browser. As these links are quire large, it is not that easy to see if the page domain is replaced - it could have been used to replace the link - or what server the connection would go through.

This version instead of showing the full link shows a short description and it replaces a public web address with an internal URI scheme that the app uses (simplex:/) – such links open directly in the app. There is an option to show the full link, if you need it, and even to open it in the browser from the app, but in this case if this link is not on https://simplex.chat website it will show as read to highlight it.

### Optional app data backup

The previous version always backed up app data in the way it was configured by the system. Now you can override it from inside the app, preventing the backup even if it's enabled by the system settings. This version requires disabling it manually, we will make it disabled by default in the next release (v4.3.1)

### Direct messages between group members

The new version does not allow them by default, but it can be enabled by group owners in the group settings when the group is created or at any later moment.

## SimpleX platform

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](./20220723-simplex-chat-v3.1-tor-groups-efficiency.md#privacy-technical-details-and-limitations).

[How SimpleX is different from Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/README.md#frequently-asked-questions).

Please also see the information on our [new website](https://simplex.chat) - it also answers all these questions.

## Help us with donations

Huge thank you to everybody who donated to SimpleX Chat!

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

It is possible to donate via:

- [GitHub](https://github.com/sponsors/simplex-chat) - it is commission-free for us.
- [OpenCollective](https://opencollective.com/simplex-chat) - it charges a commission, and also accepts donations in many crypto-currencies.
- Monero address: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Bitcoin address: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Ethereum address: 0x83fd788f7241a2be61780ea9dc72d2151e6843e2
- please let us know, via GitHub issue or chat, if you want to make a donation in some other cryptocurrency - we will add the address to the list.

Thank you,

Evgeny

SimpleX Chat founder
