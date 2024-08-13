---
layout: layouts/article.html
title: "SimpleX Chat: vision and funding, v6.0 released with the new user experience and private message routing."
date: 2024-08-14
# image: images/20230422-video.png
# imageBottom: true
# previewBody: blog_previews/20230422.html
draft: true
permalink: "/blog/20240814-simplex-chat-vision-funding-v6-private-routing-new-user-experience.html"
---

# SimpleX network: vision and funding, v6.0 released with the new user experience and private message routing.

**Published:** Aug 14, 2024

[SimpleX Chat: vision and funding 2.0](#simplex-chat-vision-and-funding-20):
- [The past](#the-past).
- [The present](#the-present).
- [The future](#the-future).

[What's new in v6.0](#whats-new-in-v60):
- [Private message routing](#private-message-routing) - now enabled by default
- [New chat experience](#new-chat-experience)
  - [Connect to your friends faster](#connect-to-your-friends-faster).
  - [Archive contacts to chat later](#archive-contacts-to-chat-later).
  - [New way to start chat](#new-way-to-start-chat).
  - [Reachable chat toolbar](#reachable-chat-toolbar).
  - [Moderate like a pro](#moderate-like-a-pro).
  - [New chat themes](#new-chat-themes)<sup>*</sup>
  - [Increase font size](#increase-font-size)<sup>**</sup>.
- [New media options](#new-media-options)
  - [Play from the chat list](#play-from-the-chat-list).
  - [Blur for better privacy](#blur-for-better-privacy).
  - [Share from other apps](#share-from-other-apps)<sup>*</sup>.
- [Improved networking and reduced battery usage](#improved-networking-and-reduced-battery-usage)

\* New for iOS app.

\*\* Android and desktop apps.

## SimpleX Chat: vision and funding 2.0

### The past

...

### The present

...

### The future

...

## What's new in v6.0

v6.0 is one of our biggest releases ever, with a lot of focus on UX and stability improvements, and the new features the users asked for. 

### Private message routing

The private message routing [we announced before](./20240604-simplex-chat-v5.8-private-message-routing-chat-themes.md) is now enabled for all users by default – it protects users IP addresses and sessions from the destination servers.

### New chat experience

#### Connect to your friends faster 

This version includes messaging protocol improvements that reduce twice the number of messages required for two users to connect. Not only it means connecting faster and using less traffic, this change allows to start sending messages sooner than before - so you would see "connecting" in the list of the chats for a much shorter time than before.

It will be improved further in the next version - you will be able to send messages straight after using the link, without waiting for your contact to be online.

#### Archive contacts to chat later

TODO image

Now you have two new options when deleting a conversation:
- only delete conversation, and archive contact - we will add archiving conversation without clearing it in the next version, as some users of our beta version asked.
- delete contact but keep the conversation.

Also, deleting a contact now requires double confirmation, so you are less likely to delete the contact accidentally.

#### New way to start chat

TODO image

Now when you tap pencil button, you will see a large New message sheet, that adds to the options you had before some new functions.

Old options:
- *Add contact* to create a new 1-time invitation link,
- *Scan / paste link*: to use the link you received, it can be 1-time invitation, a public SimpleX address, or a link to join the group.
- *Create group*

New options:
- Open archived chats.
- Accept pending contact requests.
- Connect to preset public addresses (we will add an option to add your own addresses there).
- Search for your contacts.

#### Reachable chat toolbar

TODO image

Like with the most innovative mobile browsers (e.g., Safari and Firefox), SimpleX Chat users now can use the app with one hand by moving the toolbar and search bar to the bottom of the screen, and the chat list ordered with the most recent conversation in the bottom. 

This layout is enabled by default, and you can disable it right from the list of chats when you install the new version if you prefer to use conventional UI.

But give it a try – our experience is that that after a day of using it, it seems to be the only right way. You can always toggle it in the Appearance settings.

#### New chat themes

We released the new themes [for Android and desktop apps](./20240604-simplex-chat-v5.8-private-message-routing-chat-themes.md) in the previous version, and now they are available for iOS too.

You can set different themes for different chat profiles you have, and for different conversations – it can help avoid mistakes about which conversation you are in.

Also, these themes are compatible between platforms, so you can import the theme created on Android into iOS app.

#### Moderate like a pro

TODO image

As much as we disagree with attacks on the freedom of speech on the society level – various groups must be able to express their opinions, be they far left, far right, or anybody in between – we also believe that in the small communities, community owners have full discretion to decide which content is allowed and which is not. But as communities grow, bad actors begin to join in order to disrupt, subvert and troll the conversations. So, the moderation tools are critical for small public communities to thrive. 

SimpleX Chat already has quite a few moderation tools available for community owners:
- Moderate individual messages.
- Set the default role of the new members to "observer" - they won't be able to send messages until you allow it. In addition to that, by enabling default messages for admins and owners only you can reach out to the new members and ask them some questions before allowing them to send messages.
- Block messages of a member for yourself only.
- Block members for all other members - only admins and group owners can do that.

With this version you can now select multiple messages at once and delete or moderate them, depending on your role in the community. The current version limits the number of messages that can be deleted to 20 - this limit will be increased to 200 messages in the next version.

Also, this version makes profile images of the blocked members blurred, to prevent the abuse via inappropriate profile images.

#### Increase font size

Android and desktop apps now allow to increast font size inside the app, without changing the system settings. Desktop in addition to that allows to zoom the whole screen - it can be helpful on some systems with a limited support of high density displays.

These settings can be changed via appearance settings.

### New media options

#### Play from the chat list

TODO image

Now you can interact with the media in the last message directly from the list of the chats.

This is very convenient – when somebody sends you a voice message or a video, they can be played directly from the list of chats, without opening a conversation. Similarly, an image can be opened, a file can be saved, and the link with preview can be opened in the browser.

And, in some circumstances, this is also more private, as you can interact with the media, without opening the whole conversation.

We will add the option to return missed calls from the chat list in the next version.

#### Blur for better privacy

You can set all images and videos to blur in your app, and unblur them on tap (or on hover in desktop app). The blur level can be set in Privacy and security settings.

#### Share from other apps

TODO image

Not much to brag about, as most iOS apps have it, and users expect it to be possible.

But iOS makes it much harder to develop the capability to share into the app than Android, so it's only in this version you can share images, videos, files and links into SimpleX Chat from other apps.

### Improved networking and reduced battery usage

This version includes the statistics of how your app communicates with all servers when sending and receiving messages and files. This information also includes the status of connection to all servers from which you receive messages - whether the connection is authorized to push messages from server to your device, and the share of these active connections.

Please note, that when you send a message to a group, your app has to send it to each member separately, so sent message statistics account for that - it may seem to be quite a large number if you actively participate in some large groups. Also, message counts not only include visible messages you receive and send, but also any service messages, reactions, message updates, message deletions, etc. - this is the correct reflection of how much traffic your app uses.

This information is only available to your device, we do not collect this information, even in the aggregate form.

While the main reason we added this information is to reduce traffic and battery usage, to be able to identify any cases of high traffic, this version already reduced a lot battery and traffic usage, as reported by several beta-version users.

## SimpleX network

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[Frequently asked questions](../docs/FAQ.md).

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
