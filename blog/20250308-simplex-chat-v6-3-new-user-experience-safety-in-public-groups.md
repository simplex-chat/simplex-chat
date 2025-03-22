---
layout: layouts/article.html
title: "SimpleX Chat v6.3: new user experience and safety in public groups"
date: 2025-03-08
previewBody: blog_previews/20250308.html
image: images/20250308-captcha.png
imageBottom: true
permalink: "/blog/20250308-simplex-chat-v6-3-new-user-experience-safety-in-public-groups.html"
---

# SimpleX Chat v6.3: new user experience and safety in public groups

**Published:** Mar 8, 2025

**What's new in v6.3**:
- [preventing spam and abuse in public groups](#preventing-spam-and-abuse-in-public-groups).
- [group improvements](#group-improvements): [mention other members](#mention-other-members-and-get-notified-when-mentioned), [improved performance](#better-group-performance).
- [better chat navigation](#better-chat-navigation): [organize chats into lists](#organize-chats-into-lists) and [jump to found and forwarded messages](#jump-to-found-and-forwarded-messages).
- [privacy and security improvements](#privacy-and-security-improvements): [chat retention period](#set-message-retention-period-in-chats) and [private media file names](#private-media-file-names).

Also, we added Catalan interface language to Android and desktop apps, thanks to [our users and Weblate](https://github.com/simplex-chat/simplex-chat#help-translating-simplex-chat).

The last but not the least - [server builds are now reproducible](#reproducible-server-builds).

## What's new in v6.3

## Preventing spam and abuse in public groups

[We wrote before](./20250114-simplex-network-large-groups-privacy-preserving-content-moderation.md): as the network grows, it becomes more attractive to attackers. This release adds several features that reduce the possibility of attacks and abuse.

### Spam in groups that are listed in our group directory

There is no built-in group discovery in SimpleX Chat apps. Instead, we offer an experimental chat bot that allows to submit and to discover public groups. Not so long ago, spammers started sending messages via bots attempting to disrupt these groups.

We released several changes to the groups directory to protect from spam attacks.

**Optional captcha verification**

<img src="./images/20250308-captcha.png" width="288" class="float-to-right">

Group owners can enable the requirement to pass captcha challenge before joining the group. Captcha is generated in the directory bot itself, without any 3rd party servers, and is sent to the joining member. The new member must reply with the text in the image to be accepted to the group. While not a perfect protection, this basic measure complicates programming automatic bots to join public groups. It also provides a foundation to implement "knocking" - a conversation with dedicated group admins prior to joining the group. We plan to release support for knocking in March.

**Profanity filter for member names**

While group settings support giving all joining member an "observer" role - that is, without the right to send messages - the attackers tried spaming groups by joining and leaving. We added an optional filter for member names that group owners can enable for groups listed in directory - if a member name contains profanity, they will be rejected. Further improvements will be released in March as well.

The current SimpleX directory chatbot is a hybrid of [future chat relays](./20250114-simplex-network-large-groups-privacy-preserving-content-moderation.md#can-large-groups-scale) (a.k.a. super-peers) we are now developing to support large groups, and of a directory service that will be embedded in the app UI later this year, allowing to search and to discover public groups. Anybody is able to run their own directory bots now, and there will be possibility to use third party directories via the app UI in the future too.

Read more about [SimpleX group directory](../docs/DIRECTORY.md), how to submit your groups, and which groups we now accept. Currently we accept groups related to a limited list of topics that will be expanded once we have better moderation functionality for the groups.

### More power to group owners and moderators

This release includes two new features to help group moderators.

<img src="./images/20250308-reports.png" width="288" class="float-to-right">

**Private reports**

Group members can privately bring to group moderators attention specific messages and members, even if the group does not allow direct messages. The simply need to choose report in the message context menu and choose the report reason. This report will be visible to all group owners and moderators, but not to other members.

Group moderators can see all member reports in a separate view, and quickly find the problematic messages, making moderation much easier in public groups. These reports are private to groups, they are not sent to server operators.

Please note: in the groups listed in our directory, the directory bot acts as admin, so it will receive all reports as well.

**Acting on multiple members at once**

When attackers come, they often use multiple profiles. This version allows selecting multiple members at once and perform these actions on all selected members:
- switch members role between "observer" and "member".
- block and unblock members - this is a "shadow" block, so when you block multiple members who you believe are attackers, their messages will be blocked for all other members but not for them.
- remove members from the group.

The next version will also allow to remove members together with all messages they sent - for example, if a spam bot joined and sent a lot of spam, but nothing of value.

## Group improvements

### Mention other members and get notified when mentioned

<img src="./images/20250308-mentions.png" width="288" class="float-to-right">

This feature allows you to mention other members in the group in the usual way - type `@` character, and choose the member you want to mention from the menu. Even that there is no user accounts and persistent identities we made it work by referencing members by their random group ID that is also used for replies and all other interactions in the group.

You can also now switch message notifications in the group to "mentions only" mode. You will be notified only when you are mentioned in a message, or when somebody replies to your message. Simply choose "Mute" in the context menu of the group in the list of chats to switch group notifications to "mentions only" mode. After that you can choose "Mute all" to disable all notifications, including mentions.

### Better group performance

**Send messages faster**

We didn't reduce the required network traffic to send messages to large groups yet - your client still has to send message to each member individually. But we redesigned the process of sending a message, reducing temporary storage required to schedule the message for delivery by about 100x. This creates a significant storage saving - e.g, to send one message to a group of 1,000 members previously required ~20Mb, and now it is reduced to ~200kb. It also reduces the time and battery used to send a message.

**Faster group deletion**

When you leave the group, the app preserves a copy of all your communications in the group. You can choose to keep it or to delete it completely. This final group deletion was very slow prior to this release - depending on the number of groups on your device it could sometimes take several minutes.

This release solved this problem – the time it takes to delete the group is reduced to seconds, and even in cases when the app is terminated half-way, it either rolls back or completes, but it cannot leave the group in a partially deleted state. It improves both user experience and privacy, as gives you better control over your data.

## Better chat navigation

### Organize chats into lists

<img src="./images/20250308-lists.png" width="288" class="float-to-right">

It is a common feature in many messengers – it helps organizing your conversations.

The lists also show a blue mark when any chat in the list has new messages.

There are several preset lists: contacts, groups, private notes, business chats, favourite chats and also groups with member reports - the last list is automatically shown if members of any groups where you are the moderator or the owner sent private reports, until these reports are acted on or archived.

### Jump to found and forwarded messages

This version allows to quickly navigate from message in the search results to the point in the conversation when it was sent.

You can also navigate from the forwarded message (or from the message saved to private notes) to the original message in the chat where it was forwarded or saved from.

## Privacy and security improvements

### Set message retention period in chats

Before this version, you could enable message retention period for all chats in your profile. While helpful in some cases, many of us have conversations that we want to keep for a long time, and some other conversations that we want to remove quicker.

This version allows it - you can set different retention periods in different conversations. It can be 1 day, 1 week, 1 month or 1 year. We may allow custom retention time in the future.

### Private media file names

Previously there were scenarios when original media file names were preserved - e.g., when sending a video file or when forwarding any media file. The latter problem was worse, as media file name is generated automatically, and includes timestamp. So the same name could have been used to correlate files between conversations, as one of our users pointed out.

This version fixes this problem - media file name is now changed when forwarding it to match the time of forwarding, so no additional metadata is revealed.

Please also note:
- the apps remove metadata from all static images,
- iOS app removes metadata from videos, but android and desktop apps do not do it yet,
- animated images are sent as is,
- other file types are sent as is, and their names are left unchanged - we believe that for ordinary files their name is part of their content.

We plan further improvements to reduce metadata in files in the near future – please let us know what you believe is the most important to reduce first.

## Reproducible server builds

Starting from v6.3 server releases are reproducible!

**Why it is important**

With reproducible builds anybody can build servers from our code following the same process, and the build would produce identical binaries.

This also allows us to sign releases, as we reproduce GitHub builds ourselves and by signing them we attest that our builds resulted in identical binaries.

**How to reproduce builds**

You can reproduce our builds on Linux with x86 CPU in docker container - please follow the instructions [here](../docs/SERVER.md#reproduce-builds).

We are looking for support from open-source contributors or security researchers who would also reproduce and sign our releases.

**How to verify release signature**

Please see the instructions [here](../docs/SERVER.md#verifying-server-binaries).

## SimpleX network

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-and-security-technical-details-and-limitations).

[Frequently asked questions](../docs/FAQ.md).

Please also see our [website](https://simplex.chat).

## Please support us with your donations

Huge *thank you* to everybody who donated to SimpleX Chat!

Prioritizing users privacy and security, and also raising the investment, would have been impossible without your support and donations.

Also, funding the work to transition the protocols to non-profit governance model would not have been possible without the donations we received from the users.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds &mdash; any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/#please-support-us-with-your-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
