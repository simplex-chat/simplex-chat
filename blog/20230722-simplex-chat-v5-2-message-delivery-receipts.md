---
layout: layouts/article.html
title: "SimpleX Chat v5.2 released: message delivery receipts"
date: 2023-07-22
# image: images/20230523-reactions.png
# imageBottom: true
# previewBody: blog_previews/20230523.html
preview: TODO this is a placeholder for the release announcement
permalink: "/blog/20230722-simplex-chat-v5-2-message-delivery-receipts.html"
---

# SimpleX Chat v5.2 released: message delivery receipts

**Published:** July 22, 2023

What's new in v5.2:
- [message delivery receipts](#message-delivery-receipts) – with opt-out per contact!
- [filter favorite and unread chats](#filter-favorite-and-unread-chats).
- a bit [more usable groups](#more-usable-groups):
  - view full original replied to message in info.
  - share your address with your contacts and group members via your chat profile.
  - search for new and existing members.
- [stability improvements](#stability-improvements):
  - keep your connections working after restoring from backup.
  - restart app (Android) or reconnect servers (iOS).
  - more stable message delivery.
- other improvements:
  - [better disappearing messages](#better-disappearing-messages).
  - [prohibit message reactions](#prohibit-message-reactions).

Platform evolution:
- problems of public groups:
  - why not all messages are received.
  - how to cope with these problems.
  - when will public groups be more usable.
- why there are no read receipts and typing indication.

## What's new in v5.2

### Message delivery receipts

Most messaging apps have two ticks on sent messages – the first one to indicate that the message is accepted by the server, and the second – that it is delivered to the recipient's device. It confirms that the network is functioning, and that the message is not lost or delayed. SimpleX Chat now has this feature too.

In some cases it may compromise recipients' privacy, as they show that the recipient is online, so in SimpleX Chat sending delivery receipts is optional – it can be enabled or disabled separately for each chat profile and for each contact. For the new users and new chat profiles, this feature is enabled by default.

To avoid compromising your privacy, for all your existing chat profile sending delivery receipts is disabled. The first time you start the app after the update, you will be offered to enable it for all visible profiles, and it can be enabled later via Privacy and Security settings.

### Filter favorite and unread chats

You can now mark your contacts and groups as _favorite_, to be able to find them faster. With filter enabled, you will only see favorite chats, chats that contain unread messages and also any unaccepted group invitations and contact requests.

### More usable groups

Active SimpleX Chat users know how broken the current group experience is, and that we plan some major overhaul of the groups protocol – more on that below. In the meanwhile, we added some simple features that make groups in their current shape a bit more usable.

#### What is this in reply to?

A major problem is that you can see replies to the messages you've not seen - this would happen both when you've just joined the group, and didn't connect to most other members, and also when other new members join the group and didn't yet connect to you. While this problem cannot be solved without major changes, at least there is now ability to see the original message that was replied to via the message information.

#### How to connect to this member?

To simplify direct connections with other group members, you can now share your SimpleX address via your chat profile, and group members can send you a contact request even if the group does not allow direct messages.

#### How to find a member in the list?

Large member lists (and also the long lists of contacts, if you have many of them) become hard to scroll through, so now there is a search in both lists.

### Stability improvements

This version solves many long-standing problems with the message deliverability, failed connections with group members (that also contributed to group fragmentation), and reduces traffic in groups (beta users experienced a traffic spike because of this fix, but it won't affect the final release). It's not the end of the road to making SimpleX Chat as stable as mainstream messengers, but it is a big improvement.

Please report the cases when messages are not delivered – delivery receipts should help with that.

#### Problem of messages failing to decrypt solved

Previously, a growing number of users experienced the issue when having restored the chat database from backup, messages from some contacts failed to decrypt and were showing the error in the app.

This was happening due to the quality of double ratchet protocol that protects the integrity of end-to-end encryption after the compromise - [post-compromise security](../docs/GLOSSARY.md#post-compromise-security). The protocol logic does not allow to use the old version of the database to decrypt the message.

v5.2 added the extension to the messaging protocol allowing to agree the new ratchet keys - both for connections with the contacts and with the group members. This requires a user action, and it resets the security code verification status for this contact or member – you need to verify the security code again if you need this additional protection from [man-in-the-middle attacks](../docs/GLOSSARY.md#man-in-the-middle-attack).

The negotiation of the new ratchet keys still happens via end-to-end encrypted messages, as the protocol has two layers of end-to-end encryption, so it cannot be compromised by the messaging relays.

You may still lose connection if you or your contact changed the receiving address before you made the backup, so make sure to make a new back up after any address changes.

#### Reconnect the servers

While v5.2 solved many message delivery issues, there certainly may be some others, but they are usually solved with full app restart. On Android it was difficult, as there is a continuosly running background service for notifications that doesn't restart when you restart the app. Now Android app has both Restart and Shutdown buttons that take background service into account.

On iOS you can now pull down the list of conversations to reconnect to all relays without restarting the app.

### Better disappearing messages

You can now send a separate disappearing message if the conversation preferences allow it, but do not have any time to disappear enabled – this applies both to groups and to contacts. You can also set the time to disappear up to 12 months.

### Prohibit message reactions

While most people like message reactions, some conversations make them inappropriate - you can disable them now.

## Platform evolution

### Problems of public groups

As I wrote above, not all messages are received, or, at least, they may be substantially delayed. Additional problems are various scenarios when the list of members gets out of sync for different members.

How to cope with these problems?

It really helps to only use one link shared with the members to join the group - the one created by the client that is most frequently online, ideally always online. This is often confusing, as any group admin can create group link, and share it with the members, and if this admin is not online, the new member won't be able to join.

We will be adding some new group features to make it easier to manage - for example, we plan to add an option to fix the connection with the member that you failed to connect to by passing the link out-of-band. This can be particularly helpful for average size groups of 20-50 people where it's important to see all messages.

In the long term, the only way to make groups usable is to move to a new design.

#### Why not hosted groups with MLS?

Initially, we considered a design with a dedicated server that hosts the group. This design would require adopting MLS (or similar) protocol for group-wide key agreement. Unfortunately, this design is not sufficiently resilient and easier to censor than decentralized design. Also, MLS protocol is very complex to implement, requires centralized component, and reduces forward secrecy. So we decided against this approach.

#### Why not fully decentralized groups?

We also considered roumor-mongering protocol, where all members are equal and participate in message dissemination. The problem with this approach is that it adds a lot of traffic for all members, even those who mostly read messages. Also, it still requires establishing a fully connected graph, and with large groups it becomes prohibitively expensive.

#### Members host the groups

We are currently considering a middle-ground - a design where chosen owners and admins host the group, synchronising the state between them, and re-broadcasting the messages to all members. This puts a higher burden on some members, but these clients can be hosted in the cloud, and group owners have larger incentive to maintain group integrity. At the same time, group members don't need to establish connections with all other members, only with a limited number of "hosting" members.

This approach avoids the need for a group-wide key agreement protocol, as hosting members are expected to have access to all content - pair-wise ratchets are sufficient. At the same time the content remains end-to-end encrypted, and protected from the outsiders.

This approach also simplifies moderation - the message that needs to be removed simply won't reach the members before it is moderated.

Discovery and content search in such groups will be provided via a dedicated discovery server that will participate in the group, provide an always-online client, and also content moderation.

A possible approach to moderation is described here.

We really look forward to your feedback on this design.

### What about read receipts?

We have an approximately equal number of users who ask us to add receipts, and those who ask not to add them, even as optional.

While the give some comfort to the message sender, they introduce a lot of stress for the recipients.

As one of the users in the group wrote it: "The existence of read receipts in other platforms is exhausting and is often a source of undue stress. I have to make a decision to read something and let someone know that I have read something and decided not to respond or merely didn't have the time to respond. the outcome of that is a complex social negotiation with non-theoretical social fallout as a consequence. All in all, it's an invasion of privacy of being able to read things at the pace of the individual as opposed to the pace dictated by others... Most people don't need a read receipt so leave it to a group of individuals to decide if having read receipts make sense to them for their workflow".

Also read [this post](https://neilalexander.dev/2021/04/09/read-receipts-typing-notifs) about the damage from read receipts and other invasive ideas, like typing and presense notifications.

There is also no discounting that the mere presense of read receipts, even as opt-in, creates a social pressure to enable them, with the same consequences – there are many scenarios when they would be non-optional in a given relationship. So many users belive, and we share this view, that it is better not to have these features at all. But we will be re-assessing this view.

## SimpleX platform

Some links to answer the most common questions:

[SimpleX Chat security assessment](./20221108-simplex-chat-v4.2-security-audit-new-website.md).

[How can SimpleX deliver messages without user identifiers](https://simplex.chat/#how-simplex-works).

[What are the risks to have identifiers assigned to the users](https://simplex.chat/#why-ids-bad-for-privacy).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[How SimpleX is different from Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/README.md#frequently-asked-questions).

Visit our [website](https://simplex.chat) to learn more.

## Help us with donations

Huge thank you to everybody who donated to SimpleX Chat!

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/tree/master#help-us-with-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
