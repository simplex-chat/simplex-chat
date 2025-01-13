---
layout: layouts/article.html
title: "SimpleX network: privacy preserving content moderation"
date: 2025-01-14
preview: How network operators prevent distribution of CSAM without compromising users privacy and security.
# image: images/20241218-pub.jpg
# imageWide: true
draft: true
permalink: "/blog/20250114-simplex-network-large-groups-privacy-preserving-content-moderation.html"
---

# SimpleX network: large groups and privacy preserving content moderation

**Will be published:** Jan 14, 2025

## Large groups on SimpleX networks

When we designed groups, we expected them to be used primarily for small groups of people who know each other, not more than 100 or so people.

But we learnt that people want to participate in public discussions remaining anonymous - it protects freedom of speech. As an experiment, we are curating a small directory of the groups that currently has almost 400 public groups.

## Can large groups scale?

Currently the groups are fully decentralized, and every time you send the message to group your client has to send it to all group members separately, which is very costly for traffic and battery with large groups.

We are currently working on the new group architecture when dedicated group members that run their clients on the server or on desktop with good internet connection re-broadcast messasages to all members – we call these group members super-peers. We will be offering our super-peers and you will be able to use your own, to have better ownership of the group - e.g., if we decide to remove our super peer from the group it will continue functioning because your super peer can continue re-broadcasting messages.

## Wouldn't anonymous participation lead to abuse and how to solve it?

All public discussions experience some level of abuse from spammers and trolls, whether they are anonymous or not. We have been evolving ability of group owners to moderate conversations, with the ability to remove inappropriate or off-topic messages, and to block members who send spam, and also to make all new members who join groups observers.

As support for large groups improves, we expect the attempts to abuse may increase too, unless we add better moderation capabilities in advance.

v6.3 will add ability of the group members to send reports to the group owners and aministrators - just released better version already added support to manage these reports, so you won't miss them when members start sending them.

Other features that we plan to add this year to improve moderation:
- message comments - it would allow some groups to only allow contributing comments, and not messages.
- ability to limit the maximum number of messages the members can send per day.
- ability to pre-moderate messages before they can be seen by all members.
- "knocking" - having a conversation with the member before adding to the group.

## But what about abuse of the servers - how to prevent it without compromising e2e encryption?

Some categories of content may be prohibited by servers operators. An extreme case would be child sexual abuse materials (CSAM).

Many people believe that when conversation is end-to-end encrypted, the problem is unsolvable. This incorrect belief is used by unscrupulous lobbyists and politicians who try to mandate various types of content scanning to prevent distribution of CSAM.

We wrote before about how such measures not only would fail to solve the problem, but would also make it worse - if our private photos become available to service providers, they will eventually become available to criminals too, and will be used to abuse and exploit the users and their children.

But the reality is that an absolute majority of CSAM distributed online is publicly available. Many large tech companies, including Meta and Telegram, failed to act and to remove CSAM from their services before it became epidemic. We see as the most important objective to eliminate the possibility to distribut CSAM from publicly accessible groups.

Over 2024 we started to receive a small number of complaints, and in all cases we removed the files and groups. In no case it required compromising privacy and encryption. How did it work? For example, some of our users discovers the link to join the group that distributes CSAM. They sent a complaint to our support email address or via the app (SimpleX Chat team contact in the app).

Once we have the link to join the group, we now know that the group exists and our automated bot can join it. If the complaint is verified, the bot can the delete all files sent to the group and also delete the link to join the group.

Can't users simply create another group? Yes, they can, and it happens in large systems as well. That's why we are planning additional measures in advance of support of large group, to avoid increase of this problem.

## Privacy preserving content moderation via client-side restriction

Right now, when we act on user complaints, we simply delete uploaded files or the link to join the group from our servers, and to the users it works as if something stopped working for unclear reasons.

We are currently rolling out the change to the servers that would mark the files other users complained about as blocked, so that users who download them would see that the files were blocked for violating conditions of use.

Later this year we are planning to do more than that: when the client that uploaded blocked file discovers that the file was blocked, it may, optionally, depending on the information in the blocking record, prevent further uploads from the app to the servers of these operators. Also, when the client that receives the file sees that the file was blocked, it may also refuse to receive files in the same conversation via the same servers.

This way, while servers won't identify the users, they would still restrict their future actions.

More technical users usually ask these questions:

**Can't users just modify their clients to circumvent these restrictions?**

Yes, they can, but for this to work both sender and recipient would have to modify their clients, and it's both technically complex, so most users won't do it, and it is also hard to coordinate between users who don't know and don't trust each other.

So these measures would be very effective, even though they can be in theory circumvented, pretty much as any kind of restriction can be.

**Can't users simply use some other servers?**

Yes, they can, and it is ok. In the same way as web browser is not responsible for the content you can access, SimpleX app should not restrict your communication with other servers based on blocking from one server. That would allow different servers to have different policies, depending on their jurisdiction and other factors.

**Wouldn't these measures be abused?**

While server operators can indeed abuse any restriction, but they have many other ways to discrupt communications, as described in the threat model. And this applies to any system. But server operators offer their servers because they want them to be used, whether because they expect that it may be profitable in the future or because they want to support decentralized communication.

So operators have no reason to abuse users - if they do, users would simply stop using their servers. But server operators need to have technical means to protect their servers from abuse too, and the planned client-side resrictions would allow it.

**What additional measures are considered?**

We published other technical ideas that can be used to prevent distribution of illegal content in [this docuement](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/rfcs/2024-12-30-content-moderation.md). What is important, that none of these measures compromise users privacy or end-to-end encryption, and they can (and should) only be applied to publicly accessible content that other users complained about.

We cannot, and we won't scan all content. We actively campaign against any content-scanning proposals, and we we are not planning any such measures.

The belief that it is impossible to moderate conversations when they are e2e encrypted is correct only partially. It becomes possible when users themselves share conversation contents with server operators, in which case they can identify and remove files, if necessary. It is also possible if users make these conversations publicly accessible.

## Privacy and security improvements we plan this year

We plan to add in the near future:
- quantum-resistant e2e encryption in small groups.
- receiving proxy for files, to protect users IP addresses and other transport metadata.
