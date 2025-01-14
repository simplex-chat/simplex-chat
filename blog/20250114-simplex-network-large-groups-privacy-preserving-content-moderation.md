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

Many people believe that it is impossible to moderate and prevent abuse in end-to-end encrypted conversations. This belief is incorrect – there is a way to prevent abuse and distribution of illegal content without any compromises to users privacy and security of end-to-end encryption.

Anti-privacy lobbyists use this incorrect belief to advocate for scanning of private communications, which not only would fail to prevent abuse, but would make it worse - because our private data will become available to criminals.

So it's very important to understand how privacy preserving content moderation works, and educate the politicians who you voted for and who is currently in the office that we do not need to compromise privacy and security in any way to substantially reduce online crime and abuse.

This post answers these questions:
- Why [large groups on SimpleX network](#large-groups-on-simplex-network) don't work well?
- How do we plan to [make them scale](#can-large-groups-scale)?
- How do [group owners prevent abuse](#preventing-abuse-with-anonymous-participation) when people participate anonymously?
- How do server operators [prevent abuse of their servers](#preventing-server-abuse-without-compromising-e2e-encryption) and [how these measures will evolve](#privacy-preserving-content-moderation) without any compromises to privacy and end-to-end encryption?
- Which [privacy and security improvements](#privacy-and-security-improvements-we-plan-this-year) we plan this year?

## Large groups on SimpleX network

When we designed groups, we expected them to be used primarily for small groups where people know each other, with not more than 100 or so members.

But we learnt that people want to participate in public discussions remaining anonymous - it protects their freedom of speech. As an experiment, we are curating a small [directory of the groups](../docs/DIRECTORY.md) that currently has almost 400 public groups, with the largest ones having thousands of members. You can connect to this experimental directory via [SimpleX chat address](https://simplex.chat/contact#/?v=2-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion).

## Can large groups scale?

Currently the groups are fully decentralized, and every time you send the message to some group your client has to send it to each group member, which is very costly for traffic and battery in large groups.

We are currently working on the new group architecture when dedicated group members that run their clients on the server or on desktop with good internet connection will re-broadcast messages to all members – these members are "super-peers". We will be offering pre-configured super-peers via the app, and you will be able to use your own super-peers, in case you are hosting a large private group, and to have better control and ownership of the group - e.g., if we decide to remove our super peer from the group it will continue functioning because your super peer continues re-broadcasting messages.

## Preventing abuse with anonymous participation

All public discussions are abused by spammers and trolls, whether they are anonymous or not. We have been evolving ability of group owners to moderate conversations by allowing to remove inappropriate and off-topic messages, to block members who send spam, and to make all new members who join their group unable to send messages until approved.

As support for large groups improves, we expect the attempts to abuse may increase too, unless we add better moderation capabilities in advance.

v6.3 will add ability of the group members to send reports to the group owners and administrators - the beta version we just released adds ability to manage these reports, so group admins won't miss reports when members start sending them.

Other features that we plan to add this year to improve moderation:
- message comments - some groups may choose to allow only comments, when ability to send messages is restricted to group owners or admins.
- ability to limit the maximum number of messages the members can send per day.
- ability to pre-moderate messages before they can be seen by all members.
- "knocking" - having a conversation with the new members before they are added to the group.
- sub-groups - smaller conversations with the same members.

## Preventing server abuse without compromising e2e encryption.

Some categories of content may be prohibited by servers operators. An extreme case would be child sexual abuse materials (CSAM).

Many people believe that when conversation is end-to-end encrypted, the problem is unsolvable. This incorrect belief is used by unscrupulous lobbyists and politicians who attempt to mandate various types of content scanning under the guise of preventing CSAM distribution.

We [wrote before](./20240601-protecting-children-safety-requires-e2e-encryption.md) about how such measures not only would fail to solve the problem, but would also make it worse. If our private photos become available to service providers, they will eventually become available to criminals too, and will be used to abuse and exploit the users and their children.

An absolute majority of CSAM distributed online is publicly accessible. Many large tech companies failed to act and to remove CSAM from their services before it became an epidemic. We see as the most important objective to eliminate the possibility to distribute CSAM from publicly accessible groups, even if it hurts network growth.

When we receive a user complaint about CSAM shared in any group, we remove the files and in some cases groups from our servers. Our approach to moderation preserves user privacy and security of end-to-end encryption.

How does it work? Let's go over the process step by step.

1. A user discovered the link to join the group that distributes CSAM and sent a complaint to our support email address or via the app to [SimpleX Chat team](simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) contact.

2. Once we received the link to join the group, we instruct our automated bot to join it. If the complaint is confirmed as valid, the bot sends the information about the files sent in this group to the servers that store this file.

3. Once the servers receive the file identifiers, they can now block the file.

So while file servers cannot look inside the files, and don't even know who and how many files are sent, they can delete the files if the bot whose information they trust shares this information. In this way, moderation becomes possible without any scanning or compromises to privacy or end-to-end encryption.

Can't users simply create another group? Yes, they can, and it happens in large systems without any privacy as well. That's why we are planning additional measures in advance of supporting large groups, to prevent this problem from getting bigger.

## Privacy preserving content moderation

Right now, when we act on user complaints, we delete uploaded files or the links to join the groups from our servers, and to the users it looks as if something stopped working.

We are currently rolling out the change to the servers that would mark these files and group links as blocked, so that users who try to download them or to join blocked groups can see that they were blocked for violating server operator conditions of use.

Later this year we plan to do more than that: when the client discovers that the uploaded file was blocked, it may, optionally, depending on the information in the blocking record, disable further uploads from the app to the servers of the operator that blocked the file. Also, when the client that tried to receive the file sees that the file is blocked, it may also refuse to receive further files from the same group member via the same servers.

In this way, servers preserve privacy and security of the users and content, but they are still able to restrict the future actions of the users who violate the conditions of use.

We discussed this plan with the users, and we really appreciate their feedback. The current plan is quite different from our initial ideas, the users had a real impact on these decisions. Users asked the questions below.

**Can't users modify their clients code to circumvent these restrictions?**

Yes, they can, but for this to work both sender and recipient would have to modify their clients, and it's both technically complex, so most users won't do it, and it is also hard to coordinate between users who don't know and don't trust each other.

So these measures would be effective, even though they can be in theory circumvented, as any restrictions can be.

**Can't users use other servers?**

Yes, they can. But in the same way as web browser is not responsible for the content you can access, SimpleX app should not restrict your communications with other servers based on blocking action from just one server. That would allow different server operators to have different content policies, depending on their jurisdiction and other factors.

**Wouldn't these measures be abused?**

While server operators can indeed abuse such restrictions, they have other ways to disrupt communications, as described in the [threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md#simplex-messaging-protocol-server). Any communication system, with or without servers, can be disrupted by its participants and providers.

But server operators offer their servers because they want them to be used, whether because they expect that it will be profitable in the future or because they want to support decentralized communication for charitable reasons.

So operators have no reason to abuse users - if they do, users would simply stop using their servers. At the same time, server operators need to have technical means to protect their servers from abuse too, and the planned client-side restrictions would allow it.

**What additional measures are considered?**

We published other technical ideas that can be used to prevent distribution of illegal content in [this document](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/rfcs/2024-12-30-content-moderation.md). What is important, that none of these measures compromise users privacy or end-to-end encryption, and they can (and should) only be applied to publicly accessible content that other users complained about.

We technically cannot, and we won't scan all content. We actively [campaign against any content-scanning proposals](./20240704-future-of-privacy-enforcing-privacy-standards.md), not only because it violates our right to privacy, but also because it would result in huge increase of online crime.

The belief that it is impossible to moderate conversations when they are e2e encrypted is incorrect. It is possible when users themselves share conversation contents with server operators, in which case the operators can identify and, if necessary, remove files. It is also possible to moderate conversations that users made publicly accessible.

## Privacy and security improvements we plan this year

Not only we won't reduce privacy and security, we plan to increase it this year.

We plan to add:
- quantum-resistant e2e encryption in small groups.
- receiving proxy for files, to protect users IP addresses and other transport metadata.

We see privacy and security as necessary for online safety, and prevention of abuse. If you don't already use SimpleX network, try it now, and let us know how to make it better.
