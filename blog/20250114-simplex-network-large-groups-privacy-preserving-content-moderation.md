---
layout: layouts/article.html
title: "SimpleX network: large groups and privacy-preserving content moderation"
date: 2025-01-14
preview: "This post explains how server operators can moderate end-to-end encrypted conversations without compromising user privacy or end-to-end encryption."
image: images/20250114-locked-books.jpg
permalink: "/blog/20250114-simplex-network-large-groups-privacy-preserving-content-moderation.html"
---

# SimpleX network: large groups and privacy-preserving content moderation

**Published:** Jan 14, 2025

<img src="./images/20250114-locked-books.jpg" width="345" class="float-to-right">

Many people believe that it is impossible to moderate and prevent abuse in end-to-end encrypted conversations. This belief is incorrect &mdash; there is a way to prevent abuse and distribution of illegal content without any compromises to users' privacy and security of end-to-end encryption.

Anti-privacy lobbyists use this incorrect belief to advocate for scanning of private communications, which not only would fail to prevent abuse, but would make it worse &mdash; because our private data will become available to criminals.

So it's very important to understand how privacy preserving content moderation works, and educate the politicians who you voted for, and who is currently in the office, that we do not need to compromise privacy and security in any way to substantially reduce online crime and abuse.

This post answers these questions:
- Why [large groups on SimpleX network](#large-groups-on-simplex-network) don't work well?
- How do we plan to [make them scale](#can-large-groups-scale)?
- How do [group owners prevent abuse](#preventing-abuse-with-anonymous-participation) when people participate anonymously?
- How do server operators [prevent abuse of their servers](#preventing-server-abuse-without-compromising-e2e-encryption) and [how these measures will evolve](#privacy-preserving-content-moderation) without any compromises to privacy and end-to-end encryption?
- Which [privacy and security improvements](#privacy-and-security-improvements-we-plan-this-year) we plan this year?

## Large groups on SimpleX network

When we designed groups, we expected them to be used primarily for small groups where people know each other, with not more than 100 or so members.

But we learnt that people want to participate in public discussions remaining anonymous &mdash; it protects their freedom of speech. As an experiment, we are curating a small [directory of groups](../docs/DIRECTORY.md) that currently has almost 400 public groups, with the largest ones having thousands of members. You can connect to this experimental directory via [SimpleX chat address](https://simplex.chat/contact#/?v=2-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion).

## Can large groups scale?

Currently the groups are fully decentralized, and every time you send the message to some group your client has to send it to each group member, which is very costly for traffic and battery in large groups.

We are currently working on the new group architecture when dedicated group members that run their clients on the server or on desktop with good internet connection will re-broadcast messages to all members &mdash; these members are "super-peers".

We will be offering pre-configured super-peers via the app, and you will be able to use your own super-peers, in case you are hosting a large private group, and to have a better control and ownership of the group &mdash; e.g., if we decide to remove our super peer from the group, it will continue to function thanks to your super-peer re-broadcasting messages.

This new design improves both privacy of group participation and censorship resistance of the groups, and also makes abusing the group harder.

## Preventing abuse with anonymous participation

All public discussions are abused by spammers and trolls, whether anonymous or not. We have been evolving ability of group owners to moderate conversations by allowing to remove inappropriate and off-topic messages, to block members who send spam, and to make all new members who join their group unable to send messages until approved.

As support for large groups improves, we expect that the attempts to abuse may increase too, unless we add better moderation capabilities in advance.

v6.3 will add ability of the group members to send reports to the group owners and administrators &mdash; the beta version we just released adds ability to manage these reports, so group admins won't miss reports when members start sending them.

Other features that we plan to add this year to improve both usability and safety of the groups:
- message comments &mdash; some groups may choose to allow only comments, when ability to send messages is restricted to group owners or admins.
- ability to limit the maximum number of messages the members can send per day.
- ability to pre-moderate messages before they can be seen by all members.
- "knocking" &mdash; approving new members before they can join the group.
- sub-groups &mdash; smaller conversations with the same members.

## Preventing server abuse without compromising e2e encryption

Some categories of content may be prohibited by servers operators. An extreme case would be child sexual abuse materials (CSAM).

Many people believe that when conversation is end-to-end encrypted, the problem is unsolvable. This incorrect belief is used by unscrupulous lobbyists and politicians who attempt to mandate various types of content scanning under the guise of preventing CSAM distribution.

We [wrote before](./20240601-protecting-children-safety-requires-e2e-encryption.md) about how such measures not only would fail to solve the problem, but would make it worse. If our private photos become available to service providers, they will eventually become available to criminals too, and will be used to abuse and exploit the users and their children.

An absolute majority of CSAM distributed online is publicly accessible. Many large tech companies failed to act on it and to remove CSAM from their services before it became an epidemic. We see it as a very important objective to eliminate the possibility to distribute CSAM from publicly accessible groups, even if it hurts network growth.

When we receive a user complaint about CSAM shared in any group, we remove the files and, in some cases, the links to join the group from our servers. Our approach to moderation preserves user privacy and security of end-to-end encryption.

How does it work? Let's go over the process step by step.

1. A user discovered the link to join the group that distributes CSAM and sent a complaint to our support email address or via the app to [SimpleX Chat team](simplex:/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) contact.

2. Once we received the link to join the group, we instruct our automated bot to join it. If the complaint is confirmed as valid, the bot sends the information about the files sent in this group to the servers that store these files.

3. Once the servers receive the file identifiers from the bot, they block the files.

File servers cannot look inside end-to-end encrypted files, and they don't even know file sizes &mdash; they are securely locked, and sent in chunks, across multiple servers. But if the bot that joined the group provides the address of the particular file, the server can delete this file. It doesn't allow the servers to access any other files.

In this way, the moderation is possible without any content scanning, and it preserves privacy and security of end-to-end encryption.

## Privacy-preserving content moderation

Right now, when we act on user complaints, we delete uploaded files or the links to join the groups from our servers, and to the users it looks as if something stopped working.

We are currently rolling out the change to the servers that would mark these files and group links as blocked, so that users who try to download them or to join blocked groups can see that they were blocked for violating server operator conditions of use. This will improve transparency of moderation and reliability of the network.

Later this year we plan to do more than that &mdash; client-side restrictions on the clients that violated conditions of use by uploading prohibited content.

How would it work? When the client discovers that the uploaded file was blocked, it may, optionally, depending on the information in the blocking record, disable further uploads from the app to the servers of the operator that blocked the file. Also, when the client that tried to receive the file sees that the file is blocked, it may also refuse to receive further files from the same group member via the same servers.

In this way, the servers can restrict the future actions of the users who violate the conditions of use, while preserving privacy and security of the users and content – even of those users who violated the conditions.

We discussed this plan with the users, and we really appreciate their feedback. The current plan is quite different from our initial ideas, the users had a real impact. Users asked the questions below.

**Can't users modify their clients code to circumvent these restrictions?**

Yes, they can, but for this to work both sender and recipient would have to modify their clients. It's technically complex, so most users won't do it, and it is also hard to coordinate between users who don't know and don't trust each other.

So these measures would be effective, even though they can be in theory circumvented, as any restrictions can be.

Other services that identify users reduce abuse by blocking the user account. It is even easier to circumvent than changing the client code, and yet these measures reduce abuse.

**Can't users use other servers?**

Yes, they can. But in the same way as web browser is not responsible for the content you can access, SimpleX app should not restrict your communications with other servers based on blocking action from just one server.

That approach allows different server operators to have different content policies, depending on their jurisdiction and other factors. It also prevents the possibility of abuse by server operators.

**Wouldn't these measures be abused?**

With the proposed changes, server operators will only be able to prevent uploads to their own servers, which prevents any impact on other communications.

In the future we plan to increase the resilience to any server malfunction or abuse by using multiple different servers with each contact.

If servers were to apply any upload restrictions unreasonably, the users would simply stop using them.

At the same time, server operators need to have technical means to protect their servers from users' abuse, and the proposed client-side restrictions achieve it.

**What additional measures are considered?**

We published other technical ideas that could be used to prevent distribution of illegal content in [this document](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/rfcs/2024-12-30-content-moderation.md). None of these measures compromise users' privacy or end-to-end encryption, and they can (and should) only be applied to publicly accessible content that other users complained about.

We technically cannot, and we won't scan all content. We actively [campaign against any content-scanning proposals](./20240704-future-of-privacy-enforcing-privacy-standards.md), because it violates our right to privacy, and it would result in huge increase of online crime.

The belief that it is impossible to moderate conversations when they are e2e encrypted is incorrect. It is possible when users themselves share conversation contents with server operators, in which case the operators can identify and, if necessary, remove this content. It is also possible to moderate conversations that users made publicly accessible.

## Send us comments and questions

Let us know any comments and feedback to the proposed measures. This is still an evolving design, and it won't be implemented until later this year.

Your comments will help to find the right balance between users' and server operators' requirements.

## Privacy and security improvements we plan this year

To increase privacy and security we plan to add this year:
- quantum-resistant e2e encryption in small groups.
- receiving proxy for files, to protect users IP addresses and other transport metadata from file senders' servers.

We see privacy and security as necessary for online safety, and prevention of abuse. If you don't already use SimpleX network, try it now, and let us know what you need to make it better.
