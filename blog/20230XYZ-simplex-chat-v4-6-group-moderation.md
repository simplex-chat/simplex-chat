---
layout: layouts/article.html
title: "SimpleX Chat v4.6 – with group moderation tools, audio/video calls for iOS and support of receiving files up to 1gb sent from command-line interface."
date: 2023-0X-YZ
preview: This version integrates the support of XFTP protocol, allowing to receive up to 1gb files, even when the sender is offline.
# image: images/20230301-xftp.jpg
permalink: "/blog/20230XYZ-simplex-chat-v5.html"
---

# SimpleX Chat v4.6 – with group moderation tools, audio/video calls for iOS and support of receiving files up to 1gb sent from command-line interface.

**Published:** Month Date, 2023

## What's new in v4.6

- [community moderation](#community-moderation)
- [receiving files up to 1gb and of inline videos](#support-for-receiving-files-up-to-1gb-inline-videos-and-large-voice-messages)
- [set app PIN independent from device authentication](#set-app-pin-independent-from-device-authentication)
- [audio/video calls for iOS](#audiovideo-calls-for-ios)
- [improved /help for CLI app](#improved-help-for-cli-app)
- [SMP server monitoring: status bot and page](#smp-server-monitoring)
- [reduced battery usage](#reduced-battery-usage).

### Community moderation

Initially, we did not design SimpleX Chat to support communities - our focus has always been privacy. We designed SimpleX Chat to oprate without user identities of any kind because we believe that it is as important to use different connection identifiers in different conversations, as it is important to use different passwords for different websites and apps - this is the only way to protect your privacy. iOS *Sign-in with Apple* feature allowing to create a new random email for each new app and service supports this view. We believe that using per-conversation paiwise identifiers, as SimpleX does, should be the minimal design requirement for any communication platform to be considered private, as I wrote in [this post](https://www.poberezkin.com/posts/2022-12-07-why-privacy-needs-to-be-redefined.html).

We implemented support for small, fully decentralized groups in SimpleX Chat, that are not hosted anywhere, but we have quickly learnt that for many users of messaging apps the ability to participate in and to discover communities is as important as being able to talk to friends and family, and to any other people they know. The users groups we created to let users test the app started to quickly grow, and many other communities of over 100 people emerged.

While we are committed to releasing a decentralized community platform later this year, where large hosted groups will solve all today's problems of groups in SimpleX, we are already observing some less-than-friendly messages and undesirable content that is not welcome in the communities. So this version adds features allowing to moderate groups.

Firstly, group admins and owners can revoke members rights to send messages to the group by setting "observer" role, and also make this role default for users joining via group link. Even if sender users a modified client to circumvent this restriction, all other members' clients will quietly discard these messages.

Secondly, group admins now can delete messages sent by other members (excluding the messages sent by the group owners - only owners can delete them). This will either mark message as deleted, or delete it irreversibly for all members, in line with group preferences that can be changed by the owners.

We hope that this will allow for a more balanced community participation, where group owners can decide their own rules.

TODO pictures

### Support for receiving files up to 1gb, inline videos and large voice messages

We have recently [released](./20230301-simplex-file-transfer-protocol.md) a new implemenation of SimpleX File Transport protocol that allows to send large files efficiently and receive them without the sender being online.

This version integrates the ability to receive these large files, so that by the time the sending of the large files is supported in the next version, the majority of the users had time to update the app to v4.6 and able to receive these large files.

And, if you are managing a large group you can already send some of these large files via the SimpleX Chat CLI, preliminary downloading them using XFTP CLI. While this is not a very user-friendly process for most users, it can be justified for community admins - you can start sending videos and images to the groups without incurring too much traffic, and without the need to be online for the group members to receive them. Also, the members can forward these files to other conversations, without re-uploading them - until they expire in 48 hours from initially being sent!

### Set app PIN independent from device authentication

...

### Audio/video calls for iOS

...

### Improved /help for CLI app

...

### Reduced battery usage

We know that battery usage of SimpleX Chat is suboptimal, and we are committed to reducing it. Unfortunately there is one simple change that we can make, it requires many systematic improvements and fixes to gradually reduce it to an acceptable level.

This version optimises retry strategy for sending messages in cases when the receiving message queue (mailbox) is out of capacity. Previously, the client would retry sending message quite frequently until the message expires in 2 days. That is not such a big problem in direct conversations and small groups when all members connect regularly. But this became a very big problem in large groups, where people may leave the group, in some cases without notifying other members (e.g., because the Internet was down, or they simply deleted the app, without leaving the group), in which case other members who remain in the group and send messages will be making a lot of retries on such inactive members.

In the previous version we have implemented an addition to the protocol where the receiving client is notified when the sender encountered "out-of-capacity" error. In this case the receiving client would send a control message to the sending client to resume delivery. This allows to substantially increase retry period for "out-of-capacity" scenario without creating unnecessary delays, as in case the recipient received all pending messages the delivery would be almost instantly resumed. It will also allow increasing message expiration period a bit later.

Currently, the message sending is retried every 1 minute (it was already increased from the initial 1 second some time ago), eventually growing to 20 minutes - we didn't set it higher to avoid delays in cases recipients had the old versions of the client that would not notify senders to resume delivery. The current problem is that this retry period is reset every time the app is restarted.

This version sets the initial retry period for "out-of-capacity" errors to 10 minutes gradually increasing it to 3 hours, and also preserves this retry period in the database. So, if previously before the message expired in 48 hours there were 144-2880 delivery attempts (which for a group that has 50 inactive members results in 2.3-46Mb of traffic for each sent message, depending on how frequently the app was restarted), now there will be at most 22 retries during 48 hours, resulting in 0.35Mb of traffic - 6.5-130x traffic reduction when sending messages to groups that has inactive members (any large group).

That doesn't mean that this issue affected you at all, or that it reduces overall traffic/battery usage by that factor - there are other inefficiences we will be addressing. But if you actively sent messages to large groups you should observe a substantial reduction of battery and traffic consumption.

Please let us know what your experience is, and if the battery usage is still suboptimal for you please share your aggregate usage statistics with us - they can be requested in chat console using `/get stats` command - it will return the aggregated number of network operations, per server, since the app was started. Please not that these statistics include the addresses of the servers you connect to, so if you want to keep it private please redact it. You can also reset usage statistics with `/reset stats` command.

# SMP server monitoring

...