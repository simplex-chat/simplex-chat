---
layout: layouts/article.html
title: "SimpleX Chat v4.6 – with hidden profiles, community moderation, improved audio/video calls and reduced battery usage."
date: 2023-03-28
# preview: This version integrates the support of XFTP protocol, allowing to receive up to 1gb files, even when the sender is offline.
# image: images/20230301-xftp.jpg
draft: true
permalink: "/blog/20230328-simplex-chat-v4-6-hidden-profiles.html"
---

# SimpleX Chat v4.6 – with hidden profiles, community moderation, improved audio/video calls and reduced battery usage.

**Published:** Mar 28, 2023

## What's new in v4.6

- [3x more supported Android devices!](#3x-more-supported-android-devices)
- [hidden-profiles](#hidden-profiles)
- [community moderation](#community-moderation)
- [audio/video calls for iOS](#audiovideo-calls-for-ios)
- [reduced battery usage](#reduced-battery-usage).
- [improved /help for CLI app](#improved-help-for-cli-app)
- [SMP server monitoring: status bot and page](#smp-server-monitoring)

### 3x more supported Android devices!

...

### Hidden profiles

For a long time the main way to protect SimpleX Chat app from people who have access to your phone was device authentication requested when you open the app (and some of its sensitive functions).

Many users asked for the ability to set app password or PIN independently from the device PIN, as some other apps do. But it did not seem a good enough solution - if somebody has your device PIN, then in most cases they can also ask you to provide the app PIN as well.

So instead of having an app password, that reduces convenience and doesn't improve security too much, we did what we think is better. You can now create hidden chat profiles, that are not visible anywhere in the app, and do not show any notifications, until you enter a correct password in the search bar above the list of profiles. You can have as many hidden profiles as you like, and protect them with the same or different passwords. If multiple profiles are hidden with the same password they will all show in the list when you enter this password.

It's important to remember that these hidden profiles are still stored locally on your device, so if somebody has access to your chat database (they will need to know database passphrase, that is independent from device PIN or profile passwords) or to chat console in the app, they will be able to access these profiles data and reset their passwords. We are considering how chat console can be better protected - e.g., by requiring a separate password or an by providing an option to remove it from the UI permanently - we're looking forward to your feedback.

### Community moderation

Initially we did not design SimpleX Chat to support communities - our focus has always been maximum privacy and security. We designed SimpleX Chat to operate without user identities of any kind, even without any random numbers that identify the users, because we believe that it is as important to use different connection identifiers in different conversations, as it is important to use different passwords for different websites and apps - this is the only way to protect your privacy. iOS *Sign-in with Apple* feature allowing to create a new random email for each new app supports this view. We believe that using per-conversation paiwise identifiers, as SimpleX does, should be the minimal design requirement for any communication platform to be considered private, as I wrote in [this post](https://www.poberezkin.com/posts/2022-12-07-why-privacy-needs-to-be-redefined.html).

We implemented support for small and fully decentralized groups in SimpleX Chat, that are not hosted anywhere, but we have quickly learnt that for many users of messaging apps the ability to participate in and to discover communities is as important as being able to talk to friends and family, and to any other people they know. The groups we created to let users test the app started to quickly grow, and many other communities of over 100 people emerged.

While we are committed to releasing a decentralized community platform later this year, where large hosted groups will solve all today's problems of groups in SimpleX, we are already observing some less-than-friendly messages and undesirable content that is not welcome in some communities. So this version adds features allowing to moderate groups.

Firstly, group admins and owners can revoke members rights to send messages to the group by assigning members an "observer" role, and also make this role default for users joining via a group link. Even if sender users a modified client to circumvent this restriction, all other members' clients will quietly discard these messages.

Secondly, group admins can now delete messages sent by other members (excluding the messages sent by the group owners - only owners can delete them). This will either mark message as deleted or delete it irreversibly for all members, as set in group preferences by the owners.

We hope that this will allow for a more balanced community participation, where group owners can decide their own rules.

### Improved audio/video calls

Audio and video calls have always been limited. On iOS the calls were almost completely unusable, as the call only worked while the app was in foreground, losing sound otherwise. This version includes fully re-implemented audio/video calls on iOS that no longer use web views and instead use native WebRTC library. These calls are still end-to-end encrypted, and compatible with the calls in the previous versions of the app, both on iOS and on Android platforms.

Where allowed by AppStore policy, the calls on iOS now use Apple native interface for calls, CallKit, that allows to accept calls from the lock screen, prevents call interruption by incoming phone calls and optionally allows to include calls in the phone call history where you can call back the contacts from whom you missed calls in SimpleX - the last option needs to be enabled separately.

Calls on Android were also improved. Now they support bluetooth headphones, allow to change volume in video calls and support proximity sensor during the audio call, to prevent accidental call interruption when you hold the phone to your ear.

### Reduced battery usage

We know that battery usage of SimpleX Chat is suboptimal, and we are committed to reduce it. Unfortunately, there is no simple change that we could make to solve this problem, it requires many systematic improvements and fixes.

This version optimises retry strategy for sending messages in cases when the receiving message queue (mailbox) is out of capacity. Previously, the client would retry sending quite frequently until the message expires in 2 days. That is not such a big problem in direct conversations and small groups when all members connect regularly. But it is a big problem in large groups, where people may leave the group, in some cases without notifying other members (e.g., because the Internet was down, or they simply deleted the app, without leaving the group), in which case other members who remain in the group and send messages will be making a lot of retries to send messages to these inactive members.

In the previous version we have implemented an addition to the protocol where the receiving client is notified when the sender encountered "out-of-capacity" error. In this case the receiving client would send a control message to the sending client to resume delivery. This allows to substantially increase retry period for "out-of-capacity" scenario without creating unnecessary delivery delays, as in case the recipient received all pending messages the delivery would be almost instantly resumed.

This version increases the maximum retry period for "out-of-capacity" errors to 1 hour, and also preserves this retry period in the database. So, if previously before the message expired in 48 hours there were 144-2880 delivery attempts (which for a group that has 50 inactive members results in up to 46Mb of traffic for each sent message, depending on how frequently the app was restarted), now there will be only ~50 retries, resulting in not more than 0.8Mb of traffic - up to 58x traffic reduction when sending messages to the large groups.

This issue might not have affected you at all, and also solving it won't reduce overall traffic/battery usage by that factor - there are other inefficiences we will be addressing. But if you actively sent messages to large groups you should observe a substantial reduction of battery and traffic consumption.

Please share your experience. If the battery usage is still suboptimal, please share your usage statistics - they can be requested in chat console with `/get stats` command - it will return the aggregated number of network operations, per server, since the app was started. Please note that these statistics include the addresses of the servers you connect to, so if you want to keep them private, please redact them. You can also reset usage statistics with `/reset stats` command.

### Improved /help for CLI app

...

### SMP server monitoring

...