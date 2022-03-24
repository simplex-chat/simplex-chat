# Instant notifications for SimpleX Chat mobile apps for iOS and Android

**Published:** April 04, 2022

## SimpleX Chat is the first chat platform that is 100% private by design - it has no access to your connections graph

Since we released SimpleX Chat mobile apps couple of weeks ago we had a lot of excitement from our users - nearly 2000 people downloaded the app after [the announcement](https://github.com/simplex-chat/simplex-chat/blob/stable/blog/20220308-simplex-chat-mobile-apps.md)!

Huge thanks to everybody who downloaded and connected to us via the chat - there were many great questions and suggestions, and on some days I spent most of the time chatting to our users :)

Since we released the app, we added:

- support for iPhone 7.
- configurable SimpleX servers.
- message replies, editing and deletion.
- profile images.
- and, most importantly, instant message notifications on Android devices - more on that below.

## Install the apps and make a private connection!

Once you install the app, you can connect to anybody:

1. Create your local chat profile - it is not shared with SimpleX servers, it is local to your devices, and it will be shared with your contacts when you connect.
2. To make a private connection, you need to create a one-time connection link / QR code via "Add contact" button in the app. You can either show the QR code to your contact in person or via a video call - this is the most secure way to create a connection - or you can share the link via any other channel - only one user can connect via this link.
3. Once another user scans the QR code or opens the app via the link (they also should create their profile first) the connection will be created and you can send e2e encrypted messages privately, without anybody knowing you are connected.

See [demo video](https://youtu.be/rZeVhsv_JAY) that shows how two users connect and send the first messages.

## Why we are doing it

We are building SimpleX Chat because we belive that privacy is a fundamental human right, and that protecting our personal network of contacts is even more important than the content of the messages - sharing this network can lead to various adverse consequences, from manipulating us into buying the goods we don't need, manipulating the election process, and in some cases, prosecuting innocent people - like happened with [Mohamedou Ould Salahi](https://en.wikipedia.org/wiki/Mohamedou_Ould_Slahi) who was detained in Guantanamo prison for 15 years after a single "wrong" phone call - the story told in he memoir and in The Mauritanian movie.

## Problem - users expect to be instantly notified when messages arrive!

Once our first users started using the apps they realized that what we take for granted in messaging apps - instant message notifications - is missing in our first release of SimpleX Chat apps. Quite a few people thought that it was a bug, rather than a missing feature - sorry to disappoint!

Most messaging apps appear to us very simple, even minimalist, but some features require complex engineering – so we didn't manage to include instant notifications in the first release – and it seems like it made our apps not viable for many users.

## Why it is a bigger problem for SimpleX Chat - and why can't we just do what messenger X does?

SimpleX Chat appears to be the first and the only messenger that operates without user identities of any kind - there are no phone numbers, emails, usernames, public keys, or any other addresses or identifiers to uniquely identify the users to the network or servers - that is why we say it is 100% private by design. Many people believe that to organize communications the servers have to know who the users are, but SimpleX protocol manages message routing without user identities – it authorizes the user actions without authenticating the users.

Instead, SimpleX Chat assigns these identifiers to unidirectional message queues, and what looks to SimpleX Chat users as contacts and groups [1], to SimpleX servers looks like an unorganized and unrelated collection of unidirectional message queues - servers do not know which queues belong to which users, contacts or groups, and even a single conversation can happen via two different servers (one for sent and another for received messages). This makes our personal network of contacts invisible to the servers.

But it also creates a problem for instant notifications - all push notification services require having a unique device token.

So how do we reconcile it, or is it simply impossible both operating without identities and having instant notifications?

[1] yes, we have groups in our terminal app, and the UI to manage them is coming to mobile apps soon - some users have already firgured out how to [create groups via chat console](https://medium.com/@vsevolod.mineev/how-to-collaborate-across-multiple-devices-whilst-protecting-your-metadata-371af87d0ba0).

## What we did for Android

After some research of how push notifications work on Android, and what open-source altetnatives exist to Google push notifications, it turned out to be possible to avoid sharing any device tokens with any servers.

We have implemented message reception as a background service (in Android terminology, "foreground service" - it shows a notification icon when the service is running) following the same design as [ntfy.sh](https://github.com/binwiederhier/ntfy-android) created by [Philipp Heckel](https://github.com/binwiederhier), who, in turn, credits the design to [the blog post by Roberto Huertas](https://robertohuertas.com/2019/06/29/android_foreground_services/) – thanks to them we could do what we did!

How does it work? Simplifying it a bit, when SimpleX Chat app is first started on Android device, it starts the background service that keeps the TCP connections to the messaging servers open with almost no traffic - only doing periodic checks that connections still exist. It consumes only a few percents of battery a day, depending on how stable your internet connection is, and delivers message notifications instantly, as soon as the message arrives.

This service continues running when the app is switched off, and it is started when the device is restarted even if you don't open the app - so the message notifications arrive instantly. But if you need to save whatever little battery the service uses, it can be turned off via the app settings by switching off "Instant notifications" toggle. App would still continue sending message notifications while it is running or in the background, but only until the system or user turn it off.

So, for Android we now have the solution to deliver instant message notifications without compromising users' privacy, at very low cost - this version is now available on [Play Store](https://play.google.com/store/apps/details?id=chat.simplex.app), in our [F-Droid repo](https://app.simplex.chat/) and via direct [APK](https://github.com/simplex-chat/website/raw/master/simplex.apk) downloads!

## What we plan to do for iOS

iOS is much more protective of what apss are allowed to run on the devices, and the solution that worked on Android is not viable on iOS.

We already have background refresh in the iOS app that periodically checks for the new messages, and if you use the app every day it would deliver notifications with 10-20 min delay - it is not instant, but in many cases it is usable. But if you stop using the app, or use it infrequently, this delay can become several hours, or device can stop checking the new messages completely - this is not ideal!

The only solution known to us is using Apple push notifications service (APN) to deliver push notifications.

We planned for it, so we added to [v1 of SMP](https://github.com/simplex-chat/simplex-chat/blob/stable/blog/20220112-simplex-chat-v1-released.md) (SimpleX Messaging Protocol - the protocol used by our servers) - an extension allowing to subscribe to notifications from message queues, via separate queue addresses, and using separate cryptographic keys for each queue - it has to be enabled by the app for each queue separately. We didn't use this extension so far, and now we are building a SimpleX notification service based on it.

If the user allows push notifications (it will be possible to disable them via the settings), then for each contact the app would enable notification subscription and pass credentials to the notification server together with the device token that is required to deliver push notifications to the device.

The notification server will subscribe to these notifications from SMP servers - they do not include message content, only the signal that a message arrived to the server. Notification server is only allowed to send 2-3 hidden notifications per hour (this is iOS limit) to the device that would be e2e encrypted and contain information about which server has a message, so that the client can connect to the server, retrieve and decrypt the message, and show the notification to the users including sender name and the message content - none of these would be shared with any server.

If the user receives more than 2-3 messages per hour, the notification server can send additional visible notifications that would simply say "you have a new message", and the user would have to open the app to receive and see these messages.

It is a substantial amount of development, but we are aiming to release it later this month.

What is important though - this design is a compromise between privacy and convenience - the notification server would have to have a device token to deliver notifications. Several things we did (or plan to do) to improve this compromise:

1. Notification server would only store device tokens and queue addresses in memory - they would not be saved or logged to any file, making it more complex for a potential attacker to access. It means that if the server has to be restarted, they would lose all configured notification subscriptions and the clients would have to create them again - we will program the clients to periodically check for the existence of notification subscriptions on the notification server.
2. Notificaiton server would not know the addresses of the messaging queues used to receive or send messages - a different address is used to subscribe to notifications. So while notification server would have the knowledge of how many queues your device has (and on which servers), it still won't know who is sending you the messages.
3. We are also planning to split the logic of notification subscriptions and delivering notifications to the devices to two different servers - the first one that subscribes to the notifications could be self-hosted, so you would have full control of how you deploy it, and only this server would know which messaging servers you use or how many contacts you have, and the second that delivers notifications to the device will be managed by SimpleX Chat (as we have to authorize it with Apple push notification service). This will not happen in the first release - we plan to add it a bit later.

So, with the notification servers added, our network design will look like this:

```
  User's iOS device                Internet                        Servers
---------------------   |   ------------------------   |   -------------------------
                        |                              |
                        |                              |   (can be self-hosted now)
+--------------+        |                              |      +----------------+
| SimpleX Chat |       -------------- TLS ----------------    | SimpleX        |
|    client    |------> SimpleX Messaging Protocol (SMP) ---> | Messaging      |
+--------------+        -----------------------------------   | Server         |
     ^    |             |                              |      +----------------+
     |    |             |                              |            |   |
     |    |             |                              |            | S | T
     |    |             |                              |            | M | L
     |    |             |                              |            | P | S
     |    |             |                              |            |   |
     |    |             |                              |      +----------------+       +----------------+
     |    |            -------------- TLS ----------------    | SimpleX        |       | SimpleX        |
     |    |----------->     Notifications management    ----> | Notifications  | ----> | Push           |
     |                 -----------------------------------    | Subscriber     |       | Server         |
     |                  |                              |      +----------------+       +----------------+
     |                  |                              |     (can be self-hosted                |
     |                  |                              |        in the future)                  |
     |                  |                              |                                        V
     |                 -------------- TLS ----------------                             +-----------------+
     |-----------------       Notification delivery     <----------------------------- | Apple PN server |
                       -----------------------------------                             +-----------------+
                        |                              |
```

Please let us know what you think about this design and about this privacy / convenience trade-off:

- do you agree that it is an acceptable compromise, particularly if users can choose not to use instant notifications?
- do you maybe have some ideas how this design can be improved
- also, if you think there is a completely different and better design possible - please let us know!

Thank you!
