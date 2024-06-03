---
layout: layouts/article.html
title: "SimpleX network: private message routing, v5.8 released with IP address protection and chat themes"
date: 2024-06-04
# previewBody: blog_previews/20240426.html
# image: images/20240426-profile.png
# imageBottom: true
permalink: "/blog/20240604-simplex-chat-v5.8-private-message-routing-chat-themes.html"
---

# SimpleX network: private message routing, v5.8 released with IP address protection and chat themes

**Published:** June 4, 2024

What's new in v5.8:
- [private message routing](#private-message-routing).
- [protect IP address when downloading files & media](#protect-ip-address-when-downloading-files--media).
- [chat themes](#chat-themes) for better conversation privacy - Android and desktop apps only.
- [group improvements](#group-improvements) - reduced traffic and additional preferences.
- improved networking, message and file delivery.

Also, we added Persian interface language to the Android and desktop apps, thanks to [our users and Weblate](https://github.com/simplex-chat/simplex-chat#help-translating-simplex-chat).

## Private message routing

SimpleX network design has always been focussed on protecting user identity on the messaging protocol level - there is no user profile identifiers of any kind in the protocol design, not even random numbers or cryptographic keys.

Until this release though, SimpleX network had no in-built protection of user transport identities - IP addresses. As the users choose messaging relays to receive messages, the receiving relays can observe IP addresses of the senders, and if these relays are controlled by the recipients, it means that the recipients themselves could observe them too - either by modifying server code or by using some reverse proxy to track all connecting IP addresses.

To work around this limitation, many users used SimpleX Chat either via Tor or VPN - so that the recipients' relays could not observe IP addresses of the users when they send messages. Still, it was the most important limitation of SimpleX network for the users.

This release solves this problem. It implements private message routing - essentially, 2-hop onion routing inspired by Tor design. It is the extension of SimpleX Messaging Protocol that allows message senders to instruct the relays that they trust (the same relays that they use to receive the messages) to forward messages to the destination relays. In this way the recipients' relays cannot observe the IP addresses of the senders, and also cannot observe their transport sessions.

At the same time, the relays that forward the messages cannot observe to which connections messages are sent, thanks to the additional layer of end-to-end encryption between the sender and the destination relay - similar to how onion routing works in Tor network.

```
-----------------             ----------------- --- TLS --- -----------------             -----------------
|               |  -- TLS --  |               |  -- f2d --  |               |  -- TLS --  |               |
|               |  -- s2d --  |               |  -- s2d --  |               |  -- d2r --  |               |
|    Sending    |  -- e2e --  |    sender's   |  -- e2e --  |  recipient's  |  -- e2e --  |   Receiving   |
|    client     |  message -> |  Forwarding   |  message -> |  Destination  |  message -> |    client     |
|               |  -- e2e --  |     relay     |  -- e2e --  |     relay     |  -- e2e --  |               |
|               |  -- s2d --  |               |  -- s2d --  |               |  -- d2r --  |               |
|               |  -- TLS --  |               |  -- f2d --  |               |  -- TLS --  |               |
-----------------             ----------------- --- TLS --- -----------------             -----------------
```

The diagram above shows all the encryption layers used with private message routing:

**e2e** - two end-to-end encryption layers between **sending** and **receiving** clients, one of which uses double ratchet algorithm. These encryption layers are present in the previous version of message routing too - this is end-to-end encryption of the messages between the users.

**s2d** - encryption between **sender** and recipient's **destination** relay. This new encryption layer protects message metadata (destination connection address and message notification flag) from the forwarding relay.

**f2d** - additional new encryption layer between **forwarding** and **destination** relays, protecting from traffic correlation in case TLS is compromised - there are no identifiers and cyphertext in common between incoming and outgoing traffic of both relays inside TLS.

**d2r** - additional encryption layer between destination relay and the recipient, also protecting from traffic correlation in case TLS is compromised.

**TLS** - TLS 1.3 transport encryption.

For private routing to work, both the forwardig and the destination relays should support the updated messaging protocol - it is supported from v5.8 of the messaging servers. It is already released to all relays preset in the app.

Because many self-hosted relays did not upgrade yet, private routing is not enabled by default in the app. To enable, you can open *Network & servers* settings in the app and change the settings in *Private message routing* section. We recommend setting *Private routing* option to *Unprotected* (to use it only with unknown relays and when not connecting via Tor) and *Allow downgrade* to *Yes* (so messages can still be delivered to the messaging relays that didn't upgrade yet) or to *When IP hidden* (in which case messages will fail to deliver to unknown relays that didn't upgrade yet unless you connect via Tor).

## More new things in v5.8

### Protect IP address when downloading files & media

This version also added the protection of your IP address when receiving files from unknown file servers without Tor. Images and voice messages won't automatically download from unknown servers too until you tap them, and confirm that you trust the file server where they were uploaded.

### Chat themes

In Android and desktop app you can now customize the app look by choosing wallpapers with one of the preset themes or choose your own image as a wallpaper.

But this feature is not only about customization - it allows to set different colors and wallpaper for different user profiles and even specific conversations. In case you created a different persona for some of the groups you participate in, this would allow you to avoid being confused and use colors to differentiate conversations better.

### Group improvements

additional group preferences: per-role permissions to send SimpleX links, files and media, voice messages and direct messages.

## SimpleX network

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[Frequently asked questions](../docs/FAQ.md).

Please also see our [website](https://simplex.chat).

## Help us with donations

Huge thank you to everybody who donates to SimpleX Chat!

We are planning a 3rd party security audit for the protocols and cryptography design in July 2024, and also the security audit for an implementation in December 2024/January 2025, and it would hugely help us if some part of this $50,000+ expense is covered with donations.

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX network based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/tree/master#help-us-with-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
