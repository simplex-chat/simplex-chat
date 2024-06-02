---
layout: layouts/article.html
title: "SimpleX network: private message routing, v5.8 released with IP address protection and chat themes"
date: 2024-06-04
# previewBody: blog_previews/20240426.html
draft: true
# image: images/20240426-profile.png
# imageBottom: true
permalink: "/blog/20240604-simplex-chat-v5.8-private-message-routing-chat-themes.html"
---

# SimpleX network: private message routing, v5.8 released with IP address protection and chat themes

**Published:** June 4, 2024

What's new in v5.8:
- [private message routing](#private-message-routing).
- [protect IP address when downloading files & media](#protect-ip-address-when-downloading-files--media).
- [chat themes](#chat-themes) for better conversation privacy.
- [group improvements](#group-improvements) - reduced traffic and additional preferences.
- improved networking, message and file delivery.

Also, we added Persian interface language to the Android and desktop apps, thanks to [our users and Weblate](https://github.com/simplex-chat/simplex-chat#help-translating-simplex-chat).

## Private message routing

SimpleX network design has always been focussed on protecting user identity on the messaging protocol level - there is no user profile identifiers of any kind in the protocol design, not even random numbers or cryptographic keys.

Until this release though, SimpleX network had no in-built protection of user transport identities - IP addresses. As the users choose messaging relays to receive messages, the receiving relays can observe IP addresses of the senders, and if these relays are controlled by the recipients, it means that the recipients themselves could observe them too - either by modifying server code or by using proxy to track all connecting IP addresses.

To work around this limitation, most users used SimpleX Chat either via Tor or via some VPN provider - so that the recipients' relays could not observe IP addresses of the senders. Still, it was the most important limitation of SimpleX network.

This release solves this problem - it implements the extension of SimpleX Messaging Protocol allowing message senders instruct the relays that they trust (the same relays that they used to receive the messages) to forward messages to the destination relays. In this way the recipients' relays can only observe the IP addresses of the forwarding relays, and also cannot observe the sessions of the senders.

At the same time, the relays that forward the messages cannot observe to which connections messages are sent, thanks to the additional layer of end-to-end encryption between the sender and the destination relay - similar to how onion routing works in Tor network.

```
-----------------             -----------------  -- TLS --  -----------------             -----------------
|               |  -- TLS --  |               |  -- f2d --  |               |  -- TLS --  |               |
|               |  -- s2d --  |               |  -- s2d --  |               |  -- d2r --  |               |
|    sending    |  -- e2e --  |    sender's   |  -- e2e --  |  recipient's  |  -- e2e --  |   receiving   |
|    client     |     MSG     |  forwarding   |     MSG     |  destination  |     MSG     |    client     |
|               |  -- e2e --  |     relay     |  -- e2e --  |     relay     |  -- e2e --  |               |
|               |  -- s2d --  |               |  -- s2d --  |               |  -- d2r --  |               |
|               |  -- TLS --  |               |  -- f2d --  |               |  -- TLS --  |               |
-----------------             -----------------  -- TLS --  -----------------             -----------------
```

The diagram above shows all the encryption layers that are used:

**e2e** - two end-to-end encryption layers between sending and receiving clients, one of which uses double ratchet algorithm. These encryption layers exist in the previous version of message routing.

**s2d** - encryption between **sender** and recipient's **destination** relay. This new encryption layer protects message metadata (destination connection address and message notification flag) from the forwarding relay.

**f2d** - additional new encryption layer between **forwarding** and **destination** relay, protecting from traffic correlation in case TLS is compromised - there are no identifiers and cyphertext in common between incoming and outgoing traffic of both relays.

**d2r** - additional encryption layer between destination relay and the recipient, also protecting from traffic correlation in case TLS is compromised.

**TLS** - TLS 1.3 transport encryption.

For private routing to work, both the forwardig and the destination relays should support the updated messaging protocol - it is supported from v5.8 of the messaging servers. It is already released to all relays preset in the app.

Because many self-hosted relays did not upgrade yet, private routing is not enabled by default in the app. To enable, you should choose 





## Protect IP address when downloading files & media

media or files downloads from unknown servers require user confirmation (can be disabled in settings).
improved networking.

## Chat themes

Android and desktop apps: chat themes with preset and custom wallpapers - users can set themes for all chats app-wide, per chat profile and per conversation.

## Group improvements

additional group preferences: per-role permissions to send SimpleX links, files and media, voice messages and direct messages.
