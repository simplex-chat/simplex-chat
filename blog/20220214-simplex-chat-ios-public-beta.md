---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat public beta for iOS"
date: 2022-02-14
preview: Our first prototype of mobile UI for iOS is available!
permalink: "/blog/20220214-simplex-chat-ios-public-beta.html"
---

# SimpleX announces SimpleX Chat public beta for iOS

**Published:** Feb 14, 2022

## Private and secure chat and application platform - [public beta is now available](https://testflight.apple.com/join/DWuT2LQu) for iPhones with iOS 15.

Our new iPhone app is very basic - right now it only supports text messages and emojis.

Even though the app is new, it uses the same core code as our terminal app, that was used and stabilized over a long time, and it provides the same level of privacy and security that has been available since the release of v1 a month ago:

- [double-ratchet](https://www.signal.org/docs/specifications/doubleratchet/) E2E encryption.
- separate keys for each contact.
- additional layer of E2E encryption in each message queue (to prevent traffic correlation when multiple queues are used in a conversation - something we plan later this year).
- additional encryption of messages delivered from servers to recipients (also to prevent traffic correlation).

You can read more details in our recent [v1 announcement](./20220112-simplex-chat-v1-released.md).

## Join our public beta!

Install the app [via TestFlight](https://testflight.apple.com/join/DWuT2LQu), connect to us (via **Connect to SimpleX team** link in the app) and to a couple of your friends you usually send messages to - and please let us know what you think!

We would really appreciate any feedback to improve the app and to decide which additional features should be included in our public release in March.

Should it be:

- images,
- link previews,
- or maybe something else we couldn't think of.

Please vote on the features you think are the most needed in our [app roadmap](https://app.loopedin.io/simplex).

## What is SimpleX?

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter.

We aim to provide the best possible protection of messages and metadata. Today there is no messaging application that works without global user identities, so we believe we provide better metadata privacy than alternatives. SimpleX is designed to be truly distributed with no central server, and without any global user identities. This allows for high scalability at low cost, and also makes it virtually impossible to snoop on the network graph.

The first application built on the platform is Simplex Chat, which is available for terminal (command line in Windows/Mac/Linux) and as iOS public beta - with Android app coming in a few weeks. The platform can easily support a private social network feed and a multitude of other services, which can be developed by the Simplex team or third party developers.

SimpleX also allows people to host their own servers to have control of their chat data. SimpleX servers are exceptionally lightweight and require a single process with the initial memory footprint of under 20 Mb, which grows as the server adds in-memory queues (even with 10,000 queues it uses less than 50Mb, not accounting for messages). It should be considered though that while self-hosting the servers provides more control, it may reduce meta-data privacy, as it is easier to correlate the traffic of servers with small number of messages coming through.

Further details on platform objectives and technical design are available [in SimpleX platform overview](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).
