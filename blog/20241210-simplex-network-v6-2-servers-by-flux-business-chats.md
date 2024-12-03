---
layout: layouts/article.html
title: "Servers operated by Flux - true privacy and decentralization for all users"
date: 2024-12-10
# previewBody: blog_previews/20241210.html
# image: images/simplexonflux.png
# imageWide: true
draft: true
permalink: "/blog/20241210-simplex-network-v6-2-servers-by-flux-business-chats.html"
---

# SimpleX network: preset servers operated by Flux, business chats and more with v6.2 of the apps

**Will be published:** Dec 10, 2024

This is a placeholder page for the upcoming v6.2 release announcement!

- Preset servers are now operated by two companies - SimpleX Chat and Flux. Read [this post](./20241125-servers-operated-by-flux-true-privacy-and-decentralization-for-all-users.md).
- Business chats to provide support from your business to users of SimpleX network. Read [this page](../docs/BUSINESS.md).
- and more!

## SimpleX network

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-and-security-technical-details-and-limitations).

[Frequently asked questions](../docs/FAQ.md).

Please also see our [website](https://simplex.chat).

## Please support us with your donations

Huge *thank you* to everybody who donated to SimpleX Chat!

Prioritizing users privacy and security, and also raising the investment, would have been impossible without your support and donations.

Also, funding the work to transition the protocols to non-profit governance model would not have been possible without the donations we received from the users.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds &mdash; any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/#please-support-us-with-your-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder

[1] You can also to self-host your own SimpleX servers on [Flux decentralized cloud](https://home.runonflux.io/apps/marketplace?q=simplex).

[2] The probability of connection being de-anonymized and the number of random server choices follow this equation: `(1 - s ^ 2) ^ n = 1 - p`, where `s` is the share of attacker-controlled servers in the network, `n` is the number of random choices of entry and exit nodes for the circuit, and `p` is the probability of both entry and exit nodes, and the connection privacy being compromised. Substituting `0.02` (2%) for `s`, `0.5` (50%) for `p`, and solving this equation for `n` we obtain that `1733` random circuits have 50% probability of privacy being compromised.

Also see [this presentation about Tor](https://ritter.vg/p/tor-v1.6.pdf), specifically the approximate calculations on page 76, and also [Tor project post](https://blog.torproject.org/announcing-vanguards-add-onion-services/) about the changes that made attack on hidden service anonymity harder, but still viable in case the it is used for a long time.
