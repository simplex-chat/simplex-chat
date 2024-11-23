---
layout: layouts/article.html
title: "Servers operated by Flux - true privacy and decentralization for all users"
date: 2024-11-25
# previewBody: blog_previews/20241125.html
image: images/simplexonflux.png
imageWide: true
draft: true
permalink: "/blog/20241125-servers-operated-by-flux-true-privacy-and-decentralization for-all-users.html"
---

# Servers operated by Flux - true privacy and decentralization for all users

**Published:** Nov 25, 2024

- [Welcome, Flux](#welcome-flux--the-new-servers-in-v62-beta1) - the new servers in v6.2-beta.1!
- [What's the problem?](#whats-the-problem).
- [Several operators improve connection privacy](#several-operators-improve-connection-privacy).
- [SimpleX decentralization compared with Matrix, Session and Tor](#simplex-decentralization-compared-with-matrix-session-and-tor).
- [What is next?](#what-is-next)

## Welcome, Flux – the new servers in v6.2-beta.1!

<img src="./images/simplexonflux.png" width="330" class="float-to-right">

[Flux](https://runonflux.com) is a decentralized cloud infrastructure that consists of user-operated nodes.

With v6.2 release all SimpleX Chat users can use pre-configured Flux servers to improve metadata privacy and decentralization.

Read on to learn why it is important and how having several operators improves metadata privacy.

## What's the problem?

SimpleX network is fully decentralized, without any central component or bootstrap nodes &mdash; you could use your own servers from day one. While there is no full list of SimpleX network servers, we see many hundreds of servers in public groups.

But a large number of SimpleX app users use the servers pre-configured in the app. Even though the app randomly chooses 4 servers in each connection to improve privacy and security, prior to v6.2 these servers were operated by the same company - ourselves.

Our our open-source code we are [legally bound to use](./20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.md#legally-binding-transparency) doesn't provide any metadata that could be used to learn who connects to whom. But the privacy of users' connections still depends on us honouring our promises and [privacy policy](../PRIVACY.md).

## Several operators improve connection privacy

To ensure that the users' metadata from different servers cannot be combined to discover who talks to whom, the servers in each connection have to be operated by different independent organizations.

From v6.2 the app doesn't just choose servers randomly, when both SimpleX Chat and Flux servers are enabled it will always choose servers of different operators in each connection to receive messages and for [private message routing](./20240604-simplex-chat-v5.8-private-message-routing-chat-themes.md), increasing metadata privacy for all users.

Flux servers are configured as opt-in, and the privacy policy and conditions of use that apply to Flux servers are the same as for SimpleX Chat servers - we are using the same document to minimize the friction for the users.

To improve connection privacy by having Flux servers forward messages to our servers, all you have to do is to enable Flux once the app offers it or via Network & servers settings, and accept that the same conditions apply.

Any additional servers you add to app configuration are treated as belonging to another operator, so they also will be used to improve connection privacy, together with pre-configured servers, unless they are disabled.

## SimpleX decentralization compared with Matrix, Session and Tor.

SimpleX network decentralization model is different from other decentralized networks in several important aspects.

|                                                | SimpleX | Matrix | Session | Tor-based |
|------------------------------------------------|---------|--------|---------|-----------|
| Fully decentralized                            | ✅      | -      | -       | -         |
| No user profile identity                       | ✅      | -      | -       | -         |
| Server operator transparency                   | ✅      | ✅     | -       | -         |

**Full decentralization**

To be fully decentralized, the network should not have a central component, bootstrap nodes or any global shared state, like in cryptocurrency/blockchain-based communication networks. The presense of any central components introduces an attack vector that undermines privacy and security of the network.

**User profile identity**

The presence of user profile identity, even if it is only a random number or long-term key, undermines privacy of users connections, and allows network operators and observers to establish who talks to whom. Matrix network does not provide connection privacy, as not only user identity exists, it is tied to a specific server that knows all user connections and a part of user's contacts connections. What is worse, Element - the app that most users use to access Matrix network - only offers the servers of one organization to create an account, resulting in a substantial network centralization.

**Server operator transparency**

Operator transparency means that network users know who operates the servers they use. Onion routing used in Tor-based messengers and in Session promises to hide who connects to whom, but as neither Tor nor Session users have knowledge about which entities operate servers, in some cases the clients may be using the servers of the same entity. Statistically, even if only 2% of onion network servers are operated by the attacker, and the client chooses servers randomly, then after about 1750 of such choices the probability of choosing attacker's servers and deanonymizing the connection becomes over 50%.

But if operator transparency helps users privacy, then why Session and Tor don't provide it? The downside of operator transparency is that the operators are known, and the servers data can be requested by the authorities. Such requests, in particular when multiple operators are used by all users, must follow a due legal process, and will not result in compromising the privacy of all users.

With Tor and Session networks such legal process becomes impossible, and it may be seen as advantage by some users. But on the other hand, nothing prevents the attackers, both criminal and state-funded, to compromise Tor and Session users privacy by running 2% or more of its servers - for Tor network it means less than 200 servers, and for Session network this number is even smaller - less than 50 servers.

Because of that, we see operator transparency provided by SimpleX network as a much better trade-off for users' privacy than operator anonymity provided by Session and Tor. Privacy of network participants is a zero sum game - for the users to have it, server operators should be transparent.
