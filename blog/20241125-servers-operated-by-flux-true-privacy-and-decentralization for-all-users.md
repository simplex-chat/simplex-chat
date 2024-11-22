---
layout: layouts/article.html
title: "Servers operated by Flux - true privacy and decentralization for all users"
date: 2024-11-25
previewBody: blog_previews/20241016.html
image: images/20241016-wired-privacy.jpg
imageWide: true
permalink: "/blog/20241125-servers-operated-by-flux-true-privacy-and-decentralization for-all-users.html"
---

# Servers operated by Flux - true privacy and decentralization for all users

**Published:** Nov 25, 2024

- [Welcome, Flux](#welcome-flux--the-servers-are-preset-in-v62-beta1) - the servers are preset in v6.2-beta.1!
- [What's the problem?](#whats-the-problem)
- [Several operators improve connection privacy](#several-operators-improve-connection-privacy)


- [Why didn't we just use some existing solution?](#why-didnt-we-just-use-some-existing-solution)
- [What is next?](#what-is-next)

## Welcome, Flux – the servers are preset in v6.2-beta.1!

[Flux](https://runonflux.com) is a decentralized cloud infrastructure that consists of user-operated computational nodes.

With v6.2 release all SimpleX Chat users can use Flux servers as they are pre-configured in the app - improving metadata privacy and decentralization.

Read on to learn why it is important and how having several operators improves metadata privacy.

## What's the problem?

SimpleX network is decentralized, and the users could use their own servers from day one. As it is fully decentralized, without any central component, there is no full list of SimpleX network servers, but we can observe many hundreds of servers used by the users.

To improve privacy and security of the users, the app randomly chooses a different server for reply messages in each conversation, and also a different server to forward messages.  

But a large number of SimpleX Chat users do not self-host the servers – they use the servers pre-configured in the app. Which means that while the app was choosing different servers, prior to v6.2 they were still be operated by the same company - ourselves.

While our [privacy policy](../PRIVACY.md) expressely prohibits using metadata to learn who connects to whom, and open source code we are [legally bound to use](./20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.md#legally-binding-transparency) doesn't provide the metadata that could undermine users' privacy, user security still depends on us honouring our promises and policies.

This problem is not unique to SimpleX network. For example, Tor anonymity also depends on different servers operated by different parties, which is not possible to verify, as Tor server owners are anonymous.

## Several operators improve connection privacy

To ensure that the users' metadata from different servers cannot be combined to discover who talks to whom, the servers in each conversation have to be operated by different independent organizations.

From v6.2 the app doesn't just choose servers randomly, when both SimpleX Chat and Flux servers are enabled it will always choose servers of different operators in one conversation, hugely increasing metadata privacy for all users.

Flux servers are included as opt-in, and the privacy policy and conditions of use that apply to Flux servers are the same as for SimpleX Chat servers - we agreed to use the same document to minimize the friction for the users.

To improve connection privacy by having Flux servers forward messages to our servers, all you have to do is to enable Flux once the app offers it or via Network & servers settings, and accept that the same conditions apply.

Any additional servers you add to app configuration are treated as belonging to another operator, so they also will be used to improve connection privacy, together with pre-configured servers.

## SimpleX decentralization compared with Matrix, Session and Tor.













With SimpleX servers on [Flux marketplace](https://help.runonflux.io/docs/guide-how-to-host-your-own-simplex-servers-on-flux/) it became easier to run self-hosted servers on their cloud.


- so now the users can easier launch their own servers on their decentralized cloud. But we know that most users won't bother doing it, however simple it is. So we made an agreement with Flux when their servers are added to the app, as their contribution to the network growth, and with a view to be the first pre-configured operator in SimpleX app by the time we develop commercial model for the network operators.




## SimpleX network decentralization approach compared with Matrix, Session and Tor.

Matrix is federated, and it has accounts, so no metadata privacy.

Session and Tor offer network of servers operated by different entities. The promise of this approach to decentralization is that as the client chooses 3 servers radomly to build message delivery path, they are likely to be operated by the different entities, so neither of them would know which IP addresses connect to which.

The problem with this promise though is that the client does not know which entities operate which servers, even with the random choice of the servers there is some probability that in some cases all three servers may be operated by the same entity. Counterintuitively, even if some entity operates only 2% of the servers, the probability that all 3 servers are operated by the same entity becomes in at least one case of 2000 is over 50% - so which IP address connects to which account would be known to storage server in Session and the same is the case with Tor.

We believe that to achieve users privacy, it is not sufficient to simply have a large number of operators, it is important that these operators are known to the users, and that their clients can choose the servers of the different operators.
