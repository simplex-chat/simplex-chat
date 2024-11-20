---
layout: layouts/article.html
title: "Wired’s Attack on Privacy"
date: 2024-10-16
previewBody: blog_previews/20241016.html
image: images/20241016-wired-privacy.jpg
imageWide: true
permalink: "/blog/20241016-wired-attack-on-privacy.html"
---

# Servers operated by Flux – better decentralization and metadata privacy.

**Published:** Nov 25, 2024

## What is the problem

From very early days the users of SimpleX network could use their own servers. The majority of the users do not self-host the servers – they use the servers pre-configured in the app.

When the app has multiple servers in its configuration, which is the default for all users, it chooses different servers for different parts of the conversation - it will choose a different server for reply messages in each conversation, and it will also choose a different server to forward messages, in case private message routing is enabled to be used between known servers.

Our servers code and our privacy policy do not allow to correlate multiple connections to the same user. But these are legal rather than technical restrictions. Even with private routing it is technically possible to modify servers code and to collect additional information about the traffic of the users, and it is also possible to combine the data of multiple servers that we operate, to obtain some part of communication graph between the IP addresses of the users, thus undermining promises of the network.

This problem is not unique to SimpleX network - even Tor network anonymity promises are based on certain assumptions, that are not enforced in any way - see the comparison below.

To ensure that the users' metadata from multiple servers cannot be combined to obtain the graph of users connections, these servers have to be operated by different independent entities. And the app has to take into account not only server address, but also which entity operates the server, and use not just different servers, but different operating entities in the same conversation.

## Flux servers now available in beta version of tha app!

Flux is ...

Some time ago Flux added SimpleX servers to their marketplace - so now the users can easier launch their own servers on their decentralized cloud. But we know that most users won't bother doing it, however simple it is. So we made an agreement with Flux when their servers are added to the app, as their contribution to the network growth, and with a view to be the first pre-configured operator in SimpleX app by the time we develop commercial model for the network operators.

The app now takes into account which operator controls the servers, and chooses the servers of different operators within the same conversation. Even if either or both of us modify the server code, we would not be able to reconstruct the communication graph of our users - we would have to collude and exchange data, which is against privacy policy and conditions of use with the end users.

Which means that while the source code and our privacy policy prevents us from combining the data from the

## SimpleX network decentralization approach compared with Matrix, Session and Tor.

Matrix is federated, and it has accounts, so no metadata privacy.

Session and Tor offer network of servers operated by different entities. The promise of this approach to decentralization is that as the client chooses 3 servers radomly to build message delivery path, they are likely to be operated by the different entities, so neither of them would know which IP addresses connect to which.

The problem with this promise though is that the client does not know which entities operate which servers, even with the random choice of the servers there is some probability that in some cases all three servers may be operated by the same entity. Counterintuitively, even if some entity operates only 2% of the servers, the probability that all 3 servers are operated by the same entity becomes in at least one case of 2000 is over 50% - so which IP address connects to which account would be known to storage server in Session and the same is the case with Tor.

We believe that to achieve users privacy, it is not sufficient to simply have a large number of operators, it is important that these operators are known to the users, and that their clients can choose the servers of the different operators.