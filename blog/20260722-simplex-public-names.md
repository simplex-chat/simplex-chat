---
layout: layouts/article.html
title: "SimpleX Public Names &mdash; a Name Nobody Can Take From You"
date: 2026-07-22
preview: "You can now give your channel or business a test SimpleX name &mdash; from v7-beta."
image: images/20260722-register-name.png
imageWide: true
permalink: "/blog/20260722-simplex-public-names.html"
---

# SimpleX Public Names &mdash; a Name Nobody Can Take From You

**Published:** Jul 22, 2026

You can now give your channel or business a test SimpleX name that people can remember. Test names are available from v7-beta[^testing].

## Public names for channels and businesses &mdash; without user IDs

<img src="./images/20260722-phone-name.webp" width="19%" class="float-to-right dark:hidden">
<img src="./images/20260722-phone-name-light.webp" width="19%" class="float-to-right hidden dark:block">

Before names, the only way to bring people to your channel or business on SimpleX Network was a link &mdash; but you cannot use a link in a podcast or a poster: nobody would remember it.

Every place where you could get a name so far belongs to someone else: Telegram can revoke your username, a registrar can suspend your domain.

So we designed SimpleX names for no one to own the registry &mdash; on the Ethereum blockchain. If you register `example.simplex`, people type `#example` to join your channel, or `@example.simplex` to message you. If a server operator deletes your link, you can point the name to a new one. Only you control the name with the key in your wallet[^ens].

And we did not add user identifiers to do it. Names are only for those who want to be found &mdash; channels, businesses, communities &mdash; and servers still cannot see who joined your channel or wrote to you.

We plan to launch `.simplex` names later this year, and to provide the first names to [crowdfunding investors](#community-crowdfunding) as perks.

## How to register a name

<img src="./images/20260722-register-name.png" width="38%" class="float-to-right">

To register a test name you need an Ethereum wallet, such as MetaMask, and your SimpleX address or channel link from the app.

Setting up a name takes two steps. On the SimpleX Name Service [test webpage](https://testing-names.simplex.chat), search for the name you want &mdash; currently 6 characters or more &mdash; paste your address or channel link into the page, and complete the registration.

In the app, you need to claim the name for your channel or contact address &mdash; it prevents connecting to your channel or address via any other name. Open your SimpleX address or channel page, tap **Get SimpleX name**, enter the name, and tap Save.

See [this guide](../docs/guide/register-simplex-name.md) for more details.

## How to connect via names

<img src="./images/20260722-connect-name.png" width="288" class="float-to-right">

Type the name into the search bar &mdash; `#example.testing` to join the channel, `@example.testing` to send direct messages. You can also send names in messages &mdash; they work as links.

Connecting via a name is private. Unlike most applications accessing the chain through a centralized RPC service, SimpleX Chat app resolves names via two independent servers of SimpleX Network, so that no server can see both the name and the user's IP address.

Read more about names in the [whitepaper](https://github.com/simplex-chat/simplex-chat/blob/master/docs/protocol/names-overview.md): their purpose, architecture, security model and planned future work.

## Community Crowdfunding

To ensure the long term success of SimpleX Network we established [SimpleX Network Consortium](https://simplexnetwork.org/consortium.html) &mdash; an agreement between a non-profit foundation created for protocol licensing and governance and SimpleX Chat, Inc.

The commercial model for the network that we are building aims to make both our and other businesses on the network profitable. We recently [presented the technology design](https://www.youtube.com/watch?v=UhW8AuoRgxg) for this commercial model at Web3 Summit.

Aug 20, 2026: We now launched [equity crowdfunding on Wefunder](https://wefunder.com/simplex.chat?utm_source=blog). Please read [the next blog post](./20260819-simplex-chat-crowdfunding.md) about it.

Join the channel for updates [simplex.chat/crowdfunding-news](https://simplex.chat/crowdfunding-news/).

[^testing]: Test names are free to register; you only need to pay the blockchain fee. The `.testing` namespace is temporary &mdash; test names will stop working in the app one month after `.simplex` name sales launch.

[^ens]: SimpleX Name Service (SNS) is a fork of Ethereum Name Service (ENS), but without its centralized dependencies. ENS depends on an off-chain indexer and a hosted metadata service. SNS is fully decentralized &mdash; names are indexed and hosted on the blockchain. See [Differences from ENS](https://github.com/simplex-chat/simplex-chat/blob/master/docs/protocol/names-overview.md#differences-from-ens).
