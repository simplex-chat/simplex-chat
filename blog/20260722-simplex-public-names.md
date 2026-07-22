---
layout: layouts/article.html
title: "SimpleX Public Names - for Channels and Businesses"
date: 2026-07-22
preview: "Test public names are released in v7-beta &mdash; register a name for your channel or contact address, so anyone can connect to it by typing the name in the app."
image: images/20260722-register-name.png
imageWide: true
permalink: "/blog/20260722-simplex-public-names.html"
---

# SimpleX Public Names &mdash; for Channels and Businesses

**Published:** Jul 22, 2026

Test public names are released in v7-beta &mdash; register a name for your channel or contact address, so anyone can connect to it by typing the name in the app.

## Public names &mdash; without user identifiers

<img src="./images/20260722-phone-name.webp" width="19%" class="float-to-right dark:hidden">
<img src="./images/20260722-phone-name-light.webp" width="19%" class="float-to-right hidden dark:block">

If you own the name `example.simplex`, other users will be able to type `#example` to join your channel, or `@example.simplex` to connect to your profile. The test namespace `.testing` is available now &mdash; temporary test names are free to register[^testing]. The main `.simplex` namespace will be launched later this year, and the first names will be provided as perks to [SimpleX Chat crowdfunding investors](#community-crowdfunding).

SimpleX Network still has no user IDs. Public names are opt-in and only useful for public entities &mdash; businesses, channels, and communities. Even when you use SimpleX name, network servers cannot see who connects via this name: all connections remain private.

A SimpleX name cannot be taken away. Names are registered on the Ethereum blockchain, and even if the channel link or address are deleted by the server operator, the name owner can point it to a new link, and they would remain reachable via the same name. The name registry is SimpleX Name Service (SNS) &mdash; it's a fork of ENS, but without its centralized dependencies[^ens].

## How to register a name

<img src="./images/20260722-register-name.png" width="38%" class="float-to-right">

To register a test name you need an Ethereum wallet, such as MetaMask, and your SimpleX address or channel link from the app.

Setting up a name takes two steps. On the SimpleX Name Service [test webpage](https://testing-names.simplex.chat), search for the name you want &mdash; currently 6 characters or more &mdash; paste your address or channel link into the page, and complete the registration.

In the app, you need to claim the name for your channel or user profile &mdash; it prevents your channel or profile being opened via any other name. Open your SimpleX address or channel page, tap **Get Your SimpleX name**, enter the name, and tap Save.

See [this guide](https://github.com/simplex-chat/simplex-chat/blob/master/docs/guide/register-simplex-name.md) for more details.

## How to connect via names

<img src="./images/20260722-connect-name.png" width="288" class="float-to-right">

Type the name into the search bar &mdash; `#example.testing` to join the channel, `@example.testing` to connect to the user. You can send names in messages &mdash; they work as links.

Connecting via a name is private. Unlike most blockchain applications accessing the chain through centralized RPC providers, SimpleX Chat app resolves names via two independent servers of SimpleX Network, so that no server can see both the name and the user's IP address.

Read more about names in the [whitepaper](https://github.com/simplex-chat/simplex-chat/blob/master/docs/protocol/names-overview.md): their purpose, architecture, security model and planned future work.

## Community Crowdfunding

To ensure the long term success of SimpleX Network we established [SimpleX Network Consortium](https://simplexnetwork.org/consortium.html) &mdash; an agreement between a non-profit foundation created for protocol licensing and governance and SimpleX Chat, Inc..

The commercial model for the network that we are building aims to make both our and other businesses on the network profitable. We recently [presented the design of this commercial model](https://www.youtube.com/watch?v=UhW8AuoRgxg&t=2s) at Web3 Summit.

The crowdfunding we plan will fund this development. You can [register your interest](https://simplexchat.typeform.com/crowdfunding) to participate in crowdfunding, and join the [SimpleX Crowdfunding News channel](https://smp10.simplex.im/c#q09nMBmWFGz1m2TvgfZFaEOG5D2a7Ma9mSkl6pHXEsg) for updates.

_Disclaimer: SimpleX Chat is testing the waters for a possible Reg CF offering. We’re not asking for or accepting any money right now, and we won’t accept any if sent. We can’t accept any offers to buy securities or take any payments until the official filing is done and it’s live through a regulated platform. Our testing the waters and your possible indications of interest doesn’t create any obligation or commitment of any kind._


[^testing]: Test names are free to register, you only need to pay blockchain fee. The `.testing` namespace is temporary &mdash; test names will stop working in the app one month after `.simplex` name sales launch.

[^ens]: Ethereum Name Service depends on an off-chain indexer and a hosted metadata service to render name tokens. SNS removes both &mdash; names, subnames, and records are indexed and rendered on-chain &mdash; and subnames follow the DNS ownership model: owned and transferred together with the name. See [Differences from ENS](https://github.com/simplex-chat/simplex-chat/blob/master/docs/protocol/names-overview.md#differences-from-ens).
