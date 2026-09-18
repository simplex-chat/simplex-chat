---
layout: layouts/article.html
title: "SimpleX Supporter Badges &mdash; Send Larger Files and Fund the Network Without Being Identified"
date: 2026-09-21
preview: "Supporter badges are available in v7.1 beta: a badge on your profile, larger files and longer file storage &mdash; and nobody can tell that you paid."
image: images/20260921-badge.png
permalink: "/blog/20260921-simplex-supporter-badges.html"
---

# SimpleX Supporter Badges &mdash; Send Larger Files and Fund the Network Without Being Identified

**Published:** Sep 21, 2026

You can now support SimpleX Chat and get a supporter badge, larger files and longer file storage &mdash; from v7.1 beta[^beta]. Watch [how to buy a badge](https://www.youtube.com/watch?v=gHCpFG8UsmM).

## A paid feature that cannot identify you

<img src="./images/20260921-phone-badge.png" width="19%" class="float-to-right">

A supporter badge is shown on your profile to your contacts, group members and channel subscribers. With a badge you can send files up to 2GB, or 5GB with a legend badge, instead of 1GB, and servers keep your files for longer - 7 days with a supporter badge and 21 days with a legend badge.

In every other messenger a paid feature is attached to your account, so the operator knows who paid and what they do with the feature. SimpleX Network has no accounts, and a paid feature must not create one.

So a badge is a credential kept on your device. Every time it is shown to a contact or presented to a server, the app generates a new zero-knowledge proof that reveals only the badge type and the expiry date, and no two proofs can be linked to each other or to the purchase, by the badge service, your contacts or servers. Credentials are issued for one month at a time, and all badges expire on the same day of the week, so a badge places you among all supporters of that week and nothing more.

This is only possible because the network has no user identifiers. A badge on an account identifies its holder, however the badge was paid for[^signal].

Read more about badges in the [whitepaper](https://github.com/simplex-chat/simplex-chat/blob/master/docs/protocol/badges-overview.md): what they grant, how they are issued and presented, and their privacy and security model.

## How to get a badge

Buy a code on [badges.simplex.chat](https://badges.simplex.chat), paying by card, with Bitcoin or Monero, and redeem it in the app: open Settings, tap **Supporter perks**, and enter the code. The badge appears on your profile. Badge doesn't renew by itself, and no account is created.

The v7.1 release will add purchases to the app using in-app payments, and if you downloaded the app via GitHub or F-Droid, you would still be able to pay via card or with cryptocurrencies.

## What badges will do next

The same mechanism will be used for other resources that cost the network more than the default. Two uses are coming:

- **Backups.** A backup will be a link whose content the app updates in place, so the same link restores the latest state. A badge will extend how long the backup is kept on the servers &mdash; that is, how long the app can stay offline before the backup is lost.
- **Better limits on servers.** Servers that verify the badge will apply higher rate limits for creating messaging queues, uploading file chunks, and registering notification tokens.

## Community Crowdfunding

<a href="https://wefunder.com/simplex.chat?utm_source=blog"><img src="./images/20260819-wefunder.jpg" width="40%" class="float-to-right"></a>

Investors in our [equity crowdfunding on Wefunder](https://wefunder.com/simplex.chat?utm_source=blog) receive badges as perks:

| Investment | Badge |
|---|---|
| $100 | supporter, 2 months |
| $250 | supporter, 4 months |
| $500 | supporter, 6 months |
| $1,000 | supporter, 12 months, or legend, 2 months |
| $2,500 | legend, 4 months* |
| $5,000 | legend, 6 months* |
| $10,000 | legend, 12 months |

* If you invest $1000 or more, you can also get supporter badge for 12 months, if you prefer.

If you invest by September 22, you will receive 2 extra months. We will send the codes once the rolling close starts in October.

Also, if you invest $500 or more, you will receive [a public SimpleX name](https://simplex.domains?utm_source=blog) &mdash; for 7 years if you invest by September 22, for 5 years for early bird investors, and for 3 years after that - ahead of public launch on December 12.

Learn more and invest on Wefunder: [https://wefunder.com/simplex.chat](https://wefunder.com/simplex.chat?utm_source=blog)

[^beta]: v7.1 beta is available via [Play Store](https://play.google.com/store/apps/details?id=chat.simplex.app) (Android beta), [TestFlight](https://testflight.apple.com/join/DWuT2LQu) (iOS), our [F-Droid repo](https://simplex.chat/fdroid/) and [GitHub](https://github.com/simplex-chat/simplex-chat/releases) (Android and desktop).

[^signal]: Signal's donation badges have less private design: the payment is not linked to the account cryptographically, but the badge is an attribute of the account, so Signal's server holds the list of everyone who has one, and the credential can be verified only by Signal's server. A SimpleX badge is verified by contacts and by independently operated servers, and none of them can link two presentations of the same badge.
