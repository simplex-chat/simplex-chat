---
layout: layouts/article.html
title: "SimpleX Chat: vision and funding, v5.0 released with videos and files up to 1gb"
date: 2023-04-22
preview: This version integrates the support of XFTP protocol, allowing to receive up to 1gb files, even when the sender is offline.
# image: images/20230301-xftp.jpg
permalink: "/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html"
---

# SimpleX Chat: vision and funding, v5.0 released with videos and files up to 1gb.

**Published:** Apr 22, 2023


## SimpleX Chat vision and funding

It was a big decision: whether SimpleX Chat should be a non-profit project or an open-source project led by a commercial company.

During the last 25 years of the Internet, commercial companies have shown a much higher ability to innovate than non-profit organizations. One of the most inspiring examples is NetScape that created the Web as we know it - not only as the destination to access information, but also as an application platform, inventing cookies, SSL and JavaScript, that to this day remain the main building blocks for all web applications.

While SimpleX Chat Ltd is a commercial company, SimpleX Chat software is and will remain open-source. We believe that this way we will create much more value both for the end users, and also for the shareholders and the employees of the company.

Many large tech companies prioritizing value extraction over value creation earned a bad reputation for all businesses, particularly in the communities that value decentralization and privacy. But commercial objectives do not have to result in exploitation. Our goal is to build a new kind of communication network, and also an application platform, that is private by design, fully decentralized and not owned by any single entity, where SimpleX Chat Ltd is one of many organizations that operate the network. I shared my thoughts about how the Internet and privacy might evolve in the interview in [Opt Out Podcast](https://optoutpod.com/episodes/s3e02-simplexchat/).

We started working full-time on the project in 2021 when [Portman Wills](https://www.linkedin.com/in/portmanwills/) and [Peter Briffett](https://www.linkedin.com/in/peterbriffett/) (the founders of [Wagestream](https://wagestream.com/en/) where I led the engineering team) supported the company very early on, and several other angel investors joined later. In July 2022 SimpleX Chat also raised a pre-seed funding from the VC fund [Village Global](https://www.villageglobal.vc) - its co-founder [Ben Casnocha](https://casnocha.com) was very excited about our vision of privacy-first fully decentralized messaging and community platform, both for the individual users and for the companies, independent of any crypto-currencies, that might grow to replace large centralized platforms, such as WhatsApp, Telegram and Signal.

Overall we raised from our investors approximately $370,000 for a small share of the company to allow the project team working full time for almost two years, funding product design and development, infrastructure, and also [the security assessment by Trail of Bits](https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html). A large part of this money is not spent yet.

The project was hugely supported by the community of the users as well - [they donated](https://github.com/simplex-chat/simplex-chat#help-us-with-donations) over $25,000. Without these donations the investment we raised would not be possible, because we believe that voluntary user donations can sustain the project in the long term – it already covers all infrastructure costs. There are only two ways an Internet service can exist - either users are paying for it, or the users data becomes the product for the real customers, as happened with many large Internet companies. In the latter case the users are losing much more money than they are saving by giving away their privacy and the rights to the content they create on the centralized platforms.

Going forward we plan to keep the basic usage of the platform free, and we will be providing the benefits to the project sponsors. For example, there will be additional app icons and user profile badges. There also will be higher file transfer limits – currently we don't limit it at all, but it's unlikely to be sustainable. In any case, the app will remain highly usable for everyone for free, and fully open-source. I know that several other apps are already being developed based on our app core, leading to a fully decentralized network.

Our goals for the next year and a half are to make the messaging network:
- more reliable and resilient by adding redundancy into the message delivery and delivery receipts,
- more private by adding automating rotation of the servers used to deliver messages and by adding delivery relays to better protect IP addresses of the users,
- more usable by adding and improving the functions users are expecting to see in messengers, and also adding some unique functions, like we did with incognito mode, live messages and hidden profiles.

One major initiative we will kick-off this year will be support for large, fully decentralized and private communities and groups not hosted on any servers – something that no Internet platform achieved so far.

To accelerate product development and growth we will be raising a seed funding this year, both from VCs and angel investors, and we also might offer our users an opportunity to participate in the crowd funding round on the same terms as other investors, allowing to both support the project and to benefit from its future growth. Subscribe to our updates not to miss it, connect to the team in SimpleX Chat and join users' groups.

## What's new in v5.0

- [send videos and files up to 1gb](#send-files-up-to-1gb)
- [app passcode independent from system authentication](#app-passcode)
- [networking improvements](#networking-improvements)

Also, we added Polish interface language, thanks to [the users' community and Weblate](https://github.com/simplex-chat/simplex-chat#help-translating-simplex-chat).

SimpleX Chat apps are now available in 10 languages!

### Send videos and files up to 1gb!

In the beginning of March we released servers and command-line utility to send and receive files via XFTP protocols - a very private and secure protocol that sends end-to-end encrypted files in chunks protecting meta-data better than any alternatives we know of.

Now this protocol is fully integrated in SimpleX Chat, and all files except small voice messages are sent using this protocol. Not only it is much faster than before - with the fast Internet connection I can send 25Mb file in 3 seconds and 1gb file in 2 minutes (for most users it's capped at the available Internet bandwidth), it has two other major advantages:

- when you send the file to a group it has to be uploaded only once, regardless of the group size.
- once you sent the file, you no longer need to be online at any time for your contact (or group members) to be able to receive it.

With regards to the metadata privacy it has similar guarantee to SimpleX Messaging Protocol. Even though the files as sent via TLS 1.2/1.3, with the same server identity verification, TLS channel binding and upload authorization approach as used with SMP servers, the metadata is protected even if TLS is compromised - there are no identifiers and ciphertext in common in server received and sent traffic, allowing to correlate only by sending time - which will be less efficient as the server traffic grows.

We also added the ability to send videos in the chat, so they can be played right in the conversation or on full screen without leaving the app – thanks to the efficient and fast file transfer they are very usable.

### App passcode

### Networking improvements

- finally, we added support for IPv6 addresses in both clients and servers
- in Android client you can now configure host and port of the SOCKS proxy, allowing to use apps other than Orbot and to run SOCKS proxy in the local network, to save mobile device batteries - something requested by many users
