---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat v0.5"
date: 2021-12-08
preview: Support for long-term user addresses in terminal app.
permalink: "/blog/20211208-simplex-chat-v0.5-released.html"
---

# SimpleX announces SimpleX Chat v0.5

**Published:** Dec 08, 2021

## Simplex Chat is the first chat platform that is 100% private by design - SimpleX no access to your connections graph

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat) is our first application, a chat application built on the SimpleX platform that serves as an example of the power of the platform and as a reference application.

## What is SimpleX?

We recognised that there is currently no messaging application which respects user privacy and guarantees metadata privacy -- in other words, messages could be private, but a third party can always see who is communicating with whom by examining a central service and the connection graph. SimpleX, at it's core, is designed to be truly distributed with no central server. This allows for enormous scalability at low cost, and also makes it virtually impossible to snoop on the network graph.

The first application built on the platform is Simplex Chat, which for now is terminal (command line) based with mobile apps in the pipeline. The platform can easily support a private social network feed and a multitude of other services, which can be developed by the Simplex team or third party developers.

## What's new in v0.5?

### Long-term chat addresses

Users can now create long-term chat addresses that they can share with many people (e.g. in email signature, or online), so that any chat user can send them a connection request.

This is an ALPHA feature, and we have not yet added any protection against spam contact requests. However, if the address you created starts receiving spam connection requests, you can simply delete it without losing any of your accepted connections and create another address - as many times as you like!

## We need your help!

We'd really appreciate your comments, criticism and support - a star on the GitHub repo, downloading and testing the chat or any contribution to the project will help a lot – thank you for all your support!

**Please note:** SimpleX Chat is in early stage development: we are still iterating protocols, improving privacy and security, so if you have communication scenarios requiring high security, you should consider some other options for now.

Our goal is to create a new kind of chat platform that lets you control your chat!

Originally published at [https://www.reddit.com/r/haskell/comments/rc0xkn/simplex_chat_the_first_chat_platform_that_is_100/](https://www.reddit.com/r/haskell/comments/rc0xkn/simplex_chat_the_first_chat_platform_that_is_100/)
