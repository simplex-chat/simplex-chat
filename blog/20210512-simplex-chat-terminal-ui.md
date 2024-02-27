---
layout: layouts/article.html
title: "Announcing SimpleX Chat Prototype!"
date: 2021-05-12
preview: Prototype chat app for the terminal (console).
permalink: "/blog/20210512-simplex-chat-terminal-ui.html"
---

# Announcing SimpleX Chat Prototype!

**Published:** May 12, 2021

For the last six months [me](https://github.com/epoberezkin) and my son Efim have been working to bring you a working prototype of SimpleX Chat. We're excited to announce SimpleX Chat terminal client is now available [here](https://github.com/simplex-chat/simplex-chat) on Linux, Windows and Mac (you can either build from source or download the binary for Linux, Windows or Mac from the latest release).

We’ve been using the terminal client between us and a few other people for a couple of months now, eating our own “dog food”, and have developed up to version 0.3.1, with most of the messaging protocol features we originally planned

## Features

- End-to-end encryption with protection from man in the middle attack. The connection invitation must be passed out-of-band (see [how to use SimpleX Chat](https://github.com/simplex-chat/simplex-chat#how-to-use-simplex-chat) in the repo).
- No global identity or any usernames visible to the server(s), ensuring full privacy of your contacts and conversations.
- Message signing and verification with automatically generated RSA keys, with keys being unique per each connection.
- Authorization of each command/message by the servers with automatically generated RSA key pairs, also unique per connection.
- Message integrity validation (via passing the digests of the previous messages).
- Encrypted TCP transport, independent of certificates.
- You can deploy your own server, but you don’t have to - the demo SMP server to relay your messages is available at smp1.simplex.im:5223 (pre-configured in the client).

## We need your help!

We're building a new kind of chat network - the only network that lets you control your chat. We'd really appreciate your feedback, criticism and support - a star on the github repo, signing up to the mailing list or any contribution to the project will help. There is so much more to do!

Originally published at [https://www.reddit.com/r/haskell/comments/naw6lz/simplex_chat_prototype_terminal_ui_made_in_haskell/](https://www.reddit.com/r/haskell/comments/naw6lz/simplex_chat_prototype_terminal_ui_made_in_haskell/)
