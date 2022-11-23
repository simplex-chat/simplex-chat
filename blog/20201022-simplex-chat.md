---
layout: layouts/article.html
title: "Simplex Chat"
date: 2020-10-22
preview: The prototype of SimpleX Messaging Server implementing SMP protocol.
permalink: "/blog/20201022-simplex-chat.html"
---

# Simplex chat

**Published:** Oct 22, 2020

[https://simplex.chat](https://simplex.chat)

I'd really appreciate your feedback, criticism and suggestions on the open-source idea I was slowly working on since early 2020. I recently made the demo server for the low-level message queue protocol ("simplex messaging protocol") and the website to try to explain the chat idea that would use this protocol.

Haskell protocol implementation: [https://github.com/simplex-chat/simplexmq](https://github.com/simplex-chat/simplexmq)

In short, the protocol defines a minimalist set of commands and server responses (just 7 commands and 5 responses sent over TCP) to operate encrypted message queues with in-memory persistence - the implementation uses STM.

If anything, it was definitely helping to get to know Haskell types etc. much deeper than before :)

Any criticism would be great - thank you in advance!

Originally published at [https://www.reddit.com/r/haskell/comments/jg6uh4/simplex_chat/](https://www.reddit.com/r/haskell/comments/jg6uh4/simplex_chat/)
