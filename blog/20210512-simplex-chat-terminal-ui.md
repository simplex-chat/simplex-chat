## Announcing SimpleX Chat Prototype!

**Published:** 12 May 2021

Since my post about SimpleX Messaging Protocol (SMP) six months ago, Efim (my son) and I have been working to get SimpleX chat prototype ready to use, and this day has come!

The terminal chat client is available in https://github.com/simplex-chat/simplex-chat - you can either build it from source or download the binary for Linux, Windows or Mac from the latest release.

We’ve been using it between us and some other people for a couple of months now, eating our own “dog food”, and it got to the version 0.3.1, with most of the messaging protocol features we planned:

end-to-end encryption with protection from man in the middle attack - the invitation has to be passed out-of-band (see how to use SimpleX chat in the repo).

no global identity or any usernames visible to the server(s), ensuring full privacy of your contacts and conversations.

message signing and verification with automatically generated RSA keys, with keys being unique per each connection.

authorization of each command/message by the servers with automatically generated RSA key pairs, also unique per connection.

Message integrity validation (via passing the digests of the previous messages).

Encrypted TCP transport, independent of certificates.

You can deploy your own server, but you don’t have to - the demo SMP server to relay your messages is available at smp1.simplex.im:5223 (and it's pre-configured in the client).

We'd really appreciate your feedback, criticism and support - a star on the github repo, signing up to the mailing list or any contribution to the project will help building the new kind of the chat network - the one that lets you control your chat - there is so much more to do!

---

Originally published at https://www.reddit.com/r/haskell/comments/naw6lz/simplex_chat_prototype_terminal_ui_made_in_haskell/
