### Does Simplex support post quantum cryptography?
Not fully yet, but it's a work in progress. For more details, please check:  [PQC docs](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/rfcs/2023-09-30-pq-double-ratchet.md)
### Can I self-host my own server?
Of course! Please check this tutorial: [SMP server](https://simplex.chat/docs/server.html)
### Can I use the same profile on desktop? Do messages sync cross-platform?
You can use your profile from mobile device on desktop. However, to do so you need to be on the same network, both on your mobile and desktop. More about it:[Release info](http://simplex.chat/blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.html#link-mobile-and-desktop-apps-via-secure-quantum-resistant-protocol)
### How are you funded?
We received some funding from few VCs (address or something?). Is it a bad thing? We argue it's a necessity. [Why VCs are impornat?](https://www.poberezkin.com/posts/2023-10-31-why-privacy-impossible-without-venture-funding.html) What's more, we are generously supported via donations. (: [Another link](https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html#how-is-it-funded-and-what-is-the-business-model)
### _I have nobody to chat with! Where can I find any groups?_
Please check our Directory in the first place. You might find some interesting groups and meet even more interesting people. [Directory]( http://simplex.chat/docs/directory.html)
### What is database? What can I do with it?
Database is essential for SimpleX Chat to function properly. In comparison to centralised messaging providers, it is _the user_ who is responsible for taking care of it. On the other hand, user is sure that _nobody but them_ has access to it. Please read more about it: [Database](http://simplex.chat/docs/guide/managing-data.html)
### Can I send files over Simplex? 
Of course! While doing so, you are using a _state-of-the-art_ protocol that greatly reduces metadata leaks. Please read more about it: [XFTP Protocol](https://simplex.chat/blog/20230301-simplex-file-transfer-protocol.html)
### What’s incognito profile?
 This feature is unique to SimpleX Chat – it is independent from chat profiles. 

When "Incognito Mode” is turned on, your currently chosen profile name and image are hidden from your new contacts. It allows anonymous connections with other people without any shared data – when you make new connections or join groups via a link a new random profile name will be generated for each connection. 
### How do invitations work?
It is a quite complicated process, but fortunately all of this happens in the background. 
To put it simply-whenerver somebody connects to you via your address, they basically ask your client whether they want to establish connection. After that, you can either agree or disagree.
If interested, please read more: [Addresses and invitations](https://simplex.chat/docs/guide/making-connections.html)
### How to configure and delete groups?
Please check: [Tutorial](https://simplex.chat/docs/guide/secret-groups.html)
### Are there any reactions to messages? Can I answer specific messages directly?
Yes! Currently, there are six emojis available. What's more, you can respond to specific message by holding it and selecting _Reply_.
### What do checkmarks mean?
It's quite simple:
- one checkmark -> message is delivered to the relay
- two checkmarks -> message is delivered to the recipient
"sent" means accepted by the relay for delivery, "delivered" - stored on the recipient device