# SimpleX announces SimpleX Chat v3

**Published:** Jul 11, 2022

## New in version 3

### Instant notifications for iOS

I wrote previously about our design for iOS notifications, and this is now released. Users will need to migrate the database for it to work - the app offers it when updated - and they can then choose which notifications mode they want to use – instant or periodic push notifications or previously available periodic background refresh that does not rely on push notifications. There information about the differences and limitations is available in the app.

TODO notification modes screen

### End-to-end encrypted audio/video calls

You can now call your contacts via WebRTC, connecting via SimpleX Chat relay servers or peer-to-peer, and in the near future you will be able to configure your own STUN/TURN servers used to establish the connection. The calls are end-to-end encrypted using the key that is negotiated via the connection you already have with your contact in the chat, that is also used as a signalling layer for WebRTC - in most cases only three messages in total have to be sent by your and your contact clients, including the initial call invitation, for the call to start.

TODO in-progress call picture

### Database export and import

Quite a few users asked - how can I move my chat profile to a new device? V3 has a solution to that - you can now export chat database from one device and import it into another - even if this other device is another platform, e.g. you can move chat database from Android phone to iOS or to our terminal (console) client.

Some important limitations:

- you cannot run the same chat profile from two devices, neither at the same time nor in turns. - you should only use the latest database version, and every time you want to move it to another device you need to export a new chat archive from the device that was the latest to use it, and import it on the device where you plan to use it.

You can think about some helpful use cases of this feature - e.g. the full chat profile can be forwarded to another person via some cloud storage, or you can store it for youself to pick up later - e.g. if you want to temporarily remove the app from the phone.

## SimpleX platform

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat) is our first application, a messaging application built on the SimpleX platform.

### The first (and we believe the only) messaging platform without user identifiers of any kind - 100% private by design!

To protect identities of users and their connections, SimpleX Chat has no user identifiers visible to the network – unlike any other messaging platform. Not only SimpleX doesn't use phone numbers or emails, as Signal and many other platforms, it also does not have any persistent identifiers to identify users - unlike many other messengers considered private - Session, Cwtch, Ricochet, Threema, etc., - all these platforms have user identifiers.

### Why is it bad for the users

When each user has a unique identifier on the platform, even if this is just a random number, e.g. as a Session ID, it creates risks that whoever gains access to the platform data can observe how the users are connected and how many messages are transmitted between, and then correlate this information with the existing public networks, determining the identities of some users. Even with the most private messengers built on top of Tor network, having a persistent identity means that if you talk to two different users via the same profile they can prove that they communicate with the same person, as they use the same address to send messages.

SimpleX platform avoids these risks by not having any user identity from its design.

### How does it work

Many people asked: _if SimpleX has no user identifiers, how can it deliver messages?_

I wrote about it in [v2 release announcement](./20220511-simplex-chat-v2-images-files.md) and you can get more information about SimpleX platform objectives and technical design in [the whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).
