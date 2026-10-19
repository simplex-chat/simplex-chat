# Release History

## v6.5

30 April, 2026

Public channels - speak freely!
- Reliability: many relays per channel.
- Ownership: you can run your own relays.
- Security: owners hold channel keys.
- Privacy: for owners and subscribers.

Easier to invite your friends: we made connecting simpler for new users.

Safe web links:
- opt-in to send link previews.
- use SOCKS proxy for previews (if enabled).
- prevent hyperlink phishing.
- remove link tracking.

Non-profit governance: to make SimpleX Network last.

Read more on April 30 at 20:00 UTC: https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html

## v6.4

15 July, 2025

- Connect faster: message instantly once you tap Connect.
- Review group members: chat with new members before they join.
- Chat with admins: send your private feedback to group owners.
- New group role: Moderator - can remove messages and block members.
- Improved message delivery - less traffic on mobile networks.

Read about the new UX for making connections in the blog post: https://simplex.chat/blog/20250703-simplex-network-protocol-extension-for-securely-connecting-people.html

## v6.3

7 March, 2025

Better groups.
- Mention members and get notified when mentioned.
- Send private reports to moderators.
- Delete, block and change role for multiple members at once (Android and desktop only).
- Faster sending messages and faster deletion.

Better chat navigation
- Organize chats into lists to keep track of what's important.
- Jump to found and forwarded messages.

Better privacy and security.
- Private media file names.
- Message expiration in chats.

Read more on March 8: https://simplex.chat/blog/20250308-simplex-chat-v6-3-new-user-experience-safety-in-public-groups.html

## v6.2

7 December, 2024

- SimpleX Chat and Flux (https://runonflux.com) made an agreement to include servers operated by Flux into the app – to improve metadata privacy.
- Business chats – your customers' privacy.
- Improved user experience of chats:
  - Open chat on the first unread message.
  - Jump to quoted messages anywhere in the conversation.
  - See who reacted to messages.
- Improved iOS push notifications.

Read more on December 10: https://simplex.chat/blog/20241210-simplex-network-v6-2-servers-by-flux-business-chats.html

## v6.1

12 October, 2024

Better security:
- SimpleX protocols reviewed by Trail of Bits.
- security improvements (don't worry, there is nothing critical there).

Better calls:
- you can switch audio and video during the call
- share the screen from desktop app.

Better iOS notifications:
- improved delivery, reduced traffic usage.
- more improvements are coming soon!

Better user experience:
- switch chat profile for 1-time invitations.
- customizable message shape.
- better message dates.
- forward up to 20 messages at once.
- delete or moderate up to 200 messages.

The protocols review by Trail of Bits and release announcement will be published on October 14 afternoon here: https://simplex.chat/blog/20241014-simplex-network-v6-1-security-review-better-calls-user-experience.html

## v6.0

11 August, 2024

New chat experience:
- connect to your friends faster.
- archive contacts to chat later.
- delete up to 20 messages at once.
- increase font size.
- new chat themes on iOS - same as on Android and desktop in the previous version.
- reachable chat toolbar - use the app with one hand.

New media options:
- share from other apps (iOS).
- play from the chat list.
- blur for better privacy.

Private routing: it protects your IP address and connections and is now enabled by default.

Connection and servers information: to control your network status and usage.

Read more on 8/14: https://simplex.chat/blog/20240814-simplex-chat-vision-funding-v6-private-routing-new-user-experience.html

## v5.8

3 June, 2024

- private message routing to protect IP addresses (opt-in in this version).
- protect IP address when receiving files.
- chat themes with wallpapers - set themes for all chats app-wide, per chat profile and per conversation - Android and desktop apps.
- some groups permissions can now be granted to admins only.
- improved message and file delivery with reduced battery usage.
- Persian interface language - Android and desktop apps.

Read more: https://simplex.chat/blog/20240604-simplex-chat-v5.8-private-message-routing-chat-themes.html

## v5.7

26 April, 2024

- quantum resistant end-to-end encryption – will be enabled for all direct chats!
- forward and save messages and files, without revealing the source.
- improved calls: in-call sounds when connecting calls, better support for bluetooth headphones.
- customizable shapes of profile images - from square to circle.
- more reliable network connection.

Lithuanian UI language in Android and desktop apps - thanks to our users!

Read more: https://simplex.chat/blog/20240426-simplex-legally-binding-transparency-v5-7-better-user-experience.html

## v5.6

21 March, 2024

1. **Quantum resistant end-to-end encryption** in direct chats (BETA).
   It can be enabled for the new contacts by *Post-quantum E2EE* toggle in dev tools, and for the existing contacts - both users need to tap *Allow PQ encryption* in contact information page (and the toggle in dev tools should be enable for this button to be available).
   Once quantum resistant shared secret is agreed, there will be a message indicating it - it takes about 2-3 messages from each side to be sent in turns before it gets enabled.
   Read more about end-to-end encryption in SimpleX Chat here: https://simplex.chat/blog/20240314-simplex-chat-v5-6-quantum-resistance-signal-double-ratchet-algorithm.html

2. **App data migration**.
   As suggested by one of SimpleX Chat users in our [users group](simplex:/contact#/?v=1-4&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2Fos8FftfoV8zjb2T89fUEjJtF7y64p5av%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAQqMgh0fw2lPhjn3PDIEfAKA_E0-gf8Hr8zzhYnDivRs%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22lBPiveK2mjfUH43SN77R0w%3D%3D%22%7D), you can now migrate all data from one device to a new app installation by uploading it to the configured XFTP relays and then scanning QR code from the new device – choose *Migrate to another device* from the app settings and *Migrate from another device* on the first screen after installing the app.

3. **Use the app during the audio and video calls**.
  Now you can continue using the app, with small video if it's a video call.

Also in this version:
- admins can block a member for all other members.
- much faster leaving and deleting groups.
- filtering chats no longer includes muted chats with unread messages.
- reduced memory usage when sending large files.
- desktop: scrollbars in all views with the scrolling - finally!
- iOS:
  - fixed rendering glitches with messages and context menus.
  - added Hungarian interface language.

The blog post with the announcement is coming on 3/23/2024.

## v5.5

23 January, 2024

- private notes - with encrypted files and media.
- paste link to connect - search bar now accepts invitation links.
- optional recent history in groups.
- improved message delivery - with reduced battery usage.
- reveal secrets in messages by tapping them.
- all files in local app storage are encrypted by default.
- allow deleting the last visible user profile.
- do not share contact address in member profile.
- many fixes!

Also, we added Hungarian (Android only) and Turkish interface - thanks to the users and Weblate (https://github.com/simplex-chat/simplex-chat/tree/stable#help-translating-simplex-chat).

## v5.4

25 November, 2023

- Link mobile and desktop apps via secure quantum-resistant protocol.
- Better groups:
  - Faster to join and more reliable.
  - Create groups with incognito profile.
  - Block group members to hide their messages.
  - Prohibit files and media in a group.
- Better calls:
  - Connect faster and more stable (still far from great).
  - Screen sharing in video calls in desktop app.
- Other improvements:
  - profile names now allow spaces.
  - when you delete contacts, they are optionally notified.
  - previously used and your own SimpleX links are recognised by the app.
  - many fixes and improvements.

## v5.3

22 September, 2023

All apps (Android, iOS, desktop):
- encrypt local files in app storage (except videos).
- improved groups:
  - delivery receipts (up to 20 members).
  - send direct messages to members even after contact is deleted.
  - faster and more stable.
- simplified incognito mode.
- new privacy settings: show last messages & save draft.
- faster app loading.
- reduced memory usage by 40%.
- fixed bug preventing group members connecting (it will only help the new connections).
- iOS app fixes:
  - playing videos on full screen.
  - screen reader for messages.
  - fixed most background crashes.

Also, 6 new interface languages added by the users: Arabic*, Bulgarian, Finnish, Hebrew*, Thai and Ukrainian!

\* Android and desktop only

## v5.2

22 July, 2023

- message delivery receipts – with opt out per contact!
- filter favorite and unread chats.
- keep your connections working after restoring from backup.
- share your address with group members via your chat profile.
- improved disappearing messages.
- a bit more usable groups.
- chat preference to prohibit message reactions.
- restart and shutdown buttons.
- more stable message delivery.

Read more: https://simplex.chat/blog/20230722-simplex-chat-v5-2-message-delivery-receipts.html

## v5.1

22 May, 2023

Mobile apps:
- message reactions 🚀
- self-destruct passcode
- improved messages:
  - voice messages up to 5 minutes.
  - custom time to disappear - can be set just for one message.
  - message editing history.
- setting to disable audio/video calls per contact.
- welcome message visible in group profile.

Android only:
- new design and custom themes for Android - you can share them!
- configurable SOCKS proxy port.
- improved calls on lock screen.
- fixes for sending files.
- locale-dependent formatting of time and date.

Also, the users have added Japanese and Portuguese (Brazil) interfaces (the latter is available on Android only) - huge thanks!

## v5.0

20 April, 2023

- send videos and files up to 1gb - the recipient must have at least version 4.6.1.
- you can self-host XFTP servers and configure the app to use your servers.
- passcode as an alternative to system/device authentication.
- support for IPv6 server addresses.
- configurable SOCKS proxy host and port in Android app.

Also we added Polish interface language – [thanks to the users and Weblate](https://github.com/simplex-chat/simplex-chat#help-translating-simplex-chat).

See more details in this post: https://simplex.chat/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html

## v4.6

25 March, 2023

Mobile apps:

- hidden chat profiles – you can protect them with a password!
- audio/video calls:
  - iOS: completely re-implemented using WebRTC native library and iOS CallKit. Calls now work when the app is in background, and can be answered when the app is fully stopped.
  - Android: added support for bluetooth headphones, volume control in video calls, proximity sensor turns off screen in audio calls.
- group moderation. Admins now can delete member messages and disable members (by assigning "observer" role).
- group welcome message to show to the new users when they join.
- reduced battery usage, particularly when sending messages to large groups.
- Chinese and Spanish interface.

Android app now supports Android 8+ (API 26+), and also supports 32 bit/ARMv7a devices via a separate APK. If you don't know which APK you need, try simplex.apk first. You can check your device CPU in z-cpu app.

Terminal / CLI app:

- hidden profiles are supported.
- improved help, with all supported commands included.

## v4.5

3 February, 2023

- multiple chat profiles: use different names, avatars and transport isolation.
- transport isolation: separate transport connections are used for each chat profile (default) or for each connection (BETA – enable dev tools to make this option available in Network & Servers.)
- message draft: the last message text and any attachments are now preserved when you leave the conversation (while the app is running).
- private filenames: to protect your timezone, image and voice message files now use UTC time.

## v4.4

31 December, 2022

- disappearing messages - with mutual agreement!
- live messages – they update for all recipients as you type them, every few seconds.
- connection security code verification, for contacts and group members – protect from MITM attack (e.g. invitation link substitution).
- performance improvements - faster UI loading, faster group deletion, etc.

Mobile apps:
- French language support in the UI!

iOS app:
- send animated images and "stickers" (e.g., from GIF and PNG files and from 3rd party keyboards)

## v4.3

4 December, 2022

Mobile apps:
- instant voice messages!
- irreversible deletion of sent messages on recipients devices (depends on chat preferences)
- an option to hide the app screen in the recent apps, and also prevent the screenshots on Android
- add SMP servers by scanning QR code, support for server passwords (with the new version 4.0 of SMP server)
- improved privacy and security of SimpleX invitation links in the app

## v4.2

6 November, 2022

- fixed issues from security audit!
- group links - group admins can create the links for new members to join
- auto-accept contact requests + configure to accept incognito and welcome message
- change group member role
- mark chat as unread
- on Android:
  - support for image/gif/sticker keyboards
  - fix keyboard bug with backspace

Beta features (enable Developer tools):
- manually switch contact or member to another address / server
- receive files faster (enable in Privacy settings)

## v4.1

13 October, 2022

Changes:
- automatic message deletion (set TTL per-chat or globally)
- change group member roles
- send multiple images at once
- connection aliases and information view
- share text and files from other apps into SimpleX (Android)
- image gallery (Android)
- scroll to quoted message (Android)
- German translations
- improved connection stability and performance

## v4.0

24 September, 2022

Changes:

Local database encryption with passphrase on iOS, Android, Linux, Mac!

Mobile apps:
- configurable WebRTC ICE servers - see https://github.com/simplex-chat/simplex-chat/blob/stable/docs/WEBRTC.md
- improved stability of establishing direct and group connections, files transfers and message reception.
- support for animated images on Android
- German language UI
- deleting files and media

Terminal app:
- disable messages and notifications per contact / group

For developers:
- [TypeScript SDK for integrating with SimpleX Chat](#typescript-sdk-for-integrating-with-simplex-chat) (e.g., chat bots or chat assistants).

## v3.2

20 August, 2022

Changes:
- use .onion addresses of the servers (if available) when Tor is used – it is based on a separate setting on iOS.
- endless scrolling and search in chats
- UI improvements
- reduced Android APK size (from 200 to 46Mb)

## v3.1

6 August, 2022

Mobile apps:
- secret chat groups!
- support accessing SimpleX messaging servers via Orbot (both iOS and Android)
- new app icons
- advanced network settings
- improved battery usage and traffic

Terminal app:
- support SOCKS5 proxy
- `/info` command to show information and servers for contacts and group members: use `/info <name>` for contact and `/info #<group> <name>` for member information.

## v3.0

9 July, 2022

Changes:

Chat core:
- support for push notifications on iOS
- support for database export/import in mobile clients

Terminal client:
- automatically accept contact requests and sending reply message with `/auto_accept on` and `/auto_accept on <message>` coomands

Mobile clients:
- instant push notifications for iOS (the sending clients have to be upgraded too for notifications to work),
- e2e encrypted WebRTC audio/video calls,
- export and import of chat database, allowing to move the chat profile to another device,
- improved privacy and performance of the protocol.

Please see [this post](https://github.com/simplex-chat/simplex-chat/blob/stable/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md) for more details.

## v2.2

1 June, 2022

Changes:
- WebRTC calls integrated with CallKit (iOS)
- call notifications and alerts (Android)
- local authentication / app lock (both platforms)
- call settings and invitation timeouts
- privacy settings: auto-accepting images, link previews
- paste image from clipboard (iOS)
- SMP servers settings page (iOS)

## v2.1

21 May, 2022

New commands for terminal users:
- /clear - delete all messages in a conversation
- /image - send file as image for mobile clients
- /fforward - forward file to another conversation
- /image_forward - forward image to another conversation

## v2.0

11 May, 2022

For terminal users:
- /tail command to show the last messages from a given chat or from all chats

## v1.6

16 April, 2022

Changes:
- Improved stability of network connection.
- The new protocol to exchange files, in preparation to support images, files and groups in mobile apps. It makes sending files to groups much more efficient, and allows attaching files to the text messages. This version is backwards and forwards compatible, so you can exchange the files with the previous version. It will not be possible to receive the files sent from the next version (1.7) in the previous version (1.5) - please upgrade.
- **Up arrow** key in the terminal can be used to edit the last message you sent.
- CLI option to execute a single command / send one message, e.g. to use in CI to notify about the build completion, or for any other scenario.
- Library support + [chat bot examples](https://github.com/simplex-chat/simplex-chat/tree/stable/apps) to create SimpleX Chat chat bots.

## v1.5

3 April, 2022

Edit, delete and reply to messages, in the mobile apps and from the terminal.

## v1.4

26 March, 2022

Changes:
- message edit and delete in mobile apps
- profile images
- TCP keep-alive replacing SMP protocol pings (improved connection stability)
- bug fixes for chat scrolling and empty chat views

## v1.3

26 February, 2022

Changes:
- markdown support in messages (both platforms)
- user addresses (Android)
- group member names shown in messages
- display name validation
- asynchronous message processing (improved performance)
- search in chats
- Android app UI redesign (welcome page, help view, dark mode fixes)

## v1.2

14 February, 2022

Changes:
- message sent/unread status indicators (iOS)
- search in chats
- auto-accept contact requests option
- deduplicate contact requests
- iOS public beta launch
- connection stability fixes

## v1.1

2 February, 2022

- TLS 1.3 support.
- Terminal app is now also a backend for our new mobile app - public access to our new iOS app via TestFlight is coming soon!
- The code base now includes an iOS app preview.

## v1.0

12 January, 2022

### The most private and secure chat and application platform

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat) is our first application, a messaging application built on the SimpleX platform.

### What is SimpleX?

There is currently no messaging application other than SimpleX Chat that guarantees metadata privacy - who is communicating with whom and when. SimpleX is designed to not use any permanent users identities to protect meta-data privacy. See [SimpleX overview](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more details.

### SimpleX protocol changes

Best possible E2E encryption - the only messenger using two-layer E2E encryption, with one layer using double ratchet protocol that provides forward secrecy and break-in recovery, and additional encryption layer providing meta-data protection. See more details about encryption algorithms in [SimpleXMQ change log](https://github.com/simplex-chat/simplexmq/blob/master/CHANGELOG.md#100).

Performance and space efficiency improvements - protocol overhead is reduced from circa 15% to 3.7% thanks to binary encoding, and performance is substantially improved due to more efficient cryptographic algorithms.

Shorter invitation and contact links due to switching from long RSA to much shorter Curve448/25519 keys - for example, you can connect to the team via [team's SimpleX Chat contact address](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) (you need to use it in terminal app) or just by using `/simplex` command in the chat.

This [this post](https://github.com/simplex-chat/simplex-chat/blob/master/blog/20220112-simplex-chat-v1-released.md) for more information.

