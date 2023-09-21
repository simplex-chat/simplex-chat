---
layout: layouts/article.html
title: "SimpleX Chat v5.3 released: desktop app, local file encryption and improved groups with directory service"
date: 2023-09-25
# image: images/20230925-desktop-app.png
# previewBody: blog_previews/20230722.html
permalink: "/blog/20230925-simplex-chat-v5-3-desktop-app-local-file-encryption-directory-service.html"
draft: true
---

# SimpleX Chat v5.3 released: desktop app, local file encryption and improved groups

**Published:** September 25, 2023

**What's new in v5.3:**
- [new desktop app!](#multiplatform-desktop-app)!
- [encrypted local files and media with forward secrecy](#encrypted-local-files-and-media-with-forward-secrecy).
- [directory service](#group-directory-service) and other [group improvements](#group-improvements).
- [simplified incognito mode](#simplified-incognito-mode)

There are a lot of other improvements and fixes in this release:
- improved app responsiveness and stability.
- new privacy settings: show last messages & save draft.
- fixes:
  - bug preventing group members connecting (it will only help the new connections).
  - playing videos on full screen<sup>**</sup>.
  - screen reader for messages<sup>**</sup>.
  - reduced background crashes<sup>**</sup>.

Also, we added 6 new interface languages: Arabic<sup>*</sup>, Bulgarian, Finnish, Hebrew<sup>*</sup>, Thai and Ukranian - thanks to [our users and Weblate](https://github.com/simplex-chat/simplex-chat#help-translating-simplex-chat).

\* Android only.

\*\* iOS only.

## Multiplatform desktop app

<img src="/docs/images/simplex-desktop-light.png" width="500">

The desktop app is based on the same code as our Android app, so it has most of the features of Android app!

To use desktop app you will need to **create a new profile**. As SimpleX platform has no user accounts, it's not as simple as for centralized apps to share access to the same profile from two devices.

We are working on the solution to use your mobile profile from desktop app - it will be released in the next version. For now, as a workaround, you can join groups from both mobile and desktop device, and use small groups instead of direct conversations.

When you start the app first time, you will be offered to **set database passphrase** – you have to memorize it, as there is no way to recover it.

If you skip it, a random passphrase will be generated and stored on your desktop device as plaintext (unencrypted) – you can change it later.

Other limitations:
- you can play voice messages, but you cannot record them.
- there is no support for calls yet.

## Encrypted local files and media with forward secrecy

All messages, files and media in SimpleX Chat were always end-to-end encrypted from the very beginning. SimpleX Chat uses double-ratchet algorithm with encrypted message headers, for the best possible meta-data protection.

You contacts, groups and messages are stored in the local database on your device, and this database was encrypted starting from [v4.0 released a year ago](./20220928-simplex-chat-v4-encrypted-database.md).

But until this version all files and media in the app storage were not encrypted, and when you exported the chat archive, they were unencrypted there as well.

From v5.3 all files and media (except videos for now) are encrypted with a random symmetric key - in many cases they are encrypted before they are written to the storage. The key itself is stored in the encrypted database, associated with the message, so if you delete the message with the attached file and media, the key will be irreversibly deleted as well, so even if an attacker gains access to your database, they won't be able to decrypt the file.

So unlike file encryption schemes that use the same passphrase to encrypt files as for the database, this approach provides forward secrecy for locally stored files.

## Group directory service

## Group improvements

- improved groups layout, aggregated "member connected" events.

group performance and stability improvements.
- send direct messages to group members even after contact is deleted.

- message delivery status in groups.

## Simplified incognito mode