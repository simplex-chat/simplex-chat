# Privacy and Security

SimpleX Chat default configuration aims to balance privacy, security and convenience. You may want to change the default options.

This page lists the main features and options that affect privacy and security.

## Privacy and Security settings

These settings are available in [Your
privacy](https://simplex.chat/docs/guide/app-settings.md#privacy-and-security) settings.

## End-to-end encryption

Direct chats and groups are protected by end-to-end encryption. When a
conversation starts, the app shows: "Messages, files and calls are protected by
**end-to-end encryption** with perfect forward secrecy, repudiation and break-in
recovery." In a direct chat with quantum resistant encryption, it shows
**quantum resistant e2e encryption** instead.

Messages in channels are not end-to-end encrypted: "Chat relays can see these
messages." See [How channels work](https://simplex.chat/docs/guide/channels.md#how-channels-work).

## Security code verification

<img src="./images/privacy-security-verify-code.png" alt="Security code on iOS" width="288">

SimpleX Chat always establishes connection via link passed via an independent
channel, so servers cannot substitute the encryption keys. In some scenarios,
the invitation links can be substituted in transit (MITM attack). To protect
against such attacks, you should verify the security code with your contacts:

- open the conversation with the contact
- tap the contact name in the chat toolbar
- tap "Verify security code"
- ask your contact to do the same
- the connection is secure if you and your contact have the same security code

This can be validated by one of the following:

- one of you can scan the security code from their device (iOS and Android), if
  the codes match the contact will be marked as verified on the device that
  scanned the code.
- simply tap mark verified if you trust the confirmation from your contact that the code is verified.
- you can also read the code over the voice call.

You can verify the security code of a group member in the same way, in the
member's info.

Read more in [this post](https://simplex.chat/blog/20230103-simplex-chat-v4.4-disappearing-messages.md#connection-security-verification)

## Database passphrase

When installed, the app generates a random passphrase for the chat database and
stores it securely in Keychain (iOS) or using Keystore (Android, secure hardware
is used when available). The desktop app offers to set your own passphrase when
you create your first profile and, if you skip it, stores the random passphrase
in the app settings as plaintext. You can set your own passphrase and also
remove it from the device, in which case you will need to enter it every time
you start the app, and notifications may be limited, in the [Database
passphrase & export](https://simplex.chat/docs/guide/managing-data.md#database-passphrase) settings.

## Incognito mode

This feature generates a random profile name for a new contact or group, when
you choose it for the connection. Read more in [Incognito
mode](https://simplex.chat/docs/guide/app-settings.md#incognito).

## Hidden profiles

This feature allows to hide some of your chat profiles with the password. Read more in [Hiding and muting chat profiles](https://simplex.chat/docs/guide/chat-profiles.md#hiding-and-muting-chat-profiles).

## Network settings

[Transport isolation (BETA)](https://simplex.chat/docs/guide/app-settings.md#transport-isolation-beta) allows
to isolate the traffic of each chat profile (by default) or your traffic with
each contact in a different TCP connection (and Tor circuit, on Android and
desktop).

[Private message routing](https://simplex.chat/docs/guide/app-settings.md#advanced-network-settings) is used
by default: "To protect your IP address, private routing uses your SMP servers
to deliver messages."

**Protect IP address**, in **Your privacy** > **More privacy**, makes the app
ask you to confirm downloads of files from unknown file servers, unless you use
a SOCKS proxy or the .onion address of the server. It is on by default.

## Using Tor

To connect to SMP relays (messaging servers) via Tor you need to install Orbot
app or another app that provides a SOCKS proxy to Tor network.

Android: use Orbot app as SOCKS proxy on port 9050 (default) and enable [Use SOCKS proxy](https://simplex.chat/docs/guide/app-settings.md#use-socks-proxy).

Desktop: use a SOCKS proxy to Tor network in the same way.

iOS: use Orbot app as VPN provider and enable VPN, or set a SOCKS proxy to Tor
in [Advanced network settings](https://simplex.chat/docs/guide/app-settings.md#advanced-network-settings).

You can also change which server addresses are used with [Use .onion hosts option](https://simplex.chat/docs/guide/app-settings.md#use-onion-hosts).

Calls connect without the SOCKS proxy.
