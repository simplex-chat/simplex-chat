# Privacy and Security

SimpleX Chat's default configuration aims to balance privacy, security, and convenience. You may want to change the default options to suit your specific needs.

This page lists all the features and options that affect privacy and security.

## Privacy and Security settings

These settings are available in the [Privacy & Security settings](./app-settings.md#privacy-and-security).

## Security code verification

<img src="../../blog/images/20230103-verification.png" width="288">

While SimpleX Chat always establishes connection via link passed via an independent channel, so it is already more protected than other apps, there are scenarios when the invitation links can be substituted in transit (MITM attack). To protect against such attacks, you should verify your security code with your contacts.

#### To verify security code:

1. Tap on your contact. 
2. Tap on your contact's name at the top of the screen.
3. Tap **Verify security code**.
4. Ask your contact to follow the first three steps and compare your security code with theirs. 

The connection is secure if both you and your contact have the same security code. This can be validated by one of the following methods:

- One of you can scan the other's security code in person. If the codes match, the contact will be marked as verified on the device that scanned the code.
- Simply tap **Mark verified** if you trust the confirmation from your contact that the code is verified.
- You can also read out your security code over a voice call with your contact.

Please read [this blog post](../../blog/20230103-simplex-chat-v4.4-disappearing-messages.md#connection-security-verification) for more details.

## Database passphrase

When installed, the app generates a random passphrase for the chat database and stores it securely in KeyChain (iOS) or using KeyStore (Android, TPM module is used when available). You can set your own passphrase and also remove it from the device, in which case you will need to enter it every time you start the app, and notifications may be limited, in the [Database passphrase & export](./managing-data.md#database-passphrase) settings.

## Incognito mode

This feature generates a random profile name for each new contact. Please read the [Incognito](./app-settings.md#incognito) section for more details.

## Hidden profiles

This feature allows to hide some of your chat profiles with a password. Please read the [Hiding and muting chat profiles](./chat-profiles.md#hiding-and-muting-chat-profiles) section for more details.

## Network settings

[Transport isolation (BETA)](./app-settings.md#transport-isolation-beta) allows to isolate you traffic with each contact in a different TCP connection (and Tor circuit).

## Using Tor

<img src="../../blog/images/20220808-tor1.png" width="330"> &nbsp; <img src="../../blog/images/20220808-tor2.png" width="330">

To connect to SMP relays (messaging servers) via Tor, you need to install Orbot app.

**Android:** use Orbot app as SOCKS proxy on port 9050 (default) and enable [Use SOCKS proxy](./app-settings.md#use-socks-proxy-android-only).

**iOS:** use Orbot app as VPN provider and enable VPN.

You can also change which server addresses are used with [Use .onion hosts option](./app-settings.md#use-onion-hosts).