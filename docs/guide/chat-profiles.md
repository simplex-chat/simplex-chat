---
title: Chat profiles
---
# Your chat profiles

## Creating additional chat profiles

SimpleX Chat allows creating as many chat profiles as you like. Same as the first profile, they are only stored locally on your device.

<img src="../../blog/images/20230204-profiles2.png" width="288">

To create an additional SimpleX Chat profile:

- [Open the app settings](./app-settings.md#opening-the-app-settings).
- Select "Your chat profiles".
- Unlock option via fingerprint or PIN.
- Tap on "+ Add profile”.
- Create a new profile by inputting your display name and full name (optional).
- Tap on "Create".

## Hiding and muting chat profiles

v4.6 added the ability to mute and to hide chat profiles.

<img src="../../blog/images/20230328-hidden-profiles1.png" width="288"> &nbsp;&nbsp; <img src="../../blog/images/20230328-hidden-profiles2.png" width="288"> &nbsp;&nbsp; <img src="../../blog/images/20230328-hidden-profiles3.png" width="288">

These actions are available via long-press (Android) or swipe (iOS) on the profile in the list.

To unhide hidden profiles, enter the full password in the search bar.

## Switching between profiles

<img src="../../blog/images/20230204-profiles1.png" width="288">

- Tap on your user profile image in the upper right-hand of the screen.
- Select which profile you want to use.

You can also switch profile via Your chat profiles in the settings.

## Incognito mode

<img src="../../blog/images/20220901-incognito1.png" width="330"> <img src="../../blog/images/20220901-incognito2.png" width="330"> <img src="../../blog/images/20220901-incognito3.png" width="330">

This feature is unique to SimpleX Chat – it is independent from chat profiles.

When "Incognito Mode” is turned on, your currently chosen profile name and image are hidden from your new contacts. It allows anonymous connections with other people without any shared data – when you make new connections or join groups via a link a new random profile name will be generated for each connection.

To toggle the incognito mode on/off:

- [Open the app settings](./app-settings.md#opening-the-app-settings).
- Turn on/off incognito mode by tapping on the switch on "Incognito".

Please read more in [this post](../../blog/20220901-simplex-chat-v3.2-incognito-mode.md#incognito-mode).

## Edit your profile

To edit your profile:

- [Open the app settings](./app-settings.md#opening-the-app-settings).
- Select your profile.
- Tap on "Edit".
- Input your desired profile name and/or update your full name.
- You can also set and change profile image.
- Tap on "Save and notify contacts".

## Move your chat profiles to another device

SimpleX Chat stores all user data only on client devices using a portable encrypted database format that can be exported and transferred to any supported device.

To export your SimpleX Chat data:

- [Open the app settings](./app-settings.md#opening-the-app-settings).
- Select "Database passphrase & export".
- Stop chat by toggling "Chat is running" (tap "Stop” in the confirmation dialog).
- Confirm it with fingerprint or PIN, in case you have [SimpleX Lock](./app-settings.md#simplex-lock) enabled.
- If you didn't set it before, [set a passphrase](./managing-data.md#database-passphrase) in "Database passphrase". Initially, the database is encrypted with a random passphrase that is stored in KeyChain (iOS) or with KeyStore (Android).
- Tap on "Export database" - it won't allow exporting unless you have set a passphrase.
- Save the file on your device or share it via any available option.

To import your SimpleX Chat data in the app on another device:

- Transfer the database file to the new device.
- Install SimpleX Chat app.
- Create chat profile with any name by following the steps in [Create your first chat profile](#create-yout-first-chat-profile) – you will replace it soon.
- Open the app settings
- Select "Database passphrase & export".
- Stop chat by tapping the switch on "Chat is running".
- Tap "Import database".
- Select the .zip file of the exported chat data.
- Tap "Import” in the confirmation dialog.
- Start chat via toggle or close and start the app – you will be prompted to enter the passphrase for your chat database.

**Please note**:

1. Currently you cannot move some of the profiles, only the whole database containing all profiles can be moved.

2. You must NOT use the exported database on more than one device at a time, as it may disrupt some of the connections. You also must always use the latest version of the chat database - using the old one is also likely to disrupt your connections.

3. There is no way to recover lost passphrase - make sure to store it securely.
