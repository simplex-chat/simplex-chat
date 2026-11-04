# Chat profiles

## Creating additional chat profiles

SimpleX Chat allows creating as many chat profiles as you like. Same as the first
profile, they are stored on your device and shared only with your contacts. You
can use different profiles, for example for family and work, or one with a
pseudonym. All of them receive messages at the same time.

To create an additional SimpleX Chat profile:

- Tap your profile image in the chat list toolbar to open the profile menu.
- Select "Your chat profiles".
- Tap on "Add profile".
- Unlock via fingerprint or PIN, if you have [SimpleX
  Lock](https://simplex.chat/docs/guide/app-settings.md#simplex-lock) enabled.
- Create a new profile by inputting your display name, and optionally a bio and
  a profile image.
- Tap on "Create profile".

## Hiding and muting chat profiles

v4.6 added the ability to mute and to hide chat profiles.

<img src="./images/chat-profiles-hide-profile.png" alt="Hide profile screen with the fields Password to show and Confirm password" width="300">

These actions are available via long-press (Android), swipe right (iOS) or
right-click (desktop) on the profile in the list.

To show hidden profiles, enter the full password in the search bar of "Your chat
profiles". Tap the profile to use it, or choose "Unhide" to unhide it.

## Switching between profiles

- Tap on your user profile image in the lower left-hand of the screen (upper
  left, when the toolbar is at the top).
- Select which profile you want to use.

You can also switch profile via "Your chat profiles" in the same menu.

## Incognito mode

This feature is unique to SimpleX Chat – it is independent from chat profiles.

When you choose "Incognito" for a new connection, your currently chosen profile
name and image are hidden from this contact, group or channel. It allows
anonymous connections with other people without any shared data – a new random
profile name will be generated for each connection. When you share an incognito
profile with somebody, this profile will also be used for the groups they invite
you to.

To choose incognito mode:

- When you create a 1-time link, tap your profile under "Share profile" and
  choose "Incognito".
- When you connect via a link, tap your profile under "Your profile" in the new
  chat and choose "Incognito", before you tap "Connect", "Join group" or "Join
  channel". For some links, the app asks you to choose "Use current profile" or
  "Use new incognito profile" instead.
- When you create a group, turn on "Incognito".

The app remembers your last choice for the next connection.

## Edit your profile

To edit your profile:

- Tap your profile image in the chat list toolbar to open the profile menu.
- Select your profile.
- Input your desired profile name and/or update your bio. You can also add or
  edit a description.
- You can also set and change profile image.
- Tap on "Save (and notify contacts)" (iOS) or "Save and notify contacts"
  (Android and desktop).

## Move your chat profiles to another device

SimpleX Chat stores all user data on client devices using a portable encrypted
database format that can be exported and transferred to any supported device.

You can move all profiles with "Migrate to another device" in the app settings:
the app uploads the encrypted database, and the new device downloads it after
you scan a QR code or paste a link, see [Migrate to another
device](https://simplex.chat/docs/guide/managing-data.md#migrate-to-another-device). You can also export the
database to a file and import it on the new device.

To export your SimpleX Chat data:

- [Open the app settings](https://simplex.chat/docs/guide/app-settings.md#opening-the-app-settings).
- Select "Chat data", then "Database passphrase & export".
- Stop chat by toggling "Chat is running" (tap "Stop” in the confirmation dialog).
- Confirm it with fingerprint or PIN, in case you have [SimpleX Lock](https://simplex.chat/docs/guide/app-settings.md#simplex-lock) enabled.
- If you didn't set it before, [set a
  passphrase](https://simplex.chat/docs/guide/managing-data.md#database-passphrase) in "Database passphrase".
  Initially, the database is encrypted with a random passphrase that is stored
  in Keychain (iOS), with Keystore (Android) or in the app settings as plaintext
  (desktop).
- Tap on "Export database" - it won't allow exporting unless you have set a passphrase.
- Save the file on your device or share it via any available option.

To import your SimpleX Chat data in the app on another device:

- Transfer the database file to the new device.
- Install SimpleX Chat app.
- Create chat profile with any name by following the steps in [Create your first
  chat profile](https://simplex.chat/docs/guide/readme.md#create-your-first-chat-profile) – you will replace
  it soon.
- Open the app settings
- Select "Chat data", then "Database passphrase & export".
- Stop chat by tapping the switch on "Chat is running".
- Tap "Import database".
- Select the .zip file of the exported chat data.
- Tap "Import” in the confirmation dialog.
- Start chat via toggle or close and start the app – you will be prompted to enter the passphrase for your chat database.

**Please note**:

1. Currently you cannot move some of the profiles, only the whole database containing all profiles can be moved.

2. You must not use the exported database on more than one device at a time, as it may disrupt some of the connections. You also must always use the latest version of the chat database - using the old one is also likely to disrupt your connections.

3. There is no way to recover lost passphrase - make sure to store it securely.
