# Your chat profiles

## Creating additional chat profiles

SimpleX Chat allows creating as many chat profiles as you like. Same as the first profile, they are only stored locally on your device.

To create an additional SimpleX Chat profile:

- [Open the app settings](#accessing-app-settings).
- Select "Your chat profiles".
- Unlock option via fingerprint or PIN.
- Tap on "+ Add profile”.
- Create a new profile by inputting your display name and full name (optional).
- Tap on "Create".

TODO hidden profiles - to be added in 4.6

## Switching between profiles

- Open the app.
- Tap on your user profile image in the upper right-hand of the screen.
- Select which profile you want to use.

You can also switch profile via Your chat profiles in the settings.

## Incognito mode

This feature is unique to SimpleX Chat – it is independent from chat profiles.

When "Incognito Mode” is turned on, your currently chosen profile name and image are hidden from your new contacts. It allows anonymous connections with other people without any shared data – when you make new connections or join groups via a link a new random profile name will be generated for each connection.

TODO link to post.

To toggle the incognito mode on/off:

- [Open the app settings](#accessing-app-settings).
- Turn on/off incognito mode by tapping on the switch on "Incognito".

## Edit your profile

To edit your profile:

- [Open the app settings](#accessing-app-settings).
- Select your profile.
- Tap on "Edit".
- Input your desired profile name and/or update your full name.
- You can also set and change profile image.
- Tap on "Save and notify contacts".

## Move your chat profiles to another device

SimpleX Chat stores all user data only on client devices using a portable encrypted database format that can be exported and transferred to any supported device.

To export your SimpleX Chat data:

- [Open the app settings](#accessing-app-settings).
- Tap on your user profile image in the upper right-hand of the screen.
- If you have more than one profile, tap it again or choose Settings
- Select "Database passphrase & export".
- Stop chat by toggling "Chat is running".
- Click on "Stop” on the prompt window.
- Confirm it with fingerprint or PIN.
- If you didn't set it before, set a passphrase on "Database passphrase". Initially, the database is encrypted with a random passphrase that is stored in KeyChain (iOS) or with KeyStore (Android).
- Tap on "Export database" - it won't allow exporting unless you choose a passphrase.
- Save the file on your device or share it via any available option, e.g. via AirDrop on iOS.

To import your SimpleX Chat data in the app on another device:

- Transfer the database file to this device.
- Download SimpleX and install it on your new device.
- Create chat profile by following the steps in [Create your first chat profile](#create-yout-first-chat-profile).
- Tap on your user profile image in the upper right-hand corner of the screen to open Settings.
- Select "Database passphrase & export".
- Disable chat by tapping the switch on "Chat is running".
- Click on "Stop” on the prompt window.
- Confirm it with fingerprint or PIN.
- Tap "Import database".
- Select the .zip file of the exported chat data.
- Tap "Import” on the pop-up prompt.
- Start chat via toggle or close and start the app – you will be prompted to enter the passphrase for your chat database.

_Please note_:

1. Currently you cannot move some of the profiles, only the whole database containing all profiles can be moved.

2. You must NOT use the exported database on more than one device at a time, as it may disrupt some of the connections. You also must always use the latest version of the chat database - using the old one is also likely to disrupt your connections.

3. There is no way to recover lost passphrase - make sure to store it securely.
