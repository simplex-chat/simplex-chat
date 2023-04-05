# Your chat profiles

## Creating additional chat profiles

SimpleX Chat allows creating as many chat profiles as you like. As with the first profile, they are only stored locally on your device.

<img src="../../blog/images/20230204-profiles2.png" width="288">

#### To create an additional SimpleX Chat profile:

1. [Open app settings](./app-settings.md#opening-app-settings).
2. Under the **"You""** section, tap **Your chat profiles**.
3. Confirm with fingerprint or PIN, in case you have [SimpleX Lock](./app-settings.md#simplex-lock) enabled.
4. Tap **+ Add profile**.
5. Create a new profile by entering your desired display name and full name (optional).
6. Tap **Create**.

## Hiding and muting chat profiles

v4.6 added the ability to mute and to hide chat profiles.

<img src="../../blog/images/20230328-hidden-profiles1.png" width="288"> &nbsp;&nbsp; <img src="../../blog/images/20230328-hidden-profiles2.png" width="288"> &nbsp;&nbsp; <img src="../../blog/images/20230328-hidden-profiles3.png" width="288">

These actions are available via long-press (Android) or swipe (iOS) on the profile in the list.

To reveal hidden profiles, enter the full password into the search bar.

## Switching between profiles

<img src="../../blog/images/20230204-profiles1.png" width="288">

1. Tap on your user profile picture in the top left-hand corner of the screen.
2. Select which profile you want to use.

Note: You can also switch profile via **Your chat profiles** in app settings.

## Incognito mode

<img src="../../blog/images/20220901-incognito1.png" width="330"> <img src="../../blog/images/20220901-incognito2.png" width="330"> <img src="../../blog/images/20220901-incognito3.png" width="330">

This feature is unique to SimpleX Chat – it is independent from chat profiles.

When **Incognito** mode is toggled on, your current profile name and picture are NOT shared with your new contacts. Instead a new random profile name is generated for each connection you make or group you join via link, allowing you to have as many anonymous connections with other people without any shared data between them.

#### To toggle incognito mode on/off:

1. [Open app settings](./app-settings.md#opening-app-settings).
2. Toggle on/off incognito mode by toggling the **Incognito** switch.

## Edit your profile

#### To edit your profile:

1. [Open app settings](./app-settings.md#opening-app-settings).
2. Select your profile.
3. Tap **Edit**.
4. Options:
   - Choose a new display name. 
   - Choose a new full name. 
   - Choose a new profile picture. 
5. Tap **Save and notify contacts**.

## Migrate your chat profiles to another device

SimpleX Chat stores all user data locally on client devices, using a portable encrypted database format that can be exported and migrated to any supported device.

#### To export your SimpleX Chat data:

1. [Open app settings](./app-settings.md#opening-app-settings).
2. Under the **"Settings"** section, tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop chat.(tap "Stop” in the confirmation dialog).
4. Confirm it with fingerprint or PIN, in case you have [SimpleX Lock](./app-settings.md#simplex-lock) enabled.
5. If you didn't set it before, [set a passphrase](./managing-data.md#database-passphrase) in "Database passphrase". Initially, the database is encrypted with a random passphrase that is stored in KeyChain (iOS) or with KeyStore (Android).
6. Tap **Export database** - it won't allow exporting unless you have set a passphrase.
7. Save the file on your device or share it via any available option.

#### To import your SimpleX Chat data in the app on another device:

1. [Install SimpleX Chat](./README.md#install-simplex-chat) on the new device.
2. Transfer the exported database file to the new device.
3. Create chat profile with any name by following the steps in [Create your first chat profile](#create-your-first-chat-profile) – you will replace it soon.
4. [Open app settings](./app-settings.md#opening-app-settings).
5. Under the **"Settings"** section, tap **Database passphrase & export**.
6. Toggle **Chat is running** off to stop chat.
7. Tap **Import database**.
8. Select the .zip file of your exported SimpleX Chat data.
9. Tap **Import** in the confirmation dialog.
10. Toggle **Chat is running** on to start the chat or close and restart the app – you will be prompted to enter the passphrase for your chat database.

**Please note:**

1. Currently you can't migrate your chat profiles individually, only the whole database containing all your chat profiles can be migrated.

2. You must NOT use the exported database on more than one device at a time, as it may disrupt some of the connections. You also must always use the latest version of the chat database - using the old one is also likely to disrupt your connections.

3. There is no way to recover lost database passphrase - make sure to store it securely.