---
title: Managing data

---

# Managing Your Data

SimpleX Chat stores all your data locally on your device; it is not stored on SimpleX servers or anywhere else. Only you have complete control of it. Click or tap on the following sections to learn more:

- [Automatic message deletion](#automatic-message-deletion)

- [Chat database](#chat-database)

- [Delete files & media](#delete-files--media) 

## Automatic message deletion

Here you can choose to delete messages from your local chat database after a set period of time.

#### To enable automatic message deletion:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).

2. Click or tap **Database passphrase & export**.

3. From the **Delete messages after** dropdown, choose one of the following options:
   
   - `never (default)`
   
   - `1 month`
   
   - `1 week`
   
   - `1 day`

4. Click or tap **Delete messages**.

**Please note**: This feature does not delete messages for other users, only for you. It is set independently for each chat profile.

## Chat Database

SimpleX Chat stores all your data locally in a portable, encrypted database that can be migrated to any supported device. 

### Database passphrase

<img src="../../blog/images/20220928-passphrase.png" width="330">

On mobile, SimpleX Chat encrypts the database with a random passphrase by default and stores it securely in KeyChain (iOS) or using KeyStore (Android, TPM module is used when available) before you start using the app. On desktop, SimpleX Chat gives you the option to either generate a random passphrase or set a new passphrase before you start using the app. It's highly recommended to set a new database passphrase and store it somewhere safely and securely. Without it, you won't be able to perform certain actions such as exporting your database or removing your database passphrase from app settings for increased security.

#### To set a new database passphrase:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Database passphrase**.
5. Enter your current passphrase, if you already have one set up.
6. Choose your new passphrase.
7. Confirm your new passphrase.
8. Click or tap **Update database passphrase**.

#### To remove your database passphrase from app settings:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Database passphrase**.
5. Toggle **Save passphrase in settings** off.

**Please note**: removing your passphrase from the app's settings will prompt you to manually enter it every time you start the app and this may impact receiving notifications.

### Open database folder (Desktop only)

Here you can use this option to open up the current location of your SimpleX Chat database on your desktop's file manager. 

#### To open database folder on your desktop:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Click or tap **Open database folder**. 

### Export database

Here you can export your chat database to a zipped archive from your old device. You have to set your database passphrase manually to be able to export it.

#### To export your chat database:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Confirm it with a fingerprint or PIN, in case you have [SimpleX Lock](./privacy-security.md#simplex-lock) toggled on.
6. If you didn't set it before, [set a new database passphrase](./managing-data.md#database-passphrase).
7. Click or tap **Export database**.
8. Save the file on your device or share it via any available option.

### Import database

Here you can import your chat database into a new device from a zipped archive.

#### To import your chat database:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Click or tap **Import database**.
6. Select the .zip file containing your exported SimpleX Chat data.
7. Click or tap **Import**.
8. Toggle **Chat is running** on to start the chat or close and restart the app – you will be prompted to enter the passphrase for your chat database.

### Old database archive (Mobile only)

Here you can save or delete your database archive. 

#### To save your old database archive:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Click or tap **Old database archive**. 
6. Click or tap **Save archive**. 

#### To delete your old database archive:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Click or tap **Old database archive**.
6. Click or tap **Delete archive**.
7. Click or tap **Delete**.

### Delete database

Here you can delete your chat database. 

#### To delete your chat database:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Click or tap **Delete database**.
6. Click or tap **Delete**. 

**WARNING**: This action results in a loss of all your SimpleX Chat data! Unless you have just exported the chat database.

## Delete files & media

<img src="../../blog/images/20220928-files-media.png" width="330">

You can delete all sent and received files and media stored in the app via this feature, without deleting any messages.

#### To delete files and media:

1. [Open the app settings menu](./app-settings.md#opening-the-app-settings-menu).
2. Click or tap **Database passphrase & export**.
3. Toggle **Chat is running** off to stop SimpleX Chat from running.
4. Click or tap **Stop**.
5. Click or tap **Delete files for all chat profiles**.
6. Click or tap **Delete files & media**. 
