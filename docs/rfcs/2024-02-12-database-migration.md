# Database migration and other operations

## Problem

Migrating database to another device is very complex for most people - it is multi-step and error-prone.

In addition to that, any database operation is confusing as it requires stopping chat.

## Solution

Let users migrate database to another device by scanning QR code.

Simplify other database operations by removing the need to compose multiple actions, stop chat, etc.

To support it, we already added the way to represent the file as link/QR code (by uploading file description to XFTP, and supporting "recursive" descriptions).

There will be these actions in the Database settings (no stop/start chat toggle):

- Export database.
- Import database.
- Migrate from another device.
- Set passphrase (or Change passphrase if it was set).
- Remove passphrase from device / Store passphrase on the device.

Stop chat toggle will be moved to dev tools.

Migrate to another device will be available in the top part of the settings, 


### Database export

Currently, it requires these steps:

1. Open Database settings.
2. Stop chat (many users don't understand it).
3. Tap "Export database" in settings.
4. Look at the alert that says "set passphrase".
5. Tap Ok.
6. Tap Set passphrase.
7. Enter passphrase and confirm.
8. Exit back to Database settings.
9. Tap "Export database" again.
10. Choose file location and save.
11. Tap "New archive".
12. Remove exported archive.

These steps are all very confusing, and if they were to stay as composable steps, they belong to dev tools.

Instead we can offer these simple steps:

1. Open Database settings.
2. Tap "Export database".
3. Alert will appear saying: "The chat will stop, and you will need to set (or verify) database passphrase. Continue?".
4. Tap "Ok".
5. Enter passphrase and confirm in the window that appears (or verify if it was already set, possibly allowing to skip this step).
7. Choose whether to save file or upload to XFTP and generate link.
8. File: choose file location and save.
   Link: show upload progress and then show link to copy.
9. Alert will appear saying: "Database exported!", exported archive will be automatically removed.

So instead of asking users to understand the required sequence of steps, we will guide them through the required process.

### Database import

1. Open Database settings.
2. Tap "Import database".
3. Alert will appear saying: "The chat will stop, you will import?".
4. File: choose file location and tap "Import".
   Link: paste link (or scan QR code) and tap "Import".
5. Confirm to replace database.
6. Start chat automatically once imported.

### Set or change passphrase

1. Open Database settings.
2. Tap "Set passphrase" or "Change passphrase" (if it was set).
3. Choose - store passphrase on the device or enter it every time the app starts.

### Remove / store passphrase from the device

To remove:

1. Open Database settings.
2. Tap "Remove passphrase".
3. Confirm to remove passphrase in alert.
4. Button is replaced with Store.

To store:

1. Open Database settings.
2. Tap "Store passphrase".
3. Enter current passphrase - it is verified.
4. Button is replaced with Remove.

### Migrate database to / from another device

#### User experience

This function is the most important, and it should be available from the main section in settings, under "Use from desktop" (or under "Link from mobile" on desktop).

On the receiving device it will be available via Database settings and also on the Onboarding screen, so users don't need to create a profile.

The steps are:

On the source device:
1. Tap "Migrate to another device".
2. The chat will stop showing "Stopping chat" to the user.
3. If passphrase was:
   - not set: make user set it in a separate screen.
   - set: make user verify it.
5. Show the screen to confirm the upload.
6. Upload progress (full screen circular progress showing the share, with the %s and total/uploaded size) will be shown.
7. Once upload is completed, show QR code (with option to copy link), instruct to tap "Migrate from another device" on the receiving device.

On the receiving device:
2. Tap "Migrate from another device".
2. The chat will stop (if not from Onboarding) showing "Stopping chat" to the user.
4. Scan QR code (with option to paste link on desktop only).
5. Show similar download progress, but probably in reversed direction - design TBC.
6. Once download is completed, show "Replace the current database" (if not from Onboarding).
7. Once imported, start chat automatically, and once chat started show "Tap remove database on source device".

On the source device:
1. Tap "Remove database" on the showing screen (this should also remove uploaded file).

#### Implementation considerations

The latest updates allow uploading and downloading XFTP files without messages.

So to perform the above, the second instance of the chat controller will be required, that probably requires supporting additional/optional chat controller parameter in the APIs that are required for that process.
