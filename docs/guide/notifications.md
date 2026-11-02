---
title: Notifications
---
# Notifications

The app shows notifications for new messages, contact requests, new contacts and
calls. How it receives messages while you are not using it depends on the
platform: the iOS app can use a push server, the Android app runs a notification
service on your device, and the desktop app receives messages while it runs.

When you create your first profile on iOS or Android, **Setup notifications** on
the **Your network** screen lets you choose how notifications work, see [Your
network](./README.md#your-network). **Instant** is chosen by default. You can
change it later in the settings.

## Notifications on iOS

<img src="./images/notifications-ios-modes.png" alt="iOS Push notifications screen with the modes Instant, Periodic and No push server" width="300">

The app describes the three modes this way:

- **Instant**: the device token and end-to-end encrypted notifications are sent
  to the push server; the push server does not see the message content, size or
  who it is from.
- **Periodic**: the app checks new messages every 20 minutes; only the device
  token is shared with the push server, and it does not see how many contacts
  you have or any message metadata.
- **No push server**: the push server is not used; the app checks messages in
  the background when iOS allows it, depending on how often you use the app.

The app marks **Instant** as recommended, **Periodic** as more private and **No
push server** as most private. At the first start, **How it affects privacy** on
the **Push notifications** screen shows these descriptions.

To change the mode:

1. Open **Settings** (see [Opening the app
   settings](./app-settings.md#opening-the-app-settings)) and tap
   **Notifications**.
2. Tap **Send notifications** and choose a mode.
3. Tap **Enable**, or **Turn off** for **No push server**.

When the app uses a push server, the **Notifications** screen shows it in **Push
server**. **Test notifications** checks the connection to the push server and
the state of the notification token of your device: the app then shows
**Notifications status**, or **Notifications error** with **Register** to
register the token again.

If notifications for SimpleX Chat are turned off in the iOS settings, the app
shows **Notifications are disabled!** with **Open Settings**.

## Notifications on Android

<img src="./images/notifications-android-modes.png" alt="Android Private notifications screen with the modes Instant, Periodic and When app is running" width="300">

The Android app receives messages itself, with a background service or with
periodic checks, depending on the mode; the app notes that it does this instead
of using push notifications. The modes have different names at the first start
and in the settings:

| At the first start | In **Notification service** | As the app describes it |
|---|---|---|
| **Instant** (default) | **Always on** | "Background service is always running – notifications will be shown as soon as the messages are available." The app notes that this mode uses more battery. |
| **Periodic** | **Starts periodically** | "Checks new messages every 10 minutes for up to 1 minute". The app notes that this mode is good for battery, and that you may miss calls or urgent messages. |
| **When app is running** | **Runs when app is open** | "App can receive notifications only when it's running, no background service will be started". The app notes that this mode is best for battery. |

To change the mode, open **Settings**, tap **Notifications**, then
**Notification service**, and choose a mode.

While the service runs, Android shows the notification **SimpleX Chat service**
with **Receiving messages…**. Its **Hide** button opens the system settings of
this notification.

**Starts periodically**, and **Always on** on Android 12 and later, need the app
to be allowed to run without battery optimization. When it is not allowed, the
app shows an alert: **Allow** opens the system dialog that allows it, and
**Disable notifications** switches to **Runs when app is open**. When Android
restricts the app in the background, the alert says "SimpleX can't run in
background. You will receive the notifications only when the app is running."
and asks you to choose **App battery usage** / **Unrestricted** in the app
settings: **Open app settings** opens them.

On Xiaomi, Redmi and POCO devices, the app asks you to enable **Autostart** in
the system settings for notifications to work.

On Android 13 and later, the app asks for the permission to show notifications
when the chat list opens. The notification service starts only when
notifications are allowed.

## Notifications on desktop

The desktop app shows system notifications while it runs. The **Notifications**
row of the settings is shown on iOS and Android only. When the app window is
focused, message notifications are shown only for chats other than the open one,
and incoming calls are shown in the app window.

To keep receiving messages after you close the window, turn on **Close to tray**
in **Appearance** (see [Appearance](./app-settings.md#appearance)): the app
keeps running in the system tray. The first time you close the window, the app
asks **Minimize to tray?**, with **Close the app** and **Minimize to tray**, and
explains: "If you choose Close, messages won't be received." The tray icon shows
a dot when there are unread messages, its tooltip shows their number, and its
menu has **Show SimpleX** and **Quit SimpleX**. **Close to tray** is offered
only when the system tray is available.

## Notifications and the database passphrase

If you remove the database passphrase from the device, the iOS app warns
"Instant push notifications will be hidden!", and the Android app warns
"Notifications will be delivered only until the app stops!". See [Database
passphrase](./managing-data.md#database-passphrase).

## Notification previews

By default, notifications show the name of the contact or group and the message
text. To change this:

1. Open **Settings** and tap **Your privacy**, then **More privacy**.
2. In the **Notifications** section, tap **Show preview**.
3. Choose an option:
   - **Message text**: the name and the message. Text formatted as secret is
     replaced with "...".
   - **Contact name**: the name and "new message".
   - **Hidden**: "Contact hidden:" and "new message"; on iOS, group messages
     show "Group message:". The pictures of your contacts are not shown.

The option also applies to notifications of calls and contact requests. On iOS,
the screen notes "You can set lock screen notification preview via settings."
and has **Open Settings**.

## Mute chats and profiles

Muting a chat turns off its notifications; in groups and channels, you can keep
notifications for mentions. The chat list shows a muted icon next to a muted
chat.

In a direct chat, **Mute** turns notifications off and **Unmute** turns them on.
In groups and channels, the item changes in turn:

- **Mute**: notifications only for messages that mention you or reply to your
  messages.
- **Mute all**: no notifications.
- **Unmute**: notifications for all messages.

| | iOS | Android | Desktop |
|---|---|---|---|
| Chat list | Swipe the chat right | Long press the chat | Right-click the chat |
| Open chat | The "…" menu in the chat toolbar | The "⋮" menu in the chat toolbar | The "⋮" menu in the chat toolbar |
| Chat info | The mute button under the name | The mute button under the name | The mute button under the name |

A muted chat profile shows no notifications while it is not your active profile.
The app explains: "You will still receive calls and notifications from muted
profiles when they are active." Hidden profiles are always muted. To mute or
unmute a profile, see [Hiding and muting chat
profiles](./chat-profiles.md#hiding-and-muting-chat-profiles).

The design of notifications on iOS and Android is explained in [this
post](../../blog/20220404-simplex-chat-instant-notifications.md).
