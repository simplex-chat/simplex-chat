# Problem

iOS notifications are not stable, and crash in the background for many users. The reason for that is concurrent database access from the main app and from NSE processes. While there are measures preventing NSE execution while the main app is running, there is nothing preventing continued NSE execution after the app returns to the foreground.

While iOS creates a new instance of the notification service class for each incoming mutable notification, it re-uses the same process.

In addition to that, while NSE avoids subscribing to the existing connections to avoid disrupting the subscriptions from the main app, it can create new connections, specifically for legacy file transfers (not important) and for member connections (very important), so the NSE may continue receiving the messages from these new connections, creating database contention, and at the same time the main app won't subscribe to these connections until it is restarted.

# Possible solutions

We could consider using WAL mode to increase SQLite concurrency, but it means that WAL files have to be included in the database archive, which is currently not done. It is unclear if it can only be one or multiple files.

We should also consider some mechanism of suspending NSE when the app returns to the foreground, e.g. via:
- checking the shared preference in message reception loop, although it may be too late to prevent the database contention.
- checking the shared preference frequently and suspending the core, but it can be expensive.
- using Mach messages to communicate to NSE that app is resumed so it can suspend itself.

The problem with connection subscriptions can be addressed in this way:

1. Any connections created in NSE will use additional parameter in all APIs to avoid creating subscriptions.
2. The information about these connections will be stored in the database as "requiring subscription".
3. The main app will subscribe to them when it is resumed.

This is similar to how XFTP files are marked as "to be received" when the message is received by NSE.

If this is agreed, then this require changing the whole stack, including SMP protocol, as currently the subscription is done automatically when the connection is created.
