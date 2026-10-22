# Pass the chat controller to the network info command

## Problem

The network observers checked that a chat controller exists (`hasChatCtrl()`), then sent `/_network info` with a command that reads the controller again:

- Android (`NetworkObserver.kt`, online and offline reports): `sendCmd` throws "Controller is not initialized" when the controller became null in between (chat stopped and database deleted, `DatabaseView.kt` `setChatCtrl(null)`). Nothing catches it in `withBGApi`, so the app shows "View crashed". A controller replaced by another one was simply read again and used.
- iOS (`NetworkObserver.swift`): `getChatCtrl()` is a `fatalError` when the controller is not set, and `resetChatCtrl()` (database views, migration, passcode self-destruct) can run between the check and the send.

## Change

The controller is read once and passed to the command:

- Android: `currentCtrl()` returns the controller or null (same test as `hasChatCtrl()`); `apiSetNetworkInfo(info, ctrl)` passes it to `sendCommandOkResp`.
- iOS: `currentChatCtrl()` is read inside `monitorLock`; `apiSetNetworkInfo(info, ctrl:)` passes it to `chatSendCmdSync`.

On Android the command's error alert is kept (iOS only logs the error); #7651 sends the report without it, as it sends a failed report again on the next network event.

A report sent to a controller that has just been replaced goes to the old one. That is safe: controllers are never freed (`newStablePtr`), and the command only updates the agent's network info. The new controller does not get that report; it gets the network info only when the observer next reports it (#7651 also reports it when the chat is initialized).

## Alternatives

- Catch the exception at the call site (Android only): hides other errors and does not help iOS.
- A lock around the controller reset and the observer: larger, and touches every reset path.
- Make the command path never read the global controller (`ctrl ?? getChatCtrl()` in iOS `API.swift`): a design change for all commands.

## Testing

Android, with a test-only build that pauses 15 s before the report (not committed): stop chat, make the network go offline and online, delete the database during the pause. master: "View crashed", "Controller is not initialized" 15 s after the network came back online, when the report was due; this change: no crash.

Regression (Android and desktop, master and this change): network type and status in "Network & servers", `/_network info` in the chat console, offline/online status, no crash or alert on network changes after the database is deleted, messaging, stop/start/delete database on desktop.

## Open questions

The same check-then-read pattern, or a read of the global controller with no check, exists elsewhere; each is a possible separate bug (not reproduced). Found so far:

- iOS `API.swift` `chatCloseStore` (`hasChatCtrl()` then `chat_close_store(getChatCtrl())`), called on app suspension and termination and from `LocalAuthView`;
- iOS `UserPicker.swift` (`hasChatCtrl()` then `listUsersAsync()` in a `Task`);
- iOS `API.swift` `sendSimpleXCmdStr` (used by `sendSimpleXCmd`)/`recvSimpleXMsg`: every command sent without `ctrl` uses `ctrl ?? getChatCtrl()`;
- iOS `WebRTCClient.swift` calls `getChatCtrl()` when created;
- iOS `CryptoFile.swift` `writeCryptoFile`/`encryptCryptoFile` and `API.swift` `chatReopenStore` (from `apiActivateChat`) call `getChatCtrl()` with no check;
- Android/desktop `ChatListView.kt` (`hasChatCtrl()` then `getAgentSubsTotal`, polled every second while the chat list is visible);
- Android/desktop `UserPicker.kt` (`hasChatCtrl()` then `reloadRemoteHosts()` in a coroutine);
- Android/desktop `MigrateToDevice.kt` `importArchive` (`hasChatCtrl()` then `apiDeleteStorage()`; the exception is caught and shown as an error alert).
