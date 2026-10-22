# A received file opens over a page opened after the tap

Follow-up to #7725 (`OpenWhenReceivedEffect` in its plan), stacked on its branch.

## Problem

A file tapped before it was received is opened when its download finishes (`ChatModel.fileToOpen`, `OpenWhenReceivedEffect`): an image or video viewer, inline video or voice playback, the `.md` viewer, the desktop Save dialog, or on Android the Open/Save menu. Apart from a chat opened over the Android chat list, nothing checked what was on screen by then, and the chat list and the main chat stay composed under every page, so:

- an image or video viewer opened over another viewer the user opened meanwhile (Back showed the other viewer, rebuilt);
- on desktop, a chat-list video viewer opened over a chat video viewer showed the old video (the same composition slot), and needed two Escapes;
- a viewer, menu or playback started over or behind a page opened meanwhile (on Android any page, such as chat info or Settings; on desktop a `.md` viewer over the chat, or a Settings page over the chat list).

Separately, the chat-list video preview kept `autoPlay` set after opening its viewer, so the viewer opened again by itself when the preview was composed again with a new file URI (on desktop linked to a mobile, after switching chats).

## Fix

- `FileToOpen` records, at the tap, how many pages are open (`openModalCount()`, without pages sliding out) in the panes that cover the tapped item: the fullscreen pane, and the left pane for a chat-list item or the center pane for a chat item. The file is opened only if there are no more open pages there when it is received. A page opened after the tap is above the pages counted at the tap: on desktop the counted panes are empty then for an item in the chat list or the main chat, and on Android the stack holds the pages the item's chat was opened over.
- `SmallVideoView` clears `autoPlay` when it opens the viewer, as `VideoView` does after playing.

## Not fixed

- The desktop Save dialog for one file can be taken over by another file's finished download (not a page; separate PR if confirmed).
- A page opened between the download and the open (decrypting a video, loading from a linked mobile, reading a `.md` file) still gets it.
- Only `ModalManager` pages count: alerts, the user picker, the passcode screen and the desktop floating terminal don't stop it.
- Desktop: an item in a support or reports chat (right pane) is checked like a main chat item, so a page opened in the center pane beside it (a `.md` viewer, the profile form) while it downloads stops it. A page opened over that chat removes the chat from composition, so its file is not opened then, as on master.

## Effect

- Both platforms: a file whose download finishes while a page opened after the tap covers it is not opened, played or offered; it stays in the chat to be tapped. Pages opened and closed again, or closed, before the download finishes don't stop it.
- Desktop: pages beside an item in the chat list or the main chat (chat info on the right, Settings on the left of a chat) don't stop it.
- Android: the panes are one stack, so any page opened since the tap and still open stops it.
- Chat-list video previews open their viewer once per tap or download, not again when composed again with a new file URI.
