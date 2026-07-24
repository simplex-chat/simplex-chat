# Desktop: Copy copies the image, not its caption

Reported on Windows: right-click → **Copy** on a message that is an image with a caption, then paste
into the message box — the image is attached instead of the caption text being inserted.

## Cause

`copyItemToClipboard` on desktop checks the attached file *before* the item text, so an item that has
both a file and text can never have its text copied.

`common/src/desktopMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.desktop.kt:54`

```kotlin
if (fileSource != null) {
  val filePath = filePathForShare(fileSource) ?: return@withLongRunningApi
  when {
    desktopPlatform.isWindows() -> clipboard.setText(AnnotatedString("\"${File(filePath).absolutePath}\""))
    else -> clipboard.setText(AnnotatedString(filePath))
  }
} else {
  clipboard.setText(AnnotatedString(cItem.content.text))   // only reached when there is no file
}
```

The clipboard never holds image bytes — no desktop code sets `DataFlavor.imageFlavor`. The image
reappears because the path round-trips: on `Ctrl+V` the compose field runs `parseToFiles`
(`common/src/desktopMain/kotlin/chat/simplex/common/platform/PlatformTextField.desktop.kt:222`),
which strips exactly the quotes the copy added, resolves the path, and calls `onFilesPasted` —
discarding the pasted text.

The Copy menu item itself (`common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt:443`)
is shared by all platforms and makes no distinction between captioned and uncaptioned items, so the
whole difference lives in the desktop `actual`.

## Why desktop diverges from the other clients

| Client | Image **with** caption | Image **without** caption |
| --- | --- | --- |
| iOS `apps/ios/Shared/Views/Chat/ChatView.swift:2471` | caption text | image bytes |
| Android `common/src/androidMain/.../ChatItemView.android.kt:47` | caption text | `content.text` (empty) |
| Desktop (before) | **file path** | file path |

iOS has guarded on `text == ""` since `48d24d358` (2022-12-03) and has never had this bug. Desktop is
the only client missing the guard — path-copying arrived later, in `cc95fa6b3` (2023-10-03), without
one, so this is a long-standing gap rather than a regression.

Although the report is from Windows, there is no Windows-only branch here: only the *quoting* on the
`desktopPlatform.isWindows()` arm is Windows-specific. The same ordering applies on macOS and Linux,
where the unquoted path also parses in `parseToFiles`. The fix is therefore not conditioned on the
platform.

## Fix

Copy the item text when it has any, and fall through to the existing file-path behaviour only when
the text is empty — the rule iOS already applies. Android reaches the same outcome by always copying
the text, since it never puts a path on the clipboard.

```kotlin
val fileSource = if (cItem.content.text.isNotEmpty()) null else {
  var fs = getLoadedFileSource(cItem.file)
  if (chatModel.connectedToRemote() && fs == null) {
    cItem.file?.loadRemoteFile(true)
    fs = getLoadedFileSource(cItem.file)
  }
  fs
}
```

Expressing it as "no file source when there is text" rather than as an early return keeps the
existing `if (fileSource != null) … else …` meaningful: the `else` is still the branch that copies the
item text, and the success toast is still issued once at the end of the function.

The file branch is untouched, so uncaptioned images and files still copy their path, keeping the
Windows quoting added in `cc95fa6b3` ("desktop: paste files/images to attach to message", #3165) for
`parseToFiles` to strip. Copying a path for an uncaptioned file is deliberate — that commit added it
so a file can be copied from one chat and pasted into another to re-attach it. Placing the check before
`getLoadedFileSource` also means copying a caption no longer downloads a remote file first when the
desktop is connected to a mobile device.

### What a captioned item loses

Copying a captioned file no longer puts its path on the clipboard, so it can no longer be pasted into
another chat to re-attach it. That workflow is what `cc95fa6b3` added the path-copying for, and it
stays available for captioned items through **Forward**
(`common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt:468`), which is
offered by the same menu whenever `forwardingAllowed()` holds. Forward keeps the caption:
`forwardMsgContent` (`src/Simplex/Chat/Library/Commands.hs:4456`) returns the whole `MsgContent`, and
`prepareMsgReq` only rewrites `MCChat`'s owner signature.
**Share** and **Save** also still reach the file. Uncaptioned items keep path-copying unchanged.

## Testing

Desktop, an image message with a caption:

- Copy → paste into the message box inserts the caption text, no attachment.
- Copy → paste into any external editor inserts the caption text.

Same for a video or file message with a caption. Voice messages are not covered: the compose field
refuses text while a voice preview is active
(`common/src/desktopMain/kotlin/chat/simplex/common/platform/PlatformTextField.desktop.kt:98`), so a
voice message sent from this app has no caption and keeps copying its path.

Desktop, an image or file message without a caption — unchanged:

- Copy → paste into the message box attaches the file.
- For an encrypted file, the decrypted copy in `tmpDir` is still produced by `filePathForShare`.

Desktop connected to a mobile device (remote):

- Copy on a captioned image returns the caption immediately and no longer downloads the file first.
- Copy on an uncaptioned image still downloads it via `loadRemoteFile` and copies the path.

## Not addressed here

Uncaptioned images still copy a path rather than image bytes, so pasting one into another application
gives a path, not a picture. iOS sets `UIPasteboard.general.image` for that case. Matching it on
desktop means writing `DataFlavor.imageFlavor` to the AWT clipboard, which is a separate change with
its own platform behaviour to verify, and is deliberately left out of this fix.
