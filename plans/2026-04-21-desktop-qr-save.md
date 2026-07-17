# Fix: Desktop QR code save — broken flow and non-native picker

## Context

When clicking a QR code on desktop to save it as an image file:
- **Mac**: Native-ish save dialog appears, user picks name and location, dialog closes — but no file is saved.
- **Linux**: A Java Swing `JFileChooser` directory picker appears (not native), followed by a *second* save dialog.

## Root Cause

The desktop `saveTempImageUncompressed()` is fundamentally wrong. On Android it saves to a temp directory (no UI), then `shareFile()` opens the system share sheet. On desktop, someone made `saveTempImageUncompressed()` show a save dialog instead — creating two bugs:

**Bug 1 — Mac: file never written.** `saveTempImageUncompressed` calls `saveDialog.awaitResult()` with no filename. Mac's native `FileDialog(SAVE)` returns the full file path the user entered (e.g., `/Users/name/Desktop/qr_code`). But the code treats it as a *directory* and constructs `File(filePath + "/" + generatedName)` — trying to create `/Users/name/Desktop/qr_code/IMG_xxx.png`. This fails because the parent path is a file, not a directory. Exception is caught silently, returns null. `shareFile` is never called.

**Bug 2 — Linux: two dialogs.** `saveTempImageUncompressed` calls `saveDialog.awaitResult()` with no filename. On Linux, since `filename == null`, JFileChooser switches to `DIRECTORIES_ONLY` mode — showing a directory picker. User picks a directory, file is written there. Then `shareFile()` opens a *second* save dialog asking where to save the same file. User sees two dialogs for one save.

**Why JFileChooser on Linux.** Comment in `DefaultDialog.desktop.kt:123-124`: "Has graphic glitches on many Linux distributions" — referring to `java.awt.FileDialog`. So `JFileChooser` (Swing) is used as a workaround on Linux/Windows, while Mac uses native `FileDialog`. With modern JDKs (17+), the glitches may be resolved, but this is a separate concern.

## Fix

**Change `saveTempImageUncompressed` on desktop to save to temp directory (like Android)**, not show a dialog. The dialog is then handled by `shareFile` which already works correctly — it shows a save dialog with the filename pre-filled and copies the file to the chosen location.

### File: `common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/Utils.desktop.kt`

Replace lines 191-204:
```kotlin
// CURRENT (broken):
actual suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File? {
  val file = simplexWindowState.saveDialog.awaitResult()
  return if (file != null) {
    try {
      val ext = if (asPng) "png" else "jpg"
      val newFile = File(file.absolutePath + File.separator + generateNewFileName("IMG", ext, File(file.absolutePath)))
      ImageIO.write(image.toAwtImage(), ext, newFile.outputStream())
      newFile
    } catch (e: Exception) {
      Log.e(TAG, "Util.kt saveTempImageUncompressed error: ${e.message}")
      null
    }
  } else null
}

// FIX — save to temp dir, no dialog:
actual suspend fun saveTempImageUncompressed(image: ImageBitmap, asPng: Boolean): File? {
  return try {
    val ext = if (asPng) "png" else "jpg"
    tmpDir.mkdir()
    val newFile = File(tmpDir, generateNewFileName("IMG", ext, tmpDir))
    ImageIO.write(image.toAwtImage(), ext, newFile.outputStream())
    newFile.deleteOnExit()
    newFile
  } catch (e: Exception) {
    Log.e(TAG, "Util.kt saveTempImageUncompressed error: ${e.message}")
    null
  }
}
```

This mirrors the Android implementation (`Utils.android.kt:322-338`) which saves to `tmpDir` with `deleteOnExit()`.

### No other files need changes

The caller in `QRCode.kt:90-100` already does:
```kotlin
val file = saveTempImageUncompressed(image, true)
if (file != null) {
  shareFile("", CryptoFile.plain(file.absolutePath))
}
```

`shareFile` on desktop (`Share.desktop.kt:25-44`) uses `FileChooserLauncher(false)` which:
1. Shows save dialog with filename pre-filled from `launch(fileSource.filePath)`
2. Copies the temp file to the user's chosen location

This already works correctly — it's used for saving chat attachments via `SaveContentItemAction`.

### Result after fix

1. User clicks QR code
2. Image saved to temp dir (no UI)
3. One save dialog appears with filename pre-filled (e.g., "IMG_2024-04-21_12.00.00.png")
4. User picks location, file is copied there
5. Works on both Mac and Linux

### On the non-native picker (Linux)

The JFileChooser on Linux is a known trade-off — `java.awt.FileDialog` had graphic glitches on Linux (comment in code). With the fix above, the Swing picker at least shows as a proper save dialog with pre-filled filename (not a directory-only picker). Changing to native `FileDialog` on Linux is a separate investigation — it may work fine with modern JDKs but risks regressions on some distros.

## Verification

1. Build desktop: `./gradlew :desktop:run` (or equivalent)
2. Create a new chat link (New Chat → Create link)
3. Click the QR code image
4. Verify: ONE save dialog appears with a filename pre-filled
5. Pick a location, click Save
6. Verify: image file exists at the chosen location and is a valid PNG
7. Test on both Mac and Linux if possible
