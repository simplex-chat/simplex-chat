# Markdown file viewer: implementation plan

Markdown files from chats are viewed in the Android, desktop and iOS apps, formatted or as text, full screen.

## 1. Files that qualify

By name and size only:

```
name      file.fileName ends with .md or .markdown, case-insensitive
size      file.fileSize <= MAX_MARKDOWN_VIEW_SIZE
```

```
MAX_MARKDOWN_VIEW_SIZE    1_048_576
Kotlin                    views/helpers/Utils.kt, after MAX_VIDEO_SIZE_AUTO_RCV
iOS                       SimpleXChat/FileUtils.swift, after MAX_VIDEO_SIZE_AUTO_RCV
```

Function, in each viewer file:

```
Kotlin    fun isMarkdownFile(file: CIFile): Boolean
iOS       func isMarkdownFile(_ file: CIFile) -> Bool
```

Other files, and Markdown files over the limit: tap actions unchanged.

## 2. Tap and long press

Tap on a qualifying file, every platform:

```
file status                     action
received or sent, file loaded   viewer
desktop with a linked mobile    loadRemoteFile, then viewer
RcvInvitation, RcvAborted       reception started; viewer shown on completion, in the same chat
other statuses                  as now: waiting alerts, error alerts
```

Long press: the existing Save item of the item menu (`ChatItemView.kt`, `ChatView.swift`).

Places:

```
Kotlin    CIFileView.kt, onClick and fileAction
iOS       CIFileView.swift, fileAction
```

- Android: "Open with" of `SaveOrOpenFileMenu` is not shown for qualifying files; "Share" in the item menu stays.
- Present locally: `getLoadedFilePath(file)` (Kotlin), `getLoadedFileSource(file)` (iOS), as for other files.

## 3. Open on completion

Shared by Markdown files, images and voice messages (section 6).

State in the chat model:

```
fileToOpen    chat ID and file ID of the last tapped file whose reception was started
```

```
openWhenReceived(file)      fileToOpen = (current chat ID, file.fileId)
receivedFileToOpen(file)    true when fileToOpen has file.fileId, the file is loaded and the current chat is the same; fileToOpen cleared
```

- Kotlin: `KeyChangeEffect(file.fileStatus)` in the item view calls `receivedFileToOpen`; the effect runs on a status change only, so the item is opened when reception completes while the item is in the chat view.
- iOS: `.onChange(of: file.loaded)` in the item view, with the same rule.
- Chat list previews (small views) do not call `receivedFileToOpen`.

## 4. Reading in memory

The file is read and decoded off the main thread, before the viewer is shown.

```
Kotlin    withLongRunningApi
          encrypted: readCryptoFile(path, cryptoArgs) (CryptoFile.kt:36)
          plain: File(path).readBytes()
          text: String(bytes, Charsets.UTF_8)
iOS       Task
          getFileData(path, cryptoArgs) (SimpleXChat/ImageUtils.swift:41)
          text: String(decoding: data, as: UTF8.self)
          html: MarkdownParser().html(from: text)
```

- `readCryptoFile`: core `chatReadFile` (`Mobile/File.hs:87`); the file is decrypted by `CF.readFile` into memory.
- Bytes, text and HTML are held only in memory, in the viewer's state.
- Read error: alert with the error text; the viewer is not shown.
- File absent: "File not found" alert (Kotlin); no action (iOS), as for other files.

## 5. Viewer

```
presentation   full screen, like the image viewer
toolbar        back or close
               switch between the formatted and the text view
               save
title          file name
preference     view chosen last; default formatted
```

### 5.1. Kotlin

Renderer: `com.mikepenz:multiplatform-markdown-renderer` and `-m2`, 0.27.0 → 0.34.0 (`common/build.gradle.kts:80-81`).

```
0.34.0 dependencies   kotlin-stdlib 2.1.20, org.jetbrains:markdown 0.7.3, kotlinx-collections-immutable 0.3.8
compose               compileOnly; built with Compose 1.8.0, the app uses 1.8.2
app                   Kotlin 2.1.20, Compose Multiplatform 1.8.2
added                 table component, LazyMarkdownSuccess, success lambda of Markdown()
```

New file `views/chat/item/MarkdownFileView.kt`:

```
isMarkdownFile(file)
showMarkdownFile(file)     loadRemoteFile if needed, read, ModalManager.fullscreen.showModal
MarkdownFileView           formatted or text view by appPrefs.markdownFileTextView
```

Formatted view:

```
renderer      m2 Markdown(), GFM flavour
lazy          success lambda: LazyColumnWithScrollBar, title item, one item per top-level node (MarkdownElement)
links         LocalUriHandler provided by internalUriHandler(parent) (OperatorView.kt), then openExternalLink
code spans    annotator: source text between the backticks; inlineCode style, inlineCodeBackground
images        data:image URIs: decoded by an ImageTransformer (base64ToBitmap, 4320 px limit)
              other image links: the annotator appends the source text; nothing is fetched
tables        default table component
```

Text view: `LazyColumnWithScrollBar`, title item, one monospace `Text` per line, inside `SelectionContainer`.

Toolbar end buttons:

```
switch   ic_code in the formatted view, ic_article in the text view; sets appPrefs.markdownFileTextView
save     ic_download; rememberFileChooserLauncher; copyBytesToFile(ByteArrayInputStream(bytes)) (Files.kt:65)
```

`OperatorView.kt`:

```
markdownColor(linkText = primary)    markdownColor(); link colour moved to the link text style (linkText deprecated in 0.34.0)
private fun internalUriHandler       fun internalUriHandler
```

`SimpleXAPI.kt`: `markdownFileTextView = mkBoolPreference(SHARED_PREFS_MARKDOWN_FILE_TEXT_VIEW, false)`.

`moko-resources/base/strings.xml`:

```
markdown_formatted_view   Formatted view
markdown_text_view        Text view
```

### 5.2. iOS

New file `Shared/Views/Chat/ChatItem/MarkdownFileView.swift`, added to the SimpleX target in `project.pbxproj`.

```
isMarkdownFile(file)
showMarkdownFile(file)     read, parse, showAppSheet(fullScreen: true)
MarkdownFileView           NavigationView; toolbar: close, switch, save
```

```
formatted   ConditionsWebView(html:), Ink 0.6.0
text        ScrollView, LazyVStack, one monospace Text per line, text selection
switch      @AppStorage(DEFAULT_MARKDOWN_FILE_TEXT_VIEW)
save        saveCryptoFile(fileSource) (CIFileView.swift)
close       dismiss of the top view controller
```

`AppSheet.swift`: `showAppSheet(fullScreen: Bool = false, content:)`; full screen: `modalPresentationStyle = .fullScreen`.

`SettingsView.swift`: `DEFAULT_MARKDOWN_FILE_TEXT_VIEW = "markdownFileTextView"`, default false.

`ConditionsWebView.swift`, shared with the operator conditions:

```
configuration   allowsContentJavaScript = false
head            Content-Security-Policy: default-src 'none'; style-src 'unsafe-inline'; img-src data:
style           table, th, td borders; pre overflow-x auto; img max-width 100%
navigation      linkActivated: openExternalLink, as now
                other navigation: allowed for about: URLs only
```

- Ink 0.6.0 passes inline HTML to the output; scripts, frames and loads from the network are blocked by the policy and the JavaScript setting.
- Images: data URIs only.

## 6. Images and voice messages: open on completion

Current behaviour, both apps: a tap on an image or a voice message that is not received starts reception only; a second tap opens the image or plays the message.

Change, separate from the viewer:

```
image   tap starts reception, openWhenReceived; on completion in the same chat: full screen image view; Android: keyboard hidden first, as on tap
voice   tap starts reception, openWhenReceived; on completion in the same chat: playback
```

```
Kotlin   CIImageView.kt: receive branch, KeyChangeEffect, chat view only
         CIVoiceView.kt: VoiceMsgIndicator receive branches, KeyChangeEffect, chat view only
iOS      CIImageView.swift: receive branch, onChange, chat view only
         CIVoiceView.swift: downloadButton, onChange, chat view only
```

## 7. Size limit

1 MB.
4 MB: formatted views render lazily (Kotlin: one lazy item per top-level node; iOS: WKWebView); text views render one lazy item per line.
A single top-level block (one table, code block or paragraph) is still laid out at once.

## 8. Checks

1. Plain and encrypted files: formatted and text views on Android, desktop and iOS.
2. Desktop linked to a mobile: file loaded from the mobile, then shown.
3. File over 1 MB: tap action as before.
4. File not received: one tap; viewer shown on completion in the same chat; not shown after leaving the chat.
5. Same for images and voice messages.
6. View switch kept after closing the viewer and after a restart.
7. Save: the file at the chosen destination equals the original.
8. Links: confirmation alert; SimpleX links opened in the app.
9. Link inside backticks: shown as code.
10. Remote image: not loaded; data URI image: shown.
11. iOS inline HTML with `<script>`, `<img src=https:...>`, `<iframe>`, `<meta http-equiv="refresh">`: no effect.
12. Tables on both platforms.
13. Operator conditions: rendering unchanged, links in the primary colour.
