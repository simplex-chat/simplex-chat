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
RcvInvitation, RcvAborted       reception started; viewer shown on completion, in the same place
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
fileToOpen    file ID and place of the last tapped file whose reception was started
place         InChat(chat ID): tap in the chat view
              ChatList: tap in a chat list preview
```

```
openWhenReceived(file, inChatList)      fileToOpen = (file.fileId, place of the tap)
receivedFileToOpen(file, inChatList)    false unless fileToOpen has file.fileId and the place of the calling view, and the file is loaded
                                        then fileToOpen cleared
                                        InChat: true
                                        ChatList: true when the chat list is shown
```

Chat list shown:

```
Android    no chat open; no modal open
desktop    no start or full screen modal open
iOS        no chat open; no view controller presented
```

```
cancelFileToOpen(file, inChatList)      fileToOpen cleared when it has file.fileId and the same kind of place
```

Cleared:

```
later tap                               fileToOpen replaced
RcvError, RcvCancelled, RcvAborted      cancelFileToOpen
item view disposed (Kotlin)             cancelFileToOpen: item scrolled out, chat closed
item view disappears (iOS)              cancelFileToOpen: item scrolled out, chat closed, chat list left
chat list hidden (Kotlin, list record)  cancelFileToOpen
```

Item views: `OpenWhenReceivedEffect` (Kotlin, `CIFileView.kt`), `OpenWhenReceived` view modifier (iOS, `CIFileView.swift`), on `fileStatus` changes.

## 4. Reading in memory

The file is read and decoded off the main thread, before the viewer is shown.

```
Kotlin    withLongRunningApi
          encrypted: readCryptoFile(path, cryptoArgs) (CryptoFile.kt:36)
          plain: File(path).readBytes()
          text: UTF-8 CharsetDecoder, REPLACE with "?"
          parsed: parseMarkdownFlow (renderer), once, before the viewer is shown
iOS       Task
          getFileData(path, cryptoArgs) (SimpleXChat/ImageUtils.swift:41)
          text: Unicode.UTF8.ForwardParser, "?" for each invalid byte
          html: MarkdownParser with an .html modifier, raw HTML escaped
```

Text decoding, as the core safeDecodeUtf8 (simplexmq Util.hs:383-386):

```
invalid UTF-8    "?" for each invalid byte
C0 controls      U+0000-U+001F replaced with "?", except tab, line feed, carriage return
```

Limits, checked before rendering; over a limit: text view only, the switch hidden:

```
line length          15 000 characters
nesting              Kotlin: 32 levels of the parsed tree; iOS: 16 levels estimated from line prefixes
table rows           200
parse error          Kotlin: text view only
```

Text view: lines longer than 15 000 characters are split.

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
renderer      m2 Markdown(state), GFM flavour, the parsed state from showMarkdownFile
styles        documentMarkdownTypography(), documentMarkdownComponents, documentMarkdownPadding() (OperatorView.kt), shared with the operator conditions
lazy          success lambda: LazyColumnWithScrollBar, title item, one item per top-level node (MarkdownElement)
links         LocalUriHandler: markdownFileUriHandler
code spans    annotator: source text between the backticks; inlineCode style, inlineCodeBackground
images        data:image URIs: DataImageTransformer, decoded off the main thread by decodeBase64Image
              other image links: the annotator appends the source text; nothing is fetched
tables        default table component
```

Links, classified by the core markdown parser (`parseToMarkdown`), as message links:

```
SimplexLink              openVerifiedSimplexUri
Uri, http or https       openBrowserAlert
other                    showInvalidLinkAlert
```

Displayed link targets (`displayedUri`, Kotlin and iOS; also in message link alerts and the external-link alert):

```
format characters (Cf)        [U+XXXX]
non-ASCII in the authority    [U+XXXX]
HTML                          not parsed (parseHtml = false)
```

Images:

```
decode          decodeBase64Image(base64, window width): bounds first, 4320 px per side, power-of-two subsampling to the window width
budget          4320 × 4320 decoded pixels per document; images over the budget are not shown
cache           per document, in the transformer; the desktop base64BitmapCache is not used
```

Text view: `LazyColumnWithScrollBar`, title item, one monospace `Text` per line, inside `SelectionContainer`; format characters shown as `[U+XXXX]` markers.

Bidi controls (U+061C, U+200E, U+200F, U+202A-U+202E, U+2066-U+2069): warning under the title, both views.

Toolbar end buttons:

```
switch   ic_code in the formatted view, ic_article in the text view; sets appPrefs.markdownFileTextView; hidden over the limits
save     ic_download; rememberFileChooserLauncher; copyBytesToFile(ByteArrayInputStream(bytes)) (Files.kt:65)
```

`OperatorView.kt`:

```
markdownColor(linkText = primary)   markdownColor(); link colour moved to the link text style (linkText deprecated in 0.34.0)
ConditionsMarkdown styles           extracted, unchanged: documentMarkdownTypography(), documentMarkdownComponents, documentMarkdownPadding()
```

`SimpleXAPI.kt`: `markdownFileTextView = mkBoolPreference(SHARED_PREFS_MARKDOWN_FILE_TEXT_VIEW, false)`.

`moko-resources/base/strings.xml`:

```
markdown_formatted_view      Formatted view
markdown_text_view           Text view
markdown_file_bidi_warning   This file has text direction characters. The text can be shown in a different order than it is written.
```

### 5.2. iOS

New file `Shared/Views/Chat/ChatItem/MarkdownFileView.swift`, added to the SimpleX target in `project.pbxproj`.

```
isMarkdownFile(file)
showMarkdownFile(file)     read, parse, showAppSheet(fullScreen: true)
MarkdownFileView           NavigationView; toolbar: close, switch, save
```

```
formatted   ConditionsWebView(html:openLink:), Ink 0.6.0, raw HTML escaped by an .html modifier
text        ScrollView, LazyVStack, one monospace Text per line, text selection, format characters as [U+XXXX]
switch      @AppStorage(DEFAULT_MARKDOWN_FILE_TEXT_VIEW); hidden over the limits
save        saveCryptoFile(fileSource) (CIFileView.swift)
close       dismiss of the top view controller
links       openMarkdownFileLink: parseSimpleXMarkdown; simplexLink: appOpenUrl; uri, http or https: openBrowserAlert; other: showInvalidLinkAlert
```

`AppSheet.swift`: `showAppSheet(fullScreen: Bool = false, content:)`; full screen: `modalPresentationStyle = .fullScreen`.

`SettingsView.swift`: `DEFAULT_MARKDOWN_FILE_TEXT_VIEW = "markdownFileTextView"`, default false.

`ConditionsWebView.swift`, shared with the operator conditions:

```
configuration   allowsContentJavaScript = false; websiteDataStore = .nonPersistent()
head            Content-Security-Policy: default-src 'none'; style-src 'nonce-<UUID per load>'; img-src data:; base-uri 'none'; form-action 'none'
style           the app's style block with the nonce; table, th, td borders; pre overflow-x auto; img max-width 100%
navigation      linkActivated: openLink (openExternalLink by default)
                other navigation: allowed for about: URLs only
```

- Ink 0.6.0 applies `.html` modifiers to block and inline HTML fragments (`HTMLConvertible.swift`); the viewer's modifier escapes `&`, `<`, `>`; code spans and blocks are escaped by Ink itself.
- Images: data URIs only.

## 6. Images and voice messages: open on completion

Current behaviour, both apps: a tap on an image or a voice message that is not received starts reception only; a second tap opens the image or plays the message.

Change, separate from the viewer:

```
image   tap starts reception, openWhenReceived; on completion in the same place: full screen image view; Android: keyboard hidden first, as on tap
voice   tap starts reception, openWhenReceived; on completion in the same place: playback
```

```
Kotlin   CIImageView.kt: receive branch, OpenWhenReceivedEffect
         CIVoiceView.kt: receiveFile passed to VoiceLayout, OpenWhenReceivedEffect
iOS      CIImageView.swift: receive branch, OpenWhenReceived
         CIVoiceView.swift: downloadButton, OpenWhenReceived
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
6. Tap in a chat list preview: opened on completion while the chat list is shown; not opened after a chat or a modal is opened.
7. Not opened on completion after a reception error, cancel or abort, or after the item scrolled out of view.
8. Invalid UTF-8 and control characters: shown as "?".
9. Lines over 15 000 characters, 33 tree levels (Kotlin), 17 nesting levels (iOS), 201 table rows: text view only, long lines split.
10. Bidi controls: warning; format characters as [U+XXXX] in the text view.
11. Data images: decoded to the window width; images beyond 4320 × 4320 pixels per document not shown.
12. Links: SimpleX links to the connect flow; http(s) links to the "Open web link?" alert; other schemes and relative links: "Invalid link"; targets with invisible or non-ASCII host characters shown with [U+XXXX].
13. View switch kept after closing the viewer and after a restart.
14. Save: the file at the chosen destination equals the original.
15. Link inside backticks: shown as code.
16. Remote image: not loaded; data URI image: shown.
17. iOS inline HTML with `<script>`, `<img src=https:...>`, `<iframe>`, `<style>`, `<meta http-equiv="refresh">`: shown as text.
18. Tables on both platforms.
19. Operator conditions: rendering unchanged, links in the primary colour.
