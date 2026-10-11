# Markdown file viewer: implementation plan

Markdown files from chats are viewed in the Android, desktop and iOS apps, formatted or as text.

## 1. Files that qualify

```
name      file.fileName ends with .md or .markdown, case-insensitive
size      file.fileSize <= MAX_MARKDOWN_VIEW_SIZE
Kotlin    file.forwardingAllowed() (ChatModel.kt:4610)
iOS       getLoadedFileSource(file) != nil (SimpleXChat/ImageUtils.swift:15)
```

`forwardingAllowed()` is true for a local file present with full size (`getLoadedFilePath`, Files.kt:107), and for a loaded file of a linked mobile on desktop.

Constant, the value of `MAX_VIDEO_SIZE_AUTO_RCV`:

```
MAX_MARKDOWN_VIEW_SIZE    1_047_552
Kotlin                    views/helpers/Utils.kt, after line 121
iOS                       SimpleXChat/FileUtils.swift, after line 27
```

Other files, and Markdown files over the limit: tap actions unchanged.

Function, in each viewer file:

```
Kotlin    fun isMarkdownFile(file: CIFile?): Boolean
iOS       func isMarkdownFile(_ file: CIFile?) -> Bool
```

## 2. Opening

| Platform | Place | Change |
| --- | --- | --- |
| Android | `CIFileView.kt:194-200`, `onClick` | qualifying file: `showMarkdownFile(file)`; other files: menu or `fileAction()`, as now |
| desktop | `CIFileView.kt:194-200`, `onClick` | qualifying file: `showMarkdownFile(file)`; other files: `fileAction()`, as now |
| iOS | `CIFileView.swift:117-137`, `fileAction` cases `.rcvComplete`, `.sndStored` with `.local`, `.sndComplete` | qualifying file: `showMarkdownFile(file)`; other files: `saveCryptoFile`, as now |

- The same views are used for chat list previews (`ChatPreviewView.kt:350`, `ChatPreviewView.swift:441`); the viewer is opened from there too.
- Android: "Open with" of `SaveOrOpenFileMenu` is not shown for qualifying files; "Share" in the item menu (`ChatItemView.kt:426`) stays.
- Desktop with a linked mobile: `file.loadRemoteFile(true)` before reading, as in `fileAction` (`CIFileView.kt:109-114`).

## 3. Reading in memory

The file is read and decoded off the main thread, before the viewer is shown:

```
Kotlin    withLongRunningApi (Dispatchers.Default, Utils.kt:46)
iOS       Task, the view shown on the main actor
```

Kotlin:

```
path        getLoadedFilePath(file) (Files.kt:107)
encrypted   readCryptoFile(path, cryptoArgs) (CryptoFile.kt:36)
plain       File(path).readBytes()
text        String(bytes, Charsets.UTF_8)
```

iOS:

```
path        getAppFilePath(fileSource.filePath)
read        getFileData(path, fileSource.cryptoArgs) (SimpleXChat/ImageUtils.swift:41)
text        String(decoding: data, as: UTF8.self)
```

- `readCryptoFile` (Kotlin and iOS): core `chatReadFile` (`Mobile/File.hs:87`); the file is decrypted by `CF.readFile` into memory and returned as bytes.
- Bytes and text are held only in memory, in the viewer's state.
- Invalid UTF-8 sequences are replaced by U+FFFD in both decoders.
- Read error: alert with the error text; the viewer is not opened.

```
Kotlin    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.message)
iOS       showAlert(NSLocalizedString("Error reading file", comment: "alert title"), message: error.localizedDescription)
```

## 4. Viewer

```
title       file name
views       formatted
            text
switch      toolbar button; its state is the preference
save        toolbar button
```

Preference, default formatted:

```
Kotlin    appPrefs.markdownFileTextView
iOS       DEFAULT_MARKDOWN_FILE_TEXT_VIEW
```

### 4.1. Kotlin

New file `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/MarkdownFileView.kt`:

```
fun isMarkdownFile(file: CIFile?): Boolean
fun showMarkdownFile(file: CIFile)
@Composable fun MarkdownFileView(file: CIFile, text: String)
```

`showMarkdownFile`:

```
withLongRunningApi:
  load remote file if connected to a mobile and the path is missing
  read bytes, decode text (section 3)
  error: alert, return
  ModalManager.center.showModal(endButtons = { switch; save(bytes) }) { MarkdownFileView(file, text) }
```

- Modal with end buttons: as in `CIInvalidJSONView.kt:26`.
- Switch: `IconButton`; icon `ic_code` in the formatted view, `ic_article` in the text view; sets `appPrefs.markdownFileTextView`.
- Save: `IconButton` with `ic_download`; `rememberFileChooserLauncher(false, file) { to -> copyBytesToFile(ByteArrayInputStream(bytes), to) {} }` (Files.kt:65); `launch(file.fileName)` in `withLongRunningApi`.
- On save, the decrypted bytes are written only to the chosen destination, without the temporary file of `rememberSaveFileLauncher` (`CIFileView.kt:310`).

`MarkdownFileView`:

```
title       AppBarTitle(file.fileName)
formatted   DocumentMarkdown(text, GFMFlavourDescriptor(), image = sourceImage, custom = sourceTable)
            inside ColumnWithScrollBar
            LocalUriHandler provided by internalUriHandler(parent), as in OperatorView.kt:753-755
text        SelectionContainer { Text(text, fontFamily = FontFamily.Monospace) }
            inside ColumnWithScrollBar
```

Components of the formatted view:

```
sourceImage   source text of the image node, color MaterialTheme.colors.secondary
sourceTable   GFMElementTypes.TABLE: source text of the node, monospace, horizontal scroll, inside MarkdownCodeBackground
              other node types: empty
```

Library behaviour, multiplatform-markdown-renderer 0.27.0:

```
TABLE node     rendered through components.custom only
custom set     each node passed to it counts as handled; its children are skipped
images         loaded only by an image transformer of a renderer coil module
dependencies   renderer and renderer-m2 (build.gradle.kts:80-81)
code blocks    horizontal scroll
flavour        Markdown() default: GFMFlavourDescriptor()
```

`OperatorView.kt`:

```
772-808   private fun ConditionsMarkdown(text)
          becomes fun DocumentMarkdown(
            text: String,
            flavour: MarkdownFlavourDescriptor = CommonMarkFlavourDescriptor(),
            image: MarkdownComponent = CurrentComponentsBridge.image,
            custom: CustomMarkdownComponent? = null
          )
          markdownComponents(...) gets image = image, custom = custom
755       ConditionsMarkdown(conditionsText) becomes DocumentMarkdown(conditionsText)
864       private fun internalUriHandler becomes fun internalUriHandler
```

Operator conditions: rendering unchanged, with the current arguments as defaults.

`SimpleXAPI.kt`:

```
after 144   val markdownFileTextView = mkBoolPreference(SHARED_PREFS_MARKDOWN_FILE_TEXT_VIEW, false)
after 431   private const val SHARED_PREFS_MARKDOWN_FILE_TEXT_VIEW = "MarkdownFileTextView"
```

`moko-resources/base/strings.xml`, after line 689:

```
markdown_formatted_view   Formatted view
markdown_text_view        Text view
```

The strings are the content descriptions of the switch: `markdown_text_view` in the formatted view, `markdown_formatted_view` in the text view.

`CIFileView.kt:194-200`:

```
onClick =
  isMarkdownFile(file)                            -> showMarkdownFile(file)
  appPlatform.isAndroid && loadedFilePath != null -> showOpenSaveMenu.value = true
  else                                            -> fileAction()
```

### 4.2. iOS

New file `apps/ios/Shared/Views/Chat/ChatItem/MarkdownFileView.swift`.
It is added to the SimpleX target in `SimpleX.xcodeproj/project.pbxproj`, at the four places of `CIFileView.swift` (lines 170, 562, 1125, 1602).

```
func isMarkdownFile(_ file: CIFile?) -> Bool
func showMarkdownFile(_ file: CIFile)
struct MarkdownFileView: View
```

`showMarkdownFile`:

```
Task:
  read data, decode text (section 3)
  error: alert, return
  showAppSheet { MarkdownFileView(file: file, text: text) } on the main actor
```

- `showAppSheet` (`AppSheet.swift:34`): presentation from the top view controller, with screen protection and environment objects.

`MarkdownFileView`:

```
container   NavigationView, title file.fileName, inline display mode
switch      toolbar button; @AppStorage(DEFAULT_MARKDOWN_FILE_TEXT_VIEW) private var textView = false
            symbol "chevron.left.forwardslash.chevron.right" in the formatted view, "doc.richtext" in the text view
save        toolbar button, symbol "square.and.arrow.up"; saveCryptoFile(fileSource) (CIFileView.swift:254)
formatted   ConditionsWebView(html: MarkdownParser().html(from: text)), Ink 0.6.0
text        ScrollView { Text(text).font(.system(.body, design: .monospaced)).textSelection(.enabled) }
```

- Save of an encrypted file, by `saveCryptoFile`, the current save action of file items: the file is decrypted into the temporary folder, the share sheet is shown, and the copy is removed after the sheet is closed.

`SettingsView.swift`:

```
after 86    let DEFAULT_MARKDOWN_FILE_TEXT_VIEW = "markdownFileTextView"
after 147   DEFAULT_MARKDOWN_FILE_TEXT_VIEW: false,
```

`ConditionsWebView.swift`, shared with the operator conditions:

```
makeUIView 18-31     WKWebViewConfiguration, defaultWebpagePreferences.allowsContentJavaScript = false
loadPage 37-58       head: <meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'">
                     style: table { border-collapse: collapse }
                     style: th, td { border; padding }
                     style: pre { overflow-x: auto }
Coordinator 64-79    linkActivated: cancel, openExternalLink(url), as now
                     other navigation: allowed for about:blank only
```

Library behaviour, Ink 0.6.0:

```
tables        supported
inline HTML   passed to the output
images        output as <img>
```

- Scripts, images, frames and other loads from the HTML are blocked by the Content Security Policy.
- Scripts are also blocked by the JavaScript setting.
- Navigation from inline HTML, such as `<meta http-equiv="refresh">`, is blocked by the navigation policy.
- iOS deployment target 15.0: `allowsContentJavaScript` and `textSelection` are available.

## 5. Links

```
Kotlin    LocalUriHandler -> internalUriHandler(parent) -> openExternalLink (Utils.kt:613)
iOS       linkActivated -> openExternalLink (ShareSheet.swift:94)
behaviour SimpleX contact and address links: opened in the app
          other links: confirmation alert with the link, then the system handler
```

Links in messages: the same rules.

## 6. Specs

```
apps/multiplatform/spec/services/files.md     section 2: MAX_MARKDOWN_VIEW_SIZE
apps/multiplatform/spec/client/chat-view.md   section 9, views/chat/item/: MarkdownFileView.kt
apps/ios/spec/services/files.md               size table: MAX_MARKDOWN_VIEW_SIZE
apps/ios/spec/client/chat-view.md             MarkdownFileView, next to CIFileView
```

## 7. Checks

1. Plain and encrypted local files: formatted and text views on Android, desktop and iOS.
2. Desktop linked to a mobile: file loaded from the mobile, then shown.
3. File over 1023 KB: tap action as before.
4. View switch: kept after closing the viewer and after an app restart.
5. Save: file at the chosen destination equal to the original; Kotlin: temporary folder unchanged.
6. Links: confirmation alert; SimpleX links opened in the app.
7. Remote image in the file: not loaded on either platform.
8. iOS, inline HTML with `<script>`, `<img>`, `<iframe>` and `<meta http-equiv="refresh">`: no effect.
9. Tables: monospace block in Kotlin, HTML table on iOS.
10. Operator conditions: rendering unchanged on both platforms.
