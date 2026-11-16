# SVG image support

Parked: options for a later decision; nothing is implemented.

SVG images, mostly diagrams written by agents, sent and shown in the CLI, the Node and Python libraries, the MCP server, and the iOS, Android and desktop apps. Four options are compared below; the decisions and mechanics after the options are those of option (a).

References are to master `d7d180a42`, after image processing moved into the core in #7941. Haskell paths without a directory are in `src/Simplex/Chat/`. The core resize and encode path is `src/Simplex/Chat/Image.hs`; the file-reference C API and its Haskell wrappers are in `src/Simplex/Chat/Mobile/File.hs`; the plan of #7941 is `plans/2026-10-23-core-image-resizing.md`.

## Decisions

| ID | Decision |
|---|---|
| D1 | **Recipients can save and edit the original SVG file.** A requirement of the user, for every option. |

## Options

| option | message | cost | original SVG for recipients (D1) |
|---|---|---|---|
| (a) the core rasterises, with lunasvg | an image message: a raster preview made by the core, and the original `.svg` as its file; recipients' apps rasterise the file through the core | the first C++ in the core and a C++ runtime on all 7 targets; a bundled fallback font, about 0.5 MB on every target; a pinned fork built by cabal, as hs-jpeg-turbo; changes in every app | the image message's file is the `.svg`; Save stores it (A10) |
| (b) each app's platform renderer | as (a); recipients render the file with AndroidSVG through coil-svg, Skia SVGDOM on desktop, or a WKWebView snapshot on iOS | three renderers and three sets of limits; desktop Skia draws no text, so diagram labels are lost; the CLI and the bots still need their own renderer or a fixed preview | as (a) |
| (c) the sender rasterises its own SVG, in the MCP server only, with `@resvg/resvg-js` 2.6.2 (MPL-2.0, published 2024-03-26, prebuilt for macOS, Linux, Windows and Android; the Linux x64 binary 4,384,036 bytes) | an ordinary image message, a PNG or JPEG of at most 261120 bytes with a core preview; the `.svg` source as a separate file message | one native dependency of the MCP server; recipients need no app change and render no SVG; app users, the CLI and other bots cannot send SVG as an image; the diagram is a raster, so zoom is limited to its pixels; two messages per SVG | the `.svg` arrives as a file message and is saved as any file |
| (d) a fixed preview with the `.svg` file | an image message: the core's fixed preview (`fixedImagePreview`, `Commands.hs:228-229`) and the `.svg` as its file | no diagram is shown until the file is saved and opened in another app; Android saves image files into MediaStore Images (`Share.android.kt:145-154`), so an `.svg` is saved with the file chooser instead, as files are (`ChatItemView.android.kt:40`) | iOS saves the file of an image that does not decode (`ChatView.swift:2444-2453`); desktop saves it with the save dialog (`ChatItemView.desktop.kt:38`) |

Cost order: (d) changes only the senders and the Android save; (c) adds one dependency to the MCP server and shows diagrams sent by it only; (a) and (b) render SVG in every app and cost the most, (a) with one renderer in the core, (b) with three renderers and no text on desktop.

In (c), the `.svg` is a separate file message: as the file of the image message itself, current apps show only the preview.
- iOS: `CIImageView` shows the loaded image only when `getLoadedImage` returns one, else the preview (`CIImageView.swift:28-42`); `getLoadedImage` returns `nil` when `UIImage(data:)` does not decode the file (`ImageUtils.swift:27-33`), and a tap on the preview of a received file does nothing (`CIImageView.swift:79`).
- Android: `getLoadedImage` returns `null` when `BitmapFactory` does not decode the file (`Utils.android.kt:186`, `:208-209`); `CIImageView` then shows the preview (`CIImageView.kt:222-227`), and a tap on it does nothing (`:260`); the gallery has no page for it (`ChatView.kt:3653-3660`).
- Desktop: `getLoadedImage` returns `null` when no `ImageIO` reader decodes the file (`Utils.desktop.kt:143-150`, `:191-196`); `CIImageView` shows the preview, as on Android.

## Decisions of option (a)

| ID | Decision |
|---|---|
| A1 | **Message type MCImage.** An SVG is an image message: the raster preview in `image`, the original `.svg` in the file. No new `MsgContent` constructor (`Protocol.hs:747`). Old clients show the preview (see "Old clients"). |
| A2 | **The file is the original SVG.** The sender saves and sends the `.svg` bytes unchanged; it does not re-encode the SVG. |
| A3 | **Preview from the core.** The preview is a JPEG or PNG data URI of at most `maxImagePreviewSize` 14000 characters (`Commands.hs:178`), produced by `decodeResizable` and `resizeImageDataUri` (`Image.hs:62`, `:199`), as for any image. `safeImageData` is unchanged; the preview carries no SVG (`Types.hs:951-957`). |
| A4 | **Recognition.** A byte stream is an SVG when, after an optional UTF-8 BOM, an optional XML declaration, whitespace and XML comments, the first element is `<svg`. A stream containing `<!DOCTYPE` or `<!ENTITY` is refused. |
| A5 | **Rasteriser lunasvg, pinned to master `470e78e2`**, with plutovg, built by cabal from a fixed source list, as hs-jpeg-turbo. Compiled with external resources disabled, system-font loading disabled, and stb limited to PNG and JPEG. |
| A6 | **Bundled fallback font.** A sans-serif font is bundled and registered as the fallback; the SVG cannot supply fonts. The font and its licence are an open question. |
| A7 | **Rasterisation size.** The SVG is rasterised to RGBA8 at its intrinsic size (`width`/`height`, else the `viewBox`), scaled so the larger side is at most `maxDisplaySide` 4320 (`Image.hs:291`); a small `viewBox` is not upscaled beyond a fixed factor. The existing resize and encode path then fits the preview and the display raster to their limits. |
| A8 | **Limits before rendering.** The core refuses an SVG above a byte size, an element count, a nesting depth, a `<use>` reference count, and a dash-array ratio. The values are an open question. |
| A9 | **Static only.** SMIL animation and scripts are not rendered; the raster is one frame. |
| A10 | **Display through the core; Save stores the file.** A received `.svg` is rasterised by the core for inline display and for full screen; apps render no SVG. Inline display reuses `chat_resize_image_file`/`_fd`; full screen uses a pixel-target rasterise (open question). Save stores the original `.svg` on every platform. |
| A11 | **Out of scope:** animated SVG, SVG profile images, SVG wallpapers, SVG link previews. |

## Mechanics of option (a)

### Core

- `Image.hs`, `decodeResizable` (`:62-70`): recognises an SVG by A4 and rasterises through the new lunasvg binding, returning `(DynamicImage, CPNone)`; JPEG and PNG unchanged. New helpers `isSvg`, `rasterizeSvg`.
- lunasvg binding: a package `JuicyPixels-lunasvg` in the hs-jpeg-turbo fork, exporting `rasterize :: ByteString -> (Int, Int) -> IO (Either String (Image PixelRGBA8))`; a small C++ wrapper over `Document::loadFromData` and `Document::renderToBitmap`, returning an RGBA8 buffer or an error string.
- Limits (A8): compile-time constants `maxSvgBytes`, `maxSvgElements`, `maxSvgDepth`, `maxSvgUseRefs`, `maxSvgDashRatio`, checked before and during rendering.
- Font (A6): embedded with `embedFile`, as `displayP3` embeds `DisplayP3-v4.icc` (`Image.hs:192`), and registered with the binding.
- `Commands.hs`: `imageExtensions` gains `.svg` (`:203-204`); `SendImage` accepts `.svg` in its extension check (`:3559`) and makes the preview with `imagePreview`, which rasterises through the updated `decodeResizable` (`:231-237`); `maxImageSize` applies (`:171-172`).

### Native build (hs-jpeg-turbo fork)

- lunasvg and plutovg pinned to master `470e78e2`, their C and C++ sources compiled by cabal on every target; armv7 uses the C paths.
- A C++ runtime is linked where #7941 linked none; `flake.nix` export and Windows-removal lists updated.
- `cabal.project` `source-repository-package` and `scripts/nix/sha256map.nix` updated, as for the existing forks (`2026-10-23-core-image-resizing.md`, "hs-jpeg-turbo fork").
- Licences in `docs/dependencies`: lunasvg (MIT), plutovg (MIT with a FreeType notice), stb, and the bundled font's licence.

### FFI

- Preview and send: no change; the apps call `chat_resize_image_file_uri`/`_fd` of #7941, which now rasterise an SVG file through `decodeResizable`.
- Display (A10): reuse `chat_resize_image_file`/`_fd` (byte budget), or add `chat_rasterize_image_file`/`_fd` with a target pixel side for crisp full-screen zoom.

### iOS

- Send: `ComposeView.swift` file importer (`:689-712`) and `UploadContent` (`:300-362`): a chosen `.svg` (`UTType.svg`) becomes an image — the preview from `resizeImageFileToStrSize` (`ImageUtils.swift:82`), the file the original `.svg` saved by `saveFileFromURL`, the message `MCImage`. Share extension `ShareModel.swift` `.fileURL` and `.data` (`:462-478`): an `image/svg+xml` or `.svg` item becomes `.image`.
- Receive and display: `getLoadedImage` (`ImageUtils.swift:22-39`): for an image file named `.svg`, the core recognises it (A4) and rasterises it, and the bitmap is returned; `CIImageView` (`CIImageView.swift:28`) shows it; `FullScreenMediaView` (`:138-149`) requests a larger render. Other formats keep their current handling.
- Save: `saveButton(file:)` for an `.svg` image, which shares the original file (`ChatView.swift:2444-2453`, `:2648-2657`).

### Android and desktop

- Send: `isImage` recognises `.svg` by an explicit extension check on both platforms (`Images.android.kt:99-100`, `Images.desktop.kt:159-166`); `processPickedMedia` routes an `.svg` through the image path with a core preview (`ComposeView.kt:359-412`); a new `UploadContent.VectorImage` copies the original `.svg` (as `saveAnimImage` copies, `Utils.kt:229`), not `saveImage`, which re-encodes (`Utils.kt:174`). The desktop image file dialog filter adds `svg` (`Files.desktop.kt:105-111`).
- Receive and display: `getLoadedImage` (Android `Utils.android.kt:172-194`, desktop `Utils.desktop.kt:136-154`): for an image file named `.svg`, the core recognises it (A4) and rasterises it; `CIImageView` (`CIImageView.kt`) and `providerForGallery` (`ChatView.kt:3649-3666`) show the bitmap; `base64ToBitmap` and `decodeSampledBitmapFromByteArray` are unchanged. Other formats keep their current handling.
- Save: Android saves an `.svg` image with the file chooser, as files (`ChatItemView.android.kt:33-40`); desktop saves the file with the save dialog (`ChatItemView.desktop.kt:38`).

### CLI

- `/img` accepts `.svg` (`imageExtensions`); the preview is the core raster (`imagePreview`); the `.svg` file is sent.

### Bots and MCP

- Node `resizeImage` and Python `resize_image` call the core (`packages/simplex-chat-nodejs/src/core.ts:126`, `packages/simplex-chat-python/src/simplex_chat/core.py`); passing SVG bytes rasterises them, with no library change.
- MCP sends files by path; the `send` of the MCP files plan (`plans/2026-10-30-mcp-files-and-leave.md` on `ep/mcp-files`, "Sending files") makes an image preview through the core for an `.svg`.

## Mechanics of option (c)

- MCP `send` with an `.svg` path, one more row in "Sending files" of the MCP files plan:
  1. `new Resvg(svg, {fitTo: {mode: "width", value}})`, the larger side at most 4320 px, then `render().asPng()` (`@resvg/resvg-js` 2.6.2, `index.d.ts:55-58`, `:86-88`)
  2. the image `chatResizeImage(png, 261120, "jpeg" | "png")` and the preview `chatResizeImageUri(png, 14000)` (`packages/simplex-chat-nodejs/src/core.ts:115`, `:122`)
  3. one `apiSendMessages` call (`packages/simplex-chat-nodejs/src/api.ts:448`) with two composed messages: the image with the caption, then the `.svg` as a file
- No change in the core, the apps, the CLI or the libraries.

## Rendering limits of option (a)

- The core refuses `<!DOCTYPE` and `<!ENTITY` (A4).
- lunasvg is built with external resources and system fonts disabled, and stb limited to PNG and JPEG (A5).
- The byte, element, depth, `<use>` and dash limits (A8) bound time and memory before and during rendering.
- Scripts, `foreignObject` and animation are not rendered.

## Old clients

- (a), (b): an SVG message is `MCImage` with a normal JPEG or PNG preview, so an old client shows the preview inline as any image; its platform decoder returns nothing for the `.svg` (iOS `UIImage(data:)`, `ImageUtils.swift:27`; Android `BitmapFactory`, `Utils.android.kt:197-209`; desktop `ImageIO`, `Utils.desktop.kt:191-213`), so the preview stays; iOS and desktop save the `.svg` file (`ChatView.swift:2444-2453`, `ChatItemView.desktop.kt:38`), and Android saves image files into MediaStore Images (`Share.android.kt:145-154`).
- (c): an ordinary image and a file message, shown as today.
- (d): every client shows the fixed preview, as today.

## Tests

Option (a):
- Core, `tests/ImageTests.hs` (group "Image resize", `tests/Test.hs:98`): an SVG rasterised to a preview and to image data; the recognition (A4) accepts a BOM, an XML declaration and comments before `<svg`, and refuses `<!DOCTYPE` and `<!ENTITY` and files over the limits; a diagram with text renders non-blank.
- Core, `tests/MobileTests.hs` ("Images"): `chat_resize_image_file` on an `.svg` returns a JPEG or PNG; a malformed SVG returns `""`.
- Core, `tests/ChatTests/Files.hs` ("send image with generated preview"): `/img` with an `.svg` produces a JPEG or PNG preview of at most 14000 characters.
- Node `packages/simplex-chat-nodejs/tests/core.test.ts` and the Python resize test: `resizeImage`/`resize_image` on SVG bytes returns a raster.
- App smoke tests `scripts/ci/android/smoke-test.sh`, `desktop-linux/smoke-test.sh`, `ios-sim/smoke-test.sh`: a fixture `.svg` chosen via the file chooser, previewed and sent as an image; the received item shows a raster. New fixture `tests/fixtures/*.svg`. No Claude-driven tests.

Option (c):
- MCP end-to-end test: `send` of an `.svg`; the peer receives an image of at most 261120 bytes with a preview, then a file message with the `.svg`.

## Open questions

1. Which option: (a), (b), (c) or (d)?
2. For (a): a C++ SVG rasteriser (lunasvg) in the core, with a bundled font of about 0.5 MB and a C++ runtime on all targets.
3. For (a): the fallback font and licence, Noto Sans (OFL, about 0.43 MB) or a Latin, Greek and Cyrillic DejaVu subset.
4. For (a): full-screen display through the byte-budget resize, or a pixel-target rasterise function.
5. For (a): the limit values: bytes, element count, nesting depth, `<use>` references, dash ratio.
6. For (a), (b) and (d): SVG sent via the file chooser, share and drag-and-drop only, or also via the gallery image pickers.
