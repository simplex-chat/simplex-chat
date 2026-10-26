# Image resizing in the core

The CLI sends a fixed placeholder as the preview of every image sent with `/img` or `/imgf`, and bots on the Node and Python libraries need their own image resizing code.
Images are resized by the core in one Catmull-Rom pass with sharpening, at a fixed JPEG quality, to the size whose encoding fills 97–100% of the size limit; an image that fits at its largest size is encoded at the highest quality that fits.
JPEG input is decoded by mozjpeg at the smallest scale that covers the display size, and its EXIF orientation is applied; PNG input is decoded by libspng; other formats are refused.
The core functions are called by the apps in place of platform code, wrapped in both bot libraries and used for `/img` and `/imgf` previews; mozjpeg and libspng are compiled by cabal in every build.

References are to branch `ab/resize-image` at `6021d9a6d`, based on master `ec8884f46`. Haskell paths without a directory are in `src/Simplex/Chat/`.

## Image processing decisions

The research winner is implemented: mozjpeg, Catmull-Rom and sharpening, at a fixed quality, with the size fitted to the limit. Approved on 26 Oct.

| ID | Decision | Review answer |
|---|---|---|
| D1 | **Quality bar.** The research quality is accepted as good enough. On the 8 research inputs, previews and 255 KB images from the core score at least the research scores of the D4 search (mean +3.3 and 62.2), each input within 0.5. Checked before review. | Changed on 26 Oct by the user's message: "you are overthinking it, settle for good enough already. what I've seen in previews is good enough, ship it". |
| D2 | **mozjpeg master, encoding through its libjpeg API.** mozjpeg master 08265790 (Jun 2025, libjpeg-turbo 3.0.5 base) replaces libjpeg-turbo, built through cabal from a submodule. A small C encoder calls the libjpeg API; decoding keeps the reviewed TurboJPEG 3 binding. | Confirmed. |
| D3 | **Resize in one pass with Catmull-Rom, then sharpen.** stb_image_resize with Catmull-Rom (a JuicyPixels-stbir option, no fork change), then an unsharp mask, sigma 0.6 px, amount 0.5, on gamma-encoded values. | Confirmed. |
| D4 | **Fixed quality, size fitted to the limit.** mozjpeg 4:2:0 at quality 20 for previews and profile images, 65 for full images. The size is predicted from bytes per pixel, then corrected until the encoding fills 97–100% of the limit. When the full-resolution image fits, quality rises to fill the limit, up to 95. PNG: the same size search, without quality. | Changed on 26 Oct by the user's message: "you are overthinking it, settle for good enough already. what I've seen in previews is good enough, ship it". |
| D5 | **Scaled JPEG decoding (S2).** Header first, decode at full, 1/2, 1/4 or 1/8 scale; output sizes equal the full-decode path. Implemented in `fc06d29d4`. | Confirmed. |
| D6 | **EXIF rotation in the core.** JPEG input is rotated by its EXIF orientation after decoding; outputs carry no metadata. Implemented in `37be23b6b`. | Confirmed. |
| D7 | **Apps pass a file reference to the core.** Apps pass the picked file itself, not decoded or re-encoded pixels: iOS the temporary file URL from the photo picker, desktop the chosen path, Android a file descriptor of the content URI. The core reads the file, decodes it at the needed scale and applies EXIF rotation. | Confirmed, with the comment "Could we pass file reference instead of bytes? I think iOS can create temporary links to chosen photos, so Haskell/C code just reads it instead of piping bytes via FFI. Was it considered? Same for other platforms." The file reference replaced the file bytes of the reviewed version. |
| D8 | **Limits.** JPEG side 65,500 px, PNG side 16,384 px, 268 MP, progressive-JPEG memory 512 MB on iOS and Android and 1,024 MB elsewhere. | Confirmed, with the comment "Do we need 2gb? I'd limit to 1 if it's possible, but explain". 1,024 MB replaced 2,047 MB. |
| D9 | **32-bit Android uses Neon.** mozjpeg's 32-bit Neon sources are compiled for armv7, tested by a native armv7 CI job on GitHub's ARM64 runners. | Confirmed. |
| D10 | **Balance of time and quality.** Settled by D4: one fixed quality and about 3 encodes per preview, without a quality metric in the core. Time per preview and per full image is measured on desktop and devices and reported in the PR. | Changed on 26 Oct by the user's message: "you are overthinking it, settle for good enough already. what I've seen in previews is good enough, ship it". |
| D11 | **Feature texts state behaviour only.** The TJFLAG_LIMITSCANS doc comment copied from upstream is trimmed to "Limits progressive JPEGs to 500 scans." | Confirmed. |
| D12 | **Out of scope.** Guetzli (a later PR for channels and groups, badge holders only) and other image formats. | Confirmed. |
| D13 | **Approved texts.** README line "JPEG decoding and encoding of resized images"; binding description "JPEG encoding and decoding of JuicyPixels images with mozjpeg"; a TurboJPEG3 module header like TurboJPEG2's. | Option (a), with the comment "to many words - we need what we've seen as the best result, I don't think it's even offered. To spell out the only possible choice: mozjpeg + Catmull-Rom + sharpening (recommended)  why it's not here?" The research winner became the first line of this section. |

Decisions of 26 Oct, after the review:

| ID | Decision |
|---|---|
| D14 | **No interim search.** The D3 functions and their pinned test are added now; `fitImage` changes once, with D4. |
| D15 | **Transparency in premultiplied alpha.** Images with transparency are resized and sharpened with premultiplied colour, so colours under transparent pixels do not reach visible pixels. |
| D16 | **Exact pinned test on every platform.** stb_image_resize in JuicyPixels-stbir is compiled without floating-point contraction. |
| D17 | **HEIC is converted by iOS before the core.** iOS requests the `public.jpeg` representation of picked items other than PNG and GIF; for `""` the bytes functions are used. |
| D18 | **Sharpening only of downscaled images.** An image kept at its size is not sharpened: on the github input kept at full resolution, sharpening lowered the large-screen SSIMULACRA2 from 92.53 to 89.94. |
| D19 | **PNG decoded by libspng; other formats refused.** PNG input is decoded by libspng 0.7.4 with miniz 3.1.2, bundled in JuicyPixels-spng: the header first, then the pixels, within the side and memory limits. Input other than JPEG and PNG gives `Left`, and `""` from the C API, on which the apps decode it themselves and pass JPEG or PNG bytes. JuicyPixels remains for PNG encoding and the image types. |

## Decisions

Decoding, `Image.hs`:
- `decodeResizable bytes`: the image for resizing; at the display size for JPEG, at full size for PNG; `Left` for other formats.
- JPEG, input starting `FF D8`, decoded by mozjpeg:
  1. `readJpegHeader`; `Left` for:
     - a side above 65500
     - a precision other than 8
     - a lossless JPEG
  2. target size: both sides halved, each rounded by `scaledSide` (`:320-321`), while a side exceeds 4320; `Left` when a side is below 2 before a halving
  3. scale: the smallest of `Full`, `Half`, `Quarter` and `Eighth` whose rounded-up sides are at least the target
  4. `decodeJpeg` with:
     - `maxPixels`: 2^28; `Left` above it, before the output is allocated
     - `maxMemoryMB`: `maxDecodeMemoryMB`
  5. a decode larger than the target: `convertRGB8`, then `STBIR.resize` to the target
  6. the EXIF orientation applied
- PNG, input starting with the signature `89 50 4E 47 0D 0A 1A 0A`, decoded by libspng with `Spng.decodePng`:
  1. the header (IHDR) first; `Left` for a side above 16384
  2. `maxMemoryMB`: `maxDecodeMemoryMB`; `Left` when the decoded pixels exceed it, before they are allocated
  3. `Left` for any libspng error, including short image data and a chunk with a wrong CRC
  4. constructor: `ImageRGBA8` for a colour type with alpha or a `tRNS` chunk, otherwise `ImageRGB8`; 16-bit samples converted to 8 bits
  5. the full image, halved to the display size by `fitImage`
- Other formats: `Left "unsupported image format"`.
- Constructors of a decoded JPEG:

  | JPEG colour space | constructor |
  |---|---|
  | YCbCr, RGB | `ImageRGB8` |
  | grayscale | `ImageY8` |
  | CMYK, YCCK | `ImageCMYK8` |

- `Left` from mozjpeg for:
  - any warning, including truncated data and extraneous bytes
  - more than 500 scans
  - whole-image coefficient buffers above `maxMemoryMB`
- `maxDecodeMemoryMB`, the `maxMemoryMB` of both decoders, a compile-time constant, 512 when `mobileBuild` is defined and 1024 otherwise:

  | build | `simplex-chat.cabal` condition | set in |
  |---|---|---|
  | iOS and iOS simulator | `flag(swift)` | `flake.nix:555`, `:592`; `scripts/ci/ios-sim/build-core.sh:27` |
  | Android aarch64 and armv7a | `os(android)` | the target platforms of `flake.nix:418` and `:340` |

- EXIF orientation of JPEG input:
  - source: the first APP1 segment with the `Exif\0\0` header before the first SOS; IFD0 tag `0x0112`
  - a missing tag, another value or malformed EXIF: 1

  | value | transform |
  |---|---|
  | 1 | none |
  | 2 | horizontal flip |
  | 3 | rotation by 180° |
  | 4 | vertical flip |
  | 5 | transpose |
  | 6 | rotation by 90° clockwise |
  | 7 | transverse |
  | 8 | rotation by 90° counter-clockwise |

  - output: upright pixels without EXIF

Resizing, `Image.hs`:
- `resizeImage format maxSize image`: a JPEG or PNG encoding of at most `maxSize` bytes.
- `resizeImageDataUri maxSize image`: a data URI of at most `maxSize` characters.
  - format: `imageFormat image`
  - prefix: `data:image/jpg;base64,` or `data:image/png;base64,`
- `imageFormat`:
  - PNG: an image with a pixel below full opacity, of a constructor with alpha:
    - `ImageYA8`
    - `ImageYA16`
    - `ImageRGBA8`
    - `ImageRGBA16`
  - JPEG: other images
- Pixels are converted to 8 bits per channel, RGB for JPEG and RGBA for PNG; every `DynamicImage` constructor is accepted.
- Size search (D4), `fitImage` and `fitSize`; L is the limit in the output measure, bytes or data URI characters:

  | step | JPEG | PNG |
  |---|---|---|
  | size bound | the JPEG and PNG header limits: `Left`, before decoding; an image given to `resizeImage` or `resizeImageDataUri` with a side above 16384 pixels: `Left`, before any encode or resize | the same |
  | largest size | the image sides halved while a side exceeds 4320 pixels, rounded as `scaledSide` rounds | the same |
  | quality | 20 for data URIs, 65 for image data | |
  | trial | `scaleRGB8` from the decoded image to the trial size, encoded at the quality | `scaleRGBA8`, `encodePng` |
  | trial height | `max 1 (floor (w·h_largest/w_largest + 0.5))`, rounded half up as `height_for` of the research | the same |
  | first width | from pixels `8·B/bpp0`, with B the target T = √0.97·L in JPEG bytes (for a data URI, 3/4 of the characters after the prefix) and bpp0 0.35 at quality 20, 0.9 at quality 65 | the largest width |
  | width from pixels p | `round (√(p·w_largest/h_largest))`, within 1..w_largest | the same |
  | accepted | fits L and is at least 0.97·L, or fits at the largest width | the same |
  | next width | from pixels `p·(T/size)^(1/α)`; α from the last two encodes, `log (size₂/size₁) / log (p₂/p₁)`, when within 0.4..1.3; otherwise 0.85 | the same |
  | repeated width | the midpoint of the largest fitting width and the smallest larger width that does not fit; the search ends when they differ by 1 | the same |
  | end | after 8 encodes or the end of the bisection: the largest fitting width | the same |
  | no width fits | the smallest tried width halved until the encoding fits: `Left` when a width below 2 is to be halved | the same |
  | at the largest width | below 0.97·L: quality bisected in the quality..95; otherwise the encoding itself | the encoding itself |

- The largest size applies to the image data and to the data URIs: the Android and desktop apps display images and previews only up to 4320 pixels per side (`CIImageView.kt:128`, `base64ToBitmap` in `Images.android.kt:35` and `Images.desktop.kt:42`).
- The quality bisection is ended by an encoding of exactly `maxSize`.
- Metadata of the input is dropped.

Scaling (D3), `Image.hs`:
- `scaleRGB8 size image` and `scaleRGBA8 size image`: the image at `size`.
  - the same size: the image itself, not sharpened (D18)
  - another size: one stb_image_resize pass from the image, Catmull-Rom, edges clamped, on gamma-encoded values; then the unsharp mask
- `scaleRGBA8` (D15):
  1. colour premultiplied by alpha: `(c·a + 127) div 255`
  2. stb_image_resize with `STBIR_FLAG_ALPHA_PREMULTIPLIED`
  3. the unsharp mask on the four channels
  4. colour divided by alpha: `min 255 ((c·255 + a div 2) div a)`; 0 for `a = 0`
- Unsharp mask, the arithmetic of the research code:
  - kernel: Gaussian, sigma 0.6 px, radius `ceiling (3·sigma)` = 2, weights divided by their sum, in `Double`
  - a horizontal pass on the samples, then a vertical pass on its result; edges clamped; sums in `Double`, each pass stored as `Float`
  - output: `floor (v + 0.5·(v − b) + 0.5)`, clamped to 0..255; `v − b` in `Float`
- `vector` is a direct dependency.

C API:
```
chat_resize_image_uri(data, len, maxSize)     data URI of at most maxSize characters, or ""
chat_resize_image(data, len, maxSize, png)    base64 of at most maxSize bytes of PNG (png > 0), JPEG (png == 0) or the imageFormat choice (png < 0), or ""
```
- `data`: an encoded JPEG or PNG image, read in place during the call.
- `""` is returned when:
  - the input is neither JPEG nor PNG, or it is refused by the header limits, by mozjpeg or by libspng;
  - the image exceeds the limit at every size;
  - an exception is raised.
- The result is allocated with `malloc` and freed by the caller.
- A chat controller is not required.
- `png`: 0 or 1 from the apps and the explicit library functions; −1 from the library functions with `ImageSize`.

C API, file reference (D7):
```
chat_resize_image_file_uri(path, maxSize)     as chat_resize_image_uri, from the file at path
chat_resize_image_file(path, maxSize, png)    as chat_resize_image, from the file at path
chat_resize_image_fd_uri(fd, maxSize)         as chat_resize_image_uri, from the file descriptor fd
chat_resize_image_fd(fd, maxSize, png)        as chat_resize_image, from the file descriptor fd
```
- C declarations:
  ```c
  extern char *chat_resize_image_file_uri(char *path, int maxSize);
  extern char *chat_resize_image_file(char *path, int maxSize, int png);
  extern char *chat_resize_image_fd_uri(int fd, int maxSize);
  extern char *chat_resize_image_fd(int fd, int maxSize, int png);
  ```
- `path`: a null-terminated path, decoded as `chat_write_file` decodes its path.
- `fd`: a readable file descriptor; the core reads it to its end and closes it, on success and on failure.
- `""` is also returned when the file cannot be read.
- Callers:

  | app | input | functions |
  |---|---|---|
  | iOS | the photo picker file, `public.jpeg` for items other than PNG and GIF, copied to the app's temporary directory | `chat_resize_image_file_uri`, `chat_resize_image_file` |
  | desktop | the chosen path | `chat_resize_image_file_uri`, `chat_resize_image_file` |
  | Android | `ParcelFileDescriptor.detachFd()` of the content URI | `chat_resize_image_fd_uri`, `chat_resize_image_fd` |

- For `""`, the apps use the platform decoding and the functions with bytes, as now. Formats other than JPEG and PNG reach the core only as JPEG or PNG bytes encoded by the apps:

  | app | preview | sent image | animated GIF and WebP |
  |---|---|---|---|
  | iOS | `UploadContent.imagePreview()`, `ComposeView.swift:315-320`: `resizeImageToStrSize` of the `UIImage` | `ComposeView.swift:1599`: `saveImage` of the `UIImage` | `.animatedImage`, sent as the file (`ComposeView.swift:1600-1601`) |
  | Android | `processPickedMedia`, `ComposeView.kt:372-374`, `:404`: `getBitmapFromUri`, then `resizeImageToStrSize` | `resizeImageFromUri`, `Utils.kt:218-227`: `getBitmapFromUri`, then `resizeImageToDataSize` | `UploadContent.AnimatedImage`, sent as the file (`ComposeView.kt:375-384`) |
  | desktop | as Android | as Android | as Android; the image picker offers only GIF, WebP, PNG and JPEG (`Images.desktop.kt:159-166`) |

- Haskell, `Mobile/File.hs`:
  ```haskell
  cChatResizeImageFileUri :: CString -> CInt -> IO CString
  cChatResizeImageFile :: CString -> CInt -> CInt -> IO CString
  cChatResizeImageFdUri :: CInt -> CInt -> IO CString
  cChatResizeImageFd :: CInt -> CInt -> CInt -> IO CString
  ```
  - path: `readFilePath`, `peekCString`, then `B.readFile`
  - descriptor: `readFd`, `fdToHandle`, then `B.hGetContents`, by which the handle is closed, also on an exception; the descriptor is closed with `c_close` when `fdToHandle` raises
  - `cResizeImageUri` and `cResizeImageData`: the data URI and the image data from a read action, for the buffer, path and descriptor functions
  - `cResizeImage` takes the read action in place of the buffer; the input is read inside `runCatchExceptT`.
- Exports:
  - `Mobile.hs`: `foreign export ccall` of the four functions
  - `libsimplex.dll.def` and the Android `-u` list of `flake.nix`: the four functions

Apps, with function signatures kept and call sites unchanged except iOS `saveWallpaperFile(image:)` (`ImageUtils.swift:246`):
- iOS `resizeImageToDataSize` and `resizeImageToStrSizeSync`:
  - an image with orientation other than `.up` is redrawn upright;
  - input: UIKit PNG for an image with alpha, otherwise JPEG at quality 0.85;
  - alpha:
    - `resizeImageToDataSize`: the `hasAlpha` parameter, by which PNG or JPEG output is also selected
    - `resizeImageToStrSizeSync`: `imageHasAlpha(image)`, for the input only
  - `""` is returned as `nil`; for it `nil` is also returned by `saveImage` (`ImageUtils.swift:68`) and `saveWallpaperFile(image:)` (`:246`).
- Android and desktop, common code:
  - input: `compressImageData` bytes, PNG or JPEG at quality 85, in a direct `ByteBuffer`;
  - for `""` an exception is raised by both functions, the failure mode of the platform code of master;
  - `resizeImageToStrSize`:
    - PNG input when the image has alpha;
    - the exception is caught in `getLinkPreview` (`LinkPreviews.kt:134`), and no link preview is returned;
    - in the other eight callers it is propagated, as on master.
  - `resizeImageToDataSize`:
    - input and output in the requested format;
    - the exception is caught by:
      - `saveImage`
      - `desktopSaveImageInTmp`
      - `ThemeWallpaper.importFromString`, around `saveWallpaperFile`

Apps, file reference (D7, D17):
- iOS:
  - picker, `loadImageData` (`ImagePicker.swift`): the `public.jpeg` representation when the item provider has one and the item is neither PNG nor GIF; otherwise `public.data`, as before
  - `UploadContent.imageFile(image:url:)`: a non-animated picked image, with its file copied to the temporary directory and added to `filesToDelete`
  - preview, `imagePreview()`: `resizeImageFileToStrSize` for `.imageFile`; for other content or `nil`, `resizeImageToStrSize`
  - sending: `saveImageFile(url)`; for `nil`, `saveImage(image)`
  - `SimpleX.h`: `chat_resize_image_file`, `chat_resize_image_file_uri`
- Android and desktop:
  - common: `resizeImageFileToStrSize` and `resizeImageFileToDataSize`, on `resizeImageFileUri` and `resizeImageFileData` of each platform:
    - Android: `contentResolver.openFileDescriptor(uri, "r").detachFd()`, then `chatResizeImageFdUri` or `chatResizeImageFd`
    - desktop: `uri.toFile().absolutePath`, then `chatResizeImageFileUri` or `chatResizeImageFile`
  - `processPickedMedia` and the Android camera: the preview from the file; the bitmap only for an animated image or when the preview is `null`
  - `saveImage(uri)` and `desktopSaveImageInTmp(uri)`, through `resizeImageFromUri`: the core data from the file with `png = -1`, extension `png` for a first byte `0x89`, otherwise `jpg`; for `null`, the bitmap path, as before
  - JNI: the descriptor functions in the Android `simplex-api.c`, the path functions in the desktop one

Bot libraries:
- Node, `core`:
  - `resizeImage(image: ArrayBuffer | Uint8Array, size: ImageSize): Promise<Buffer | string>`, with overloads: `Buffer` for `ImageSize.Image`, `string` for the previews
  - `chatResizeImageUri(image: ArrayBuffer | Uint8Array, maxSize: number): Promise<string>`
  - `chatResizeImage(image: ArrayBuffer | Uint8Array, maxSize: number, format: "jpeg" | "png"): Promise<Buffer>`
- Python, `simplex_chat.core`:
  - `async def resize_image(data: bytes, size: ImageSize) -> bytes | str`, with `typing.overload`: `bytes` for `ImageSize.IMAGE`, `str` for the previews
  - `async def chat_resize_image_uri(data: bytes, max_size: int) -> str`
  - `async def chat_resize_image(data: bytes, max_size: int, image_format: Literal["jpeg", "png"]) -> bytes`
- `ImageSize`, a string enum of both libraries:

  | value | Node | Python | call | limit |
  |---|---|---|---|---|
  | `"image"` | `Image` | `IMAGE` | `chat_resize_image`, `png = -1` | 261120 bytes |
  | `"profilePreview"` | `ProfilePreview` | `PROFILE_PREVIEW` | `chat_resize_image_uri` | 12500 characters |
  | `"messagePreview"` | `MessagePreview` | `MESSAGE_PREVIEW` | `chat_resize_image_uri` | 14000 characters |

- The limits are literals in `resizeImage` and `resize_image`; `ImageSize` is absent from the C API.
- The explicit functions stay public, for other sizes.
- Doc comments:
  - `resizeImage` and `resize_image`: "Resize image to the app size limit: JPEG or PNG for an image, a data URI for a preview", with a final period in Python
  - Node `ImageSize`: "App image size limit"
  - Python `ImageSize`: none
- The core module texts name image resizing; in Python, with `resize_image`, `chat_resize_image` and `chat_resize_image_uri`.
- The core is called on a worker thread:
  - Node: an N-API async worker
  - Python: `asyncio.to_thread`
- Errors:

  | condition | Node | Python |
  |---|---|---|
  | `""` from the core | `Error("image cannot be decoded or does not fit maxSize")` | `RuntimeError("image cannot be decoded or does not fit max_size")` |
  | export absent from the loaded libsimplex | `Error("loaded libsimplex does not export <name>; image resizing needs a newer libsimplex")` | `RuntimeError` with the same text |
  | function absent from the loaded Node add-on | `TypeError: addon[name] is not a function` | |
  | size limit outside 32-bit integers | `RangeError("Expected 32-bit integer maxSize")` | `ValueError` |
  | input above 2^31 − 1 bytes | `RangeError("Buffer is too large")` | `ValueError` |

- Pinned releases:
  - Until a core release built with this change is pinned by `scripts/ci/packages_release.py prepare`, `core-7.1.0.14` is loaded (`packages/simplex-chat-nodejs/src/release.ts:3`, `packages/simplex-chat-python/src/simplex_chat/_version.py:8`), and the export error is raised by the Python wrappers.
  - The Node add-on is pinned to `nodejs-addon-308c0960c707` (`release.ts:5`), built without either function: the promises of the Node wrappers are rejected with `TypeError: addon[name] is not a function` (`src/simplex.js:20`). The export error is raised with an add-on built from this branch, as in the tests (`jest.config.js:4`).
  - The core and the add-on are pinned together by `prepare` (`packages_release.py:360-363`); for nodejs an error is raised by `prepare` until the add-on release of the current sources is published (`:779-780`).

`/img` and `/imgf`:
- The preview is generated from the file: read with `CF.readFile` (encrypted files included), decoded, and resized to a data URI of at most 14000 characters.
- On any failure the fixed placeholder is sent, as before; a GIF, which `/img` accepts by its extension (`Library/Commands.hs:202`), gets the fixed placeholder.

Native code:
- mozjpeg master `08265790` (libjpeg-turbo 3.0.5 base) is the submodule `jpeg-turbo/cbits/mozjpeg` of the hs-jpeg-turbo fork; cabal compiles it in the `jpeg-turbo` package of every build:
  - 114 C sources on every target
  - aarch64: 15 Neon sources
  - armv7: `simd/arm/aarch32/jsimd.c`, and 14 Neon sources in the internal library `neon`, compiled with `-mfpu=neon`
  - configuration headers `jconfig.h`, `jconfigint.h`, `jversion.h` and `neon-compat.h` in `cbits`, with the target-dependent values taken from compiler macros; `THREAD_LOCAL` empty on 32-bit Android Arm
  - the C code paths on x86-64
  - built with cabal and the C compiler only
- JPEG input is decoded by the TurboJPEG API of mozjpeg; JPEG output is encoded by `jpeg_turbo_encode` (`cbits/encode.c`) through the libjpeg API, with mozjpeg's default profile.
- libspng 0.7.4 and miniz 3.1.2 are the submodules `JuicyPixels-spng/cbits/libspng` and `JuicyPixels-spng/cbits/miniz` of the hs-jpeg-turbo fork; cabal compiles `spng.c`, `miniz.c`, `miniz_tdef.c` and `miniz_tinfl.c` in the `JuicyPixels-spng` package of every build, with `SPNG_USE_MINIZ`.
- Package sources:

  | package | during review | before the final pull request |
  |---|---|---|
  | hs-jpeg-turbo (`jpeg-turbo`, `JuicyPixels-jpeg-turbo`, `JuicyPixels-spng`) | evgeny-simplex/hs-jpeg-turbo, branch `ep/jpeg-decode` at `e94e035` | a simplex-chat/hs-jpeg-turbo commit |
  | JuicyPixels-stbir, with nothings/stb as a submodule | evgeny-simplex/JuicyPixels-stbir, branch `ep/stb-submodule` at `234dde8` | a simplex-chat/JuicyPixels-stbir commit |
  | JuicyPixels 3.3.8 | Hackage | Hackage |

- The hashes of both forks in `scripts/nix/sha256map.nix` are computed with submodules, as for simplexmq.
- In Windows builds, `dist-newstyle/src/hs-jpeg-t*` and `dist-newstyle/src/JuicyPixe*` are removed beside `direct-sq*` and `simplexmq*`.

License notices, in `docs/dependencies`:
- `README.md`: mozjpeg, with the IJG sentence, and stb_image_resize
- `HASKELL.md`: JuicyPixels, JuicyPixels-jpeg-turbo, JuicyPixels-spng and JuicyPixels-stbir as direct dependencies, jpeg-turbo as an indirect one; the five are added to the "Also depended upon by" and "Depended upon by" cells of the packages they depend on
- `licences/`: the license files of each, copied from the pinned commits; libspng's BSD-2-Clause `LICENSE` and the libpng-2.0 notice of `spng.c`, and miniz's MIT `LICENSE`

Out of scope: WebP, a separate PR after this one.

## hs-jpeg-turbo fork

Branch `ep/jpeg-decode` at `e94e035`:
- mozjpeg compiled by cabal, as in "Native code"; arithmetic-coded JPEGs are decoded, the arithmetic encoder is not compiled.
- `encodeRGB`: `jpeg_turbo_encode` (`cbits/encode.c`, `MozJPEG.hsc`): `jpeg_set_defaults`, `jpeg_set_quality`, the output in a buffer grown by doubling, its size returned as `size_t`; libjpeg errors and warnings end the encoding with their text.
- `Codec.Picture.JPEGTurbo` exports, besides `encodeRGB` and `EncodableRGB`:
  - `readJpegHeader :: ByteString -> IO (Either String JpegHeader)`
  - `decodeJpeg :: DecodeOptions -> ByteString -> IO (Either String DynamicImage)`
  - `JpegHeader`:
    - `jpegWidth :: Int`
    - `jpegHeight :: Int`
    - `jpegSubsamp :: TJSAMP`
    - `jpegColorspace :: TJCS`
  - `DecodeOptions`:
    - `scale :: Scale`
    - `maxPixels :: Int`
    - `maxMemoryMB :: Int`, 1..2047
  - `data Scale = Full | Half | Quarter | Eighth`
- API:
  - header and parameters: `tj3DecompressHeader`, `tj3Get`, `tj3Set`, in the new module `TurboJPEG3`
  - decoding: `tjDecompress2` with the scaled width and height
- `readJpegHeader` and `decodeJpeg` give `Left` for:
  - an input longer than `CULong`
  - any mozjpeg error or warning, including an empty input
  - a side below 1
  - a precision other than 8
  - a lossless JPEG
  - more than `maxPixels` pixels, checked before the output buffer is allocated
- Decoding parameters:
  - stop on the first warning (`TJPARAM_STOPONWARNING`, `TJFLAG_STOPONWARNING`)
  - at most 500 scans (`TJFLAG_LIMITSCANS`)
  - `TJPARAM_MAXMEMORY`: `maxMemoryMB`
- Output:

  | JPEG colour space | pixel format | bytes per pixel | constructor |
  |---|---|---|---|
  | YCbCr, RGB | `TJPF_RGB` | 3 | `ImageRGB8` |
  | grayscale | `TJPF_GRAY` | 1 | `ImageY8` |
  | CMYK | `TJPF_CMYK` | 4 | `ImageCMYK8` |
  | YCCK | `TJPF_CMYK`, samples inverted in the output buffer | 4 | `ImageCMYK8` |

- Sides: the JPEG sides divided by 1, 2, 4 or 8, rounded up.
- The bytes per pixel are passed with each pixel format; the output buffer size is their product with the sides.
- The pixels are written into a pinned buffer that becomes the image data without a copy.
- `readJpegHeader` and `decodeJpeg` run in a bound thread when the RTS is threaded (`runInBoundThread`): the error text, kept by mozjpeg per handle or per OS thread, is read in the thread of the failed call.
- Tests (`JuicyPixels-jpeg-turbo/test/JPTest.hs`), on generated JPEGs:
  - header: dimensions, subsampling and colour space of YCbCr, RGB, grayscale, YCCK, CMYK, 4:4:1 and unknown subsampling
  - 12-bit and lossless JPEGs: `Left` from both functions
  - scales: 37×23, 1001×999 and 65500×1 at the four scales; sides rounded up; a solid colour within 2 per byte
  - YCbCr, grayscale, YCCK and CMYK at full scale: within 3 per sample of JuicyPixels `decodeJpeg`
  - `encodeRGB` of a gradient at quality 50 and of a texture at qualities 20 and 85: the lengths and FNV-1a hashes of mozjpeg 4.1.5
  - truncation after 2 bytes, at the end of the SOS header, in the middle and by one byte: `Left`
  - malformed input: empty, text, PNG bytes, a JPEG without a frame, an SOF width of 0 or 65501: `Left`; width 65500 accepted
  - limits: `maxMemoryMB` 1 refuses a progressive 1000×1000 JPEG and decodes the baseline one; `maxPixels` one below the pixel count refused, equal accepted; invalid options refused
  - a progressive JPEG from `test/progressive.c` with its AC scan repeated 600 times: `Left`
- Tests (`jpeg-turbo/test/FFITest.hs`): mozjpeg's `testimgari.jpg` decoded as `testimgint.jpg` at the four scales.
- CI:
  - `.github/scripts/encode.c`: baseline and progressive JPEGs at five qualities, and their decodes at the four scales, compared byte for byte between the cabal and CMake builds
  - `armv7-check.sh`, on `ubuntu-24.04-arm`: the cabal sources cross-compiled for armv7 and run natively; Neon selected only with the `linux32` personality; JPEGs and decodes identical with Neon and with the C code paths
  - `png-check.sh`: the libspng test images decoded identically by armv7 and aarch64 builds of the `JuicyPixels-spng` C sources
- `jpeg-turbo`: `THREAD_LOCAL` empty on 32-bit Android Arm (`cbits/jconfigint.h`).
- `JuicyPixels-spng`, new package, module `Codec.Picture.Spng`:
  - `readPngHeader :: ByteString -> IO (Either String PngHeader)`
  - `decodePng :: DecodeOptions -> ByteString -> IO (Either String DynamicImage)`
  - `PngHeader`: `pngWidth`, `pngHeight`, `pngBitDepth`, `pngColorType`
  - `newtype DecodeOptions = DecodeOptions {maxMemoryMB :: Int}`, at least 1
  - limits: sides 16384 (`spng_set_image_limits`), chunks 64 MiB (`spng_set_chunk_limits`), decoded pixels `maxMemoryMB`
  - output: `ImageRGBA8` (`SPNG_FMT_RGBA8`) for a colour type with alpha or a `tRNS` chunk, otherwise `ImageRGB8` (`SPNG_FMT_RGB8`)
  - tests (`test/SpngTest.hs`): 16 PngSuite images equal to JuicyPixels' decodes and to a pinned FNV-1a hash; headers; a generated PNG; short image data, a 65537×65537 header, wrong CRCs, a missing image and truncations: `Left`; the memory limit

## JuicyPixels-stbir fork

Branch `ep/stb-submodule` at `234dde8`, on the Hackage release 0.1.0.0 (`917cd53`):
- `cbits/stb`: nothings/stb at `1ee679ca` as a submodule, `stb_image_resize.h` v0.97, replacing the bundled v0.95 copy.
- `cbits/stb_image_resize.c`: `STBIR_ASSERT` defined empty.
- `cc-options: -ffp-contract=off` (D16).
- `LICENSE` and the cabal `copyright` field: "2026 SimpleX Chat Ltd" added.
- Test suite `scale-test` (`test/ScaleTest.hs`):
  - a solid colour, RGB8 and RGBA8 with alpha 128, kept in every output pixel, for repeated halvings and for scales k/64, k in 65..127
  - a 1001×999 RGB gradient resized to 320×240 with Catmull-Rom: a pinned FNV-1a hash of the samples
- CI (`.github/workflows/build.yml`): build and test on Linux x86-64 with GHC 9.6.3 and 8.10.7, Linux ARM64, macOS and Windows with GHC 9.6.3.

## Mechanics

Core:
- `Image.hs`, new:
  - exports (`:7-16`):
    - `ImageFormat (IFJpeg, IFPng)`
    - `imageFormat`
    - `decodeResizable`
    - `resizeImage`
    - `resizeImageDataUri`
    - `scaleRGB8`
    - `scaleRGBA8`
  - `imageFormat` (`:42-53`)
  - `decodeResizable` (`:55-59`): `decodeDisplayJpeg` for input starting `FF D8`; `Spng.decodePng` with `maxDecodeMemoryMB` for input starting with the PNG signature; otherwise `Left`
  - `decodeDisplayJpeg` (`:61-65`): `readJpegHeader`, `displaySize`, `jpegScale`, `decodeJpeg`, `resizeToSize`, `orientImage` with `exifOrientation`
  - `displaySize` (`:67-73`): the target size and the number of halvings, rounded by `scaledSide`
  - `jpegScale` (`:75-80`): `Full`, `Half`, `Quarter`, `Eighth` for 0, 1, 2, and 3 or more halvings
  - `resizeToSize` (`:82-85`): `convertRGB8` and `STBIR.resize` when the decoded size differs from the target
  - `exifOrientation` (`:87-109`): the orientation value of IFD0, 1 when absent or malformed
  - `exifTiff` (`:111-119`): the TIFF data of the first APP1 `Exif` segment before the first SOS
  - `orientImage` (`:121-133`): the 8 transforms with `generateImage`, applied with `dynamicPixelMap`
  - `ImageOutput` (`:135`): `ImageBytes`, `ImageDataUri`
  - `resizeImage` (`:137-138`)
  - `resizeImageDataUri` (`:140-141`)
  - `fitImage` (`:143-162`): the size bound; the largest size from `displaySize`; the output, the quality and bpp0 by `ImageOutput`; `fitSize`; the quality bisection at the largest size below `minFill`; JPEG with `encodeRGB`
  - `fitSize` (`:164-202`): the size search of D4
  - `minFill = 0.97` (`:204-205`)
  - `imageDoesNotFit` (`:207-208`)
  - `bisect` (`:210-220`)
  - `maxImageSide = 16384` (`:222-223`)
  - `maxDisplaySide = 4320` (`:225-226`)
  - `maxJpegPixels = 2 ^ 28` (`:228-229`)
  - `maxDecodeMemoryMB` (`:231-236`), 512 or 1024 by `mobileBuild`
  - `hasSideAbove` (`:238-239`)
  - `hasSize` (`:241-242`)
  - `scaleRGB8` (`:244-249`)
  - `scaleRGBA8` (`:251-256`)
  - `catmullRom` (`:258-261`): `STBIR.resize` with Catmull-Rom and the given flags
  - `premultiply` (`:263-266`)
  - `unpremultiply` (`:268-273`)
  - `sharpen` (`:275-280`)
  - `blurLines` (`:282-299`)
  - `sharpenOffsets` (`:301-304`)
  - `sharpenWeights` (`:306-309`)
  - `sharpenRadius` (`:311-312`)
  - `sharpenAmount = 0.5` (`:314-315`)
  - `sharpenSigma = 0.6` (`:317-318`)
  - `scaledSide` (`:320-321`)
  - `toDataUri` (`:323-329`), with the approved comment at `:327`
- `Mobile/File.hs`:
  - exported at `:13-18`:
    - `cChatResizeImageUri` (`:153-154`)
    - `cChatResizeImage` (`:156-157`)
    - `cChatResizeImageFileUri` (`:159-160`)
    - `cChatResizeImageFile` (`:162-163`)
    - `cChatResizeImageFdUri` (`:165-166`)
    - `cChatResizeImageFd` (`:168-169`)
  - `readFilePath` (`:171-172`)
  - `readFd` (`:174-175`): `c_close` (`:55`) of the descriptor when `fdToHandle` raises
  - `cResizeImageUri` (`:177-178`)
  - `cResizeImageData` (`:180-187`): the format by `compare png 0`, `imageFormat` for `LT`
  - `cResizeImage` (`:189-192`):
    - the read action
    - `decodeResizable`, in place of `decodeImage`
    - `runCatchExceptT`
    - `newCStringFromLazyBS`
- `Mobile.hs:167-177`: `foreign export ccall` of the six functions.
- `Library/Commands.hs`:
  - `maxImagePreviewSize = 14000` (`:176-177`)
  - `imagePreview :: CryptoFile -> IO ImageData` (`:229-235`): `decodeResizable`, in place of `decodeImage` (`:234`)
  - `SendImage` (`:3553`): the preview from `imagePreview` (`:3560`)
- `simplex-chat.cabal`:
  - library: `if flag(swift) || os(android)`, `cpp-options: -DmobileBuild` (`:415-416`)
  - module `Simplex.Chat.Image` (`:52`)
  - library dependencies:
    - `JuicyPixels` (`:387`)
    - `JuicyPixels-jpeg-turbo` (`:388`)
    - `JuicyPixels-spng` (`:389`)
    - `JuicyPixels-stbir` (`:390`)
    - `vector` (`:409`)
  - test suite:
    - module `ImageTests` (`:726`)
    - dependencies `JuicyPixels` and `JuicyPixels-jpeg-turbo` (`:818-819`)
- `libsimplex.dll.def:30-35`, `flake.nix:487-492`: the export names.

iOS:
- `apps/ios/SimpleXChat/ImageUtils.swift`:
  - `saveImageFile` (`:75-80`)
  - `resizeImageFileToStrSize` (`:82-86`)
  - `resizeImageToDataSize` (`:101-107`)
  - `resizeImageToStrSizeSync` (`:109-115`)
  - `resizeImageWithCore` (`:121-127`), private
  - `saveWallpaperFile(image:)` (`:246`): `guard let` in place of a force-unwrap
  - `compressImageStr` and `reduceSize` removed
- `apps/ios/SimpleXChat/SimpleX.h:36-46`: the four declarations, each with its approved comment.
- `apps/ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift`:
  - `UploadContent.imageFile` (`:302`)
  - `imagePreview()` (`:315-320`)
  - `loadImageFileFromURL` (`:322-334`)
  - previews (`:675`, `:1278`)
  - sending (`:1598-1599`)
- `apps/ios/Shared/Views/Helpers/ImagePicker.swift`:
  - `loadImageData` (`:79-90`)
  - `imageFileType` (`:92-95`)
- `apps/ios/spec/architecture.md`:
  - `:101-105`: FFI list
  - `:61, 67, 69, 336`: header range `SimpleX.h#L1-L63`

Android and desktop, `apps/multiplatform/common/src/`:
- `commonMain/.../platform/Images.kt`:
  - `expect` of `resizeImageFileUri` and `resizeImageFileData` (`:34-35`)
  - `resizeImageFileToStrSize` (`:37-38`)
  - `resizeImageFileToDataSize` (`:40-41`)
  - `resizeImageToStrSize` (`:43-47`)
  - `resizeImageToDataSize` (`:49-55`)
  - the `expect` declarations of `resizeImageToStrSize`, `resizeImageToDataSize` and `compressImageStr` removed
- `androidMain/.../platform/Images.android.kt`:
  - `resizeImageFileUri` (`:112-113`)
  - `resizeImageFileData` (`:115-116`)
  - `imageFileDescriptor` (`:118-124`)
- `desktopMain/.../platform/Images.desktop.kt`:
  - `resizeImageFileUri` (`:176`)
  - `resizeImageFileData` (`:178`)
- In both platform files, the three `actual` functions of master are removed.
- `commonMain/.../platform/Core.kt`:
  - `chatResizeImageUri` (`:42`)
  - `chatResizeImage` (`:43`)
  - `chatResizeImageFileUri`, `chatResizeImageFile`, `chatResizeImageFdUri`, `chatResizeImageFd` (`:44-47`)
  - `ByteArray.toDirectBuffer` (`:56-61`)
- `commonMain/.../model/CryptoFile.kt:25`: `toDirectBuffer` is used by `writeCryptoFile`.
- `commonMain/.../views/helpers/Utils.kt`:
  - `saveImage(uri)` (`:174-200`)
  - `desktopSaveImageInTmp` (`:202-216`)
  - `resizeImageFromUri` (`:218-227`)
- `commonMain/.../views/chat/ComposeView.kt`: `processPickedMedia` (`:360`), the preview from the file (`:372`).
- `androidMain/.../views/chat/ComposeView.android.kt`: the camera preview (`:23-31`).
- `commonMain/cpp/android/simplex-api.c`: declarations (`:86-89`), JNI functions (`:282`, `:289`, `:296`, `:301`) with `decode_and_free`.
- `commonMain/cpp/desktop/simplex-api.c`: declarations (`:49-52`), JNI functions (`:283`, `:290`, `:297`, `:305`) with `decode_to_utf8_string`.
- `apps/multiplatform/spec/architecture.md`:
  - `:142-155`: JNI table, with a `chatParseBadgeCode` row, the rows of the file and descriptor functions, and the total of 25
  - `:348`: `base64ToBitmap` line
- `apps/multiplatform/spec/services/files.md:59, 81-84, 208`: `CryptoFile.kt` lines.
- `apps/multiplatform/spec/client/compose.md:318`: the image previews of `processPickedMedia`.

Node, `packages/simplex-chat-nodejs/`:
- `cpp/simplex.h:38-42`: function types, with the approved comments.
- `cpp/simplex.cc`:
  - `Library` fields (`:39-40`), resolved as optional symbols (`:217-218`)
  - helpers, with their other users:
    - `RequireExport` (`:141-146`): `ChatMigrateInitQueue` (`:573`)
    - `GetInt32Arg` (`:148-154`): `ChatMigrateInitQueue` (`:583`)
    - `BufferArg` (`:156-159`) and `GetBufferArg` (`:161-178`): `ChatWriteFile` (`:696-697`)
  - with `ResultAsyncWorker` and `KeepAlive` of the input:
    - `ChatResizeImage` (`:811-840`)
    - `ChatResizeImageUri` (`:842-869`)
  - exports (`:890-891`)
- `src/simplex.d.ts:13-14`, `src/simplex.js:3-4`: bindings.
- `src/core.ts`:
  - `chatResizeImageUri` (`:112-117`)
  - `chatResizeImage` (`:119-124`)
  - `resizeImage` (`:126-138`): the doc comment, three overloads and the implementation
  - `resizeImageData` (`:140-142`), private
  - `resizedImage` (`:144-147`), private
  - `ImageSize` (`:170-177`), with its doc comment
- `docs/`, by `npm run docs`:
  - new:
    - `core.Enumeration.ImageSize.md`
    - `core.Function.chatResizeImage.md`
    - `core.Function.chatResizeImageUri.md`
    - `core.Function.resizeImage.md`
  - `Namespace.core.md`: the lists and the module text
  - other pages: updated source line references
- Texts:
  - `README.md:137`: `- [core](./docs/Namespace.core.md): a low level API to the core library - the same that is used in desktop clients. You are unlikely to ever need to use this module directly, except for image resizing.`
  - `src/index.ts:15`: ` * You are unlikely to ever need to use this module directly, except for image resizing.`
  - `src/core.ts:127`, `resizeImage`: ` * Resize image to the app size limit: JPEG or PNG for an image, a data URI for a preview`
  - `src/core.ts:171`, `ImageSize`: ` * App image size limit`

Python, `packages/simplex-chat-python/src/simplex_chat/`:
- `_native.py`:
  - `_optional_export` (`:280-289`), also used by `migrate_init_queue` (`:292-298`)
  - `resize_image` (`:301-302`)
  - `resize_image_uri` (`:305-306`)
- `core.py`:
  - module docstring (`:1-6`):
    ```
    """Typed async wrapper around libsimplex's C ABI functions.

    Users interact with `Bot` / `ChatApi`. This module is exposed as
    `simplex_chat.core` for tests, the api.ChatApi class and image resizing
    (`resize_image`, `chat_resize_image`, `chat_resize_image_uri`).
    """
    ```
  - `ImageSize` (`:75-78`)
  - `_check_c_int` (`:99-102`), also used by `chat_migrate_init` (`:156`)
  - `chat_resize_image_uri` (`:264-273`)
  - `chat_resize_image` (`:276-280`)
  - `resize_image` (`:283-305`): three overloads and the implementation, with `match`; docstring (`:298`): `"""Resize image to the app size limit: JPEG or PNG for an image, a data URI for a preview."""`
  - `_resize_image_data` (`:308-316`)
  - `_resized_image` (`:319-322`)

Build:
- `cabal.project`, `source-repository-package`:
  - hs-jpeg-turbo (`:76-80`): evgeny-simplex/hs-jpeg-turbo at `e94e035`, subdirectories `JuicyPixels-jpeg-turbo`, `JuicyPixels-spng` and `jpeg-turbo`
  - JuicyPixels-stbir (`:82-85`): evgeny-simplex/JuicyPixels-stbir at `234dde8`
- `scripts/nix/sha256map.nix:12-13`: the hashes of both forks, computed with submodules.
- `dist-newstyle/src/hs-jpeg-t*` and `dist-newstyle/src/JuicyPixe*` in the Windows removals:
  - `.github/workflows/build.yml:1512, 1515, 1638, 1680, 1682`, with the comment `:1495-1496`
  - `scripts/desktop/build-lib-windows.sh:43`, with the comment `:41-42`
- `scripts/simplex-chat-reproduce-builds-android.sh:103`: submodules are updated after checkout.

CI, comments of the avatar scripts:
- `scripts/ci/ios-sim/avatar.swift:1`: `// Makes a group avatar:`
- `scripts/ci/desktop-linux/backend.sh:70`: `# Prints a profile image data URI: JPEG at quality 85, scaled down by the square root`

CI, Claude-driven app tests (run with `claude=true`, which needs the `app-sim-claude` environment):
- images: `tests/fixtures/test.jpg` and `tests/fixtures/logo-large-rgba.png`, added before Claude starts by the step "Add test images":

  | app | where | HEIC |
  |---|---|---|
  | Android (`android-emu.yml`) | `/sdcard/Pictures/test-card.jpg` and `simplex-logo.png`, with a media scan broadcast | |
  | desktop (`desktop-linux.yml`) | `images/` in Claude's working directory | |
  | iOS (`ios-sim.yml`) | the photo library, with `xcrun simctl addmedia` | `test.jpg` turned to portrait by `sips`, when `sips` writes HEIC |

- steps: pick the JPEG, then the PNG with transparency (and on iOS the HEIC); check the compose preview; send; open the sent image
  - Android: `scripts/ci/android/claude-test.md`, part D
  - desktop: `scripts/ci/desktop-linux/claude-test.md`, part D
  - iOS: `scripts/ci/ios-sim/device-test-plan.md`, cases I1-I4, named in `claude-scope` with `max-turns: 100`; the photo picker in `claude-prompt.md`
- `scripts/ci/desktop-linux/claude-test.sh`: `--max-turns 90`, for Android and desktop

License notices, `docs/dependencies/`:
- `README.md:3`:
  `[mozjpeg](https://github.com/mozilla/mozjpeg): JPEG decoding and encoding of resized images ([IJG and BSD-3-clause](./licences/apps/mozjpeg/LICENSE.md)). This software is based in part on the work of the Independent JPEG Group.`
- `README.md:7`:
  `[stb_image_resize](https://github.com/nothings/stb): image scaling ([MIT or public domain](./licences/apps/stb_image_resize/LICENSE))`
- `licences/apps/mozjpeg/LICENSE.md` and `README.ijg`: copies from mozjpeg `08265790`, the submodule of the hs-jpeg-turbo fork.
- `licences/apps/stb_image_resize/LICENSE`: `cbits/stb/stb_image_resize.h:2596-2632` of the JuicyPixels-stbir fork at `234dde8`.
- `licences/apps/libspng/LICENSE`: libspng's `LICENSE` (BSD-2-Clause); `licences/apps/libspng/LICENSE-libpng`: `spng/spng.c:6149-6180` (libpng-2.0), without the comment markers; from libspng `fb768002`, the submodule of the hs-jpeg-turbo fork at `e94e035`.
- `licences/apps/miniz/LICENSE`: miniz's `LICENSE` (MIT), from miniz 3.1.2 (`77d0dce8`), the submodule of the hs-jpeg-turbo fork at `e94e035`.
- `HASKELL.md`:
  - direct dependencies (`:25-28`):
    - `JuicyPixels` 3.3.8, Hackage
    - `JuicyPixels-jpeg-turbo` 0.1.0.0, hs-jpeg-turbo fork at `e94e035`
    - `JuicyPixels-spng` 0.1.0.0, hs-jpeg-turbo fork at `e94e035`, `BSD-3-Clause AND BSD-2-Clause AND libpng-2.0 AND MIT`
    - `JuicyPixels-stbir` 0.1.0.0, JuicyPixels-stbir fork at `234dde8`
  - direct dependency (`:55`): `vector` 0.13.1.0, moved from the indirect dependencies
  - indirect dependency (`:145`): `jpeg-turbo` 0.1.0.0, hs-jpeg-turbo fork at `e94e035`, `BSD-3-Clause AND IJG AND Zlib`
  - each of the five added to the dependents of the packages it depends on, among:
    - `bytestring` (`:11`)
    - `containers` (`:14`)
    - `mtl` (`:31`)
    - `vector` (`:55`)
    - `binary` (`:70`)
    - `deepseq` (`:94`)
    - `data-default-class` (`:98`)
    - `primitive` (`:138`)
    - `transformers` (`:167`)
    - `zlib` (`:180`)
- `licences/haskell/<package>-<version>/LICENSE`: the `LICENSE` of each of the five packages, from Hackage and from the pinned fork commits; the JuicyPixels-spng and JuicyPixels-stbir copies with the line `Copyright 2026 SimpleX Chat Ltd`.

## Tests

`tests/ImageTests.hs`, group "Image resize" (`tests/Test.hs:98`):
- Same aspect ratio, in every comparison below: `|w·h' − h·w'| ≤ w' + h'` (`:333-334`).
- data URI (`:32-35`), limit 14000; each result is decoded and compared by aspect ratio with the input:
  - `test.jpg` (1280×720): `data:image/jpg;base64,`
  - `logo-large-rgba.png` (1630×466, transparent pixels): `data:image/png;base64,`
  - `preview-issue1.png` (1200×600, opaque): `data:image/jpg;base64,`
- 16-bit images (`:36`), 128×128: a JPEG URI for RGB16; for RGBA16 with half opacity, a PNG URI decoded as RGBA8.
- image data (`:37-40`), limit 10000:
  - JPEG from `test.jpg`: starts `FF D8`, width below 1280, same aspect ratio
  - PNG from `logo-large-rgba.png`: RGBA8, width below 1630, same aspect ratio
  - 1×1 image with limit 10: `Left` for JPEG and PNG
- size bound (`:41`), transparent images, limit 100000, for PNG, JPEG and the data URI:
  - 16384×4 and 4×16384: `Right`
  - 16385×4 and 4×16385: `Left`
- display halving (`:42`), opaque and transparent images, `imageFormat`; the image data with limit 261120 and the data URIs with limits 12500 and 14000, each decoded:

  | input | output |
  |---|---|
  | 8640×8 | 4320×4 |
  | 8×8640 | 4×4320 |
  | 4320×4 | 4320×4 |

- fill (`:43`, `:132-141`): `test.jpg` and `preview-issue1.png`, the data URI with limit 14000 and the JPEG data with limit 20000: 97–100% of the limit
- quality at the image size (`:44`, `:143-151`): a 64×48 gradient with a limit midway between its quality-65 and quality-95 encodings: at most the limit, larger than the quality-65 encoding, 64×48
- image preview (`:45-49`):
  - `test.jpg`: JPEG data URI of at most 14000 characters
  - an encrypted copy of `test.jpg`: the same
  - `test.pdf`: `fixedImagePreview`
- JPEG display size (`:51`, `:166-180`), blank W×16 JPEGs from `encodeRGB` and the same with the sides swapped, `decodeResizable`, then the image data with limit 261120, decoded:
  - 4320, 4321, 8640, 8641: the size of the full-decode path, JuicyPixels `decodeImage` then `resizeImage`
  - expected sizes:

    | W | output |
    |---|---|
    | 4320 | 4320×16 |
    | 4321 | 2160×8 |
    | 8640 | 4320×8 |
    | 8641 | 4320×8 |
    | 17280 | 4320×4 |
    | 17281 | 4320×4 |
    | 17282 | 4320×4 |
    | 34561 | 4320×2 |
    | 65500 | 4094×1 |

- JPEG colour spaces (`:52`, `:182-190`), 64×48, `decodeResizable`:

  | input | result |
  |---|---|
  | progressive YCbCr, `encodeRGB` | `ImageRGB8` |
  | baseline YCbCr, JuicyPixels `encodeJpegAtQuality` | `ImageRGB8` |
  | grayscale, JuicyPixels `Pixel8` encoding | `ImageY8` |
  | YCCK, JuicyPixels `PixelCMYK8` encoding | `ImageCMYK8` |

- JPEG refusals (`:53`, `:192-207`), a baseline 64×48 JPEG changed as below, `decodeResizable`, `Left`:
  - precision 12 written into the SOF
  - SOF marker `FF C3` (lossless)
  - a side of 65501 written into the SOF
  - 16385×16384 written into the SOF
  - the input shortened by one byte
  - a byte inserted before the SOF marker
- EXIF orientation (`:54`, `:209-253`): a 64×32 JPEG with four solid quadrants and an APP1 Exif segment, little-endian for odd and big-endian for even values; `decodeResizable`:
  - values 1–8: the sides swapped for 5–8; the quadrant colours placed by the transform; the resized JPEG without `Exif`
  - an IFD0 offset beyond the segment: as value 1
- PNG decoding (`:55-57`), `decodeResizable`:
  - `logo-large-rgba.png` (`:255-265`): `ImageRGBA8` with the samples of JuicyPixels `decodeImage`, and `imageFormat` PNG; `preview-issue1.png`: `ImageRGB8` with the samples of JuicyPixels `decodeImage`
  - refusals (`:267-298`):

    | input | `Left` |
    |---|---|
    | `pngShortImageData`: the 2048×1 grayscale PNG from JuicyPixels `encodePng` with its header set to 2048×2048 | `IDAT stream too short` |
    | `pngSideAbove16384`: the 1×1 grayscale PNG with its header set to 65537×65537 | `image width exceeds user limit` |
    | a 16×16 GIF and a 16×16 BMP from JuicyPixels | `unsupported image format` |

- scaling (`:58-61`):
  - research code (`:300-306`): `scaleRGB8` of `preview-issue1.png` to 400×200 equals `preview-issue1-400x200-sharpened.png` pixel for pixel; the fixture is the output of the research code (`pvsf`, resize `catmullrom/g/0.5@0.6/single`) for the same decoded pixels
  - the same size (`:308-313`): `scaleRGB8` and `scaleRGBA8` of 64×48 images to 64×48 return the input samples
  - transparency (`:315-325`): a 64×64 opaque red disc on transparent pixels alternately green and blue, `scaleRGBA8` to 40×40: every pixel with alpha above 0 is red; some pixels have alpha below 255

`tests/MobileTests.hs`, "Images" (`:99-106`), with buffers from `mallocBytes`:
- `cChatResizeImageUri` on `test.jpg`, limit 14000 (`:364-371`): `data:image/jpg;base64,`, at most 14000 characters; on `"not an image"`: `""`.
- `cChatResizeImage` on `test.jpg`, limit 100000 (`:373-381`): base64 decoded to at most 100000 bytes, starting `FF D8` for `png = 0` and with the PNG signature for `png = 1`.
- `cChatResizeImage` with `png = -1`, limit 100000 (`:383-391`): `FF D8` for `test.jpg`; the PNG signature for `logo-large-rgba.png`.
- `cChatResizeImage` with `png = -1`, limit 261120, on a blank 20000×16 JPEG from `encodeRGB` (`:393-397`): `FF D8`.
- file path (`:399-406`):
  - `cChatResizeImageFileUri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters
  - `cChatResizeImageFile` with `png = -1`, limit 100000, on `logo-large-rgba.png`: the PNG signature
  - a missing file: `""`
- file descriptor (`:408-428`), opened with `GHC.IO.FD.openFile`; closed afterwards: `fdStat` fails or names another file
  - `cChatResizeImageFdUri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters; closed afterwards
  - `cChatResizeImageFd` on `test.pdf`: `""`; closed afterwards
  - `cChatResizeImageFdUri` on the `tests/fixtures` directory, opened with `c_open`: `""`; closed afterwards
  - descriptors 1000000 and −1, not open: `""`
- malformed PNG (`:430-438`): `pngShortImageData` and `pngSideAbove16384` from `ImageTests`: `""` from `cChatResizeImageUri` with limit 14000 and from `cChatResizeImage` with `png = -1` and limit 261120

`tests/ChatTests/Files.hs`, "send image with generated preview" (`:47`, `:179-192`):
- `/img @bob ./tests/fixtures/test.jpg`: the received item has a JPEG data URI preview of at most 14000 characters, read with `APIGetChat` (`lastImagePreview`, `:194-200`).

`packages/simplex-chat-nodejs/tests/core.test.ts`, "image resizing" (`:149-239`), with a generated noise PNG:
- data URI, limit 14000: `data:image/jpg;base64,`, at most 14000 characters
- a `Uint8Array` view at offset 3, limit 20000: JPEG starting `FF D8 FF`; PNG from an `ArrayBuffer` with the PNG signature; both at most 20000 bytes
- `resizeImage` with each `ImageSize`:

  | input | `Image`, at most 261120 bytes | `ProfilePreview`, at most 12500 characters | `MessagePreview`, at most 14000 characters |
  |---|---|---|---|
  | 512×512, opaque (`:201-214`) | JPEG starting `FF D8 FF` | `data:image/jpg;base64,` | `data:image/jpg;base64,` |
  | 256×256, random alpha (`:216-229`) | PNG signature | `data:image/png;base64,` | `data:image/png;base64,` |

- `"not an image"`: the resize error
- `maxSize` 2^31: `Expected 32-bit integer maxSize`
- With a libsimplex lacking the exports, the export error is expected in each test (`:155-158`).

`packages/simplex-chat-nodejs/tests/loader.test.ts:39-42`: `chat_resize_image` and `chat_resize_image_uri` are added to the add-on exports called before `load`; from each, `libsimplex is not loaded, call core.loadLibrary(backend) first` is expected (`RequireLoaded`, `cpp/simplex.cc:135`).

`packages/simplex-chat-python/tests/test_core_resize_image.py`, with a fake library:
- `chat_resize_image_uri`: the export is called with `(data, len(data), max_size)`; the data URI is returned (`:52`)
- `chat_resize_image`: `png` is 0 for `"jpeg"` and 1 for `"png"`; the result is base64-decoded (`:59`)
- `resize_image` with `ImageSize.IMAGE`: `chat_resize_image` is called with limit 261120 and `png = -1`; the result is base64-decoded (`:65`)
- `resize_image` with `PROFILE_PREVIEW` and `MESSAGE_PREVIEW`: `chat_resize_image_uri` is called with limits 12500 and 14000; the data URI is returned (`:75`)
- errors:

  | condition | raised | raised by |
  |---|---|---|
  | `""` from the export (`:89`) | `RuntimeError` | both functions, `resize_image` with `IMAGE` |
  | absent export (`:107`) | `RuntimeError` with the export text | both functions, `resize_image` with `IMAGE` and `PROFILE_PREVIEW` |
  | `max_size` 2^31 or −2^31 − 1 (`:115`) | `ValueError`, before the call | both functions |
