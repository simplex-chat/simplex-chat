# Image resizing in the core

The CLI sends a fixed placeholder as the preview of every image sent with `/img` or `/imgf`, and bots on the Node and Python libraries need their own image resizing code.
Images are resized by the core: halved until the encoding fits the size limit; then the JPEG quality is bisected, and a halved PNG is rescaled by bisection between the last two sizes.
JPEG input is decoded by libjpeg-turbo at the smallest scale that covers the display size, and its EXIF orientation is applied.
The core functions are called by the apps in place of platform code, wrapped in both bot libraries and used for `/img` and `/imgf` previews; libjpeg-turbo is compiled by cabal in every build.

References are to branch `ab/resize-image` at `d27d70438`, based on master `66e381bf5`. Haskell paths without a directory are in `src/Simplex/Chat/`.

## Decisions

Decoding, `Image.hs`:
- `decodeResizable bytes`: the image for resizing; at the display size for JPEG, at full size for other formats.
- JPEG, input starting `FF D8`, decoded by libjpeg-turbo:
  1. `readJpegHeader`; `Left` for:
     - a side above 65500
     - a precision other than 8
     - a lossless JPEG
  2. target size: both sides halved, each rounded as `halve` rounds (`:97-105`), while a side exceeds 4320; `Left` when a side is below 2 before a halving
  3. scale: the smallest of `Full`, `Half`, `Quarter` and `Eighth` whose rounded-up sides are at least the target
  4. `decodeJpeg` with:
     - `maxPixels`: 2^28; `Left` above it, before the output is allocated
     - `maxMemoryMB`: 512 on iOS and Android, 2047 in other builds
  5. a decode larger than the target: `convertRGB8`, then `STBIR.resize` to the target
  6. the EXIF orientation applied
- Other formats: JuicyPixels `decodeImage`.
- Constructors of a decoded JPEG:

  | JPEG colour space | constructor |
  |---|---|
  | YCbCr, RGB | `ImageRGB8` |
  | grayscale | `ImageY8` |
  | CMYK, YCCK | `ImageCMYK8` |

- `Left` from libjpeg-turbo for:
  - any warning, including truncated data and extraneous bytes
  - more than 500 scans
  - whole-image coefficient buffers above `maxMemoryMB`
- `maxMemoryMB`, a compile-time constant, 512 when `mobileBuild` is defined and 2047 otherwise:

  | build | `simplex-chat.cabal` condition | set in |
  |---|---|---|
  | iOS and iOS simulator | `flag(swift)` | `flake.nix:551`, `:588`; `scripts/ci/ios-sim/build-core.sh:27` |
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
- Size search:

  | step | JPEG | PNG |
  |---|---|---|
  | size bound | the JPEG header limits: `Left`, before decoding; other formats: a side above 16384 pixels after decoding: `Left`, before any encode or resize | the same |
  | display halving | while a side exceeds 4320 pixels | the same |
  | halving | while the image at quality 20 exceeds the limit | while the image exceeds the limit |
  | final | quality bisected in 20..94 at the last size | the image itself when it fits after the display halving; otherwise the sides of the size before it are divided by a divisor bisected between 2 and 1 while the interval is at least 1/64: at most 7 encodings |
  | failure | a side below 2 pixels before the image fits: `Left` | the same |

- The display halving applies to the image data and to the data URIs: the Android and desktop apps display images and previews only up to 4320 pixels per side (`CIImageView.kt:128`, `base64ToBitmap` in `Images.android.kt:34` and `Images.desktop.kt:42`).
- The bisection is ended by an encoding of exactly `maxSize`.
- Metadata of the input is dropped.

C API:
```
chat_resize_image_uri(data, len, maxSize)     data URI of at most maxSize characters, or ""
chat_resize_image(data, len, maxSize, png)    base64 of at most maxSize bytes of PNG (png > 0), JPEG (png == 0) or the imageFormat choice (png < 0), or ""
```
- `data`: an encoded image, JPEG or a format decoded by JuicyPixels `decodeImage`, read in place during the call.
- `""` is returned when:
  - the input is undecodable, or a JPEG is refused by the header limits or by libjpeg-turbo;
  - the image exceeds the limit at every size;
  - an exception is raised.
- The result is allocated with `malloc` and freed by the caller.
- A chat controller is not required.
- `png`: 0 or 1 from the apps and the explicit library functions; −1 from the library functions with `ImageSize`.

Apps, with function signatures kept and call sites unchanged except iOS `saveWallpaperFile(image:)` (`ImageUtils.swift:233`):
- iOS `resizeImageToDataSize` and `resizeImageToStrSizeSync`:
  - an image with orientation other than `.up` is redrawn upright;
  - input: UIKit PNG for an image with alpha, otherwise JPEG at quality 0.85;
  - alpha:
    - `resizeImageToDataSize`: the `hasAlpha` parameter, by which PNG or JPEG output is also selected
    - `resizeImageToStrSizeSync`: `imageHasAlpha(image)`, for the input only
  - `""` is returned as `nil`; for it `nil` is also returned by `saveImage` (`ImageUtils.swift:68`) and `saveWallpaperFile(image:)` (`:233`).
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
  - The core and the add-on are pinned together by `prepare` (`packages_release.py:350-353`); for nodejs an error is raised by `prepare` until the add-on release of the current sources is published (`:768-769`).

`/img` and `/imgf`:
- The preview is generated from the file: read with `CF.readFile` (encrypted files included), decoded, and resized to a data URI of at most 14000 characters.
- On any failure the fixed placeholder is sent, as before.

Native code:
- libjpeg-turbo 3.1.4.1, commit `9217719d`, is the submodule `jpeg-turbo/cbits/libjpeg-turbo` of the hs-jpeg-turbo fork; cabal compiles it in the `jpeg-turbo` package of every build:
  - 113 C sources on every target, and 15 Neon sources on aarch64
  - configuration headers `jconfig.h`, `jconfigint.h`, `jversion.h` and `neon-compat.h` in `cbits`, with the target-dependent values taken from compiler macros
  - the C code paths on x86-64, Neon on aarch64
  - built with cabal and the C compiler only
- JPEG input is decoded, and JPEG output encoded, by libjpeg-turbo.
- Package sources:

  | package | during review | before the final pull request |
  |---|---|---|
  | hs-jpeg-turbo (`jpeg-turbo`, `JuicyPixels-jpeg-turbo`) | evgeny-simplex/hs-jpeg-turbo, branch `ep/jpeg-decode` (PR #3, on `ep/bundled-libjpeg-turbo`, PR #2) | a simplex-chat/hs-jpeg-turbo commit |
  | JuicyPixels-stbir, with nothings/stb as a submodule | evgeny-simplex/JuicyPixels-stbir, branch `ep/stb-submodule` | a simplex-chat/JuicyPixels-stbir commit |
  | JuicyPixels 3.3.8 | Hackage | Hackage |

- The hashes of both forks in `scripts/nix/sha256map.nix` are computed with submodules, as for simplexmq.
- In Windows builds, `dist-newstyle/src/hs-jpeg-t*` and `dist-newstyle/src/JuicyPixe*` are removed beside `direct-sq*` and `simplexmq*`.

License notices, in `docs/dependencies`:
- `README.md`: libjpeg-turbo, with the IJG sentence, and stb_image_resize
- `HASKELL.md`: JuicyPixels, JuicyPixels-jpeg-turbo and JuicyPixels-stbir as direct dependencies, jpeg-turbo as an indirect one; the four are added to the "Also depended upon by" and "Depended upon by" cells of the packages they depend on
- `licences/`: the license files of each, copied from the pinned commits

Out of scope: WebP, a separate PR after this one.

## hs-jpeg-turbo fork

PR #2, branch `ep/bundled-libjpeg-turbo`: libjpeg-turbo compiled by cabal, as in "Native code".

PR #3, branch `ep/jpeg-decode` at `9e5cb68`, on PR #2:
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
  - any libjpeg-turbo error or warning, including an empty input
  - a side below 1
  - a precision other than 8
  - a lossless JPEG
  - more than `maxPixels` pixels, checked before the output buffer is allocated
- Decoding parameters:
  - stop on the first warning (`TJPARAM_STOPONWARNING`, `TJFLAG_STOPONWARNING`)
  - at most 500 scans (`TJFLAG_LIMITSCANS`)
  - `TJPARAM_MAXMEMORY`: `maxMemoryMB`
  - `TJPARAM_SAVEMARKERS`: 0
- Output:

  | JPEG colour space | pixel format | constructor |
  |---|---|---|
  | YCbCr, RGB | `TJPF_RGB` | `ImageRGB8` |
  | grayscale | `TJPF_GRAY` | `ImageY8` |
  | CMYK | `TJPF_CMYK` | `ImageCMYK8` |
  | YCCK | `TJPF_CMYK`, samples inverted | `ImageCMYK8` |

- Sides: the JPEG sides divided by 1, 2, 4 or 8, rounded up.
- The pixels are written into a pinned buffer that becomes the image data without a copy.
- The error text of libjpeg-turbo is kept per OS thread; it is used as is.
- Tests (`JuicyPixels-jpeg-turbo/test/JPTest.hs`), on generated JPEGs:
  - header: dimensions, subsampling and colour space of YCbCr, RGB, grayscale, YCCK, CMYK, 4:4:1 and unknown subsampling
  - 12-bit and lossless JPEGs: `Left` from both functions
  - scales: 37×23, 1001×999 and 65500×1 at the four scales; sides rounded up; a solid colour within 2 per byte
  - YCbCr, grayscale, YCCK and CMYK at full scale: within 3 per sample of JuicyPixels `decodeJpeg`
  - arithmetic coding, sequential and progressive: decoded as the Huffman-coded JPEG at the four scales
  - truncation after 2 bytes, at the end of the SOS header, in the middle and by one byte: `Left`
  - malformed input: empty, text, PNG bytes, a JPEG without a frame, an SOF width of 0 or 65501: `Left`; width 65500 accepted
  - limits: `maxMemoryMB` 1 refuses a progressive 1000×1000 JPEG and decodes the baseline one; `maxPixels` one below the pixel count refused, equal accepted; invalid options refused
  - a progressive JPEG with 704 scans (`test/many-scans.c`): `Left`
- CI (`.github/scripts/encode.c`): baseline and progressive JPEGs at five qualities, and their decodes at the four scales, compared byte for byte between the cabal and CMake builds.

## JuicyPixels-stbir fork

PR #1, branch `ep/stb-submodule`, on the Hackage release 0.1.0.0 (`917cd53`):
- `cbits/stb`: nothings/stb at `1ee679ca` as a submodule, `stb_image_resize.h` v0.97, replacing the bundled v0.95 copy.
- `cbits/stb_image_resize.c`: `STBIR_ASSERT` defined empty.
- `LICENSE` and the cabal `copyright` field: "2026 SimpleX Chat Ltd" added.
- Test suite `scale-test` (`test/ScaleTest.hs`): output sides equal the input sides divided by the scale and rounded, for halvings and for scales k/64, k in 65..127, on RGB8 and RGBA8 images.
- CI (`.github/workflows/build.yml`): build and test on Linux with GHC 9.6.3 and 8.10.7, macOS and Windows with GHC 9.6.3.

## Mechanics

Core:
- `Image.hs`, new:
  - exports (`:5-11`):
    - `ImageFormat (IFJpeg, IFPng)`
    - `imageFormat`
    - `decodeResizable`
    - `resizeImage`
    - `resizeImageDataUri`
  - `decodeResizable`: `decodeDisplayJpeg` for input starting `FF D8`, otherwise `decodeImage`
  - `decodeDisplayJpeg`: `readJpegHeader`, `displaySize`, `jpegScale`, `decodeJpeg`, the resize to the display size, `orientImage` with `exifOrientation`
  - `displaySize`: the target size and the number of halvings, with the rounding of `downscale`, shared through `scaledSide`
  - `jpegScale`: `Full`, `Half`, `Quarter`, `Eighth` for 0, 1, 2, and 3 or more halvings
  - `exifOrientation`: the orientation value from the JPEG markers before the first SOS, 1 when absent or malformed
  - `orientImage`: the 8 transforms with `generateImage`, applied with `dynamicPixelMap`
  - `maxJpegPixels = 2 ^ 28`; `maxJpegMemoryMB`, 512 or 2047 by `mobileBuild`
  - `fitImage` (`:44-66`): the size bound, the display halving (`fitDisplay`, `:55`), then the search, parameterized by the output (bytes or data URI); JPEG with `encodeRGB`
  - `bisect` (`:68-78`)
  - `minJpegQuality = 20` (`:80-81`)
  - `maxImageSide = 16384` (`:83-84`)
  - `maxDisplaySide = 4320` (`:86-87`)
  - `hasSideAbove` (`:89-90`)
  - `halveToSide` (`:92-95`)
  - `halve` (`:97-100`)
  - `downscale` (`:102-105`): `STBIR.resize` with `defaultOptions`
  - `toDataUri` (`:107-113`), with the approved comment at `:111`
- `Mobile/File.hs`:
  - exported at `:13-14`:
    - `cChatResizeImageUri` (`:147-148`)
    - `cChatResizeImage` (`:150-157`): the format by `compare png 0`, `imageFormat` for `LT`
  - `cResizeImage` (`:159-163`):
    - `getByteString`
    - `decodeResizable`, in place of `decodeImage` (`:162`)
    - `runCatchExceptT`
    - `newCStringFromLazyBS`
- `Mobile.hs:167, 169`: `foreign export ccall` of both functions.
- `Library/Commands.hs`:
  - `maxImagePreviewSize = 14000` (`:176-177`)
  - `imagePreview :: CryptoFile -> IO ImageData` (`:229-235`): `decodeResizable`, in place of `decodeImage` (`:234`)
  - `SendImage` (`:3558`): the preview from `imagePreview`
- `simplex-chat.cabal`:
  - library: `if flag(swift) || os(android)`, `cpp-options: -DmobileBuild`
  - module `Simplex.Chat.Image` (`:52`)
  - library dependencies (`:385-387`):
    - `JuicyPixels`
    - `JuicyPixels-jpeg-turbo`
    - `JuicyPixels-stbir`
  - test suite:
    - module `ImageTests` (`:720`)
    - dependency `JuicyPixels` (`:812`)
- `libsimplex.dll.def:30-31`, `flake.nix:487-488`: the export names.

iOS:
- `apps/ios/SimpleXChat/ImageUtils.swift`:
  - `resizeImageToDataSize` (`:88-94`)
  - `resizeImageToStrSizeSync` (`:96-102`)
  - `resizeImageWithCore` (`:108-114`), private
  - `saveWallpaperFile(image:)` (`:233`): `guard let` in place of a force-unwrap
  - `compressImageStr` and `reduceSize` removed
- `apps/ios/SimpleXChat/SimpleX.h:36-40`: both declarations, with the approved comments.
- `apps/ios/spec/architecture.md`:
  - `:101-103`: FFI list
  - `:61, 67, 69, 334`: header range `SimpleX.h#L1-L55`

Android and desktop, `apps/multiplatform/common/src/`:
- `commonMain/.../platform/Images.kt`:
  - `resizeImageToStrSize` (`:34-38`)
  - `resizeImageToDataSize` (`:40-46`)
  - the `expect` declarations of both and of `compressImageStr` removed
- `androidMain/.../platform/Images.android.kt`, `desktopMain/.../platform/Images.desktop.kt`: the three `actual` functions removed.
- `commonMain/.../platform/Core.kt`:
  - `chatResizeImageUri` (`:42`)
  - `chatResizeImage` (`:43`)
  - `ByteArray.toDirectBuffer` (`:52-57`)
- `commonMain/.../model/CryptoFile.kt:25`: `toDirectBuffer` is used by `writeCryptoFile`.
- `commonMain/cpp/android/simplex-api.c`: declarations (`:86-87`), JNI functions (`:280`, `:287`) with `decode_and_free`.
- `commonMain/cpp/desktop/simplex-api.c`: declarations (`:49-50`), JNI functions (`:281`, `:288`) with `decode_to_utf8_string`.
- `apps/multiplatform/spec/architecture.md`:
  - `:142-151`: JNI table, with a `chatParseBadgeCode` row, rows renumbered and the total of 21
  - `:344`: `base64ToBitmap` line
- `apps/multiplatform/spec/services/files.md:59, 81-84, 208`: `CryptoFile.kt` lines.

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
  - hs-jpeg-turbo (`:76-80`): evgeny-simplex/hs-jpeg-turbo at `9e5cb68`, subdirectories `JuicyPixels-jpeg-turbo` and `jpeg-turbo`
  - JuicyPixels-stbir (`:82-85`): evgeny-simplex/JuicyPixels-stbir at `6cf27aad`
- `scripts/nix/sha256map.nix:12-13`: the hashes of both forks, computed with submodules.
- `dist-newstyle/src/hs-jpeg-t*` and `dist-newstyle/src/JuicyPixe*` in the Windows removals:
  - `.github/workflows/build.yml:1439, 1442, 1565, 1607, 1609`, with the comment `:1422-1423`
  - `scripts/desktop/build-lib-windows.sh:43`, with the comment `:41-42`
- `scripts/simplex-chat-reproduce-builds-android.sh:103`: submodules are updated after checkout.

CI, comments of the avatar scripts:
- `scripts/ci/ios-sim/avatar.swift:1`: `// Makes a group avatar:`
- `scripts/ci/desktop-linux/backend.sh:70`: `# Prints a profile image data URI: JPEG at quality 85, scaled down by the square root`

License notices, `docs/dependencies/`:
- `README.md:3`:
  `[libjpeg-turbo](https://github.com/libjpeg-turbo/libjpeg-turbo): JPEG encoding of resized images ([IJG and BSD-3-clause](./licences/apps/libjpeg-turbo/LICENSE.md)). This software is based in part on the work of the Independent JPEG Group.`
- `README.md:7`:
  `[stb_image_resize](https://github.com/nothings/stb): image scaling ([MIT or public domain](./licences/apps/stb_image_resize/LICENSE))`
- `licences/apps/libjpeg-turbo/LICENSE.md` and `README.ijg`: copies from libjpeg-turbo `9217719d`, the submodule of the hs-jpeg-turbo fork.
- `licences/apps/stb_image_resize/LICENSE`: `cbits/stb/stb_image_resize.h:2596-2632` of the JuicyPixels-stbir fork at `6cf27aad`.
- `HASKELL.md`:
  - direct dependencies (`:25-27`):
    - `JuicyPixels` 3.3.8, Hackage
    - `JuicyPixels-jpeg-turbo` 0.1.0.0, hs-jpeg-turbo fork at `9e5cb68`
    - `JuicyPixels-stbir` 0.1.0.0, JuicyPixels-stbir fork at `6cf27aad`
  - indirect dependency (`:143`): `jpeg-turbo` 0.1.0.0, hs-jpeg-turbo fork at `9e5cb68`, `BSD-3-Clause AND IJG AND Zlib`
  - the four added to the dependents of:
    - `bytestring` (`:11`)
    - `containers` (`:14`)
    - `mtl` (`:30`)
    - `binary` (`:68`)
    - `deepseq` (`:92`)
    - `data-default-class` (`:96`)
    - `primitive` (`:136`)
    - `transformers` (`:165`)
    - `vector` (`:174`)
    - `zlib` (`:179`)
- `licences/haskell/<package>-<version>/LICENSE`: the `LICENSE` of each of the four packages, from Hackage and from the pinned fork commits; the JuicyPixels-stbir copy with the line `Copyright 2026 SimpleX Chat Ltd`.

## Tests

`tests/ImageTests.hs`, group "Image resize" (`tests/Test.hs:98`):
- Same aspect ratio, in every comparison below: `|w·h' − h·w'| ≤ w' + h'` (`:128-129`).
- data URI (`:26-29`), limit 14000; each result is decoded and compared by aspect ratio with the input:
  - `test.jpg` (1280×720): `data:image/jpg;base64,`
  - `logo-large-rgba.png` (1630×466, transparent pixels): `data:image/png;base64,`
  - `preview-issue1.png` (1200×600, opaque): `data:image/jpg;base64,`
- 16-bit images (`:30`), 128×128: a JPEG URI for RGB16; for RGBA16 with half opacity, a PNG URI decoded as RGBA8.
- image data (`:31-34`), limit 10000:
  - JPEG from `test.jpg`: starts `FF D8`, width below 1280, same aspect ratio
  - PNG from `logo-large-rgba.png`: RGBA8, width below 1630, same aspect ratio
  - 1×1 image with limit 10: `Left` for JPEG and PNG
- size bound (`:35`), transparent images, limit 100000, for PNG, JPEG and the data URI:
  - 16384×4 and 4×16384: `Right`
  - 16385×4 and 4×16385: `Left`
- display halving (`:36`), opaque and transparent images, `imageFormat`; the image data with limit 261120 and the data URIs with limits 12500 and 14000, each decoded:

  | input | output |
  |---|---|
  | 8640×8 | 4320×4 |
  | 8×8640 | 4×4320 |
  | 4320×4 | 4320×4 |

- image preview (`:37-41`):
  - `test.jpg`: JPEG data URI of at most 14000 characters
  - an encrypted copy of `test.jpg`: the same
  - `test.pdf`: `fixedImagePreview`
- JPEG display size, blank W×16 JPEGs from `encodeRGB` and the same with the sides swapped, `decodeResizable`, then the image data with limit 261120, decoded:
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

- JPEG formats, 64×48, `decodeResizable`:

  | input | result |
  |---|---|
  | baseline YCbCr, JuicyPixels `encodeJpegAtQuality` | `ImageRGB8` |
  | progressive, `encodeRGB` | `ImageRGB8` |
  | grayscale, JuicyPixels `Pixel8` encoding | `ImageY8` |
  | YCCK, JuicyPixels `PixelCMYK8` encoding | `ImageCMYK8` |
  | precision 12, written into the SOF | `Left` |
  | lossless, SOF marker `FF C3` | `Left` |

- JPEG refusals, `decodeResizable`, `Left`:
  - a side of 65501 written into the SOF
  - 16385×16384 written into the SOF
  - the input shortened by one byte
  - a byte inserted before a marker
- EXIF orientation: a 64×32 JPEG with four solid quadrants and an APP1 Exif segment, little-endian for odd and big-endian for even values; `decodeResizable`:
  - values 1–8: the sides swapped for 5–8; the quadrant colours placed by the transform
  - an IFD0 offset beyond the segment: as value 1

`tests/MobileTests.hs`, "Images" (`:93-96`), with buffers from `mallocBytes`:
- `cChatResizeImageUri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters; on `"not an image"`: `""`.
- `cChatResizeImage` on `test.jpg`, limit 100000: base64 decoded to at most 100000 bytes, starting `FF D8` for `png = 0` and with the PNG signature for `png = 1`.
- `cChatResizeImage` with `png = -1`, limit 100000 (`:373-381`): `FF D8` for `test.jpg`; the PNG signature for `logo-large-rgba.png`.
- `cChatResizeImage` with `png = -1`, limit 261120, on a blank 20000×16 JPEG: `FF D8`.

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

`packages/simplex-chat-nodejs/tests/loader.test.ts:35`: `chat_resize_image` and `chat_resize_image_uri` are added to the add-on exports called before `load`; from each, `libsimplex is not loaded, call core.loadLibrary(backend) first` is expected (`RequireLoaded`, `:42`).

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
