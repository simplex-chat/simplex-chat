# Image resizing in the core

The CLI sends a fixed placeholder as the preview of every image sent with `/img` or `/imgf`, and bots on the Node and Python libraries need their own image resizing code.
Images are resized by the core: halved until the encoding fits the size limit; then the JPEG quality is bisected, and a halved PNG is rescaled by bisection between the last two sizes.
The core functions are called by the apps in place of platform code, wrapped in both bot libraries and used for `/img` and `/imgf` previews; libjpeg-turbo is added as a submodule and linked statically on every target.

References are to branch `ab/resize-image` at `62778b300`, based on master `66e381bf5`. Haskell paths without a directory are in `src/Simplex/Chat/`.

## Decisions

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
  | halving | while the image at quality 20 exceeds the limit | while the image exceeds the limit |
  | final | quality bisected in 20..94 at the last size | the image itself when it fits at full size; otherwise the sides of the size before it are divided by a divisor bisected between 2 and 1 while the interval is at least 1/64: at most 7 encodings |
  | failure | a side below 2 pixels before the image fits: `Left` | the same |

- The bisection is ended by an encoding of exactly `maxSize`.
- EXIF orientation of the input is ignored, and EXIF metadata is dropped.

C API:
```
chat_resize_image_uri(data, len, maxSize)     data URI of at most maxSize characters, or ""
chat_resize_image(data, len, maxSize, png)    base64 of at most maxSize PNG (png != 0) or JPEG bytes, or ""
```
- `data`: an encoded image of a format decoded by JuicyPixels `decodeImage`, read in place during the call.
- `""` is returned when:
  - the input is undecodable;
  - the image exceeds the limit at every size;
  - an exception is raised.
- The result is allocated with `malloc` and freed by the caller.
- A chat controller is not required.

Apps, with function signatures and call sites unchanged:
- iOS `resizeImageToDataSize` and `resizeImageToStrSizeSync`:
  - an image with orientation other than `.up` is redrawn upright;
  - input: UIKit PNG for an image with alpha, otherwise JPEG at quality 0.85;
  - alpha:
    - `resizeImageToDataSize`: the `hasAlpha` parameter, which also selects PNG or JPEG output
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
  - `chatResizeImageUri(image: ArrayBuffer | Uint8Array, maxSize: number): Promise<string>`
  - `chatResizeImage(image: ArrayBuffer | Uint8Array, maxSize: number, format: "jpeg" | "png"): Promise<Buffer>`
- Python, `simplex_chat.core`:
  - `async def chat_resize_image_uri(data: bytes, max_size: int) -> str`
  - `async def chat_resize_image(data: bytes, max_size: int, image_format: Literal["jpeg", "png"]) -> bytes`
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
- libjpeg-turbo 3.1.4.1, commit `9217719d`, is the submodule `external/libjpeg-turbo`.
- It is built as a static library: by `scripts/desktop/prepare-libjpeg-turbo.sh` with PIC and `BUILD=20260327`, and by nix.
- x86-64 SIMD code is built when NASM is installed.
- libturbojpeg is linked statically on every target.
- hs-jpeg-turbo: gitlab.com/dpwiz/hs-jpeg-turbo at `f1bc073`, packages `jpeg-turbo` and `JuicyPixels-jpeg-turbo`.
- JuicyPixels 3.3.8 and JuicyPixels-stbir 0.1.0.0 are taken from Hackage.
- Planned, in later commits of this branch:
  - JuicyPixels-stbir pinned to a fork with nothings/stb as a submodule;
  - hs-jpeg-turbo pinned to github.com/simplex-chat/hs-jpeg-turbo at a commit with the git LFS fixtures replaced.

`scripts/desktop/prepare-libjpeg-turbo.sh`:
- prefix: `LIBJPEG_TURBO_PREFIX`, default `~/.cache/libjpeg-turbo-3.1.4.1`
- skipped when `<prefix>/lib/libturbojpeg.a` exists
- stdout: the prefix; with `cygpath` (Windows), in `cygpath -m` form
- with `cygpath` (Windows): `CC` is `cygpath -m` of GHC's clang, `$(ghc --print-libdir)/../mingw/bin/clang.exe`; `LDFLAGS` is `-fuse-ld=lld --rtlib=compiler-rt`

libjpeg-turbo per build:

| build | libjpeg-turbo | jpeg-turbo flags and paths |
|---|---|---|
| Linux, `build.yml` | `Dockerfile.build` image, prefix `/usr/local` | `+pkgconfig`, default pkg-config path |
| macOS, `build.yml` | `brew install nasm`, the script | `+pkgconfig`, `PKG_CONFIG_PATH` in `GITHUB_ENV` |
| Windows, `build.yml` and `build-lib-windows.sh` | the script | `-pkgconfig +static`, include and lib directories in `cabal.project.local` |
| `build-lib-linux.sh` (also the `desktop-linux.yml` core), `build-lib-mac.sh` | the script | `+pkgconfig`, `PKG_CONFIG_PATH` |
| `desktop-linux.yml`, `ios-sim.yml` and `android-emu.yml` backends | the script, default prefix | `+pkgconfig`, `PKG_CONFIG_PATH` |
| `ios-sim.yml` core (`scripts/ci/ios-sim/build-core.sh`) | the script, default prefix; `libturbojpeg.a` copied into the package | `+pkgconfig`, `PKG_CONFIG_PATH` |
| `Dockerfile` | the submodule initialized, the script, prefix `/usr/local` | `+pkgconfig`, default pkg-config path |
| support-bot-light and badge-service Dockerfiles | the script, prefix `/usr/local` | `+pkgconfig`, default pkg-config path |
| calculator-bot Dockerfile | `build-lib-linux.sh` | as in `build-lib-linux.sh` |
| nix, every target | `mkLibjpegTurbo`: nixpkgs `libjpeg_turbo`, static, source at the submodule commit | `-pkgconfig +static`, `libs` set to `mkLibjpegTurbo` |

CI:
- `submodules: recursive` is set on the checkouts of jobs that run the script or hash the submodule.
- The hash of the submodule files is part of the `build.yml` dependency and build-directory cache keys and of the artifact names of cores and backends built with cabal.
- `external/libjpeg-turbo` is added to:
  - the pull-request paths of `build.yml`
  - `CORE_SOURCES` of `packages_release.py`
  - the `"haskell"` list of `BUILD_CHANGES` in `packages_release.py`

Out of scope: `scripts/relay/Dockerfile`; the libjpeg-turbo steps are added to it when its `CHAT_REF` (`:3`) is set to a commit with this change.

## Mechanics

Core:
- `Image.hs`, new:
  - exports (`:5-11`):
    - `ImageFormat (IFJpeg, IFPng)`
    - `imageFormat`
    - `resizeImage`
    - `resizeImageDataUri`
  - `fitImage` (`:44-63`): the search, parameterized by the output (bytes or data URI)
  - `bisect` (`:65-75`)
  - `minJpegQuality = 20` (`:77-78`)
  - `halve` (`:80-83`), `downscale` (`:85-88`): `STBIR.resize` with `defaultOptions`
  - `toDataUri` (`:90-96`), with the approved comment at `:94`
- `Mobile/File.hs`:
  - `cChatResizeImageUri` (`:147-148`), `cChatResizeImage` (`:150-153`), exported at `:13-14`
  - `cResizeImage` (`:155-159`): `getByteString`, `decodeImage`, `runCatchExceptT`, `newCStringFromLazyBS`
- `Mobile.hs:167, 169`: `foreign export ccall` of both functions.
- `Library/Commands.hs`:
  - `maxImagePreviewSize = 14000` (`:176-177`)
  - `imagePreview :: CryptoFile -> IO ImageData` (`:229-235`)
  - `SendImage` (`:3558`): the preview from `imagePreview`
- `simplex-chat.cabal`:
  - module `Simplex.Chat.Image` (`:52`)
  - library dependencies (`:385-387`):
    - `JuicyPixels`
    - `JuicyPixels-jpeg-turbo`
    - `JuicyPixels-stbir`
  - test suite:
    - module `ImageTests` (`:720`)
    - dependency `JuicyPixels` (`:812`)
- `libsimplex.dll.def:30-31`, `flake.nix:506-507`: the export names.

iOS:
- `apps/ios/SimpleXChat/ImageUtils.swift`:
  - `resizeImageToDataSize` (`:88-94`), `resizeImageToStrSizeSync` (`:96-102`)
  - `resizeImageWithCore` (`:108-114`), private
  - `saveWallpaperFile(image:)` (`:233`): `guard let` in place of a force-unwrap
  - `compressImageStr` and `reduceSize` removed
- `apps/ios/SimpleXChat/SimpleX.h:36-40`: both declarations, with the approved comments.
- `apps/ios/SimpleX.xcodeproj/project.pbxproj:199, 598, 774, 865`: `libturbojpeg.a` in the SimpleXChat frameworks phase and the Libraries group.
- `apps/ios/spec/architecture.md`:
  - `:101-103`: FFI list
  - `:61, 67, 69, 334`: header range `SimpleX.h#L1-L55`

Android and desktop, `apps/multiplatform/common/src/`:
- `commonMain/.../platform/Images.kt`: `resizeImageToStrSize` (`:34-38`) and `resizeImageToDataSize` (`:40-46`); the `expect` declarations of both and of `compressImageStr` removed.
- `androidMain/.../platform/Images.android.kt`, `desktopMain/.../platform/Images.desktop.kt`: the three `actual` functions removed.
- `commonMain/.../platform/Core.kt`: `chatResizeImageUri` (`:42`), `chatResizeImage` (`:43`), `ByteArray.toDirectBuffer` (`:52-57`).
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
  - `ChatResizeImage` (`:811-840`), `ChatResizeImageUri` (`:842-869`): `ResultAsyncWorker` with `KeepAlive` of the input
  - exports (`:890-891`)
- `src/simplex.d.ts:13-14`, `src/simplex.js:3-4`: bindings.
- `src/core.ts:112-130`: `chatResizeImageUri`, `chatResizeImage`, `resizedImage`.
- `docs/`: `core.Function.chatResizeImage.md` and `core.Function.chatResizeImageUri.md` new, `Namespace.core.md` list; other pages regenerated with updated source line references.

Python, `packages/simplex-chat-python/src/simplex_chat/`:
- `_native.py`:
  - `_optional_export` (`:280-289`), also used by `migrate_init_queue` (`:292-298`)
  - `resize_image` (`:301-302`), `resize_image_uri` (`:305-306`)
- `core.py`:
  - `_check_c_int` (`:92-95`), also used by `chat_migrate_init` (`:149`)
  - `chat_resize_image_uri` (`:257-266`), `chat_resize_image` (`:269-281`), `_resized_image` (`:284-287`)

Build:
- `.gitmodules:4-6`, `external/libjpeg-turbo`: the submodule.
- `scripts/desktop/prepare-libjpeg-turbo.sh`, new.
- `cabal.project`: `package jpeg-turbo` with `flags: +pkgconfig` (`:19-20`); hs-jpeg-turbo `source-repository-package` (`:79-83`).
- `scripts/nix/sha256map.nix:12`: hs-jpeg-turbo hash.
- `flake.nix`: `mkLibjpegTurbo` (`:42-53`); the `jpeg-turbo` module (`:85-89`); `libturbojpeg.a` copied by `iosPostInstall` (`:119`).
- `scripts/desktop/build-lib-linux.sh:29-30`, `scripts/desktop/build-lib-mac.sh:27-28`: the script is run; `PKG_CONFIG_PATH` is set to `<prefix>/lib/pkgconfig`.
- `scripts/desktop/build-lib-windows.sh:38, 51-54`: the script is run; a `package jpeg-turbo` stanza is written to `cabal.project.local`.
- `scripts/ci/desktop-linux/build-backend.sh:25-26`, `scripts/ci/ios-sim/build-backend.sh:29-30`, `scripts/ci/ios-sim/build-core.sh:16-17`: the script is run; `PKG_CONFIG_PATH` is set to `<prefix>/lib/pkgconfig`.
- `scripts/ci/ios-sim/build-core.sh:58`: `libturbojpeg.a` is copied into the package.
- `Dockerfile.build:65-66, 69-73`: NASM and pkgconf are installed; the script and the submodule are copied into the image; the script is run with prefix `/usr/local`.
- `Dockerfile:8, 29-30`: cmake, NASM and pkgconf are installed; the submodule is initialized with `git submodule update --init`; the script is run with prefix `/usr/local`.
- `apps/simplex-support-bot-light/Dockerfile:30-31, 34-37`, `scripts/badge-service/Dockerfile:27-28, 31-34`: cmake and NASM are installed; the script and the submodule are copied into the image; the script is run with prefix `/usr/local`; their `Dockerfile.dockerignore:14-20`: the script and the submodule are included in the build context.
- `scripts/calculator-bot/Dockerfile:19-20, 38`: cmake and NASM are installed; the submodule is copied into the image; `Dockerfile.dockerignore:12`: the submodule is included in the build context.
- `scripts/simplex-chat-reproduce-builds-android.sh`: submodules updated after checkout (`:103`); `Dockerfile.build` context is the checkout (`:162`).

CI:
- `.github/workflows/build.yml`:
  - pull-request path (`:27`)
  - cache key prefixes (`:340`, `:870`, `:1043`, `:1385`)
  - postgres job checkout (`:825`)
  - macOS step "Prepare libjpeg-turbo" (`:1015-1021`)
  - Windows: `nasm:p` (`:1377`); Build CLI (`:1442`, `:1450-1453`)
- `.github/workflows/desktop-linux.yml:63, 88-89, 114, 156`, `ios-sim.yml:59, 84-85, 110, 159`, `android-emu.yml:64, 91, 116`: checkouts and artifact names.
- `scripts/ci/packages_release.py:49, 64`: `CORE_SOURCES`, the `"haskell"` list of `BUILD_CHANGES`.
- `scripts/ci/desktop-linux/README.md:9`, `scripts/ci/ios-sim/README.md:13`: artifact name sources.

## Tests

`tests/ImageTests.hs`, group "Image resize" (`tests/Test.hs:98`):
- Same aspect ratio, in every comparison below: `|w·h' − h·w'| ≤ w' + h'` (`:102-103`).
- data URI (`:25-28`), limit 14000; each result is decoded and compared by aspect ratio with the input:
  - `test.jpg` (1280×720): `data:image/jpg;base64,`
  - `logo-large-rgba.png` (1630×466, transparent pixels): `data:image/png;base64,`
  - `preview-issue1.png` (1200×600, opaque): `data:image/jpg;base64,`
- 16-bit images (`:29`), 128×128: a JPEG URI for RGB16; for RGBA16 with half opacity, a PNG URI decoded as RGBA8.
- image data (`:30-33`), limit 10000:
  - JPEG from `test.jpg`: starts `FF D8`, width below 1280, same aspect ratio
  - PNG from `logo-large-rgba.png`: RGBA8, width below 1630, same aspect ratio
  - 1×1 image with limit 10: `Left` for JPEG and PNG
- image preview (`:34-38`):
  - `test.jpg`: JPEG data URI of at most 14000 characters
  - an encrypted copy of `test.jpg`: the same
  - `test.pdf`: `fixedImagePreview`

`tests/MobileTests.hs`, "Images" (`:93-95`), with buffers from `mallocBytes`:
- `cChatResizeImageUri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters; on `"not an image"`: `""`.
- `cChatResizeImage` on `test.jpg`, limit 100000: base64 of at most 100000 bytes, starting `FF D8` for `png = 0` and with the PNG signature for `png = 1`.

`tests/ChatTests/Files.hs`, "send image with generated preview" (`:47`, `:179-192`):
- `/img @bob ./tests/fixtures/test.jpg`: the received item has a JPEG data URI preview of at most 14000 characters, read with `APIGetChat` (`lastImagePreview`, `:194-200`).

`packages/simplex-chat-nodejs/tests/core.test.ts`, "image resizing" (`:149-209`), with a generated noise PNG:
- data URI, limit 14000: `data:image/jpg;base64,`, at most 14000 characters
- a `Uint8Array` view at offset 3, limit 20000: JPEG starting `FF D8 FF`; PNG from an `ArrayBuffer` with the PNG signature; both at most 20000 bytes
- `"not an image"`: the resize error
- `maxSize` 2^31: `Expected 32-bit integer maxSize`
- With a libsimplex lacking the exports, the export error is expected in each test (`:155-158`).

`packages/simplex-chat-nodejs/tests/loader.test.ts:35`: `chat_resize_image` and `chat_resize_image_uri` are added to the add-on exports called before `load`; from each, `libsimplex is not loaded, call core.loadLibrary(backend) first` is expected (`RequireLoaded`, `:42`).

`packages/simplex-chat-python/tests/test_core_resize_image.py`, with a fake library:
- `chat_resize_image_uri`: the export is called with `(data, len(data), max_size)`; the data URI is returned (`:52`)
- `chat_resize_image`: `png` is 0 for `"jpeg"` and 1 for `"png"`; the result is base64-decoded (`:59`)
- errors, raised by both functions:

  | condition | raised |
  |---|---|
  | `""` from the export (`:72`) | `RuntimeError` |
  | absent export (`:85`) | `RuntimeError` with the export text |
  | `max_size` 2^31 or −2^31 − 1 (`:93`) | `ValueError`, before the call |

## Open questions

1. One function per library, with an `ImageSize` value in place of a size limit? Recommended: yes.
   - `ImageSize` values:
     - image: 261120 bytes
     - profile preview: 12500 characters
     - message preview: 14000 characters
   - TypeScript: `resizeImage(image, size)` with overloads; `Buffer` for the image, `string` for the previews.
   - Python: `resize_image(data, size)` with `typing.overload`; `bytes` for the image, `str` for the previews.
   - Sizes: private constants next to each function.
   - Image format: chosen by `imageFormat` when `png < 0` is passed to `chat_resize_image`, in `cChatResizeImage` (`Mobile/File.hs:153`).
   - `chatResizeImage`, `chatResizeImageUri` and the Python functions are kept public.
   - Costs:
     - yes: a third `png` mode in the C API (question 2); the sizes duplicated in both libraries, two of them also in `Library/Commands.hs:174, 177`.
     - no: the size limit and the image format are passed by each bot.
2. The text of the comment at `apps/ios/SimpleXChat/SimpleX.h:36` and `packages/simplex-chat-nodejs/cpp/simplex.h:38`, if question 1 is accepted? Recommended:
   `// chat_resize_image returns base64 of PNG (png > 0), JPEG (png == 0), or PNG for images with transparency and JPEG for others (png < 0), of at most maxSize bytes, or an empty string`
3. The Python docstrings (`core.py:258, 272`) are written with `max_size` and a final period, as the other docstrings of `core.py`, in place of the approved `maxSize` without a period: keep them? Recommended: keep.
4. In the manual builds, a plain `cabal install` or `cabal build` is run, and libjpeg-turbo must be installed first: which of these options?
   - Documents with these builds:
     - `docs/CLI.md:116, 143-148`
     - `docs/CONTRIBUTING.md:37-49`
     - `docs/contributing/CODE.md:77-89`
     - `apps/simplex-directory-service/README.md:17-21`
     - `apps/simplex-badge-service/README.md:40`
     - `docs/lang/cs/CLI.md:113, 124`, `docs/lang/fr/CLI.md:115, 126`, `docs/lang/pl/CLI.md:115, 146`
   - (a) the build script kept; a step is added to these documents: the submodule checked out (the clones at `docs/CLI.md:95, 116` are made without it), cmake, NASM and pkg-config installed, `scripts/desktop/prepare-libjpeg-turbo.sh` run, `PKG_CONFIG_PATH` set; costs: this step in every manual build.
   - (b) libjpeg-turbo as a submodule of the hs-jpeg-turbo fork, compiled by cabal with `c-sources`, as simplexmq compiles blst; the submodule is checked out by cabal with the package (cabal-install 3.10.2.0 `VCS.hs:406-407`), so the documents are kept; one build path everywhere, without CMake, NASM, pkg-config and the script; costs: 3 configuration headers per target and a list of 113 C files kept per libjpeg-turbo release, with SIMD lost on every CPU, or 4 headers and 128 files with Arm Neon.
   - (c) a cabal flag, off by default: in manual builds JPEG is encoded with JuicyPixels `encodeJpegAtQuality`; libjpeg-turbo is turned on by the scripts, CI and nix; costs: a second encoder path behind the flag, JPEGs 7–33% larger at the same quality and encoded 3–7 times slower (`test.jpg` and `preview-issue1.png`, qualities 20 to 94).
   - With every option, the Docker build of `docs/CLI.md:94-98` is kept: the submodule is initialized in `Dockerfile:29`.

   Recommended: (c), the cheapest option with the documents unchanged.
5. The core modules are described as rarely needed in Node ("unlikely to ever need", `README.md:137`, `src/index.ts:15`) and as internal in Python ("Internal", "for tests and the api.ChatApi class only", `core.py:1-4`), while image resizing is exported from them; the Python text also counts "8 C ABI functions", 9 at master and 11 with this change: change the texts? Recommended: yes, to the texts below.
   - `packages/simplex-chat-nodejs/README.md:137`:
     `- [core](./docs/Namespace.core.md): a low level API to the core library - the same that is used in desktop clients. You are unlikely to ever need to use this module directly, except for image resizing.`
   - `packages/simplex-chat-nodejs/src/index.ts:15`, generated into `docs/Namespace.core.md:10`:
     ` * You are unlikely to ever need to use this module directly, except for image resizing.`
   - `packages/simplex-chat-python/src/simplex_chat/core.py:1-5`:
     ```
     """Typed async wrapper around libsimplex's C ABI functions.

     Users interact with `Bot` / `ChatApi`. This module is exposed as
     `simplex_chat.core` for tests, the api.ChatApi class and image resizing
     (`chat_resize_image`, `chat_resize_image_uri`).
     """
     ```
6. License notices, put on hold by the user: added in this PR? Recommended: yes.
   - `docs/dependencies/README.md`, with `LICENSE.md` and `README.ijg` of the submodule copied into `licences/apps/libjpeg-turbo/` and the license text of `stb_image_resize.h` into `licences/apps/stb_image_resize/LICENSE`:
     ```
     [libjpeg-turbo](https://github.com/libjpeg-turbo/libjpeg-turbo): JPEG encoding of resized images ([IJG and BSD-3-clause](./licences/apps/libjpeg-turbo/LICENSE.md)). This software is based in part on the work of the Independent JPEG Group.

     [stb_image_resize](https://github.com/nothings/stb): image scaling ([MIT or public domain](./licences/apps/stb_image_resize/LICENSE))
     ```
   - `docs/dependencies/HASKELL.md`, with each package's `LICENSE` copied into `licences/haskell/<package>-<version>/`; direct dependencies, after `http2` (`:24`):
     ```
     | `JuicyPixels` | [`3.3.8`](http://hackage.haskell.org/package/JuicyPixels-3.3.8) | [`BSD-3-Clause`](./licences/haskell/JuicyPixels-3.3.8/LICENSE) | Picture loading/serialization (in png, jpeg, bitmap, gif, tga, tiff and radiance) | `JuicyPixels-jpeg-turbo`, `JuicyPixels-stbir` |
     | `JuicyPixels-jpeg-turbo` | [`0.1.0.0`](https://gitlab.com/dpwiz/hs-jpeg-turbo/-/tree/f1bc073427e60341a3359e2345c1683d17ea2a35/JuicyPixels-jpeg-turbo) | [`BSD-3-Clause`](./licences/haskell/JuicyPixels-jpeg-turbo-0.1.0.0/LICENSE) | JuicyPixels wrappers for libjpeg-turbo |  |
     | `JuicyPixels-stbir` | [`0.1.0.0`](http://hackage.haskell.org/package/JuicyPixels-stbir-0.1.0.0) | [`BSD-3-Clause`](./licences/haskell/JuicyPixels-stbir-0.1.0.0/LICENSE) | Scale JuicyPixels images with stb_image_resize |  |
     ```
     indirect dependencies, after `iso8601-time` (`:139`):
     ```
     | `jpeg-turbo` | [`0.1.0.0`](https://gitlab.com/dpwiz/hs-jpeg-turbo/-/tree/f1bc073427e60341a3359e2345c1683d17ea2a35/jpeg-turbo) | [`BSD-3-Clause`](./licences/haskell/jpeg-turbo-0.1.0.0/LICENSE) | FFI bindings to libjpeg-turbo | `JuicyPixels-jpeg-turbo` |
     ```
   - The "Depended upon by" cells of the existing rows of their dependencies gain the new packages, or the report is regenerated:
     - `JuicyPixels`: `bytestring`, `mtl`, `binary`, `zlib`, `transformers`, `vector`, `primitive`, `deepseq`, `containers`
     - `JuicyPixels-stbir`: `vector`, `data-default-class`
     - `JuicyPixels-jpeg-turbo`: `bytestring`, `vector`
     - `jpeg-turbo`: `bytestring`
7. The comments at `scripts/ci/ios-sim/avatar.swift:1` and `scripts/ci/desktop-linux/backend.sh:70` attribute the old resizing method to the apps; the scripts still implement that method (`avatar.swift:81-85`, `backend.sh:79-84`). Change them to the method only? Recommended: yes.
   - `scripts/ci/ios-sim/avatar.swift:1`:
     `// Makes a group avatar:`
   - `scripts/ci/desktop-linux/backend.sh:70`:
     `# Prints a profile image data URI: JPEG at quality 85, scaled down by the square root`
