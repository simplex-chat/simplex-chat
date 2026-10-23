# Image resizing in the core

Each app resizes images with its own platform code, the CLI sends a fixed placeholder as the preview of every image, and bots built on the Node and Python libraries must produce image previews with their own code.
The core decodes, resizes and encodes images with JuicyPixels, stb_image_resize and libjpeg-turbo; the apps, both bot libraries and `/img` call it.

References are to branch `ab/resize-image` at `23c84b6eb`, based on master `66e381bf5`. Haskell paths without a directory are in `src/Simplex/Chat/`.

## Decisions

Resizing, `Image.hs`:
- `resizeImage format maxSize image`: JPEG or PNG bytes of at most `maxSize` bytes.
- `resizeImageDataUri maxSize image`: a data URI of at most `maxSize` characters.
  - format: `imageFormat image`
  - prefix: `data:image/jpg;base64,` or `data:image/png;base64,`
- `imageFormat`:
  - PNG: `ImageYA8`, `ImageYA16`, `ImageRGBA8` and `ImageRGBA16` images with a pixel below full opacity
  - JPEG: other images
- Pixels are converted to 8-bit RGB for JPEG and to 8-bit RGBA for PNG; every `DynamicImage` constructor is accepted.
- Size search:

  | step | JPEG | PNG |
  |---|---|---|
  | halving | while the image at quality 20 exceeds the limit | while the image exceeds the limit |
  | final | quality bisected in 20..94 at the last size | the image itself when it fits at full size; otherwise scale bisected between the last size and the size before it, to 1/64 |
  | failure | a side below 2 pixels before the image fits: `Left` | the same |

- An encoding of exactly `maxSize` ends the bisection.
- EXIF orientation of the input is ignored, and EXIF metadata is dropped.

C API:
```
chat_resize_image_uri(data, len, maxSize)     data URI, or ""
chat_resize_image(data, len, maxSize, png)    base64 of PNG (png != 0) or JPEG bytes, or ""
```
- `data`: an encoded image of a format decoded by JuicyPixels `decodeImage`, read in place during the call.
- `""`: the input is undecodable, the image exceeds the limit at every size, or an exception is raised.
- The result is allocated with `malloc` and freed by the caller.
- A chat controller is not required.

Apps, with function signatures and call sites unchanged:
- iOS `resizeImageToDataSize` and `resizeImageToStrSizeSync`:
  - an image with orientation other than `.up` is redrawn upright;
  - input: UIKit PNG with alpha, JPEG at quality 0.85 without;
  - alpha: the `hasAlpha` parameter of `resizeImageToDataSize`, `imageHasAlpha(image)` for the data URI;
  - `""` is returned as `nil`.
- Android and desktop, common code:
  - input: `compressImageData` bytes, PNG or JPEG at quality 85, in a direct `ByteBuffer`;
  - `resizeImageToStrSize`: PNG input when the image has alpha; `""` is returned to the caller;
  - `resizeImageToDataSize`: input in the requested format; for `""` an exception is raised, caught by `saveImage`, `desktopSaveImageInTmp`, and `ThemeWallpaper.importFromString` around `saveWallpaperFile`.

Bot libraries:
- Node, `core`:
  - `chatResizeImageUri(image: ArrayBuffer | Uint8Array, maxSize: number): Promise<string>`
  - `chatResizeImage(image: ArrayBuffer | Uint8Array, maxSize: number, format: "jpeg" | "png"): Promise<Buffer>`
- Python, `simplex_chat.core`:
  - `chat_resize_image_uri(data: bytes, max_size: int) -> str`
  - `chat_resize_image(data: bytes, max_size: int, image_format: Literal["jpeg", "png"]) -> bytes`
- The core is called on a worker thread: an N-API async worker in Node, `asyncio.to_thread` in Python.
- Errors:

  | condition | Node | Python |
  |---|---|---|
  | `""` from the core | `Error("image cannot be decoded or does not fit maxSize")` | `RuntimeError("image cannot be decoded or does not fit max_size")` |
  | export absent from the loaded libsimplex | `Error("loaded libsimplex does not export <name>; image resizing needs a newer libsimplex")` | `RuntimeError` with the same text |
  | size limit outside 32-bit integers | `RangeError("Expected 32-bit integer maxSize")` | `ValueError` |
  | input above 2^31 − 1 bytes | `RangeError("Buffer is too large")` | `ValueError` |

- Until `scripts/ci/packages_release.py prepare` pins a core release built with this change, `core-7.1.0.14` is loaded (`packages/simplex-chat-nodejs/src/release.ts:3`, `packages/simplex-chat-python/src/simplex_chat/_version.py:8`), and the wrappers raise the export error.

`/img` and `/imgf`:
- The preview is generated from the file: read with `CF.readFile` (encrypted files included), decoded, and resized to a data URI of at most 14000 characters.
- On any failure the fixed placeholder is sent, as before.

Native code:
- libjpeg-turbo 3.1.4.1, commit `9217719d`, is the submodule `external/libjpeg-turbo`.
- It is built static and PIC with `BUILD=20260327` by the script, and static by nix.
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
- stdout: the prefix, in `cygpath -m` form on Windows
- Windows: `CC` is GHC's clang, `$(ghc --print-libdir)/../mingw/bin/clang.exe`; `LDFLAGS` is `-fuse-ld=lld --rtlib=compiler-rt`

libjpeg-turbo per build:

| build | libjpeg-turbo | jpeg-turbo flags and paths |
|---|---|---|
| Linux, `build.yml` | `Dockerfile.build` image, prefix `/usr/local` | `+pkgconfig`, default pkg-config path |
| macOS, `build.yml` | `brew install nasm`, the script | `+pkgconfig`, `PKG_CONFIG_PATH` in `GITHUB_ENV` |
| Windows, `build.yml` and `build-lib-windows.sh` | the script | `-pkgconfig +static`, include and lib directories in `cabal.project.local` |
| `build-lib-linux.sh`, `build-lib-mac.sh` | the script | `+pkgconfig`, `PKG_CONFIG_PATH` |
| `desktop-linux.yml`, `ios-sim.yml`, `android-emu.yml` backend | the script, default prefix | `+pkgconfig`, `PKG_CONFIG_PATH` |
| `Dockerfile`, support-bot-light and badge-service Dockerfiles | the script, prefix `/usr/local` | `+pkgconfig`, default pkg-config path |
| calculator-bot Dockerfile | `build-lib-linux.sh` | as in `build-lib-linux.sh` |
| nix, every target | `mkLibjpegTurbo`: nixpkgs `libjpeg_turbo`, static, source at the submodule commit | `-pkgconfig +static`, `libs` set to `mkLibjpegTurbo` |

CI:
- `submodules: recursive` is set on the checkouts of jobs that run the script or hash the submodule.
- The hash of the submodule files is part of the `build.yml` cache keys and of the artifact names of cores and backends built with cabal.
- `external/libjpeg-turbo` is added to the pull-request paths of `build.yml` and to `CORE_SOURCES` and the haskell `BUILD_CHANGES` of `packages_release.py`.

Out of scope: `scripts/relay/Dockerfile`; the libjpeg-turbo steps are added to it when its `CHAT_REF` (`:3`) passes this change.

## Mechanics

Core:
- `Image.hs`, new:
  - exports `ImageFormat (IFJpeg, IFPng)`, `imageFormat`, `resizeImage`, `resizeImageDataUri` (`:5-11`)
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
- `simplex-chat.cabal`: module `Simplex.Chat.Image` (`:52`); `JuicyPixels`, `JuicyPixels-jpeg-turbo`, `JuicyPixels-stbir` in the library (`:385-387`); `ImageTests` (`:720`) and `JuicyPixels` (`:812`) in the test suite.
- `libsimplex.dll.def:30-31`, `flake.nix:506-507`: the export names.

iOS:
- `apps/ios/SimpleXChat/ImageUtils.swift`:
  - `resizeImageToDataSize` (`:88-94`), `resizeImageToStrSizeSync` (`:96-102`)
  - `resizeImageWithCore` (`:108-114`), private
  - `compressImageStr` and `reduceSize` removed
- `apps/ios/SimpleXChat/SimpleX.h:36-40`: both declarations, with the approved comments.
- `apps/ios/SimpleX.xcodeproj/project.pbxproj:199, 598, 774, 865`: `libturbojpeg.a` in the SimpleXChat frameworks phase and the Libraries group.
- `apps/ios/spec/architecture.md:101-103`: FFI list.

Android and desktop, `apps/multiplatform/common/src/`:
- `commonMain/.../platform/Images.kt`: `resizeImageToStrSize` (`:34-35`) and `resizeImageToDataSize` (`:37-43`); the `expect` declarations of both and of `compressImageStr` removed.
- `androidMain/.../platform/Images.android.kt`, `desktopMain/.../platform/Images.desktop.kt`: the three `actual` functions removed.
- `commonMain/.../platform/Core.kt`: `chatResizeImageUri` (`:42`), `chatResizeImage` (`:43`), `ByteArray.toDirectBuffer` (`:52-57`).
- `commonMain/.../model/CryptoFile.kt:25`: `writeCryptoFile` uses `toDirectBuffer`.
- `commonMain/cpp/android/simplex-api.c`: declarations (`:86-87`), JNI functions (`:280`, `:287`) with `decode_and_free`.
- `commonMain/cpp/desktop/simplex-api.c`: declarations (`:49-50`), JNI functions (`:281`, `:288`) with `decode_to_utf8_string`.
- `apps/multiplatform/spec/architecture.md:147-150`: JNI table and count.

Node, `packages/simplex-chat-nodejs/`:
- `cpp/simplex.h:38-42`: function types, with the approved comments.
- `cpp/simplex.cc`:
  - `Library` fields (`:39-40`), resolved as optional symbols (`:217-218`)
  - `RequireExport` (`:141-146`), `GetInt32Arg` (`:148-154`), `BufferArg` (`:156-159`), `GetBufferArg` (`:161-178`); also used by `ChatMigrateInitQueue` (`:573`, `:583`) and `ChatWriteFile` (`:697`)
  - `ChatResizeImage` (`:811-840`), `ChatResizeImageUri` (`:842-869`): `ResultAsyncWorker` with `KeepAlive` of the input
  - exports (`:890-891`)
- `src/simplex.d.ts:13-14`, `src/simplex.js:3-4`: bindings.
- `src/core.ts:112-130`: `chatResizeImageUri`, `chatResizeImage`, `resizedImage`.
- `docs/`: `core.Function.chatResizeImage.md` and `core.Function.chatResizeImageUri.md` new, `Namespace.core.md` list; other pages regenerated with updated source line references.

Python, `packages/simplex-chat-python/src/simplex_chat/`:
- `_native.py`:
  - `_optional_export` (`:280-289`)
  - `migrate_init_queue` (`:292-298`) uses it
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
- `scripts/desktop/build-lib-linux.sh:29-30`, `scripts/desktop/build-lib-mac.sh:27-28`: script, `PKG_CONFIG_PATH`.
- `scripts/desktop/build-lib-windows.sh:38, 51-54`: script, `package jpeg-turbo` stanza.
- `scripts/ci/desktop-linux/build-backend.sh:25-26`, `scripts/ci/ios-sim/build-backend.sh:29-30`, `scripts/ci/ios-sim/build-core.sh:16-17`: script, `PKG_CONFIG_PATH`; `scripts/ci/ios-sim/build-core.sh:58`: `libturbojpeg.a` in the package.
- `Dockerfile.build:65-66, 69-73`: NASM, pkgconf, script into `/usr/local`.
- `Dockerfile:8, 29`: cmake, NASM, pkgconf, script into `/usr/local`.
- `apps/simplex-support-bot-light/Dockerfile:30-31, 34-37`, `scripts/badge-service/Dockerfile:27-28, 31-34`: cmake, NASM, script into `/usr/local`; their `Dockerfile.dockerignore:14-20`: the script and the submodule.
- `scripts/calculator-bot/Dockerfile:19-20, 38`, `Dockerfile.dockerignore:12`: cmake, NASM, the submodule.
- `scripts/simplex-chat-reproduce-builds-android.sh`: submodules updated after checkout (`:103`); `Dockerfile.build` context is the checkout (`:162`).

CI:
- `.github/workflows/build.yml`:
  - pull-request path (`:27`)
  - cache key prefixes (`:340`, `:870`, `:1043`, `:1385`)
  - postgres job checkout (`:825`)
  - macOS step "Prepare libjpeg-turbo" (`:1015-1021`)
  - Windows: `nasm:p` (`:1377`); Build CLI (`:1442`, `:1450-1453`)
- `.github/workflows/desktop-linux.yml:63, 88-89, 114, 156`, `ios-sim.yml:59, 84-85, 110, 159`, `android-emu.yml:64, 91, 116`: checkouts and artifact names.
- `scripts/ci/packages_release.py:49, 64`: `CORE_SOURCES`, haskell `BUILD_CHANGES`.
- `scripts/ci/desktop-linux/README.md:9`, `scripts/ci/ios-sim/README.md:13`: artifact name sources.

## Tests

`tests/ImageTests.hs`, group "Image resize" (`tests/Test.hs:98`):
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
- Same aspect ratio: `|w·h' − h·w'| ≤ w' + h'` (`:102-103`).

`tests/MobileTests.hs`, "Images" (`:93-95`), through the C functions:
- `chat_resize_image_uri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters; on `"not an image"`: `""`.
- `chat_resize_image` on `test.jpg`, limit 100000: base64 of at most 100000 bytes, starting `FF D8` for `png = 0` and with the PNG signature for `png = 1`.

`tests/ChatTests/Files.hs`, "send image with generated preview" (`:47`, `:179-192`):
- `/img @bob ./tests/fixtures/test.jpg`: the received item has a JPEG data URI preview of at most 14000 characters, read with `APIGetChat` (`lastImagePreview`, `:194-200`).

`packages/simplex-chat-nodejs/tests/core.test.ts`, "image resizing" (`:149-209`), with a generated noise PNG:
- data URI, limit 14000: `data:image/jpg;base64,`, at most 14000 characters
- a `Uint8Array` view at offset 3, limit 20000: JPEG starting `FF D8 FF`; PNG from an `ArrayBuffer` with the PNG signature; both at most 20000 bytes
- `"not an image"`: the resize error
- `maxSize` 2^31: `Expected 32-bit integer maxSize`
- With a libsimplex lacking the exports, each test expects the export error (`:155-158`).

`packages/simplex-chat-nodejs/tests/loader.test.ts:41`: both bindings throw the not-loaded error.

`packages/simplex-chat-python/tests/test_core_resize_image.py`, with a fake library:
- the data URI is returned; the export receives the data, its length and `max_size` (`:52`)
- `png` is 0 for `"jpeg"` and 1 for `"png"`; the result is base64-decoded (`:59`)
- for `""`, `RuntimeError` is raised, in both functions (`:72`)
- for an absent export, `RuntimeError` with the export text is raised, in both functions (`:85`)
- for `max_size` 2^31 and −2^31 − 1, `ValueError` is raised before the call, in both functions (`:93`)

## Open questions

1. A function per library that takes an `ImageSize` value in place of a size: image (261120 bytes), profile preview (12500 characters), message preview (14000 characters)? Recommended: yes.
   - TypeScript `resizeImage(image, size)` with overloads, Python `resize_image(data, size)` with `typing.overload`: `Buffer` or `bytes` for image, `string` or `str` for the previews.
   - The sizes: private constants next to each function.
   - Format for image: chosen by `imageFormat` when `png < 0` is passed to `chat_resize_image`, in `cChatResizeImage` (`Mobile/File.hs:153`).
   - `chatResizeImage`, `chatResizeImageUri` and the Python functions stay public.
2. The text of the comment at `apps/ios/SimpleXChat/SimpleX.h:36` and `packages/simplex-chat-nodejs/cpp/simplex.h:38`, if question 1 is accepted? Recommended:
   `// chat_resize_image returns base64 of PNG (png > 0), JPEG (png == 0), or PNG for images with transparency and JPEG for others (png < 0), of at most maxSize bytes, or an empty string`
3. The Python docstrings (`core.py:258, 272`) use `max_size` and a final period, as the other docstrings of `core.py`, in place of the approved `maxSize` without a period: keep them? Recommended: keep.
4. Manual builds by `docs/CLI.md:143-148` run a plain `cabal install simplex-chat` and need libjpeg-turbo installed first: which of these options?
   - (a) the build script kept; `docs/CLI.md` gains a step: cmake, NASM and pkg-config installed, `scripts/desktop/prepare-libjpeg-turbo.sh` run, `PKG_CONFIG_PATH` set.
   - (b) libjpeg-turbo compiled by cabal from the submodule (`c-sources`), as simplexmq compiles blst: one build path everywhere, without CMake, NASM, pkg-config and the script; costs: 4 configuration headers per target, a list of 113 C files kept per libjpeg-turbo release, x86-64 SIMD lost.
   - (c) a cabal flag, off by default: manual builds encode JPEG with JuicyPixels `encodeJpegAtQuality`; libjpeg-turbo is turned on by the scripts, CI and nix; costs: a second encoder path behind the flag, JPEGs 7–33% larger at the same quality and encoded 3–7 times slower (`test.jpg` and `preview-issue1.png`, qualities 20 to 94).

   Recommended: (c), the cheapest option that keeps `docs/CLI.md` unchanged.
5. The texts on the core modules: "unlikely to ever need" in Node, "Internal" and "8 C ABI functions" in Python? Recommended:
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
6. License notices, on hold by the user. Recommended: added in this PR.
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
