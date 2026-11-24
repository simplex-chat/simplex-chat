# WebP images decoded by the core

A desktop user who picks a WebP image gets the alert "Decoding error", and the image is dropped from the message.
Link previews made by the core (#8297) fail for a page with a WebP `og:image` and for a link to a WebP image; the apps preview both today.
Bots get an error from `resizeImage` for WebP input; CLI users get one from `/set profile image file` and `--user-image-file` for a `.webp` file.
On Android and iOS, a picked WebP image is decoded by the platform after the core returns `""`.

WebP input is decoded in the core by libwebp: still images, the first frame of animations, alpha and the ICC profile; the EXIF orientation is applied as for JPEG. Encoding WebP is out of scope.
libwebp's decoder and demuxer are compiled by cabal in `JuicyPixels-webp`, a new package of simplex-chat/hs-jpeg-turbo.
`decodeResizable` passes WebP input to it within half of `maxDecodeMemoryMB`, as PNG input. Resizing, output formats and colour profiles are unchanged.
The C functions are unchanged; the apps and the bots call them as now.

References are to branch `ep/core-webp` at the commit of this plan, based on master `8f5dfa3d4`.
Haskell paths without a directory are in `src/Simplex/Chat/`.
Kotlin paths start at `apps/multiplatform/common/src/`: `common/` is `commonMain/kotlin/chat/simplex/common/`, `desktop/` is `desktopMain/kotlin/chat/simplex/common/`, `android/` is `androidMain/kotlin/chat/simplex/common/`.
libwebp paths are at `v1.6.0`; a file name without a directory is in `src/dec`, `src/demux`, `src/dsp`, `src/utils` or `src/webp`.
Other sources:

| Name | Source |
|---|---|
| hs-jpeg-turbo | simplex-chat/hs-jpeg-turbo master `c1b5080`; pinned at `d0a4d95` (cabal.project:76-80) |
| libwebp release | `v1.6.0`, commit `4fa21912`, 7 Jul 2025 |
| libwebp main | `f054d0c3`, 21 Nov 2026 (chromium.googlesource.com/webm/libwebp, mirrored at github.com/webmproject/libwebp) |
| libwebp test data | chromium.googlesource.com/webm/libwebp-test-data at `06ddd96e` |
| link previews | branch `ep/link-previews` of evgeny-simplex/simplex-chat at `3771cd6a`, simplex-chat/simplex-chat#8297 |
| SwiftyGif | kirualex/SwiftyGif at `5e861933`, pinned in apps/ios/SimpleX.xcodeproj/project.pbxproj:2883-2890 |
| Android | android.googlesource.com, tag `android-9.0.0_r1`: platform/external/skia and platform/frameworks/base |

## Acceptance

| Who | Before | After |
|---|---|---|
| desktop user, still WebP | alert "Decoding error", image dropped | compose preview; sent as `IMG_*.jpg` or `IMG_*.png` (Q2 b) |
| desktop user, animated WebP | alert "Decoding error", image dropped | compose preview of frame 1; the file sent as it is (Q2 b) |
| Android user, still WebP | preview and image from the platform decode, upright by the EXIF orientation; log line `core returned no image` | preview and image from the core, upright by the EXIF orientation; the log line is absent |
| iOS user, WebP file from the photo picker | preview and image from `UIImage`, after `""` from the core | preview and image from the core, upright by the EXIF orientation |
| user who types a link to a page with a WebP `og:image` (#8297) | preview fails | preview with the image |
| bot on the Node or Python library | `resizeImage` error | JPEG or PNG, with the next pinned core release |
| CLI user, `/set profile image file x.webp` | `unsupported image extension` | profile image set |

Evidence at review:
- the groups `/Image resize/` and `/Mobile API Tests/`, the test `/SimpleX chat client/profile tests/user profiles/set profile image from file/` and desktopTest pass in the reduced build;
- the test "should apply EXIF orientation of WebP" fails at `2c4ca02`, the commit before the fix, with sides 1000×307 for the expected 307×1000 ([run](https://github.com/simplex-chat/simplex-chat/actions/runs/45287298996));
- hs-jpeg-turbo CI, the full matrix and the armv7 job ([run](https://github.com/evgeny-simplex/hs-jpeg-turbo/actions/runs/45286775715) of `5b8ae10`, a dispatch with the input `full`):
  - the package built and its tests passed with GHC 8.10.7 and 9.6.3 on Linux x86-64, with GHC 9.6.3 on Linux aarch64, macOS aarch64 and x86-64, and Windows;
  - the conformance files decoded to the md5 of libwebp's test data;
  - two images taller than the window of rows of the lossless decoder: the lossless one decoded to its source pixels by the package tests and by the armv7, aarch64 and sanitizer builds; the alpha of the lossy one equal to its source in the package tests, its outputs identical in the three builds; the line coverage of the window functions in the job summary;
  - the armv7 and aarch64 outputs identical;
  - the sanitizer run clean on truncated and changed copies of all these files;
  - the decoder size per platform in the job summaries, in "Binary size";
- the memory of libwebp decoding at `v1.6.0` and at `f054d0c3`, measured in CI (Q1);
- pending: desktop-linux, android-emu and ios-sim smoke tests pick, preview and send the WebP fixtures.

## Decode paths today

Core:

| Path | Input | WebP input today |
|---|---|---|
| `decodeResizable` (Image.hs:69-77) | bytes | `Left "unsupported image format"` (:77) |
| `cChatResizeImage`, `cChatResizeImageUri`, file and descriptor variants (Mobile/File.hs:153-169, 204-207), exported at Mobile.hs:167-177 | buffer, path, descriptor | `""` |
| `imagePreview` (Library/Commands.hs:232-238), for `/img` (:3578-3586) | file | refused by extension (:204-205, :3582) |
| `loadImageData` (Library/Commands.hs:208-224), for `/set profile image file` (:3699-3702) and `--user-image-file` (Core.hs:227-228) | file | refused by extension (:209-211) |
| `imageDataPreview` (Image.hs:229-232 at `3771cd6a`), for link previews (LinkPreview.hs:119, 125) | fetched bytes, at most 5 MiB (:88-89) | `Left`; a page of type `image/webp` refused (:505) |

Apps, with the platform decode after `""` from the core:

| App | Path | WebP input today |
|---|---|---|
| iOS | picker, `.imageFile`: `resizeImageFileToStrSize`, then `resizeImageToStrSize` of the `UIImage` (Shared/Views/Chat/ComposeMessage/ComposeView.swift:315-320); sending: `saveImageFile`, then `saveImage` (:1609-1614) | `""`, then `UIImage` encoded as JPEG or PNG for the core (SimpleXChat/ImageUtils.swift:132-158) |
| iOS | animated: `setGifFromData` throws without `kCGImagePropertyGIFDictionary` (SwiftyGif UIImage+SwiftyGif.swift:170-177), so `loadFromURL` returns `.simpleImage` (ComposeView.swift:336-353) | the first frame, as above |
| iOS | file type from the picker: `public.jpeg` when offered for items other than PNG and GIF, otherwise the file itself (Shared/Views/Helpers/ImagePicker.swift:92-95) | the WebP file, when `public.jpeg` is absent |
| iOS share extension | `downsampleImage`, then `saveImage` of the `UIImage` (SimpleX SE/ShareModel.swift:442-447, 525-534) | platform decode |
| Android | still: `SimpleImage`, preview `resizeImageFileToStrSize(uri) ?: bitmapPreview()` (common/views/chat/ComposeView.kt:382); sending: `resizeImageFromUri` (common/views/helpers/Utils.kt:218-227) | `""`, log line, then `ImageDecoder` |
| Android | animated: `AnimatedImageDrawable` (android/platform/Images.android.kt:102-107) | the file sent as it is (Utils.kt:229-254); preview from the bitmap (ComposeView.kt:375) |
| desktop | `.webp` is animated by extension (desktop/platform/Images.desktop.kt:175-178) | `bitmapPreview`: `ImageIO.read` returns null, `toComposeImageBitmap` throws, alert "Decoding error", media dropped (desktop/views/helpers/Utils.desktop.kt:163-175; ComposeView.kt:370-376, 397-399; apps/multiplatform/product/gaps.md:231) |

Link previews of the apps on master, replaced by #8297: Android and desktop decode the image with `loadImageBitmap`, WebP included (common/views/helpers/LinkPreviews.kt:43, 92-96, 124-128); iOS with `LPMetadataProvider` and `UIImage` (SimpleXChat/ImageUtils.swift:455-489).

## Decisions

| ID | Decision | Options, recommended first |
|---|---|---|
| D1 | **Binding in hs-jpeg-turbo.** New package `JuicyPixels-webp`, module `Codec.Picture.Webp`, in simplex-chat/hs-jpeg-turbo; libwebp a submodule at `JuicyPixels-webp/cbits/libwebp`. | (a) package in hs-jpeg-turbo: one cabal package; CI, nix hash line, `subdir` list and Windows removal glob `hs-jpeg-t*` reused.<br>(b) new repository: created by the user; CI copied; one more `source-repository-package` and nix hash line.<br>(c) cbits in simplex-chat: a submodule in every simplex-chat checkout (CI, nix, Docker, reproducible Android build) and 50 C files in simplex-chat.cabal.<br>(d) Hackage `webp` 0.1.2.0: GPL-3; system libwebp through pkg-config, while the iOS and Android builds link bundled C sources only; c2hs; `error` on animated input; the decoded pointer used unchecked (`decodeAbsBS`).<br>(e) iko-soy/JuicyPixels-webp: pure Haskell, GitHub only, repository created 6 Feb 2026, about 11,650 lines to review (its STATUS.md). |
| D2 | **One decode function.** `decodeWebp :: DecodeOptions -> ByteString -> IO (Either String (DynamicImage, Metadata))`, `newtype DecodeOptions = DecodeOptions {maxMemoryMB :: Int}`, `data Metadata = Metadata {iccProfile, exif :: Maybe ByteString}`: the image and the payloads of the `ICCP` and `EXIF` chunks from one pass of the demuxer, as the image and the ICC profile from `Spng.decodePng` (Spng.hsc:36-45). | (a) one function.<br>(b) header, decode and metadata functions: three passes over the container; the header function used by tests only. |
| D3 | **First frame of an animation through the demuxer.** Frame 1 of `WebPDemuxGetFrame`, decoded by `WebPDecode` into a zeroed canvas at the frame offset: the pixels of `WebPAnimDecoderGetNext` for frame 1 (anim_decode.c:202-203, 347-384). | (a) demuxer and decoder: one canvas.<br>(b) `WebPAnimDecoder`: two canvases allocated at creation (anim_decode.c:137-143), then a copy out.<br>(c) `Left` for animations: link previews of animated images fail. |
| D4 | **RGB for opaque still images, RGBA otherwise.** Table in "Decoding". The output format is chosen by `imageFormat` (Image.hs:51-67), unchanged. | (a) RGB or RGBA by the frame's alpha.<br>(b) RGBA always: a third more memory for opaque images. |
| D5 | **ICC profile and EXIF orientation as for JPEG.** The payload of the first `ICCP` chunk is passed to `colourProfile` (Image.hs:181-185): `CPDisplayP3` or `CPNone` (plan 2026-10-23, D25). The payload of the first `EXIF` chunk is passed to `exifOrientation` (Image.hs:107-129), and the image is turned by `orientImage` (:154-166), as JPEG input (plan 2026-10-23, D6). The payload is read as TIFF data from its first byte, tag `0x0112` of IFD0, as by the WebP codec of Android's `ImageDecoder` (Android: SkWebpCodec.cpp:102-108, SkJpegCodec.cpp:63-103, ImageDecoder.cpp:110-111), which decodes still WebP images picked on SDK 28 and later on master (Utils.android.kt:278-283, Utils.kt:218-227). `XMP ` chunks are ignored. | (a) EXIF orientation applied.<br>(b) EXIF ignored, as by dwebp (imageio/tiffdec.c:239, imageio/image_enc.c:412): a still WebP with orientation 6, picked on Android SDK 28 and later, sent upright on master, sent turned by 90 degrees. |
| D6 | **Full-size decode within half of `maxDecodeMemoryMB`.** Limits in "Decoding". | (a) full size, halved by `fitImage`, as PNG.<br>(b) libwebp scaled decoding to the display size: a smaller output for lossy images, the same internal frame for lossless ones (vp8l_dec.c:1556-1574), libwebp's rescaler before Catmull-Rom. |
| D7 | **Decoder and demuxer sources only, SIMD by compiler macros.** 50 C files; list in "hs-jpeg-turbo". | (a) SSE2, NEON or C paths, selected by the macros of `src/dsp/cpu.h:47-137`.<br>(b) internal libraries for SSE4.1, AVX2 and armv7 NEON with their flags, as jpeg-turbo's `neon` (jpeg-turbo.cabal:214-215): three more stanzas, for one decode per picked image. |
| D8 | **App fallbacks kept; iOS unchanged.** The platform path is kept for formats other than JPEG, PNG and WebP: HEIC on Android (Images.android.kt:99-100, ComposeView.kt:382), pasted and camera images on iOS (`.simpleImage`, ComposeView.swift:1596-1597), other types picked on iOS (ImagePicker.swift:92-95). Each app ships with its core; the fallbacks are kept for those formats. | (a) all kept.<br>(b) Android and iOS fallbacks removed: HEIC, pasted and camera images fail. |
| D9 | **CLI profile images from WebP; `/img` unchanged.** `loadImageData` accepts `.webp`; `/img` keeps `.jpg .jpeg .png .gif` (Library/Commands.hs:204-205), because a WebP file is sent as it is and desktop shows a received WebP file only as its preview (product/gaps.md:231). Bot libraries are unchanged and accept WebP with the next pinned core release (packages/simplex-chat-nodejs/src/release.ts:3 `core-7.1.4.0`, packages/simplex-chat-python/src/simplex_chat/_version.py:8 `core-7.1.3.0`). | (a) profile images only.<br>(b) `/img` too: WebP files that desktop recipients open only as previews. |
| D10 | **Link previews in the second merge.** In whichever of #8297 and this pull request is merged second, `image/webp` is added to the image types of `pageContent` (LinkPreview.hs:505) and to `Accept` (:388), with two cases in `/SimpleX chat client/link preview tests/` (tests/ChatTests/LinkPreviews.hs). `og:image` responses are decoded by signature (plans/2026-11-21-link-previews-core.md:274 on `ep/link-previews`). | (a) in the second merge.<br>(b) a commit on `ep/link-previews` now: its tests fail until WebP decoding is in master. |
| D11 | **Desktop picker (Q2 b).** Desktop: `.webp` is animated only with the animation flag of its `VP8X` chunk; the preview of an `AnimatedImage` with the WebP signature is made by the core first, on Android and desktop; the preview of a GIF is made from the bitmap alone, as now. Details in "Apps". | Q2. |
| D12 | **Licence notices.** libwebp's BSD-3-Clause `COPYING` and its `PATENTS` grant; details in "Licence notices". | |
| D13 | **Security: limits, pinned libwebp, conformance and sanitizer runs in CI.** Threat model and measures in "Security". | |
| D14 | **Binary size measured in CI.** Method in "Binary size"; figures in the pull request before review. | |

## Open questions

```mermaid
flowchart TD
  Q1{"Q1 libwebp commit"} -->|"b, recommended: main"| Q2{"Q2 desktop picker"}
  Q1 -->|"a: v1.6.0"| Q2
  Q2 -->|"b, recommended: core and desktop"| B["build"]
  Q2 -->|"a: core only"| B
```

| Q | Question | Options | Recommendation |
|---|---|---|---|
| Q1 | libwebp commit of the submodule | (a) `v1.6.0` (`4fa21912`, 7 Jul 2025), the latest release: decodes within `maxDecodeMemoryMB` only with a lower canvas limit for lossless and alpha input; refuses lossless images above about 134 million pixels on desktop.<br>(b) main `f054d0c3` (21 Nov 2026), unreleased: the memory fixes and windows of rows below; the lossless decoder rewritten after `v1.6.0`, the last change one day before the pin.<br>Details: "Q1: libwebp commit". | (b) |
| Q2 | Desktop picker in this pull request | (a) core only: desktop keeps the alert for every WebP image.<br>(b) core and desktop picker (D11): about 20 lines of Kotlin, desktopTest cases and WebP cases in the desktop-linux smoke test. | (b) |

Implemented with the recommended options, pending the answers: Q1 (b), libwebp main `f054d0c3`; Q2 (b).

### Q1: libwebp commit

Commits after `v1.6.0` to the 50 C files of D7, up to `f054d0c3` (`git rev-list`, `git log` in the submodule):
- 148 commits, 119 of them without merges; 20 in November 2026;
- src/dec/vp8l_dec.c: 807 lines added, 385 removed;
- `git describe --tags f054d0c3`: `v1.6.0-420-gf054d0c3`; `v1.6.0` is the latest release tag.

Fixes in (b) on paths that `decodeWebp` reaches:

| Commit | Date | Change |
|---|---|---|
| `5755839a` | 24 Mar 2026 | memory of Huffman tables limited for images not encoded by libwebp |
| `36f34452` | 18 Sep 2026 | Huffman table allocation sized by the groups in use |
| `e7a90451` | 22 Sep 2026 | window of rows in place of the full-frame pixel buffer of the lossless decoder |
| `bb3ad5bc` | 24 Sep 2026 | window of rows in place of the full alpha plane |
| `0ff181e8` | 24 Sep 2026 | truncated Huffman-table streams refused before allocation |
| `f9c049d5` | 13 Nov 2026 | a paletted alpha stream truncated at its last pixel refused; `v1.6.0` accepts it (`br->eos && pos < end`, vp8l_dec.c:1119 at `v1.6.0`) |

Fixes in (b) outside the paths or limits of `decodeWebp`:
- `d29e00aa` (`ZeroFillFrameRect`, anim_decode.c): called only from `WebPAnimDecoderGetNext` (anim_decode.c:386, 395 at `f054d0c3`), outside the 9 functions that the binding imports (hs-jpeg-turbo: JuicyPixels-webp).
- `54f23b04`: the `_WIN32` branch of src/dsp/cpu.h; the DSP initialisation locked on Windows, as `WEBP_USE_THREAD` is defined by the cabal file; desktop on Windows only.
- `ace1fa52`: 32-bit overflows of row × stride; with sides of at most 16384 and 4 bytes per pixel, every offset is below 2^30.

Counter-risk of (b), unreleased rewrites of the lossless decoder:
- `9a151dc6` (15 Apr 2026): alpha decoding reorganised;
- `e7a90451` (22 Sep) and `bb3ad5bc` (24 Sep): the windows of rows;
- `e9e8ece3` (4 Nov): literal decoding;
- `03816d47` and `b4254dca` (12 Nov): colour cache insertion;
- `820b4c71` (18 Nov) and `ff3953d7` (20 Nov): `DecodeImageData`.

Defects of code added after `v1.6.0`, fixed on main:
- `73730aba` (24 Aug 2026): valid images with more than 200 Huffman groups decoded to wrong pixels or refused; fixed by `21c53c13` (6 Oct 2026), 43 days later.
- `bb3ad5bc`: the alpha window shifted wrongly with cropping; fixed by `a53957a4` (7 Oct 2026); `decodeWebp` decodes without cropping.

Measures against the counter-risk:
- libwebp's fuzzers run on main in OSS-Fuzz (tests/fuzzer/oss-fuzz/build.sh);
- hs-jpeg-turbo CI: images taller than the windows of rows decoded and compared with their sources, also with sanitizers and on their truncated and changed copies ("hs-jpeg-turbo: JuicyPixels-webp").

Memory, measured in CI ([run](https://github.com/evgeny-simplex/hs-jpeg-turbo/actions/runs/45103156185) of branch `ep/webp-measure`, commit `82639d8`): the heap high-water mark of libwebp (`PRINT_MEM_INFO`, src/utils/utils.c:27-52 at `f054d0c3`) and the maximum resident set of `.github/scripts/webp.c`, which decodes as `decodeWebp` does, with the same 50 files and `cc-options`. The canvas, allocated outside libwebp, is the limit of mobile builds (256 MiB, half of `maxDecodeMemoryMB`) or of desktop builds (512 MiB).

| Image | Canvas | (a) heap | (a) process | (b) heap | (b) process |
|---|---|---|---|---|---|
| lossless, opaque, 16383×5461 | 256.0 MiB | 342.4 MiB | 600.0 MiB | 11.6 MiB | 269.5 MiB |
| lossless, alpha, 16383×4096 | 256.0 MiB | 257.1 MiB | 514.8 MiB | 11.7 MiB | 269.7 MiB |
| lossy, alpha in 64-pixel blocks with the default alpha filter, 16383×4096 | 256.0 MiB | 195.1 MiB | 387.8 MiB | 12.5 MiB | 270.2 MiB |
| lossy, alpha gradient without alpha filter, 16383×4096 | 256.0 MiB | 387.0 MiB | 579.9 MiB | 14.4 MiB | 272.1 MiB |
| lossy, opaque, 16383×5461 | 256.0 MiB | 2.0 MiB | 260.8 MiB | 1.4 MiB | 261.2 MiB |
| lossless, opaque, 16383×10922 | 511.9 MiB | refused | refused | 11.6 MiB | 525.7 MiB |
| lossless, alpha, 16383×8192 | 512.0 MiB | refused | refused | 11.6 MiB | 525.6 MiB |

- The heaps of the two lossy images with alpha at (a) differ by 192 MiB, 3 bytes per pixel: the lossless alpha stream is decoded with 1 or with 4 bytes per pixel, by its transforms and colour cache (vp8l_dec.c:1644-1653 at `v1.6.0`).
- Refused at (a): the pixel buffer of the lossless decoder, 4 bytes per pixel of the whole image (`AllocateInternalBuffers32b`, vp8l_dec.c:1556-1574 at `v1.6.0`), exceeds the 512 MiB of one allocation (`WEBP_MAX_ALLOCABLE_MEMORY`), so lossless images above about 134 million pixels fail with `out of memory`.
- At (a), a decode on mobile takes up to 600 MiB besides the input, above `maxDecodeMemoryMB` (512 MiB); D6 then requires a lower canvas limit for lossless input and for lossy input with alpha. At (b), at most 272 MiB.

## Decoding

Composition:

```mermaid
flowchart LR
  subgraph HJT["simplex-chat/hs-jpeg-turbo"]
    JT["JuicyPixels-jpeg-turbo, jpeg-turbo: mozjpeg"]
    SP["JuicyPixels-spng: libspng, miniz"]
    WP["JuicyPixels-webp, new: libwebp decoder and demuxer"]
  end
  IMG["Image.hs: decodeResizable"] --> JT
  IMG --> SP
  IMG --> WP
  MF["Mobile/File.hs: chat_resize_image*"] --> IMG
  CMD["Library/Commands.hs: imagePreview, loadImageData"] --> IMG
  LP["LinkPreview.hs, #8297: imageDataPreview"] --> IMG
  APPS["apps, bot libraries"] --> MF
```

`decodeResizable` (Image.hs:69-77):

```mermaid
flowchart TD
  IN["input bytes"] --> SIG{"signature"}
  SIG -->|"FF D8"| JPEG["decodeDisplayJpeg"]
  SIG -->|"89 50 4E 47 0D 0A 1A 0A"| PNG["Spng.decodePng, half of maxDecodeMemoryMB"]
  SIG -->|"RIFF, 4 bytes, WEBP: new"| WEBP["Webp.decodeWebp, half of maxDecodeMemoryMB"]
  SIG -->|other| ERR["Left: unsupported image format"]
  JPEG --> CP["colourProfile of the ICC profile, forced"]
  PNG --> CP
  WEBP --> OR["orientWebp: orientImage by exifOrientation of the EXIF payload, new"]
  OR --> CP
  CP --> OUT["image and ColourProfile"]
```

- WebP input: bytes 0-3 `RIFF` and bytes 8-11 `WEBP`.
- ``Webp.decodeWebp (Webp.DecodeOptions $ maxDecodeMemoryMB `div` 2)``, as `Spng.decodePng` (Image.hs:76).
- `orientWebp`: the image turned by `orientImage` with `exifOrientation` of the `EXIF` payload, as JPEG input with the APP1 `Exif` segment (Image.hs:83); the `ICCP` payload passed on.
- The profile is passed to `colourProfile` and forced with `$!` (Image.hs:72), so the `ColourProfile` references no part of the input.

`decodeWebp`:

```mermaid
flowchart TD
  A["maxMemoryMB below 1"] -->|yes| L0["Left: invalid decode options"]
  A -->|no| B["WebPDemuxInternal, whole input"]
  B -->|NULL| L1["Left: invalid WebP container"]
  B --> C["canvas width, height, flags, frame count"]
  C -->|"a side above 16384"| L2["Left: image side exceeds 16384"]
  C -->|"canvas bytes above maxMemoryMB MiB"| L3["Left: image is too large"]
  C --> D["frame 1, ICCP chunk 1 and EXIF chunk 1"]
  D --> E["canvas buffer: RGB or RGBA, zeroed for an animation"]
  E --> F["WebPDecode of frame 1 into the canvas at its offset"]
  F -->|"status other than OK"| L4["Left: the status name"]
  F --> G["ImageRGB8 or ImageRGBA8; Metadata: ICCP and EXIF payloads or Nothing"]
```

Pixels (D4):

| Input | libwebp mode | Constructor |
|---|---|---|
| still image, frame without alpha | `MODE_RGB` | `ImageRGB8` |
| still image, frame with alpha | `MODE_RGBA` | `ImageRGBA8` |
| animation | `MODE_RGBA` | `ImageRGBA8`, transparent outside frame 1 |

- Frame alpha: `has_alpha` of the frame iterator (demux.h:157).
- Animation: the animation flag `0x02` of the format flags (mux_types.h:34); the background colour is ignored, as by `WebPAnimDecoder`, which zeroes the canvas of a key frame (anim_decode.c:355-357).

Limits (D6):
- canvas sides at most 16384, the value of `maxImageSide` (Image.hs:309-310), read from the demuxer and checked before the canvas is allocated (the demuxer's own records: Security); frames are at most 16384 per side by format (format_constants.h:29), canvases up to 2^24 (:82)
- canvas bytes, 3 or 4 per pixel, at most `maxMemoryMB` MiB, checked before the canvas is allocated
- `maxMemoryMB` from `decodeResizable`: half of `maxDecodeMemoryMB`, so that one more copy of the canvas also fits: the turned copy of `orientImage` for EXIF orientations 2 to 8, made from the canvas, and the premultiplied copy of `scaleRGBA8` (Image.hs:338-346), made from the turned image, by which the canvas is replaced; at `f054d0c3` the pixel buffer of the lossless decoder contains a window of rows, not a second frame (`GetWindowRows`, src/dec/vp8l_dec.c:1217-1239); the heap of libwebp measured at most 14.4 MiB for a 256 MiB canvas (Q1)
- each libwebp allocation at most 512 MiB: `-DWEBP_MAX_ALLOCABLE_MEMORY=536870912` (src/utils/utils.h:34-41, src/utils/utils.c:173-176); the default is 16 GiB on 64-bit targets
- input of the path and descriptor functions at most `maxDecodeMemoryMB` MiB (Mobile/File.hs:179-190), unchanged
- link preview images at most 5 MiB (LinkPreview.hs:88-89), unchanged
- the image at full size, halved to at most 4320 per side by `fitImage` (Image.hs:225), as PNG
- `use_threads` 0; `WEBP_USE_THREAD` defined, for the locked DSP initialisation: at `v1.6.0` on targets other than Windows (src/dsp/cpu.h:192-204), at main on Windows as well (`54f23b0`)

Errors:

| Condition | `Left` |
|---|---|
| `maxMemoryMB` below 1 | `invalid decode options` |
| `WebPDemuxInternal` returns NULL: truncated input, wrong `RIFF` size, invalid chunks | `invalid WebP container` |
| a canvas side above 16384 | `image side exceeds 16384` |
| canvas bytes above `maxMemoryMB` MiB | `image is too large` |
| `WebPDecode` status (decode.h:236-243) | `out of memory`, `invalid parameter`, `bitstream error`, `unsupported feature`, `not enough data` |

Resizing, output formats, `ColourProfile` and the C functions are unchanged.

## hs-jpeg-turbo: JuicyPixels-webp

Branch `ep/webp` of evgeny-simplex/hs-jpeg-turbo from master `c1b5080`; simplex-chat/hs-jpeg-turbo#3.

Package `JuicyPixels-webp`:
- `cabal-version: 3.0`, version `0.1.0.0`
- synopsis "JuicyPixels wrappers for libwebp"
- description "WebP decoding of JuicyPixels images with libwebp."
- `license: BSD-3-Clause`
- `license-files`:
  - `LICENSE`, with "Copyright 2026 SimpleX Chat Ltd"
  - `cbits/libwebp/COPYING`
  - `cbits/libwebp/PATENTS`
- `cc-options`:
  - `-DWEBP_USE_THREAD`
  - `-DWEBP_MAX_ALLOCABLE_MEMORY=536870912`
- `include-dirs: cbits/libwebp`; `HAVE_CONFIG_H` undefined
- `extra-source-files`: `CHANGELOG.md` and `cbits/libwebp/src/**/*.h`
- `build-depends`:
  - `JuicyPixels`
  - `base`
  - `bytestring`
  - `transformers`
  - `vector`
- module `Codec.Picture.Webp` (`.hsc`), exports `DecodeOptions (..)`, `Metadata (..)` and `decodeWebp`; FFI to:
  - `WebPDemuxInternal`
  - `WebPDemuxGetI`
  - `WebPDemuxGetFrame`
  - `WebPDemuxReleaseIterator`
  - `WebPDemuxGetChunk`
  - `WebPDemuxReleaseChunkIterator`
  - `WebPDemuxDelete`
  - `WebPInitDecoderConfigInternal`
  - `WebPDecode`
- the demuxer and the iterators released with `bracket` on every path
- the canvas a pinned vector that becomes the image data without a copy, as in `Spng.decodePng` (Spng.hsc:56-63)
- the `ICCP` and `EXIF` payloads slices of the input bytes

C sources (D7), as `dec_srcs`, `demux_srcs`, `dsp_dec_srcs` and `utils_dec_srcs` of libwebp's Android.mk:47-106 and 162-172, without MIPS and MSA; 23,492 lines at `v1.6.0`:

| Directory | Files |
|---|---|
| `src/dec` | `alpha_dec.c`<br>`buffer_dec.c`<br>`frame_dec.c`<br>`idec_dec.c`<br>`io_dec.c`<br>`quant_dec.c`<br>`tree_dec.c`<br>`vp8_dec.c`<br>`vp8l_dec.c`<br>`webp_dec.c` |
| `src/demux` | `anim_decode.c`<br>`demux.c` |
| `src/dsp` | `alpha_processing.c`<br>`alpha_processing_neon.c`<br>`alpha_processing_sse2.c`<br>`alpha_processing_sse41.c`<br>`cpu.c`<br>`dec.c`<br>`dec_clip_tables.c`<br>`dec_neon.c`<br>`dec_sse2.c`<br>`dec_sse41.c`<br>`filters.c`<br>`filters_neon.c`<br>`filters_sse2.c`<br>`lossless.c`<br>`lossless_neon.c`<br>`lossless_sse2.c`<br>`lossless_sse41.c`<br>`rescaler.c`<br>`rescaler_neon.c`<br>`rescaler_sse2.c`<br>`upsampling.c`<br>`upsampling_neon.c`<br>`upsampling_sse2.c`<br>`upsampling_sse41.c`<br>`yuv.c`<br>`yuv_neon.c`<br>`yuv_sse2.c`<br>`yuv_sse41.c` |
| `src/utils` | `bit_reader_utils.c`<br>`color_cache_utils.c`<br>`filters_utils.c`<br>`huffman_utils.c`<br>`palette.c`<br>`quant_levels_dec_utils.c`<br>`random_utils.c`<br>`rescaler_utils.c`<br>`thread_utils.c`<br>`utils.c` |

- Excluded:
  - `src/enc`
  - `src/mux`
  - `sharpyuv`
  - `src/dsp/lossless_avx2.c`
  - the encoder files of `src/dsp` and `src/utils`
  - `examples`
  - `imageio`
- SIMD by compiler macros (src/dsp/cpu.h:47-137): SSE2 on x86-64; NEON on aarch64, and on armv7 when the toolchain defines `__ARM_NEON__`; the C code paths otherwise. Without `-msse4.1`, the SSE4.1 files compile to init stubs (src/dsp/dec_sse41.c:45-49), which are called only with `WEBP_HAVE_SSE41` (src/dsp/lossless.c:655-660).

Repository files:
- `.gitmodules`: `JuicyPixels-webp/cbits/libwebp`, `https://github.com/webmproject/libwebp`, at the commit of Q1
- `cabal.project`, `stack.yaml`, `stack.ghc-8107.yaml`: the package added
- `.github/workflows/build.yml`:
  - before `cabal test`, in the shared steps: libwebp-test-data cloned at `06ddd96e` into `$RUNNER_TEMP`; `.github/scripts/webp-encode.c`, compiled with all C files of the submodule, writes the tall images into the same directory; `WEBP_TEST_DATA` set
  - armv7 job: `bash .github/scripts/webp-check.sh` after `png-check.sh`
  - each matrix job: the size of `libHSJuicyPixels-webp-*.a` in the job summary
  - `workflow_dispatch` input `full`: the jobs `build-full` and `armv7` also on a dispatch
- `.github/scripts/webp-encode.c`, the tall images:
  - 1024×4096 RGBA; the window of the lossless decoder at width 1024 is 2094 rows (`VP8LGetWindowRows`, src/dec/vp8l_dec.c:1217-1233 at `f054d0c3`), so the window is shifted twice during a decode
  - the left half noise, the right half gradients, alpha in 16×16 blocks of 32 levels; the image repeats every 960 rows, a multiple of the transform tiles, so that each pixel after the first 960 rows equals the pixel 983,040 positions back, within the 1024 rows that the window keeps (`GetMaxDistanceRows`, :1210-1215); with this period the lossless file is 2,330,300 bytes, with a period of 1023 rows 5,753,236
  - `tall-lossless.webp`: lossless, `exact`, quality 80, above which the encoder searches its whole LZ77 window (src/enc/backward_references_enc.c:240-248 at `f054d0c3`); `tall-lossless.pam`: the source pixels
  - `tall-lossy-alpha.webp`: lossy at method 0, the alpha plane lossless without filtering; at method 0 the alpha plane is coded with the colour-indexing transform alone and without colour cache (src/enc/vp8l_enc.c:381-388, src/enc/backward_references_enc.c:885-888 at `f054d0c3`), which the decoder reads with 1 byte per pixel in its own window of rows (src/dec/vp8l_dec.c:2055-2059 at `f054d0c3`, `ShiftWindow8b`); `tall-lossy-alpha.pgm`: the source alpha
- `.github/scripts/webp-check.sh`, as `png-check.sh` (sources and `cc-options` read from the cabal file):
  - each conformance file decoded by `.github/scripts/webp.c` as by `decodeWebp`, and written as PAM or PPM
  - armv7 with `-mfpu=vfpv3-d16` (C code paths) and aarch64 (NEON): outputs identical and equal to `libwebp_tests.md5`
  - the tall images decoded by the armv7, aarch64 and sanitizer builds: outputs identical; `tall-lossless.webp` equal to its source pixels
  - aarch64, built by clang 18 with `-fsanitize=address,undefined`: the conformance files and the tall images, each also truncated at 64 lengths and with 64 single-byte changes; a truncated copy has its `RIFF` size and the size of its last chunk set to the remaining length, so that the cut bitstream reaches the decoder, as `WebPDemuxInternal` refuses input shorter than its `RIFF` size (src/demux/demux.c:756-757 at `f054d0c3`); every run exits with 0 or a decoder status; GCC instruments the Neon additions of its `arm_neon.h` as `signed char` arithmetic
  - `size` of the armv7 and aarch64 objects in the job summary
- `.github/scripts/coverage.sh`: the hpc report of `Codec.Picture.Webp`; the gcov line coverage of `ShiftWindow`, `ShiftWindow8b`, `ShiftWindow32b`, `VP8LShiftWindowBuffer` and `WebPShiftAlphaWindow` in the package tests

Package tests, `JuicyPixels-webp/test/WebpTest.hs` (tasty, as `SpngTest.hs`):
- conformance: every `.webp` with a `.pam` or `.ppm` entry in `libwebp_tests.md5` of `$WEBP_TEST_DATA`, 96 files; the decoded image, written as dwebp writes PAM for RGBA and PPM for RGB (imageio/image_enc.c:223-250), has the listed md5 (`cryptohash-md5`); the group fails when `WEBP_TEST_DATA` is unset
- animation: a container assembled in the test from two still frames of the conformance files (`VP8X` with the animation flag, `ANIM`, two `ANMF` with offsets) is decoded as frame 1 at its offset on a transparent canvas
- metadata: the payloads of the `ICCP` and `EXIF` chunks of a `VP8X` container are returned; without the chunks, `Nothing`
- limits: with `maxMemoryMB` 1, a 512×512 RGBA canvas is decoded and a 513×512 one refused; a canvas side of 16385 and `maxMemoryMB` 0 are refused
- malformed input, refused: empty input, `RIFF` without `WEBP`, truncations, a `RIFF` size beyond the input
- sliding window: `tall-lossless.webp` decoded to the pixels of `tall-lossless.pam`; the alpha of `tall-lossy-alpha.webp` equal to `tall-lossy-alpha.pgm`

## Apps (Q2 b)

```mermaid
flowchart LR
  subgraph Before
    W1["desktop: .webp picked"] --> A1["animated by extension"] --> B1["AnimatedImage"] --> C1["bitmapPreview: ImageIO.read returns null"] --> D1["alert Decoding error, media dropped"]
  end
  subgraph After
    W2["desktop: .webp picked"] --> A2{"VP8X animation flag"}
    A2 -->|unset| S2["SimpleImage: preview and JPEG or PNG file from the core"]
    A2 -->|set| N2["AnimatedImage: preview of frame 1 from the core, the file sent as it is"]
  end
```

- `readFileHeader(uri, size)` in common/platform/Images.kt: the first `size` bytes of the file, read through `URI.inputStream` (common/platform/Files.kt:192); `null` for a shorter file or a read error. `isWebpFile(uri)`: the first 12 bytes match `ImageFileType.WEBP` (Images.kt:32-41).
- `isAnimImage` of desktop (desktop/platform/Images.desktop.kt:175-178): true for `.gif`, and for `.webp` when `readFileHeader(uri, 21)` returns `RIFF`, 4 bytes, `WEBP`, `VP8X`, 4 bytes, and a byte with bit `0x02`.
- `processPickedMedia`, `AnimatedImage` branch (common/views/chat/ComposeView.kt:375): `preview = if (isWebpFile(uri)) resizeImageFileToStrSize(uri, maxDataSize = 14000) ?: bitmapPreview() else bitmapPreview()`, on Android and desktop. The preview of a GIF is made by `bitmapPreview` alone, as on master.
- Sending is unchanged: `SimpleImage` through `saveImage` or `desktopSaveImageInTmp` (Utils.kt:174-216), `AnimatedImage` as the file (ComposeView.kt:922-924).
- Specs: `apps/multiplatform/spec/client/compose.md:318`, step 2 of `processPickedMedia`; `apps/multiplatform/product/gaps.md:231`, the clause on picked WebP images.
- User guide: the image steps of `docs/guide/send-messages.md:34-50` stay true and unchanged.

## Link previews, after both merges (D10)

```mermaid
flowchart LR
  R["response to a link"] --> T{"Content-Type"}
  T -->|"text/html, application/xhtml+xml"| H["head scanner, then og:image"]
  T -->|"image/jpeg, image/png, image/webp: new"| I["the image itself"]
  T -->|other| F["failure with the type"]
  H --> D["imageDataPreview: decoded by signature"]
  I --> D
```

- `pageContent` (LinkPreview.hs:505): `["image/jpeg", "image/png", "image/webp"]`.
- `Accept` (LinkPreview.hs:388): `text/html,application/xhtml+xml,image/png,image/webp,image/jpeg;q=0.9,*/*;q=0.1`.
- Cases in `/SimpleX chat client/link preview tests/` (tests/ChatTests/LinkPreviews.hs at `3771cd6a`, the fetch tests with a local HTTPS server; `/Link preview/` holds the unit tests without servers):
  - a route `/image.webp` with `Content-Type: image/webp` beside `/image.png` (:340-341, :364);
  - a direct link to `/image.webp`, as `testImagePreview` (:141-148): a preview titled `image.webp`;
  - a page whose `og:image` is `/image.webp`, as `testPagePreview` (:121-130): a preview with the image.

## Builds

| Build | Change |
|---|---|
| `cabal.project:76-80` | tag of the hs-jpeg-turbo commit; `subdir: JuicyPixels-jpeg-turbo JuicyPixels-spng JuicyPixels-webp jpeg-turbo` |
| `scripts/nix/sha256map.nix:12` | hash of that commit, computed with submodules |
| `simplex-chat.cabal:391-394` | `JuicyPixels-webp ==0.1.*` in the library |
| nix: Android aarch64 and armv7a, iOS, iOS simulator, macOS (flake.nix) | unchanged; the package is built by haskell.nix as `JuicyPixels-spng` is; its licence, BSD-3-Clause, is accepted as declared (flake.nix:73-75 overrides `JuicyPixels-spng` only) |
| Android armv7a, GHC 8.10.7 (flake.nix:11-13, :344-346) | `JuicyPixels-webp` built and its tests passed with GHC 8.10.7 on ubuntu-22.04 in hs-jpeg-turbo CI ([run](https://github.com/evgeny-simplex/hs-jpeg-turbo/actions/runs/45286775715)); pending: the core built by android-emu with the input `armv7a`; the armv7a build of this branch is stopped by a compilation error of `wide-word`, a dependency unchanged from master, before `JuicyPixels-webp` is built ([run](https://github.com/evgeny-simplex/simplex-chat/actions/runs/45147694496)) |
| Windows (`.github/workflows/build.yml:1652-1655, 1778, 1820-1822`; `scripts/desktop/build-lib-windows.sh:43`) | unchanged: the libwebp submodule is removed with the others by the glob `dist-newstyle/src/hs-jpeg-t*` |
| reproducible Android build | unchanged: submodules are updated after checkout (`scripts/simplex-chat-reproduce-builds-android.sh:103`) |

Until the pull request in simplex-chat/hs-jpeg-turbo is merged, `cabal.project`, `sha256map.nix` and the `JuicyPixels-webp` row of docs/dependencies/HASKELL.md reference the commit of evgeny-simplex/hs-jpeg-turbo. After the merge, one commit changes them to the squash commit of simplex-chat/hs-jpeg-turbo:
- `cabal.project`: `location` and `tag`;
- `scripts/nix/sha256map.nix`: the URL, the commit and its hash, computed with submodules;
- docs/dependencies/HASKELL.md: the links of the rows of `JuicyPixels-jpeg-turbo` (:26), `JuicyPixels-spng` (:27), `JuicyPixels-webp` (:29) and `jpeg-turbo` (:146), which name `d0a4d95` or the fork commit until then.

## Licence notices

- `docs/dependencies/README.md`, after libspng:
  `[libwebp](https://chromium.googlesource.com/webm/libwebp): WebP decoding of resized images ([BSD-3-clause](./licences/apps/libwebp/COPYING) and [patent grant](./licences/apps/libwebp/PATENTS))`
- `docs/dependencies/licences/apps/libwebp/COPYING` and `PATENTS`: copies from the pinned commit.
- `docs/dependencies/HASKELL.md`:
  - direct dependency, after `JuicyPixels-stbir` (:28): `JuicyPixels-webp` 0.1.0.0, the hs-jpeg-turbo commit of `cabal.project` ("Builds"), `BSD-3-Clause`, "JuicyPixels wrappers for libwebp"
  - `JuicyPixels-webp` added to the dependents of `JuicyPixels` (:25), `bytestring` (:11) and `vector` (:55)
- `docs/dependencies/licences/haskell/JuicyPixels-webp-0.1.0.0/LICENSE`, with "Copyright 2026 SimpleX Chat Ltd".
- `docs/dependencies/README.md`, after the list: the WebP test images in `tests/fixtures/webp` are made from the repository's own test images (Tests, "Fixtures"), under the licence of this repository.

## Security

Threat model:

| Who | Supplies | Through |
|---|---|---|
| website owner | a crafted WebP image | link previews (#8297), for every user who types a link to the site |
| contact | a crafted WebP file | a received file that the user picks and sends again |
| bot operator | own input | `resizeImage` of the bot libraries |

- Received images are displayed by the apps' platform decoders. The core decodes images picked in the apps, sent with `/img`, set as profile images and fetched for link previews (callers of `decodeResizable`: Mobile/File.hs:206, Library/Commands.hs:223, :237, LinkPreview.hs via Image.hs:229-232).
- Measures:
  - the limits of "Decoding"
  - libwebp pinned by submodule commit (Q1)
  - conformance, armv7 and sanitizer runs in hs-jpeg-turbo CI, with images taller than the window of the lossless decoder
  - libwebp's fuzzers, run upstream on OSS-Fuzz (tests/fuzzer, tests/fuzzer/oss-fuzz/build.sh)
- WebP is decoded in the chat process, as JPEG by mozjpeg and PNG by libspng; the `EXIF` payload is read by `exifOrientation` in Haskell, as the APP1 segment of JPEG input.
- Memory of the demuxer before the limits of "Decoding": `WebPDemuxInternal` allocates a 24-byte record (12 bytes on 32-bit targets) for each chunk of an unknown type after `VP8X` (`ParseVP8XChunks`, demux.c:531-545 at `f054d0c3`; `StoreChunk`, :378-388), before `decodeWebp` checks the canvas. The number of chunks is bounded by the input size: at most one per 8 bytes. Measured in CI ([run](https://github.com/evgeny-simplex/hs-jpeg-turbo/actions/runs/45103156185)), the maximum resident set of `.github/scripts/webp.c` for files of empty chunks after `VP8X`, input buffer included, at `f054d0c3`; at `v1.6.0` within 0.4 MiB of these:

  | Input | Limit of | Chunks | Maximum resident set |
  |---|---|---|---|
  | 5 MiB | link preview images (LinkPreview.hs:88-89 on `ep/link-previews`) | 655,356 | 27.0 MiB |
  | 512 MiB | files read by the core in mobile builds (Mobile/File.hs:179-190) | 67,108,860 | 2.50 GiB |
  | 1 GiB | files read by the core in desktop builds | 134,217,724 | 5.00 GiB |

  A website owner reaches at most the first row; a contact, through a received file that the user picks, the second or the third.

## Binary size

Measured in CI and reported in the pull request before review:
- hs-jpeg-turbo, [run](https://github.com/evgeny-simplex/hs-jpeg-turbo/actions/runs/45286775715) of `5b8ae10`: `size` of the 50 decoder objects for armv7 and aarch64 (webp-check.sh); size of `libHSJuicyPixels-webp-*.a` on each matrix OS; the full matrix runs for a pull request ready for review and for a dispatch with the input `full` (hs-jpeg-turbo build.yml, job `build-full`).

  | Build | Bytes | text | data | bss |
  |---|---|---|---|---|
  | 50 objects, armv7 C code paths, GCC 13 `-O2` | | 126,383 | 148 | 1,684 |
  | 50 objects, aarch64 Neon, GCC 13 `-O2` | | 203,488 | 304 | 2,752 |
  | `.a`, Linux x86-64, GHC 9.6.3 | 570,602 | 289,754 | 2,992 | 2,856 |
  | `.a`, Linux x86-64, GHC 8.10.7 | 581,150 | 294,097 | 3,264 | 2,888 |
  | `.a`, Linux aarch64, GHC 9.6.3 | 545,102 | 230,167 | 3,040 | 2,720 |
  | `.a`, macOS aarch64, GHC 9.6.3 | 535,456 | | | |
  | `.a`, macOS x86-64, GHC 9.6.3 | 663,776 | | | |
  | `.a`, Windows x86-64, GHC 9.6.3 | 534,588 | | | |

- pending: simplex-chat `libsimplex.so` of android-emu, aarch64 and armv7a, on this branch and on master; armv7a after the `wide-word` fix ("Builds").

## Tests

`tests/ImageTests.hs`, group "Image resize" (tests/Test.hs:98), new group "WebP decoding":
- decoding, each with `CPNone`, the samples written as dwebp writes PAM or PPM with the md5 of the output of dwebp 1.6.0 (Fixtures), and its data URI with limit 14000:

  | Fixture | Image | md5 | Data URI |
  |---|---|---|---|
  | `lossy.webp`, lossy | `ImageRGB8` 128×128 | `33817498…` of the PPM | `data:image/jpg;base64,` |
  | `lossless-alpha.webp`, lossless with alpha | `ImageRGBA8` 1000×307 | `16ddf590…` of the PAM | `data:image/png;base64,` |
  | `lossy-alpha.webp`, lossy with an `ALPH` chunk | `ImageRGBA8` 1000×307 | `ea2f149f…` of the PAM | `data:image/png;base64,` |

- animation, containers assembled in the test (`VP8X` with the animation flag, `ANIM`, two `ANMF`), decoded as `ImageRGBA8` of the canvas size with frame 1 at its offset and transparent pixels elsewhere:
  - `lossy.webp` at (0, 0) and at (32, 0) on 160×128: the left 128 columns with the samples of `lossy.webp` and alpha 255
  - `lossless-alpha.webp` at (34, 16) and `lossy.webp` at (0, 0) on 1034×323: the samples of `lossless-alpha.webp` at the offset
- colour profile: a `VP8X` container with an `ICCP` chunk of `p3Profile` and the `VP8 ` chunk of `lossy.webp` is decoded with `CPDisplayP3`, and its JPEG and PNG image data with limit 261120 read back as `CPDisplayP3`; with rX 3/65536 above Display P3, `CPNone`
- EXIF orientation: a `VP8X` container with the `VP8L` chunk of `lossless-alpha.webp` and an `EXIF` chunk of orientation 6, the little-endian TIFF data of `orientationTiff`, which `testJpegOrientation` also uses, is decoded as `ImageRGBA8` 307×1000, the pixels of `lossless-alpha.webp` turned by 90 degrees clockwise; the test fails without `orientWebp`
- refusals, `Left`:
  - an animation canvas of 16385×128, frame 1 inside it: `image side exceeds 16384`
  - an animation canvas of 16384×16384, 1 GiB as RGBA, above the limit of every build: `image is too large`
  - `lossy.webp` cut at half its length and by its last byte, and with the `RIFF` size increased by 1: `invalid WebP container`
  - `RIFF`, 4 bytes, `WAVE`: `unsupported image format`

`tests/MobileTests.hs`, "Images" (:103-110), cases added to the existing tests:
- `cChatResizeImageUri` on `lossy.webp`, limit 14000: `data:image/jpg;base64,`
- `cChatResizeImageFile` with `png = -1`, limit 100000, on `lossless-alpha.webp`: the PNG signature

`tests/ChatTests/Profiles.hs`, "set profile image from file" (:76, :1250-1255): `/set profile image file ./tests/fixtures/webp/lossy.webp` sets a `data:image/jpg;base64,` image of at most 12500 characters. Pattern `/SimpleX chat client/profile tests/user profiles/set profile image from file/`, added to the `ci-tests` block with the code.

Fixtures, `tests/fixtures/webp/`, made from the repository's own test images by cwebp of the libwebp 1.6.0 release archive for Linux x86-64 (sha256 `1c5ffab7…`, signature of the WebP release key `6B0E6B70976DE303EDF2F601F9C3D6BDB8232B5D` verified), in CI ([run](https://github.com/evgeny-simplex/hs-jpeg-turbo/actions/runs/45103156185), `.github/scripts/measure-fixtures.sh` of branch `ep/webp-measure`); the md5 sums are of dwebp 1.6.0's output in the same run. In libwebp-test-data at `06ddd96e`, the only licence notice is "This file distributed under the same terms as libwebp" in each of the three `test_*.sh` scripts, which covers those scripts; its images are therefore used in hs-jpeg-turbo CI only, cloned at run time.

| Fixture | Command | Source, master `8f5dfa3d4` |
|---|---|---|
| `lossy.webp` | `cwebp -q 75 -crop 80 152 128 128` | tests/fixtures/test.jpg |
| `lossless-alpha.webp` | `cwebp -lossless -exact -crop 315 80 1000 307`; decoded, equal to the crop of the source pixels | tests/fixtures/logo-large-rgba.png |
| `lossy-alpha.webp` | `cwebp -q 75 -crop 315 80 1000 307` | tests/fixtures/logo-large-rgba.png |

Apps (Q2 b):
- desktopTest:
  - desktop `isAnimImage` on temporary files: a `VP8X` header with the animation flag, true; without it, false; a simple `VP8 ` file, false; `.gif`, true;
  - `isWebpFile` on temporary files: a WebP header in a `.gif` file, true; a GIF in a `.webp` file, false; 11 bytes of a WebP header, false.
- Pending, with the runs of the app workflows on the branch:
  - desktop-linux, `send_images` (scripts/ci/desktop-linux/smoke-test.sh:373-379): `webp/lossy.webp` sent as `.jpg`, and an animated container assembled from the fixtures sent as `.webp`; each with the compose preview and the unchanged count of `core returned no image`; `send_image` takes a shot label besides the extension.
  - android-emu: `lossy.webp` added in its own folder and sent as `.jpg`, with the unchanged count of `core returned no image`; a container assembled from `lossless-alpha.webp` with an `EXIF` chunk of orientation 6, sent as `.png` of 307×1000, as on master.
  - ios-sim: `lossy.webp` added to the photo library and sent; the sent file 128x128; the log lines of the app's own resizing absent.
  - Each workflow run once on the branch, with the app uploaded; READMEs: `scripts/ci/desktop-linux/README.md:27`, `scripts/ci/android/README.md:32`, `scripts/ci/ios-sim/README.md:3`.

Link previews (D10): cases in `/SimpleX chat client/link preview tests/` in the second merge ("Link previews, after both merges").

## Mechanics

hs-jpeg-turbo, branch `ep/webp`: the files of "hs-jpeg-turbo: JuicyPixels-webp".

simplex-chat, branch `ep/core-webp`:
- `Image.hs`:
  - `import qualified Codec.Picture.Webp as Webp`
  - `decodeResizable` (:69-77): the WebP branch before `otherwise`, and `orientWebp`
  - `exifOrientation` (:107-129): the TIFF data as its argument; `decodeDisplayJpeg` (:83) passes `exifTiff segments`
- `Library/Commands.hs`:
  - `loadImageData` (:209-211): `.webp` accepted; error text `(only .png, .jpg, .jpeg, .webp)`
  - comment (:207): `.webp` named
- `Controller.hs:671`, comment: `.webp` named.
- `Options.hs:493`, help: `.png/.jpg/.jpeg/.webp`.
- `docs/CHAT-RELAY.md`:
  - `--user-image-file` (:110): `.png`, `.jpg`, `.jpeg` or `.webp`
  - Docker picture (:172): `.png`/`.jpg`/`.jpeg`/`.webp`
- `cabal.project`, `scripts/nix/sha256map.nix`, `simplex-chat.cabal`: "Builds".
- `docs/dependencies`: "Licence notices".
- Tests and fixtures: "Tests".
- Apps (Q2 b):
  - `common/platform/Images.kt`: `isWebpFile`, `readFileHeader`
  - `desktop/platform/Images.desktop.kt:175-178`
  - `common/views/chat/ComposeView.kt:375`
  - `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/ImageFileTypeTest.kt`
  - specs of "Apps"
  - pending: smoke tests and READMEs of "Tests"

Commits:
1. `docs: plan of WebP images decoded by the core`
2. `core: decode WebP images`: binding pin, `decodeResizable`, profile images, licences, core tests and fixtures, this plan updated
3. `desktop: attach WebP images` (Q2 b): desktop picker, preview line, desktopTest, specs
4. `android, desktop: make animated GIF previews without the core`: the preview line called the core only for WebP files, `isWebpFile`, desktopTest, spec
5. `tests: make the WebP fixtures from the repository's own test images`: fixtures, core tests, docs/dependencies
6. `core: pin JuicyPixels-webp with tests of the lossless decoder's window`: binding pin
7. `docs: complete Q1, the threat model and the tests of the WebP plan`
8. `tests: check the EXIF orientation of picked WebP images`: the test alone, run before the fix
9. `core: apply the EXIF orientation of WebP images`: binding pin, `decodeResizable`, `exifOrientation`
10. `tests: remove the WebP descriptor case that passes without WebP decoding`
11. `docs: state the EXIF orientation and the binary sizes in the WebP plan`
12. pending: `ci: send WebP images in the app smoke tests`

## Out of scope

- WebP encoding.
- Display of received WebP files on desktop (product/gaps.md:231; Images.desktop.kt:31-35).
- Animation of WebP on iOS: an animated WebP is sent as its first frame, as today (SwiftyGif UIImage+SwiftyGif.swift:170-177).
- `/img` with WebP files (D9).
- GIF, HEIC and other formats in the core.
