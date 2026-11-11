# Image resizing in the core

The CLI sends a fixed placeholder as the preview of every image sent with `/img` or `/imgf`, and bots on the Node and Python libraries need their own image resizing code.
Images are resized by the core in one Catmull-Rom pass with sharpening, at a fixed JPEG quality, to the size whose encoding fills 97–100% of the size limit; an image that fits at its largest size is encoded at the highest quality that fits.
JPEG input is decoded by mozjpeg at the smallest scale that covers the display size, and its EXIF orientation is applied; PNG input is decoded by libspng; other formats are refused.
The output has one fixed Display P3 colour profile when the input profile describes Display P3 and the fixed profile is at most a tenth of the size limit, and no profile otherwise.
The core functions are called by the apps in place of platform code, wrapped in both bot libraries and used for `/img` and `/imgf` previews; mozjpeg and libspng are compiled by cabal in every build.

References are to branch `ab/resize-image` at the commit of this plan, based on master `ec8884f46`. Haskell paths without a directory are in `src/Simplex/Chat/`.

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
| D7 | **Apps pass a file reference to the core.** Apps pass the picked file itself, not decoded or re-encoded pixels: iOS a copy of the photo picker's file in the app's temporary directory, desktop the chosen path, Android a file descriptor of the content URI. The core reads the file, decodes it at the needed scale and applies EXIF rotation. | Confirmed, with the comment "Could we pass file reference instead of bytes? I think iOS can create temporary links to chosen photos, so Haskell/C code just reads it instead of piping bytes via FFI. Was it considered? Same for other platforms." The file reference replaced the file bytes of the reviewed version. |
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
| D19 | **PNG decoded by libspng; other formats refused.** PNG input is decoded by libspng 0.7.4 with miniz 3.1.2, bundled in JuicyPixels-spng: the header first, then the pixels, within the side and memory limits. Input other than JPEG and PNG gives `Left`, and `""` from the C API, on which the apps decode it themselves and pass JPEG or PNG bytes. JuicyPixels remains for the image types and the pixel conversions. |
| D20 | **PNG output: Catmull-Rom with sharpening, encoded by libspng.** Each PNG trial is encoded by libspng at zlib level 9 with row filters off and on, and the smaller output is kept; the RGBA source is premultiplied once per image for all trials. Research path, the smaller of two at miniz level 9: 3.19 / 87.72 / 86.73. Resize time of the core on the 8 PNG research inputs, on the development machine: previews 0.57–15.6 s, mean 4.09 s; full images 0.24–20.3 s, mean 4.53 s. |
| D21 | **Nix licence of JuicyPixels-spng.** The nix builds take its licence as "BSD-3-Clause AND BSD-2-Clause AND MIT" (`flake.nix:73-75`). haskell.nix counts a licence as free only when its SPDX list marks it OSI-approved or FSF-libre, which libpng-2.0 is not, and refuses to evaluate the package. The cabal file, the licence files and HASKELL.md keep libpng-2.0. |

Decisions of 27 Oct, after the second review:

| ID | Decision |
|---|---|
| D22 | **iOS removes the copy of a sent image.** The copy of a picked image in the temporary directory is removed after the image is saved for sending, and taken out of `filesToDelete`; the copy of an image that is not sent is removed at app termination, as before. |
| D23 | **File input within the memory limit.** The file and file descriptor functions read at most `maxDecodeMemoryMB` MiB + 1 byte and return `""` for a larger input, also from a pipe; the descriptor is closed on every path. |
| D24 | **Decoding and sharpening memory within the limit.** PNG is decoded with half of `maxDecodeMemoryMB`, which leaves room for the premultiplied copy. Sharpening keeps 5 horizontally blurred rows in place of two whole-image `Float` buffers, with the same arithmetic. Maximum RSS for a 4320×4320 RGBA PNG: 323 MB before and 223 MB after for the image data, 163 MB and 162 MB for the preview. |
| D25 | **One fixed Display P3 profile.** No input profile bytes are copied. When the ICC profile of JPEG input (APP2 `ICC_PROFILE` segments) or of PNG input (iCCP) describes Display P3, the JPEG or PNG output has the fixed Display P3 v4 profile of Compact-ICC-Profiles (CC0, 480 bytes), when its length in output units is at most a tenth of the size limit: bytes for image data, base64 characters for a data URI. Otherwise, and for an inconsistent, malformed or other profile, the output has no profile, byte for byte as for an input without one. |

## Decisions

Decoding, `Image.hs`:
- `decodeResizable bytes`: the image for resizing and its `ColourProfile` of D25, `CPDisplayP3` or `CPNone`; the image at the display size for JPEG, at full size for PNG; `Left` for other formats.
- JPEG, input starting `FF D8`, decoded by mozjpeg:
  1. `readJpegHeader`; `Left` for:
     - a side above 65500
     - a precision other than 8
     - a lossless JPEG
  2. target size: both sides halved, each rounded by `scaledSide` (`:398-399`), while a side exceeds 4320; `Left` when a side is below 2 before a halving
  3. scale: the smallest of `Full`, `Half`, `Quarter` and `Eighth` whose rounded-up sides are at least the target
  4. `decodeJpeg` with:
     - `maxPixels`: 2^28; `Left` above it, before the output is allocated
     - `maxMemoryMB`: `maxDecodeMemoryMB`
  5. a decode larger than the target: `convertRGB8`, then `STBIR.resize` to the target
  6. the EXIF orientation applied
  7. the input ICC profile, `jpegIccProfile`:
     - source: the APP2 segments with the `ICC_PROFILE\0` header before the first SOS, from the segment list that `exifTiff` reads, walked once by `jpegSegments`
     - each segment: the sequence number, the segment count, the profile chunk
     - profile: the chunks ordered by sequence number and concatenated
     - `Nothing` for missing or duplicate sequence numbers, a segment count different from the number of segments, a segment shorter than 14 bytes or an empty profile; the image is decoded as without a profile
- PNG, input starting with the signature `89 50 4E 47 0D 0A 1A 0A`, decoded by libspng with `Spng.decodePng`:
  1. the header (IHDR) first; `Left` for a side above 16384
  2. `maxMemoryMB`: half of `maxDecodeMemoryMB`, so that the decoded pixels and their premultiplied copy fit `maxDecodeMemoryMB`; `Left` when the decoded pixels exceed it, before they are allocated
  3. `Left` for any libspng error, including short image data, a chunk with a wrong CRC, the last chunk among them, a missing IEND, any other truncation and a zTXt or iTXt chunk that decompresses to more than 512 KiB; a PNG cut within the IEND CRC decodes (libspng)
  4. constructor: `ImageRGBA8` for a colour type with alpha or a `tRNS` chunk, otherwise `ImageRGB8`; 16-bit samples converted to 8 bits
  5. the full image, halved to the display size by `fitImage`
  6. the input ICC profile: the decompressed iCCP profile from `Spng.decodePng`; `Left` for a profile that decompresses to more than 1 MiB
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
- `maxDecodeMemoryMB`, a compile-time constant in MiB:
  - the `maxMemoryMB` of mozjpeg
  - twice the `maxMemoryMB` of libspng
  - the largest input of the file and file descriptor functions
- `maxDecodeMemoryMB` is 512 when `mobileBuild` is defined and 1024 otherwise:

  | build | `simplex-chat.cabal` condition | set in |
  |---|---|---|
  | iOS and iOS simulator | `flag(swift)` | `flake.nix:559`, `:596`; `scripts/ci/ios-sim/build-core.sh:27` |
  | Android aarch64 and armv7a | `os(android)` | the target platforms of `flake.nix:422` and `:344` |

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

- `ColourProfile` (D25), `colourProfile` of the input ICC profile of either format, `CPNone` without one, forced with `$!` in `decodeResizable`, so that it holds no reference to the input:
  - header: data colour space `RGB ` at offset 16, PCS `XYZ ` at offset 20
  - tag table at offset 128: a big-endian count, then entries of signature, offset and size; the table inside the profile
  - `rXYZ`, `gXYZ` and `bXYZ`: the first entry of each signature; a size of at least 20, the tag inside the profile, type `XYZ `, 4 reserved bytes, 3 s15Fixed16 values
  - Display P3: all 9 values within 2/65536 of those of `displayP3`; the result is `CPDisplayP3`
  - anything malformed, missing or different: `CPNone`
  - `displayP3`: `profiles/DisplayP3-v4.icc` of Compact-ICC-Profiles at `bdd84663`, 480 bytes, CC0 1.0; its primaries are those of Display P3 (D65 white, Bradford-adapted to D50) within 1/65536

Resizing, `Image.hs`:
- `resizeImage format maxSize image profile`: a JPEG or PNG encoding of at most `maxSize` bytes; `profile` a `ColourProfile`.
- `resizeImageDataUri maxSize image profile`: a data URI of at most `maxSize` characters.
- Colour profile (D25), `keptProfile`:
  - `displayP3`, written by the encoder for `CPDisplayP3` when its length in output units is at most `maxSize div 10`
  - output units: bytes for image data; base64 characters, `4·⌈n/3⌉` for n bytes, for a data URI
  - JPEG: `encodeRGB`, APP2 segments after APP0
  - PNG: `encodeSmallerPng`, an iCCP chunk named "ICC Profile", in both encodings
  - otherwise no profile, and the output is byte for byte the output without a profile
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
  | trial | `scaleRGB8` from the decoded image to the trial size, encoded at the quality; the image at the largest size scaled once, for its trial and the quality bisection | `scaleRGBA8`; `encodeSmallerPng`: libspng at zlib level 9 with row filters off and on, the smaller kept |
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
- Metadata of the input is dropped; the colour profile is set by D25.

Scaling (D3), `Image.hs`:
- `scaleRGB8 image size` and `scaleRGBA8 image size`: the image at `size`.
  - the same size: the image itself, not sharpened (D18)
  - another size: one stb_image_resize pass from the image, Catmull-Rom, edges clamped, on gamma-encoded values; then the unsharp mask
- `scaleRGBA8` (D15, D20):
  1. colour premultiplied by alpha: `(c·a + 127) div 255`, once per `scaleRGBA8 image`, for every size scaled from it
  2. stb_image_resize with `STBIR_FLAG_ALPHA_PREMULTIPLIED`
  3. the unsharp mask on the four channels
  4. colour divided by alpha: `min 255 ((c·255 + a div 2) div a)`; 0 for `a = 0`
- Unsharp mask, the arithmetic of the research code:
  - kernel: Gaussian, sigma 0.6 px, radius `ceiling (3·sigma)` = 2, weights divided by their sum, in `Double`
  - a horizontal pass on the samples, then a vertical pass on its result; edges clamped; sums in `Double`, each pass stored as `Float`
  - output: `floor (v + 0.5·(v − b) + 0.5)`, clamped to 0..255; `v − b` in `Float`
- Unsharp mask, memory: a ring of 5 horizontally blurred rows (2·radius + 1); before output row y, row y + 2 is blurred into the slot of row y − 3, and row y is computed from the ring.
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
- The colour profile is set by D25.
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
- `fd`: a readable file descriptor; the core reads it and closes it, on success and on failure.
- Input from a file or a file descriptor:
  - with a file size: `""` for a size above `maxDecodeMemoryMB` MiB, before reading; otherwise the file is read into one buffer of its size
  - without a file size (a pipe): at most `maxDecodeMemoryMB` MiB + 1 byte is read; `""` when that byte is read
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
  | iOS | `UploadContent.imagePreview()`, `ComposeView.swift:315-320`: `resizeImageToStrSize` of the `UIImage` | `ComposeView.swift:1610`: `saveImage` of the `UIImage` | `.animatedImage`, sent as the file (`ComposeView.swift:1600-1601`) |
  | Android | `processPickedMedia`, `ComposeView.kt:370`, `:382`: `bitmapPreview`, `getBitmapFromUri` then `resizeImageToStrSize` | `resizeImageFromUri`, `Utils.kt:218-227`: `getBitmapFromUri`, then `resizeImageToDataSize` | `UploadContent.AnimatedImage`, sent as the file (`ComposeView.kt:371-380`) |
  | desktop | as Android | as Android | as Android; the image picker offers only GIF, WebP, PNG and JPEG (`Images.desktop.kt:159-166`) |

- Haskell, `Mobile/File.hs`:
  ```haskell
  cChatResizeImageFileUri :: CString -> CInt -> IO CString
  cChatResizeImageFile :: CString -> CInt -> CInt -> IO CString
  cChatResizeImageFdUri :: CInt -> CInt -> IO CString
  cChatResizeImageFd :: CInt -> CInt -> CInt -> IO CString
  ```
  - path: `readFilePath`, `peekCString`, then `hGetImage` in `withBinaryFile`
  - descriptor: `readFd`, `fdToHandle`, then `hGetImage` in `bracket` with `hClose`, by which the handle is closed, also on an exception; the descriptor is closed with `c_close` when `fdToHandle` raises
  - `hGetImage`, with the size from `tryIO (hFileSize h)`:
    - a size: above `maxDecodeMemoryMB` MiB, `fail`, which `runCatchExceptT` turns into `Left`; otherwise `B.hGet` of exactly that size
    - `hFileSize` failed (a pipe): `LB.hGet` of `maxDecodeMemoryMB` MiB + 1 byte, read in chunks of about 32 KB until that size or the end; above `maxDecodeMemoryMB` MiB, `fail`; otherwise `LB.toStrict`
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
  - sending, `imageCryptoFile`: `saveImageFile(url)`; for `nil`, `saveImage(image)`; then the copy is removed and taken out of `filesToDelete`
  - a copy of an image that is not sent is removed at app termination, with the other `filesToDelete`, as before
  - `SimpleX.h`: `chat_resize_image_file`, `chat_resize_image_file_uri`
- Android and desktop:
  - common: `resizeImageFileToStrSize` and `resizeImageFileToDataSize`, on `resizeImageFileUri` and `resizeImageFileData` of each platform:
    - Android: `contentResolver.openFileDescriptor(uri, "r").detachFd()`, then `chatResizeImageFdUri` or `chatResizeImageFd`
    - desktop: `uri.toFile().absolutePath`, then `chatResizeImageFileUri` or `chatResizeImageFile`
    - for `""`: the debug log line `<function>: core returned no image`, and `null`
  - `processPickedMedia` and the Android camera: the preview from the file; the bitmap only for an animated image within the file size limit or when the preview is `null`; the media is skipped when it has no preview
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
  - Node `chatResizeImageUri`: "Resize image to a data URI of at most maxSize characters"
  - Node `chatResizeImage`: "Resize image to JPEG or PNG of at most maxSize bytes"
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
  - The core and the add-on are pinned together by `prepare` (`packages_release.py:362-365`); for nodejs an error is raised by `prepare` until the add-on release of the current sources is published (`:781-782`).

`/img` and `/imgf`:
- The preview is generated from the file: read with `CF.readFile` (encrypted files included), decoded, and resized to a data URI of at most 14000 characters, with the colour profile by D25.
- On any failure the fixed placeholder is sent, as before; a GIF, which `/img` accepts by its extension (`Library/Commands.hs:204`), gets the fixed placeholder.

Native code:
- mozjpeg master `08265790` (libjpeg-turbo 3.0.5 base) is the submodule `jpeg-turbo/cbits/mozjpeg` of simplex-chat/hs-jpeg-turbo; cabal compiles it in the `jpeg-turbo` package of every build:
  - 114 C sources on every target
  - aarch64: 15 Neon sources
  - armv7: `simd/arm/aarch32/jsimd.c`, and 14 Neon sources in the internal library `neon`, compiled with `-mfpu=neon`
  - configuration headers `jconfig.h`, `jconfigint.h`, `jversion.h` and `neon-compat.h` in `cbits`, with the target-dependent values taken from compiler macros; `THREAD_LOCAL` empty on 32-bit Android Arm
  - the C code paths on x86-64
  - built with cabal and the C compiler only
- JPEG input is decoded by the TurboJPEG API of mozjpeg; JPEG output is encoded by `jpeg_turbo_encode` (`cbits/encode.c`) through the libjpeg API, with mozjpeg's default profile.
- libspng 0.7.4 and miniz 3.1.2 are the submodules `JuicyPixels-spng/cbits/libspng` and `JuicyPixels-spng/cbits/miniz` of simplex-chat/hs-jpeg-turbo; cabal compiles `spng.c`, `miniz.c`, `miniz_tdef.c` and `miniz_tinfl.c` in the `JuicyPixels-spng` package of every build, with `SPNG_USE_MINIZ`. PNG input is decoded and PNG output encoded by libspng, with miniz's inflate and deflate.
- Package sources:

  | package | source |
  |---|---|
  | hs-jpeg-turbo (`jpeg-turbo`, `JuicyPixels-jpeg-turbo`, `JuicyPixels-spng`) | simplex-chat/hs-jpeg-turbo at `d0a4d95`, the squash merge of simplex-chat/hs-jpeg-turbo#1 |
  | JuicyPixels-stbir, with nothings/stb as a submodule | simplex-chat/JuicyPixels-stbir at `e0090e1`, the squash merge of simplex-chat/JuicyPixels-stbir#3 |
  | JuicyPixels 3.3.8 | Hackage |

- The hashes of both repositories in `scripts/nix/sha256map.nix` are computed with submodules, as for simplexmq.
- In Windows builds, `dist-newstyle/src/hs-jpeg-t*` and `dist-newstyle/src/JuicyPixe*` are removed beside `direct-sq*` and `simplexmq*`.

License notices, in `docs/dependencies`:
- `README.md`: Compact ICC Profiles, libspng, miniz, mozjpeg with the IJG sentence, and stb_image_resize
- `HASKELL.md`: JuicyPixels, JuicyPixels-jpeg-turbo, JuicyPixels-spng and JuicyPixels-stbir as direct dependencies, jpeg-turbo as an indirect one; the five are added to the "Also depended upon by" and "Depended upon by" cells of the packages they depend on
- `licences/`: the license files of each, copied from the pinned commits; libspng's BSD-2-Clause `LICENSE` and the libpng-2.0 notice of `spng.c`, and miniz's MIT `LICENSE`; Compact-ICC-Profiles' CC0 1.0 `license`, for the profile of D25

Out of scope: WebP, a separate PR after this one.

Later, with the picker rework: on iOS, the preview and the image resized in the `loadFileRepresentation` completion handler, from the picker's file, without the copy in the temporary directory; `loadFromURL` reads a picked still image twice and creates a `UIImage` twice.

## hs-jpeg-turbo fork

simplex-chat/hs-jpeg-turbo at `d0a4d95`:
- mozjpeg compiled by cabal, as in "Native code"; arithmetic-coded JPEGs are decoded, the arithmetic encoder is not compiled.
- `encodeRGB :: EncodableRGB px => Int -> Maybe ByteString -> Image px -> IO ByteString`: `jpeg_turbo_encode` (`cbits/encode.c`, `MozJPEG.hsc`): `jpeg_set_defaults`, `jpeg_set_quality`, the output in a buffer grown by doubling, its size returned as `size_t`; libjpeg errors and warnings end the encoding with their text; the encoder state is kept in one allocation, and it and the output buffer are freed on every path, after an error too.
  - the ICC profile, when given: `jpeg_write_icc_profile`, APP2 segments after APP0; `error` for a length outside 1..16707345 (255 segments of 65519 bytes)
  - `error` when the buffer size cannot be doubled
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
- The bytes per pixel are TurboJPEG's `tjPixelSize` of the pixel format; the output buffer size is their product with the sides.
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
  - ICC profiles of 1, 65519, 65520 and 16707345 bytes: read back by libjpeg's `jpeg_read_icc_profile` (`test/icc.c`), and the JPEG without its APP2 segments equal to the JPEG without a profile; 0 and 16707346 bytes: `error`
  - `encodeRGB` of a pixel format without a pixel size (`TJPF_UNKNOWN`): `error` "encodeRGB: unknown pixel format"
- Tests (`jpeg-turbo/test/FFITest.hs`): mozjpeg's `testimgari.jpg` decoded as `testimgint.jpg` at the four scales; `jpeg_turbo_encode` of a 0×1 image: −1, libjpeg's message "Empty JPEG image (DNL not supported)" and no output buffer; `tjPixelSize` and `tjMCUWidth` equal to TurboJPEG's tables (`test/tables.c`) for every named pixel format and subsampling, `Nothing` for the unknown ones.
- CI:
  - `.github/scripts/encode.c`: baseline and progressive JPEGs at five qualities, and their decodes at the four scales, compared byte for byte between the cabal and CMake builds
  - `armv7-check.sh`, on `ubuntu-24.04-arm`: the cabal sources cross-compiled for armv7 and run natively; Neon selected only with the `linux32` personality; JPEGs and decodes identical with Neon and with the C code paths
  - `png-check.sh`: the libspng test images decoded identically by armv7 and aarch64 builds of the `JuicyPixels-spng` C sources
  - `coverage.sh`, on Linux x86-64 with GHC 9.6.3: the hpc coverage of `TurboJPEG2`, `TurboJPEG3`, `MozJPEG`, `Codec.Picture.JPEGTurbo` and `Codec.Picture.Spng`, and the gcov line and branch coverage of `cbits/encode.c`, in the job log and summary
- `jpeg-turbo`: `THREAD_LOCAL` empty on 32-bit Android Arm (`cbits/jconfigint.h`).
- `JuicyPixels-spng`, new package, module `Codec.Picture.Spng`, synopsis "JuicyPixels wrappers for libspng", description "PNG encoding and decoding of JuicyPixels images with libspng.":
  - `readPngHeader :: ByteString -> IO (Either String PngHeader)`
  - `decodePng :: DecodeOptions -> ByteString -> IO (Either String (DynamicImage, Maybe ByteString))`: the image and the decompressed iCCP profile
  - `PngHeader`: `pngWidth`, `pngHeight`, `pngBitDepth`, `pngColorType`
  - `newtype DecodeOptions = DecodeOptions {maxMemoryMB :: Int}`, at least 1
  - limits:
    - sides 16384 (`spng_set_image_limits`)
    - `spng_set_chunk_limits` 1 MiB and 64 MiB: `Left` for an iCCP chunk that decompresses to more than 1 MiB and for a zTXt or iTXt chunk that decompresses to more than 512 KiB; the chunk data kept by libspng at most 64 MiB
    - decoded pixels `maxMemoryMB`
  - the chunks after the image data are read (`spng_decode_chunks`): `Left` for a wrong CRC of the last chunk and a missing IEND
  - output: `ImageRGBA8` (`SPNG_FMT_RGBA8`) for a colour type with alpha or a `tRNS` chunk, otherwise `ImageRGB8` (`SPNG_FMT_RGB8`)
  - `encodePng :: EncodeOptions -> Image PixelRGBA8 -> IO (Either String ByteString)`: 8-bit truecolour with alpha, to a buffer; the buffer of `spng_get_png_buffer` is taken into the `ByteString` with asynchronous exceptions masked (`mask_`), so that it is freed after one too
  - `EncodeOptions`: `compressionLevel :: Int`, 0..9; `filtering :: Bool`, `False` for `SPNG_DISABLE_FILTERING`; `iccProfile :: Maybe ByteString`, an iCCP chunk named "ICC Profile", `Left` for an empty profile
  - tests (`test/SpngTest.hs`): 16 PngSuite images equal to JuicyPixels' decodes and to a pinned FNV-1a hash; headers; a generated PNG; short image data, a 65537×65537 header, wrong CRCs, the last one included, a missing IEND, a missing image and truncations: `Left`; the memory limit; encoding: gradients of 7 sizes with and without filters decoded back to the same samples, a pinned FNV-1a hash of the PNGs, invalid options, a 0×0 image, short image data and an empty profile refused; a 1 MiB profile decoded from a generated iCCP chunk and kept by encoding; a profile above 1 MiB: `Left`; 1000 zTXt chunks with broken streams that decompress to 512 KiB: decoded; to 512 KiB + 1 byte: `Left`

## JuicyPixels-stbir fork

simplex-chat/JuicyPixels-stbir at `e0090e1`, on the Hackage release 0.1.0.0 (`917cd53`):
- `cbits/stb`: nothings/stb at `1ee679ca` as a submodule, `stb_image_resize.h` v0.97, replacing the bundled v0.95 copy.
- `cbits/stb_image_resize.c`: `STBIR_ASSERT` defined empty.
- `cc-options: -ffp-contract=off` (D16).
- cabal `license`: `BSD-3-Clause AND (MIT OR Unlicense)`, with `cabal-version: 2.2`.
- `LICENSE` and the cabal `copyright` field: "2026 SimpleX Chat Ltd" added.
- `resize`: `error` before calling stb_image_resize for a side below 1, an output or working buffers of 2 GiB or more, a `Region` outside the input, or a `Scale` that maps sample centres outside it (#3).
- Test suite `scale-test` (`test/ScaleTest.hs`):
  - a solid colour, RGB8 and RGBA8 with alpha 128, kept in every output pixel, for repeated halvings and for scales k/64, k in 65..127
  - a 1001×999 RGB gradient resized to 320×240 with Catmull-Rom: a pinned FNV-1a hash of the samples
  - a 46341×46341 RGBA8 output: refused; 16384×1 to 4320×1 and 1×16384 to 1×4320, RGB8 and RGBA8: accepted
- CI (`.github/workflows/build.yml`): build and test on Linux x86-64 with GHC 9.6.3 and 8.10.7, Linux ARM64, macOS and Windows with GHC 9.6.3.

## Mechanics

Core:
- `Image.hs`, new:
  - exports (`:9-20`):
    - `ImageFormat (IFJpeg, IFPng)`
    - `ColourProfile (CPNone, CPDisplayP3)`
    - `imageFormat`
    - `decodeResizable`
    - `resizeImage`
    - `resizeImageDataUri`
    - `maxDecodeMemoryMB`, also used by `Mobile/File.hs` and `MobileTests`
    - `scaleRGB8`
    - `scaleRGBA8`
  - `ColourProfile` (`:46-47`): `CPNone`, `CPDisplayP3`; `Eq`, `Show`
  - `imageFormat` (`:49-60`)
  - `decodeResizable` (`:62-70`): `decodeDisplayJpeg` for input starting `FF D8`; `Spng.decodePng` with ``maxDecodeMemoryMB `div` 2`` for input starting with the PNG signature; otherwise `Left`; then `colourProfile` of the input profile, `CPNone` without one, forced with `$!` (`:65`)
  - `decodeDisplayJpeg` (`:72-78`): `readJpegHeader`, `displaySize`, `jpegScale`, `decodeJpeg`, `resizeToSize`, `orientImage` with `exifOrientation`, and `jpegIccProfile`; the image and the input profile, as from `Spng.decodePng`; `segments` (`:78`), the one walk of `jpegSegments`, for `exifOrientation` and `jpegIccProfile`
  - `displaySize` (`:80-86`): the target size and the number of halvings, rounded by `scaledSide`
  - `jpegScale` (`:88-93`): `Full`, `Half`, `Quarter`, `Eighth` for 0, 1, 2, and 3 or more halvings
  - `resizeToSize` (`:95-98`): `convertRGB8` and `STBIR.resize` when the decoded size differs from the target
  - `exifOrientation` (`:100-122`): the orientation value of IFD0 from the segments, 1 when absent or malformed
  - `exifTiff` (`:124-125`): the TIFF data of the first APP1 `Exif` segment
  - `jpegIccProfile` (`:127-137`): the APP2 `ICC_PROFILE` segments (`iccSegments`), checked, ordered and concatenated
  - `jpegSegments` (`:139-145`): the marker and the data of each segment after SOI and before the first SOS; the list ends at a byte other than `FF`, at fewer than 4 bytes or at SOS
  - `orientImage` (`:147-159`): the 8 transforms with `generateImage`, applied with `dynamicPixelMap`
  - `colourProfile` (`:161-165`): `CPDisplayP3` when the 9 primaries from `iccPrimaries` are within 2 of those of `displayP3`, otherwise `CPNone`
  - `iccPrimaries` (`:167-189`): the header check; the tag table as one `slice`; `tagEntry`, the first entry of a signature, walked without a list; `colorant`, the size, the tag `slice`, the type and the 3 `s15Fixed16` values; numbers as `Integer`, `slice` bounds checked against the profile length
  - `displayP3` (`:191-192`): `embedFile "src/Simplex/Chat/Image/DisplayP3-v4.icc"`, as `Operators.hs:70` embeds `PRIVACY.md`
  - `ImageOutput` (`:194`): `ImageBytes`, `ImageDataUri`
  - `resizeImage` (`:196-197`)
  - `resizeImageDataUri` (`:199-200`)
  - `fitImage` (`:202-222`): the size bound; the largest size from `displaySize`; the output, the quality, bpp0 and `outputLength` by `ImageOutput`; `keptProfile` (`:210`), `displayP3` for `CPDisplayP3` within a tenth of the limit; `fitSize`; the quality bisection at the largest size below `minFill`; JPEG with `encodeRGB`, the trial at the largest size from `largestRGB` (`scale`, `:215`), shared with the bisection; PNG with `encodeSmallerPng` of `scaleRGBA8` applied once to the image
  - `fitSize` (`:224-262`): the size search of D4
  - `minFill = 0.97` (`:264-265`)
  - `imageDoesNotFit` (`:267-268`)
  - `bisect` (`:270-281`): the JPEG quality bisection, over `Int`
  - `encodeSmallerPng` (`:283-286`): `Spng.encodePng` at level 9 without and with row filters, with `keptProfile`, the shorter output; `Left` raised as an exception, as `encodeRGB` raises its errors
  - `maxImageSide = 16384` (`:288-289`)
  - `maxDisplaySide = 4320` (`:291-292`)
  - `maxJpegPixels = 2 ^ 28` (`:294-295`)
  - `maxDecodeMemoryMB` (`:297-302`), 512 or 1024 by `mobileBuild`
  - `hasSideAbove` (`:304-305`)
  - `hasSize` (`:307-308`)
  - `scaleRGB8` (`:310-315`)
  - `scaleRGBA8` (`:317-325`): the premultiplied image bound once, shared by the returned function
  - `catmullRom` (`:327-330`): `STBIR.resize` with Catmull-Rom and the given flags
  - `premultiply` (`:332-335`)
  - `unpremultiply` (`:337-342`)
  - `sharpen` (`:344-369`): the ring `rows` of 5 rows of `Float`; `blurRow`, the horizontal pass of one row into its slot `rowStart`; rows 0 and 1 first, then row y + 2 before output row y; the vertical pass from the ring and `unsharp` into the output
  - `convolve` (`:371-377`): the sum of the weighted samples at the 5 offsets, in `Double`, in order; monadic, for the samples and for the ring
  - `sharpenOffsets` (`:379-382`)
  - `sharpenWeights` (`:384-387`)
  - `sharpenRadius` (`:389-390`)
  - `sharpenAmount = 0.5` (`:392-393`)
  - `sharpenSigma = 0.6` (`:395-396`)
  - `scaledSide` (`:398-399`)
  - `toDataUri` (`:401-407`), with the approved comment at `:405`
- `src/Simplex/Chat/Image/DisplayP3-v4.icc`, new: the profile of D25, the upstream file unchanged (git blob `4ee40fb5`)
- `Mobile/File.hs`:
  - exported at `:13-18`:
    - `cChatResizeImageUri` (`:153-154`)
    - `cChatResizeImage` (`:156-157`)
    - `cChatResizeImageFileUri` (`:159-160`)
    - `cChatResizeImageFile` (`:162-163`)
    - `cChatResizeImageFdUri` (`:165-166`)
    - `cChatResizeImageFd` (`:168-169`)
  - `readFilePath` (`:171-174`): `withBinaryFile`, then `hGetImage`
  - `readFd` (`:176-177`): `c_close` (`:55`) of the descriptor when `fdToHandle` raises; `hGetImage` in `bracket` with `hClose`
  - `hGetImage` (`:179-190`): with `tryIO (hFileSize h)`, `fail` for a size above `maxDecodeMemoryMB * 1048576`, otherwise `B.hGet` of the size; when `hFileSize` fails, at most `maxDecodeMemoryMB * 1048576 + 1` bytes with `LB.hGet` and `fail` above `maxDecodeMemoryMB * 1048576`; `tooLarge`, the one `fail`
  - `cResizeImageUri` (`:192-193`)
  - `cResizeImageData` (`:195-202`): the format by `compare png 0`, `imageFormat` for `LT`
  - `cResizeImage` (`:204-207`):
    - the read action
    - `decodeResizable`, in place of `decodeImage`
    - the resize function applied to the image and its `ColourProfile`
    - `runCatchExceptT`
    - `newCStringFromLazyBS`
- `Mobile.hs:167-177`: `foreign export ccall` of the six functions.
- `Library/Commands.hs`:
  - `maxImagePreviewSize = 14000` (`:178-179`)
  - `imagePreview :: CryptoFile -> IO ImageData` (`:231-237`): `decodeResizable`, in place of `decodeImage` (`:236`), and `resizeImageDataUri` with the profile (`:237`)
  - `SendImage` (`:3555`): the preview from `imagePreview` (`:3562`)
- `simplex-chat.cabal`:
  - `extra-source-files`: `src/Simplex/Chat/Image/DisplayP3-v4.icc` (`:21`), as `PRIVACY.md`
  - library: `if flag(swift) || os(android)`, `cpp-options: -DmobileBuild` (`:419-420`)
  - module `Simplex.Chat.Image` (`:54`)
  - library dependencies:
    - `JuicyPixels` (`:391`)
    - `JuicyPixels-jpeg-turbo` (`:392`)
    - `JuicyPixels-spng` (`:393`)
    - `JuicyPixels-stbir` (`:394`)
    - `vector` (`:413`)
  - test suite:
    - module `ImageTests` (`:730`)
    - dependencies `JuicyPixels` and `JuicyPixels-jpeg-turbo` (`:823-824`)
- `libsimplex.dll.def:30-35`, `flake.nix:491-496`: the export names.
- `flake.nix:73-75`: the nix licence of JuicyPixels-spng (D21).

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
  - sending (`:1598-1599`), with `imageCryptoFile` (`:1609-1614`): the saved image, then `removeFile` of the copy and its removal from `ChatModel.shared.filesToDelete`
- `apps/ios/Shared/Views/Helpers/ImagePicker.swift`:
  - `loadImageData` (`:79-90`)
  - `imageFileType` (`:92-95`)
- `apps/ios/spec/architecture.md`:
  - `:101-105`: FFI list
  - `:61, 67, 69, 336`: header range `SimpleX.h#L1-L63`

Android and desktop, `apps/multiplatform/common/src/`:
- `commonMain/.../platform/Images.kt`:
  - `expect` of `resizeImageFileUri` and `resizeImageFileData` (`:34-35`)
  - `resizeImageFileToStrSize` (`:37-41`), with the debug log line `resizeImageFileToStrSize: core returned no image` for `""`
  - `resizeImageFileToDataSize` (`:43-47`), with the debug log line `resizeImageFileToDataSize: core returned no image` for `""`
  - `resizeImageToStrSize` (`:49-53`)
  - `resizeImageToDataSize` (`:55-61`): `png` 1 or 0 by `usePng`
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
  - `chatResizeImage` (`:43`), with `png: Int` as the file and descriptor functions
  - `chatResizeImageFileUri`, `chatResizeImageFile`, `chatResizeImageFdUri`, `chatResizeImageFd` (`:44-47`)
  - `ByteArray.toDirectBuffer` (`:56-61`)
- `commonMain/.../model/CryptoFile.kt:25`: `toDirectBuffer` is used by `writeCryptoFile`.
- `commonMain/.../views/helpers/Utils.kt`:
  - `saveImage(uri)` (`:174-200`)
  - `desktopSaveImageInTmp` (`:202-216`)
  - `resizeImageFromUri` (`:218-227`)
- `commonMain/.../views/chat/ComposeView.kt`: `processPickedMedia` (`:359`), one `preview` per URI (`:364`), `bitmapPreview` (`:370`), the preview from the file (`:382`), the media added when it has a preview (`:397`).
- `androidMain/.../views/chat/ComposeView.android.kt`: the camera preview (`:23-31`).
- `commonMain/cpp/android/simplex-api.c`: declarations (`:86-89`), JNI functions (`:282`, `:289`, `:296`, `:301`) with `decode_and_free`; `png` as `jint`.
- `commonMain/cpp/desktop/simplex-api.c`: declarations (`:49-52`), JNI functions (`:283`, `:290`, `:297`, `:305`) with `decode_to_utf8_string`; `png` as `jint`.
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
  - `chat_resize_image_uri` (`:264-272`), without a docstring
  - `chat_resize_image` (`:275-278`), without a docstring
  - `resize_image` (`:281-303`): three overloads and the implementation, with `match`; docstring (`:296`): `"""Resize image to the app size limit: JPEG or PNG for an image, a data URI for a preview."""`
  - `_resize_image_data` (`:306-314`)
  - `_resized_image` (`:317-320`)

Build:
- `cabal.project`, `source-repository-package`:
  - hs-jpeg-turbo (`:76-80`): simplex-chat/hs-jpeg-turbo at `d0a4d95`, subdirectories `JuicyPixels-jpeg-turbo`, `JuicyPixels-spng` and `jpeg-turbo`
  - JuicyPixels-stbir (`:82-85`): simplex-chat/JuicyPixels-stbir at `e0090e1`
- `scripts/nix/sha256map.nix:12-13`: the hashes of both repositories, computed with submodules.
- `dist-newstyle/src/hs-jpeg-t*` and `dist-newstyle/src/JuicyPixe*` in the Windows removals:
  - `.github/workflows/build.yml:1519, 1522, 1645, 1687, 1689`, with the comment `:1502-1503`
  - `scripts/desktop/build-lib-windows.sh:43`, with the comment `:41-42`
- `scripts/simplex-chat-reproduce-builds-android.sh:103`: submodules are updated after checkout.

CI, comments of the avatar scripts:
- `scripts/ci/ios-sim/avatar.swift:1`: `// Makes a group avatar:`
- `scripts/ci/desktop-linux/backend.sh:70`: `# Prints a profile image data URI: JPEG at quality 85, scaled down by the square root`

CI, the armv7a Android core library, on request (D9):
- `.github/workflows/android-emu.yml`:
  - input `armv7a` (`:18-21`), passed by the `plan` job as `run-armv7a` (`:48`, `:61`, `:66`)
  - job `core-armv7a` (`:183-204`): `build-core.sh ~/core/android-core-armeabi-v7a.tar.gz armv7a` on `ubuntu-24.04`, built only
- `scripts/ci/android/build-core.sh:13-14`: the architecture argument, `aarch64` by default or `armv7a`, in the Hydra job names `<arch>-android:lib:simplex-chat` and `<arch>-android:lib:support`
- `scripts/ci/README.md:158`, `scripts/ci/android/README.md:7`, `:21`: the input and the job.

CI, scripted app smoke tests, in every app workflow run:
- `tests/fixtures/test.jpg` and `logo-large-rgba.png` picked, previewed and sent in a local group; for each image:
  - the compose preview
  - a new `IMG_*.jpg` or `IMG_*.png` file; on iOS, also its pixel size
  - no new log line of the app's own resizing during the send

  | app | steps | images | log line of the app's own resizing |
  |---|---|---|---|
  | Android | `scripts/ci/android/smoke-test.sh:263-317`, from `:355` | `/sdcard/Pictures/test-card/test-card.jpg` and `/sdcard/Pictures/simplex-logo/simplex-logo.png`, one folder each, with a media scan broadcast (`add_images`, `:263-269`); in Google Photos' "Select photos": the folder by its name, its photo ("Photo taken on …"), then "Done" (or "Add") | `core returned no image` (`core_returned_no_image`, `:283-285`), a check at `:308` |
  | desktop | `scripts/ci/desktop-linux/smoke-test.sh:320-379`, from `:438` | the fixture paths typed in the file dialog | `core returned no image` (`core_returned_no_image`, `:346-348`), a check at `:370` |
  | iOS | `scripts/ci/ios-sim/smoke-test.sh:107-171`, from `:216-224` with `images`, after `snapshots.sh` (`:211-214`): the app's chats, files and hints are shown in the snapshot renders; first "Don’t Allow" in the camera access alert that the scanner views of the snapshot tests leave (`:217`) | local file encryption off for the images case (`disable_file_encryption`, `:125-129`): `privacyEncryptLocalFiles` false in the app-group defaults while the app is stopped, as `ui layout` sets `oneHandUI`, because the app stores sent images encrypted by default and `sips` reads only plain files; both images added to the photo library with one `xcrun simctl addmedia`, which lists the test card first and the logo second; the attachment button by its label "Attachements", "Choose from library"; the photo picker, absent from the accessibility tree, by position (`:21-25`): the test card or the logo, then the ✓ button; the pixel size of the newest sent file by `sips` (`sent_size`, `:119-123`, checked at `:168`): 1280x720 for the test card and 1630x466 for the logo, the sizes the core keeps at the 261120-byte limit | `resizeImageToStrSize final` and `resizeImageToDataSize final` (`app_resized_images`, `:147-149`), `fail` at `:169` |

- `.github/workflows/ios-sim.yml:269`: the app job's smoke test with `images`.
- `scripts/ci/android/README.md:32`, `scripts/ci/desktop-linux/README.md:27`, `scripts/ci/ios-sim/README.md:3`: the image steps.

CI, Claude-driven app tests (run with `claude=true`, which needs the `app-sim-claude` environment):
- images: `tests/fixtures/test.jpg` and `tests/fixtures/logo-large-rgba.png`, added before Claude starts by the step "Add test images":

  | app | where | HEIC |
  |---|---|---|
  | Android (`android-emu.yml`) | `/sdcard/Pictures/test-card.jpg` and `simplex-logo.png`, with a media scan broadcast | |
  | desktop (`desktop-linux.yml`) | `images/` in Claude's working directory | |
  | iOS (`ios-sim.yml`) | the photo library, with `xcrun simctl addmedia` | `test.jpg` turned to portrait by `sips`, when `sips` writes HEIC |

- steps: pick the JPEG, then the PNG with transparency (and on iOS the HEIC); check the compose preview; send; open the sent image
  - Android: `scripts/ci/android/claude-test.md`, part D
  - desktop: `scripts/ci/desktop-linux/claude-test.md`, part I, after the channel parts, in a new group
  - iOS: `scripts/ci/ios-sim/device-test-plan.md`, cases I1-I4, after the channel cases, named in `claude-scope` with `max-turns: 390`; the photo picker in `claude-prompt.md`
- `scripts/ci/desktop-linux/claude-scope`: `max-turns: 330`, read by `claude-test.sh` for Android and desktop

License notices, `docs/dependencies/`:
- `README.md:3`:
  `[Compact ICC Profiles](https://github.com/saucecontrol/Compact-ICC-Profiles): Display P3 colour profile of resized images ([CC0 1.0](./licences/apps/compact-icc-profiles/license))`
- `README.md:5`:
  `[libspng](https://github.com/randy408/libspng): PNG decoding and encoding of resized images ([BSD-2-clause](./licences/apps/libspng/LICENSE) and [libpng-2.0](./licences/apps/libspng/LICENSE-libpng))`
- `README.md:7`:
  `[miniz](https://github.com/richgel999/miniz): zlib compression and decompression for libspng ([MIT](./licences/apps/miniz/LICENSE))`
- `README.md:9`:
  `[mozjpeg](https://github.com/mozilla/mozjpeg): JPEG decoding and encoding of resized images ([IJG and BSD-3-clause](./licences/apps/mozjpeg/LICENSE.md)). This software is based in part on the work of the Independent JPEG Group.`
- `README.md:13`:
  `[stb_image_resize](https://github.com/nothings/stb): image scaling ([MIT or public domain](./licences/apps/stb_image_resize/LICENSE))`
- `licences/apps/mozjpeg/LICENSE.md` and `README.ijg`: copies from mozjpeg `08265790`, the submodule of simplex-chat/hs-jpeg-turbo.
- `licences/apps/stb_image_resize/LICENSE`: `cbits/stb/stb_image_resize.h:2596-2632` of simplex-chat/JuicyPixels-stbir at `e0090e1`.
- `licences/apps/libspng/LICENSE`: libspng's `LICENSE` (BSD-2-Clause); `licences/apps/libspng/LICENSE-libpng`: `spng/spng.c:6149-6180` (libpng-2.0), without the comment markers; from libspng `fb768002`, the submodule of simplex-chat/hs-jpeg-turbo at `d0a4d95`.
- `licences/apps/miniz/LICENSE`: miniz's `LICENSE` (MIT), from miniz 3.1.2 (`77d0dce8`), the submodule of simplex-chat/hs-jpeg-turbo at `d0a4d95`.
- `licences/apps/compact-icc-profiles/license`: the `license` (CC0 1.0) of saucecontrol/Compact-ICC-Profiles at `bdd84663`, the source of `DisplayP3-v4.icc`.
- `HASKELL.md`:
  - direct dependencies (`:25-28`):
    - `JuicyPixels` 3.3.8, Hackage
    - `JuicyPixels-jpeg-turbo` 0.1.0.0, simplex-chat/hs-jpeg-turbo at `d0a4d95`
    - `JuicyPixels-spng` 0.1.0.0, simplex-chat/hs-jpeg-turbo at `d0a4d95`, `BSD-3-Clause AND BSD-2-Clause AND libpng-2.0 AND MIT`, "JuicyPixels wrappers for libspng": the description column holds each package's synopsis
    - `JuicyPixels-stbir` 0.1.0.0, simplex-chat/JuicyPixels-stbir at `e0090e1`, `BSD-3-Clause AND (MIT OR Unlicense)`
  - direct dependency (`:55`): `vector` 0.13.1.0, moved from the indirect dependencies
  - indirect dependency (`:145`): `jpeg-turbo` 0.1.0.0, simplex-chat/hs-jpeg-turbo at `d0a4d95`, `BSD-3-Clause AND IJG AND Zlib`
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
- `licences/haskell/<package>-<version>/LICENSE`: the `LICENSE` of each of the five packages, from Hackage and from the pinned commits; the JuicyPixels-spng and JuicyPixels-stbir copies with the line `Copyright 2026 SimpleX Chat Ltd`.

## Tests

`tests/ImageTests.hs`, group "Image resize" (`tests/Test.hs:98`):
- Same aspect ratio, in every comparison below: `|w·h' − h·w'| ≤ w' + h'` (`:457-458`).
- Without a colour profile, `CPNone`, in every `resizeImage` and `resizeImageDataUri` call below.
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

- fill (`:43`, `:138-147`): `test.jpg` and `preview-issue1.png`, the data URI with limit 14000 and the JPEG data with limit 20000: 97–100% of the limit
- quality at the image size (`:44`, `:149-157`): a 64×48 gradient with a limit midway between its quality-65 and quality-95 encodings: at most the limit, larger than the quality-65 encoding, 64×48
- image preview (`:45-49`):
  - `test.jpg`: JPEG data URI of at most 14000 characters
  - an encrypted copy of `test.jpg`: the same
  - `test.pdf`: `fixedImagePreview`
- JPEG display size (`:51`, `:172-186`), blank W×16 JPEGs from `encodeRGB` and the same with the sides swapped, `decodeResizable`, then the image data with limit 261120, decoded:
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

- JPEG colour spaces (`:52`, `:188-196`), 64×48, `decodeResizable`:

  | input | result |
  |---|---|
  | progressive YCbCr, `encodeRGB` | `ImageRGB8` |
  | baseline YCbCr, JuicyPixels `encodeJpegAtQuality` | `ImageRGB8` |
  | grayscale, JuicyPixels `Pixel8` encoding | `ImageY8` |
  | YCCK, JuicyPixels `PixelCMYK8` encoding | `ImageCMYK8` |

- JPEG refusals (`:53`, `:198-213`), a baseline 64×48 JPEG changed as below, `decodeResizable`, `Left`:
  - precision 12 written into the SOF
  - SOF marker `FF C3` (lossless)
  - a side of 65501 written into the SOF
  - 16385×16384 written into the SOF
  - the input shortened by one byte
  - a byte inserted before the SOF marker
- EXIF orientation (`:54`, `:215-257`): a 64×32 JPEG with four solid quadrants and an APP1 Exif segment, little-endian for odd and big-endian for even values; `decodeResizable`:
  - values 1–8: the sides swapped for 5–8; the quadrant colours placed by the transform; the resized JPEG without `Exif`
  - an IFD0 offset beyond the segment: as value 1
- PNG decoding (`:55-57`), `decodeResizable`:
  - `logo-large-rgba.png` (`:259-269`): `ImageRGBA8` with the samples of JuicyPixels `decodeImage`, and `imageFormat` PNG; `preview-issue1.png`: `ImageRGB8` with the samples of JuicyPixels `decodeImage`; both without a colour profile
  - refusals (`:271-304`):

    | input | `Left` |
    |---|---|
    | `pngShortImageData`: the 2048×1 grayscale PNG from JuicyPixels `encodePng` with its header set to 2048×2048 | `IDAT stream too short` |
    | `pngSideAbove16384`: the 1×1 grayscale PNG with its header set to 65537×65537 | `image width exceeds user limit` |
    | a 16×16 GIF and a 16×16 BMP from JuicyPixels | `unsupported image format` |

- scaling (`:58-61`):
  - research code (`:321-327`): `scaleRGB8` of `preview-issue1.png` to 400×200 equals `preview-issue1-400x200-sharpened.png` pixel for pixel; the fixture is the output of the research code (`pvsf`, resize `catmullrom/g/0.5@0.6/single`) for the same decoded pixels
  - the same size (`:329-334`): `scaleRGB8` and `scaleRGBA8` of 64×48 images to 64×48 return the input samples
  - transparency (`:336-346`): a 64×64 opaque red disc on transparent pixels alternately green and blue, `scaleRGBA8` to 40×40: every pixel with alpha above 0 is red; some pixels have alpha below 255
- colour profiles (`:62-67`, `:348-449`), the 64×48 `gradient` and `transparentGradient`; the `ColourProfile` of an output read back by `decodeResizable` (`resizedProfile`, `uriProfile`):
  - fixtures built in the test:
    - ICC (`:432-443`): `iccProfile`, a 228-byte profile with a given colour space, PCS `XYZ ` and the tags `rXYZ`, `gXYZ` and `bXYZ` of given primaries; `p3Profile`, with the values of Apple's Display P3 profile (`p3Primaries`), bY 1/65536 from `displayP3`
    - JPEG (`:312-316`): APP2 segments inserted after SOI by `withIccProfile`, each `ICC_PROFILE\0`, the sequence number, the segment count and the chunk (`iccChunk`)
    - PNG (`:295-310`): an iCCP chunk named `test` inserted after IHDR by `pngWithIccProfile`, the profile in a stored zlib block with its Adler-32 (`zlibStored`), the chunk CRC by `crc32` (`pngChunk`)
  - Display P3 JPEG (`:348-360`): `p3Profile` in two segments, the second first: decoded as `CPDisplayP3`; with rX 2/65536 above: `CPDisplayP3`; the JPEG image data with limit 261120 contains the bytes of `src/Simplex/Chat/Image/DisplayP3-v4.icc`, read by the test (`displayP3File`); the JPEG and PNG image data with limit 261120 read back as `CPDisplayP3`
  - Display P3 PNG (`:362-372`): decoded as `CPDisplayP3`; the JPEG and PNG image data with limit 261120 and the PNG data URI with limit 14000 read back as `CPDisplayP3`
  - other profiles (`:374-392`), in one JPEG segment: decoded as `CPNone`; the JPEG data with limit 261120 equal to that of the JPEG without a profile, which is also decoded as `CPNone`:
    - sRGB primaries
    - rX 3/65536 above Display P3
    - colour space `GRAY` with the Display P3 tags
    - tag count 9: the table ends past the profile
    - `bXYZ` offset 209: the tag ends past the profile
  - above a tenth of the limit (`:394-405`), `CPDisplayP3`, whose `displayP3` has 480 bytes:
    - image data, JPEG and PNG: limit 4799 gives the output of `CPNone`, byte for byte; limit 4800 keeps the profile
    - data URI: limit 6399 gives the data URI of `CPNone` (640 base64 characters); limit 6400 keeps the profile
  - inconsistent JPEG profiles (`:407-421`), `p3Profile` split after 100 bytes: one of two segments; a repeated sequence number; segment counts 2 and 3; sequence number 0; an empty chunk; a 13-byte segment: decoded as `CPNone`

`tests/MobileTests.hs`, "Images" (`:103-110`), with buffers from `mallocBytes`:
- `cChatResizeImageUri` on `test.jpg`, limit 14000 (`:368-375`): `data:image/jpg;base64,`, at most 14000 characters; on `"not an image"`: `""`.
- `cChatResizeImage` on `test.jpg`, limit 100000 (`:377-385`): base64 decoded to at most 100000 bytes, starting `FF D8` for `png = 0` and with the PNG signature for `png = 1`.
- `cChatResizeImage` with `png = -1`, limit 100000 (`:387-395`): `FF D8` for `test.jpg`; the PNG signature for `logo-large-rgba.png`.
- `cChatResizeImage` with `png = -1`, limit 261120, on a blank 20000×16 JPEG from `encodeRGB` (`:397-401`): `FF D8`.
- an input above the limit (`oversizedJpegFile`, `:450-455`): `test.jpg` extended with zeros to `maxDecodeMemoryMB` MiB + 1 byte by `hSetFileSize`, a sparse file; resized as `test.jpg` is when no limit applies
- file path (`:403-412`):
  - `cChatResizeImageFileUri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters
  - `cChatResizeImageFile` with `png = -1`, limit 100000, on `logo-large-rgba.png`: the PNG signature
  - a missing file: `""`
  - `cChatResizeImageFileUri` on the input above the limit: `""`
- file descriptor (`:414-448`), opened with `GHC.IO.FD.openFile`; closed afterwards: `fdStat` fails or names another file
  - `cChatResizeImageFdUri` on `test.jpg`, limit 14000: `data:image/jpg;base64,`, at most 14000 characters; closed afterwards
  - `cChatResizeImageFdUri` on the read end of a pipe (`pipeFd`, `createPipeFd`), with `test.jpg` written by another thread, limit 14000: `data:image/jpg;base64,`; closed afterwards
  - `cChatResizeImageFd` on `test.pdf`: `""`; closed afterwards
  - `cChatResizeImageFdUri` on the `tests/fixtures` directory, opened with `c_open`: `""`; closed afterwards
  - descriptors 1000000 and −1, not open: `""`
  - `cChatResizeImageFdUri` on the input above the limit: `""`; closed afterwards
- malformed PNG (`:457-465`): `pngShortImageData` and `pngSideAbove16384` from `ImageTests`: `""` from `cChatResizeImageUri` with limit 14000 and from `cChatResizeImage` with `png = -1` and limit 261120

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
