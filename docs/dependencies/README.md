# SimpleX Chat and SimpleX servers dependencies

[Compact ICC Profiles](https://github.com/saucecontrol/Compact-ICC-Profiles): Display P3 colour profile of resized images ([CC0 1.0](./licences/apps/compact-icc-profiles/license))

[libspng](https://github.com/randy408/libspng): PNG decoding and encoding of resized images ([BSD-2-clause](./licences/apps/libspng/LICENSE) and [libpng-2.0](./licences/apps/libspng/LICENSE-libpng))

[libwebp](https://chromium.googlesource.com/webm/libwebp): WebP decoding of resized images ([BSD-3-clause](./licences/apps/libwebp/COPYING) and [patent grant](./licences/apps/libwebp/PATENTS))

[miniz](https://github.com/richgel999/miniz): zlib compression and decompression for libspng ([MIT](./licences/apps/miniz/LICENSE))

[SIMDe](https://github.com/simd-everywhere/simde): WebAssembly SIMD of the sandboxed image decoders, translated to the SIMD of the device ([MIT](./licences/apps/simde/COPYING))

[mozjpeg](https://github.com/mozilla/mozjpeg): JPEG decoding and encoding of resized images ([IJG and BSD-3-clause](./licences/apps/mozjpeg/LICENSE.md)). This software is based in part on the work of the Independent JPEG Group.

[SQLCipher](https://github.com/sqlcipher/sqlcipher): Extension of [SQLite](https://sqlite.org) with encryption ([BSD-style](./licences/apps/sqlcipher/LICENSE.md))

[stb_image_resize](https://github.com/nothings/stb): image scaling ([MIT or public domain](./licences/apps/stb_image_resize/LICENSE))

[vlc](https://github.com/videolan/vlc): VLC media player library ([LGPLv2](./licences/apps/vlc/COPYING.LIB))

[wasm2c](https://github.com/WebAssembly/wabt): the runtime of the image decoders compiled to WebAssembly and translated to C, which decode JPEG, PNG and WebP images in a sandbox ([Apache-2.0](./licences/apps/wasm2c/LICENSE))

[WebRTC](https://webrtc.googlesource.com/src/): RTC for calls ([BSD-3-clause](./licences/apps/webrtc/LICENSE))

[Haskell dependencies](./HASKELL.md).

The WebP test images in [tests/fixtures/webp](../../tests/fixtures/webp) are made from the repository's own test images [test.jpg](../../tests/fixtures/test.jpg) and [logo-large-rgba.png](../../tests/fixtures/logo-large-rgba.png) and from images of one colour by cwebp of libwebp 1.6.0, `animated.webp` with two frames of `lossy.webp`, and are distributed under the licence of this repository.
