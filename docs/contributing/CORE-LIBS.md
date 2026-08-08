---
title: Building the core library
revision: 05.08.2026
---

# Building the core library

Every client app — Android, desktop, iOS — links against the Haskell core (`libsimplex`) through FFI. The core is not in the repo, and no app build system produces it: Gradle and Xcode expect it to be already in place. Build it for your target first, otherwise the app build fails, on Android with:

```
ninja: error: '.../cpp/android/libs/arm64-v8a/libsimplex.so', needed by '.../libapp-lib.so', missing and no known rule to make it
```

`scripts/build-core.sh <target>` builds it and puts it where the app build looks for it. Run it from the repo root.

## Prerequisites

Android and iOS cores are cross-compiled with nix from `flake.nix`, and need Nix >= 2.22 with `nix-command flakes` enabled and roughly 50 GB free in `/nix`. The Android jobs are defined only under the `x86_64-linux` builder, the iOS jobs only under the darwin ones, so build each on a matching host. Add the IOG cache to `/etc/nix/nix.conf` before the first build, or nix compiles the cross-GHC itself and the build takes 12-24 hours:

```
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

Note that distro nix packages are usually too old — Ubuntu's `nix-bin` (2.6) fails to evaluate the flake with `input 'haskellNix/hydra/nixpkgs' follows a non-existent input 'nix/nixpkgs'`. Use the upstream installer.

Desktop cores are built natively with cabal and need ghcup's GHC 9.6.3 and cabal 3.10.2, plus a `cabal.project.local` for SQLCipher (`cp scripts/cabal.project.local.linux cabal.project.local`, or `.mac`). Windows additionally needs MinGW/UCRT64 — see the header comment of `scripts/desktop/build-lib-windows.sh`.

## Android

```sh
./scripts/build-core.sh android-aarch64   # arm64-v8a
./scripts/build-core.sh android-armv7a    # armeabi-v7a
```

Produces `libsimplex.so` (~190 MB) and `libsupport.so` (~20 MB, stripped to ~14 MB when AGP packages the APK) in `apps/multiplatform/common/src/commonMain/cpp/android/libs/<abi>/`, which is gitignored. OpenSSL is linked statically, so no other `.so` files are needed.

Build only the ABIs you actually deploy to. Studio injects `android.injected.build.abi` for the connected device, so AGP configures only that CMake variant; from the command line pass it yourself:

```sh
./gradlew -Pandroid.injected.build.abi=arm64-v8a assembleDebug
```

There is no x86/x86_64 core library, so x86_64 emulator images cannot run the app — use an `arm64-v8a` AVD (system images exist up to API 36; on an x86_64 host they run under full emulation and are slow) or a physical device.

Per [CONTRIBUTING](../CONTRIBUTING.md), the armv7a core is built from the `master-android` branch with GHC 8.10.7.

## Desktop

```sh
./scripts/build-core.sh desktop-linux [sqlite|postgres]
./scripts/build-core.sh desktop-mac [x86_64|arm64] [sqlite|postgres]
./scripts/build-core.sh desktop-windows [x86_64]
```

Produces `libsimplex.{so,dylib,dll}` and its runtime dependencies in `apps/multiplatform/common/src/commonMain/cpp/desktop/libs/<os>-<arch>/`, prepares the bundled VLC, and creates the symlink under `apps/multiplatform/build/links/` that the Compose packaging uses. After that `./gradlew :desktop:run` or `:desktop:packageDistributionForCurrentOS` work.

The JNI wrapper around the core is a separate, much smaller library; Gradle does build that one itself (`cmakeBuildAndCopy` in `desktop/build.gradle.kts`).

## iOS

```sh
./scripts/build-core.sh ios
```

macOS only. Builds the `aarch64-darwin-ios` job and runs `scripts/ios/prepare.sh`, which fills `apps/ios/Libraries/{mac,ios,sim}` using `mac2ios` from the flake input.

## Using prebuilt cores instead

The libraries the release apps ship with are built by the project's nix CI. If you don't need to build the core yourself, download the job artifacts and unpack them with `scripts/android/prepare.sh` (Android, expects the zips in `~/Downloads`) or `scripts/ios/download-libs.sh <job repo>` (iOS).
