# Haskell builds with Nix

The compiler and all cross-compilers come from the haskell.nix input pinned in
`flake.lock`. The `ghc914-sh` compiler is the stable-haskell fork, not the stock
GHC 9.14 release from GHCup. Use the same source checkout and lock file for every
platform; Android ARMv7 does not require a separate branch or tag.

`compiler-selection.nix` and `compiler-cache-overlay.nix` match Hydra's compiler
build context, including auxiliary iserv projects, so the pinned compiler can use
its existing cache. Application dependencies keep the project's current packages.

Run these commands from the repository root:

```sh
make help
make nix-list                  # list this host's existing Hydra outputs
make nix-native                # native library and CLI
make nix-android               # ARM64 + ARMv7 libraries, on x86_64-linux
make nix-windows               # Windows library and CLI, on x86_64-linux
make nix-linux-static          # x86_64, i686, and ARM64 static Linux CLIs
make nix-build                 # all existing outputs for this build host
```

Each command builds this checkout, without cloning another branch. Output links
are placed under `result-nix/<build-system>/<package>`. ZIP filenames inside the
outputs retain the existing Hydra names. For example, the ARMv7 client library is
`result-nix/x86_64-linux/armv7a-android:lib:simplex-chat/pkg-armv7a-android-libsimplex.zip`.
Nix includes tracked working-tree changes; add new source files to Git before
building so they are included.

For a remote builder, select its build system explicitly. Nix must already have
that builder configured:

```sh
make nix-ios NIX_SYSTEM=aarch64-darwin
make nix-darwin-libs NIX_SYSTEM=x86_64-darwin
make nix-build NIX_SYSTEM=aarch64-linux
```

`nix-ios` produces the Haskell static-library ZIP. It does not run Xcode, sign an
iOS application, or upload to TestFlight. ARM64 device libraries and Intel
simulator libraries retain their separate outputs. The existing iOS preparation
scripts retag ARM64 archives with `mac2ios -s` when an Apple Silicon simulator is
needed.

The Cabal v2 builder stores ordinary package archives in its staged store.
`link-mobile-library.sh` relinks the selected package and its dependencies before
that store is pruned. iOS ZIPs contain both the plain `libHSsimplex-chat-*.a` and
the rolled `libHSsimplex-chat-*-ghc*.a` used by `update-pbxproj.sh`. Android and
Windows clients link the threaded RTS into `libsimplex.so` or `libsimplex.dll`;
Android's separate `libsupport.so` resolves RTS symbols from the client library.
ARM64 Android libraries retain the 16 KiB linker alignment. These steps run in
the derivation's build/install phases because v2 component overrides do not run
the older `postBuild` and `postInstall` hooks.

Mobile builds keep the RTS's small-address-space setting. On Darwin, the final
link uses ABI-checked replacement RTS registrations, plus a replacement `text`
on ARM64. These boot libraries and all ARM64 mobile app C/C++ code target the
Apple A7 instruction baseline before `mac2ios` retags the archives. The cached
native compiler itself stays unchanged. Android applies the RTS source patch
when building its boot libraries. The Android overlay also supplies GNU iconv
headers and a static PIC archive to `ghc-internal`, including the separate
iserv projects. The `ghc-internal` system dependency propagates GNU iconv to
interpreter consumers as well. The minimum Android API stays at 26.

Android ARM32's QEMU wrapper preserves the guest's `PER_LINUX32` execution
domain when an ARM64 host kernel rejects that domain. Upper personality flags
still go through the host, and queries and setters retain the previous guest
value. This applies only to the Android ARM32 emulator; native compiler cache
paths and the other emulator targets stay unchanged. Android's libc initializes
this state again at startup after an exec.

A standalone C regression checks startup, full personality values, the host's
`UNAME26` effect, and independent thread/fork state. Run it from this checkout
with configured Linux builders; Nix supplies the pinned Android API26 NDK and
patched QEMU. The NDK compiles the tiny payload on x86_64 Linux; QEMU then runs
on the selected Linux architecture. For example, on an Apple Silicon Mac whose
Linux builder supports both x86_64 and ARM64:

```sh
nix build --impure --file scripts/nix/check-android-personality.nix \
  --argstr system aarch64-linux --max-jobs 1 --cores 1 --print-build-logs
```

Use `--argstr system x86_64-linux` for an x86_64 Linux builder. This optional
check does not add a Hydra output. It checks C startup and personality handling;
it does not qualify GHC/base under Rosetta, cross-TH, or Android device behavior.
The static C fixture supplies the ARM
[TLS alignment input](https://android.googlesource.com/platform/bionic/+/cc9b100%5E!/)
added to newer Android CRTs; its minimum API stays at 26. On 2026-10-15 this
regression passed on native ARM64 Linux, native x86_64 Linux, and x86_64 Linux
through Rosetta on an Apple Silicon Mac.

`NIX_ARGS` passes additional options to `nix build`, and `NIX_OUT_DIR` changes the
output-link directory. Each derivation has a two-hour timeout by default; set
`NIX_BUILD_TIMEOUT` in seconds to change it. For a wall-clock limit on a local
batch, use `timeout` from GNU coreutils to include environment setup in the limit:

```sh
timeout 6h nix develop -c make nix-build
make nix-native NIX_ARGS=--dry-run
```

`.github/workflows/nix.yml` runs these same outputs on Linux ARM64/x86_64 and
Darwin ARM64/x86_64, with a six-hour job limit. It uploads the existing library ZIPs
and CLIs under an artifact named for the source SHA. Each native CLI also runs
the encrypted-store and profile-password smoke test, which can be repeated with
`python3 scripts/nix/smoke-cli.py result-nix/BUILD_SYSTEM/exe:simplex-chat/bin/simplex-chat`.
Hydra continues to consume
the same `hydraJobs` attributes. The older application release workflow still
uses its existing GHCup/Docker compiler settings; migrating that packaging and
release path is separate from this Nix build lane.

Hydra needs builders advertising `aarch64-darwin` and `x86_64-darwin` to run the
corresponding Mac/iOS jobs. Check its [builder pool](https://ci.zw3rk.com/machines)
when jobs remain queued. For cross builds, the pinned haskell.nix also uses
`recursive-nix` as a scheduling marker for slices that require a real x86_64
Linux host. Advertise that feature on a native x86_64 Linux builder; keep it off
x86_64 Linux slots running through Rosetta on Apple Silicon. The GitHub workflow
sets it only for its native x86_64 Linux runner.

To build both Android APK ABIs from the current checkout, preserving the same
compiler and source, use the existing builder's source/skip-checkout flags:

```sh
scripts/android/build-android.sh -sg VERSION_CODE
```

This command also needs the Android SDK, Gradle, and its other existing tools.
The APK reproduction script now checks out the supplied tag for both ABIs.
Historical releases that used a separate `-armv7a` tag must be reproduced with
their historical build scripts.

## Migration validation

On 2026-10-15, all 21 existing Hydra derivations evaluated successfully at
`52cbb2c7f796bfeca0824bd3e2168e344ce84774`: 11 on x86_64 Linux, two on ARM64 Linux,
and four on each Darwin architecture. This includes both Android ABIs, Windows,
and all three static Linux targets.

The native ARM64 Darwin library and CLI built with Nix at `e1d2f1ebc` and
passed `python3 scripts/nix/smoke-cli.py /path/to/simplex-chat`, covering encrypted
stores and profile passwords. Native BBS checks also passed. The later Android
iconv commit leaves the native Haskell/C/Cabal sources, compiler, flags, and
other dependencies unchanged. It changes the overall source tarball and hence
the native derivation paths; those later native derivations have not been rebuilt.

A native build with the iOS Swift/CommonCrypto flags passed mobile C API checks
using the rolled archive produced by `link-mobile-library.sh`: Swift JSON,
encrypted stores, UTF-8, reopen behavior, and wrong-key errors with copying and
nonmoving GC. This ran on macOS before archive retagging. The CommonCrypto
SQLCipher component also built with Nix and registered both Apple frameworks.
The replacement RTS and `text` built with Nix and passed ABI,
instruction-baseline, UTF-8, and GC checks.

Both full Nix ARM64 iOS library ZIPs, Swift JSON and tagged JSON, built at
`ef3a3b0b78aa108f9ab192278e4dae9a3d978d07`. Each ZIP contained its five expected
archive entries with valid CRCs and 3,630 Mach-O members tagged ARM64/iOS. Their
Hydra product markers pointed to the actual ZIPs. The production FFI archive
passed an instruction-baseline check. The package uses the full Cabal v2 unit ID
in its archive filenames; the existing Xcode project updater accepts those names.
The later Android-only changes leave these build inputs unchanged. This verifies
the packaged library, not execution after retagging.

The Android API26 ARMv7 `ghc-internal` built at
`52cbb2c7f796bfeca0824bd3e2168e344ce84774`, including all 292 Haskell modules and
C sources. Its unit ID and ABI match the earlier build, and GNU iconv is retained
in its installed paths and consumer dependencies. The production interpreter
build stopped before startup because this local external-builder backend rejects
its `recursive-nix` scheduling requirement. The upstream native-x86 guard stays
in place; the full interpreter and cross-TH checks still need a suitable builder.

A diagnostic build of ARMv7 `base`, with only that scheduling metadata relaxed,
completed all 241 modules under Rosetta. A small GHC 9.14 Haskell program using
that diagnostic `base` and the production core libraries then passed on both
Rosetta QEMU and native ARM64 Linux QEMU. The same static ARM32 executable checked
32-bit `Int`, arithmetic, `forkIO`, and GC; it needed no TLS adjustment. This
proves basic Haskell execution under software emulation, not the full TH path or
qualification for removing the production scheduling guard.

The final Linux/Windows and Intel Darwin outputs, ARMv7 cross-TH, Android device
tests, and an Xcode application build remain unverified. Derivation evaluation
and native mobile checks do not establish that those builds and runtime checks pass.

## Updating package hashes

1. Install `nix`, `gawk` and `jq`.

2. Start nix-shell from repo root:

```sh
nix-shell -p nix-prefetch-git
```

3. Run in nix shell:

```sh
gawk -f ./scripts/nix/update-sha256.awk cabal.project > ./scripts/nix/sha256map.nix
```
