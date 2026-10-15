# Haskell builds with Nix

The compiler and all cross-compilers come from the haskell.nix input pinned in
`flake.lock`. The `ghc914-sh` compiler is the stable-haskell fork, not the stock
GHC 9.14 release from GHCup. Use the same source checkout and lock file for every
platform; Android ARMv7 does not require a separate branch or tag.

`compiler-selection.nix` matches Hydra's compiler build context so the pinned
compiler can use its existing cache. That context is scoped to the compiler and
its build tools; application dependencies keep the project's current packages.

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
when building its boot libraries.

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

To build both Android APK ABIs from the current checkout, preserving the same
compiler and source, use the existing builder's source/skip-checkout flags:

```sh
scripts/android/build-android.sh -sg VERSION_CODE
```

This command also needs the Android SDK, Gradle, and its other existing tools.
The APK reproduction script now checks out the supplied tag for both ABIs.
Historical releases that used a separate `-armv7a` tag must be reproduced with
their historical build scripts.

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
