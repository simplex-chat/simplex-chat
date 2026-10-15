# Haskell builds with Nix

The compiler and all cross-compilers come from the haskell.nix input pinned in
`flake.lock`. The `ghc914-sh` compiler is the stable-haskell fork, not the stock
GHC 9.14 release from GHCup. Use the same source checkout and lock file for every
platform; Android ARMv7 does not require a separate branch or tag.

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
and CLIs under an artifact named for the source SHA. Hydra continues to consume
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
