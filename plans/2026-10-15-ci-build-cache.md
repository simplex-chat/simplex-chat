# CI build cache

Repository: simplex-chat/simplex-chat.
Branch: `ep/ci-build-cache`.
Model: simplexmq at `e91711c7`:
- `.github/workflows/build.yml`
- `scripts/ci/source-timestamps.sh`
- `scripts/ci/prune-old-builds.sh`

Repository cache limit: 80 GB.

## Files

- `.github/workflows/build.yml`
- `.github/actions/prepare-build/action.yml`
- `scripts/ci/source-timestamps.sh`: copied from simplexmq.
- `scripts/ci/prune-old-builds.sh`: new.
- `scripts/desktop/build-lib-linux.sh`: build directory from `DIST_DIR`, default `dist-newstyle`.

## Job environment

Jobs: build-linux, build-linux-postgres, build-macos, build-windows.

opt
- pull request: `O0`
- other events: `O2`

ghc_options
- pull request: `--ghc-options=-O0`
- other events: empty

release
- `v*` tag: `true`

branch
- pull request: base branch
- other events: ref name

cache_o0
- push to master or stable: `true`

ZSTD_CLEVEL
- `9`: zstd level of cache archives

## Cache entries

deps
- path: cabal store
  - build-linux, build-linux-postgres: `~/.cabal/store`
  - build-macos, build-windows: output `cabal-store` of prepare-build
- key: `deps-<target>-<branch>-<store hash>`
- restore key 1: `deps-<target>-<branch>-`
- restore key 2: `deps-<target>-`
- saved when the store hash differs from the matched key

toolchain
- job: build-macos
- path:
  - `~/.ghcup/ghc/<ghc>`
  - `~/.ghcup/bin/*-<ghc>`
  - `~/.ghcup/bin/cabal-3.10.2.0`
- key: `ghcup-<os>-<arch>-ghc<ghc>-cabal3.10.2.0-<toolchain>`
- restored before "Prepare build"
- saved after "Prepare build" when the key is not matched

src
- jobs: build-linux, build-macos, build-windows
- path: `dist-newstyle/src`
- key: `src-<runner OS>-<hash of cabal.project>`
- restore key: `src-<runner OS>-`
- saved when the matched key differs from the key, by:
  - build-linux: ubuntu-22.04 x86_64, GHC 9.6.3
  - build-macos: x86_64
  - build-windows

dist
- jobs: build-linux, build-macos, build-windows
- path:
  - `dist-newstyle/*`
  - `!dist-newstyle/src`
- key: `dist-<target>-<opt>-<branch>-<sha>`
- restore key 1: `dist-<target>-<opt>-<branch>-`
- restore key 2: `dist-<target>-<opt>-`
- restored except on libs release pushes in build-macos and build-windows
- saved when the matched key differs from the key

dist-lib
- job: build-linux
- path:
  - `dist-lib/*`
  - `!dist-lib/src`
- key: `dist-lib-<target>-<branch>-<sha>`
- restore key 1: `dist-lib-<target>-<branch>-`
- restore key 2: `dist-lib-<target>-`
- restored on pull requests that change the chat core, `libs` matrix entries, x86_64
- saved when the library build succeeds and the matched key differs from the key

store hash
- first 16 hex characters of sha256 of the store's package directory listing

target
- build-linux: `ubuntu-<os>-<arch>-ghc<ghc>-<toolchain>`
- build-linux-postgres: `ubuntu-22.04-x86_64-postgres-ghc<ghc>-<toolchain>`
- build-macos: `<os>-<arch>-ghc<ghc>-<toolchain>`
- build-windows: `windows-x86_64-ghc<ghc>-<toolchain>`

toolchain
- Linux: first 16 hex characters of sha256 of the installed `binutils*`, `cpp*`, `gcc*` and `*-dev` package versions in the container
- macOS: first 16 hex characters of sha256 of `sw_vers -productVersion` and `clang --version`
- Windows: first 16 hex characters of sha256 of MSYS2 `gcc --version`

Release tags: caches are neither restored nor saved.
Each missed restore: `::warning title=Cache miss::` annotation.

## build-linux

Job permissions
- `contents: write`
- `packages: write`

Build image
- registry cache: `ghcr.io/simplex-chat/simplex-chat-build-cache:<os>-<arch>-ghc<ghc>-<ISO week>`
- read: every build except release tags
- written: pushes to master, `mode=max`, `ignore-error=true`
- login: `docker login ghcr.io` with `GITHUB_TOKEN`, every build except release tags

Container
- volume: `~/.cabal:/home/builder/.cabal`
- environment: `CABAL_DIR=/home/builder/.cabal`

Steps after "Start container":
1. Compute cache keys.
2. Restore deps.
3. Restore src.
4. Restore dist.
5. Restore dist-lib.
6. GHC 8.10.7, dist restored: `scripts/ci/source-timestamps.sh restore dist-newstyle/sources.sha256`.
7. Prepare cabal.project.local: unchanged.
8. Build CLI:
   - `scripts/ci/prune-old-builds.sh dist-newstyle`
   - `cabal update`
   - `cabal build -j --enable-tests <ghc_options>`
   - binaries copied to `/out`
   - `cabal clean` removed
9. Build library:
   - `scripts/ci/prune-old-builds.sh dist-lib`
   - `dist-lib/src`: symlink to `../dist-newstyle/src`
   - `DIST_DIR=dist-lib`
10. GHC 8.10.7: `scripts/ci/source-timestamps.sh record dist-newstyle/sources.sha256`.
11. Save deps.
12. Save src.
13. Save dist.
14. Save dist-lib.
15. Release and test steps: unchanged.
16. -O0 refresh, when `cache_o0`, the run is not cancelled and "Build CLI" succeeded:
    - remove `dist-newstyle` contents except `src`
    - restore dist with opt `O0`
    - GHC 8.10.7: restore source timestamps
    - `scripts/ci/prune-old-builds.sh dist-newstyle`
    - `cabal build -j --enable-tests --ghc-options=-O0`
    - GHC 8.10.7: record source hashes
    - save dist with opt `O0`

"Fix permissions for cache": removed.

## build-linux-postgres

- condition: unchanged
- caches: deps
- container: volume and environment as in build-linux
- `cabal clean` removed
- "Fix permissions for cache": removed

## build-macos and build-windows

prepare-build action
- inputs `os` and `cache_path`: removed
- "Restore cached build": removed
- output `cabal-store`: output `cabal-store` of setup-haskell-action

Steps:
1. build-macos: restore toolchain.
2. Prepare build.
3. build-macos: save toolchain.
4. Compute cache keys.
5. Restore deps.
6. Restore src.
7. Restore dist.
8. Build CLI:
   - `scripts/ci/prune-old-builds.sh dist-newstyle`
   - `<ghc_options>`
9. Save deps.
10. Save src.
11. Save dist.
12. build-macos "Run tests": aarch64 only, `cabal test` with `<ghc_options>`.
13. Other steps: unchanged.
14. -O0 refresh as in build-linux, after all other steps.

build-macos
- step before "Prepare build": `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk` linked to `xcrun --sdk macosx --show-sdk-path`
- Build CLI: `bin_path` from the last line of `cabal list-bin simplex-chat`

build-windows
- cache key and store hash steps: `msys2` shell
- store hash: store path converted with `cygpath -u`
- keeps the `dist-newstyle/src` removals
- -O0 refresh:
  - skipped on libs release pushes
  - runs `scripts/desktop/prepare-openssl-windows.sh` before the build

## prune-old-builds.sh

Arguments: build directories.
In each `<dir>/build/*/*/`:
- `simplex-chat-<version>`: deleted unless the version equals `simplex-chat.cabal`

Version: carriage returns removed.

## Verification

Baseline, run 38730229359:
- Linux "Restore cached build": 0-4 s
- Linux "Build and cache Docker image": 3.5-5 min
- Linux "Build CLI", GHC 9.6.3: 23-30 min
- Linux "Build CLI", GHC 8.10.7: 80 min
- macOS "Build CLI": 1.5-30 min
- Windows "Build CLI": 23 min

Test pull request: `ep/ci-build-cache` into master.

On the second run of the test pull request:
1. build-linux, build-macos and build-windows restore deps and dist.
2. "Build CLI" compiles only modules changed since the first run.
3. GHC 8.10.7 "Build CLI" compiles only changed modules.
4. Tests pass.

After merge, on the first push to master:
1. build-linux, build-macos and build-windows save deps, dist with opt `O2` and dist with opt `O0`.
2. build-linux writes the image cache.

On the next pull request:
1. build-linux, build-macos and build-windows restore dist with opt `O0` from master.
2. build-linux reads the image cache.
