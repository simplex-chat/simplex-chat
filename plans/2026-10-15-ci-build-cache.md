# CI build cache

Repository: simplex-chat/simplex-chat.
Branch: `ep/ci-build-cache`.
Model: simplexmq at `e91711c7`:
- `.github/workflows/build.yml`
- `scripts/ci/source-timestamps.sh`
- `scripts/ci/prune-old-builds.sh`

Repository cache limit: 100 GB.

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

dist
- jobs: build-linux, build-macos, build-windows
- path: `dist-newstyle`
- key: `dist-<target>-<opt>-<branch>-<sha>`
- restore key 1: `dist-<target>-<opt>-<branch>-`
- restore key 2: `dist-<target>-<opt>-`
- restored except on libs release pushes in build-macos and build-windows
- saved when the matched key differs from the key

dist-lib
- job: build-linux
- path: `dist-lib`
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
3. Restore dist.
4. Restore dist-lib.
5. GHC 8.10.7, dist restored: `scripts/ci/source-timestamps.sh restore dist-newstyle/sources.sha256`.
6. Prepare cabal.project.local: unchanged.
7. Build CLI:
   - `scripts/ci/prune-old-builds.sh dist-newstyle`
   - `cabal update`
   - `cabal build -j --enable-tests <ghc_options>`
   - binaries copied to `/out`
   - `cabal clean` removed
8. Build library:
   - `scripts/ci/prune-old-builds.sh dist-lib`
   - `DIST_DIR=dist-lib`
9. GHC 8.10.7: `scripts/ci/source-timestamps.sh record dist-newstyle/sources.sha256`.
10. Save deps.
11. Save dist.
12. Save dist-lib.
13. Release and test steps: unchanged.
14. -O0 refresh, when `cache_o0`, the run is not cancelled and "Build CLI" succeeded:
    - remove `dist-newstyle`
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
1. Prepare build.
2. Compute cache keys.
3. Restore deps.
4. Restore dist.
5. Build CLI:
   - `scripts/ci/prune-old-builds.sh dist-newstyle`
   - `<ghc_options>`
6. Save deps.
7. Save dist.
8. build-macos "Run tests": `cabal test` with `<ghc_options>`.
9. Other steps: unchanged.
10. -O0 refresh as in build-linux, after all other steps.

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
