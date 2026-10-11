# Core library releases: implementation plan

Libraries for the Node.js package are released per core version, from pushes to `master` and `stable`.

## 1. Release identity

```
repository     simplex-chat/simplex-chat-libs
tag            core-<version>
version        version: field of simplex-chat.cabal, e.g. 7.1.0.10
trigger        every push to master or stable
build          when `gh release view core-<version>` fails in simplex-chat-libs
```

Rule: a core version is final once a published package depends on it.
Its migrations and chat protocol versions stay unchanged.
A further change requires a new cabal version.

## 2. Assets

| Asset | Runner | Backend |
|---|---|---|
| `simplex-chat-libs-linux-x86_64.zip` | `ubuntu-22.04` | sqlite |
| `simplex-chat-libs-linux-x86_64-postgres.zip` | `ubuntu-22.04` | postgres |
| `simplex-chat-libs-linux-aarch64.zip` | `ubuntu-22.04-arm` | sqlite |
| `simplex-chat-libs-macos-aarch64.zip` | `macos-latest` | sqlite |
| `simplex-chat-libs-macos-x86_64.zip` | `macos-15-intel` | sqlite |
| `simplex-chat-libs-windows-x86_64.zip` | `windows-latest` | sqlite |
| `SHA256SUMS` | `ubuntu-latest` | — |

Zip content: directory `libs/` with the top-level files of `apps/multiplatform/common/src/commonMain/cpp/desktop/libs/<os>-<arch>/`.
Windows zip: also `libs/libsimplex.lib`.
Asset names: as built by `download-libs.js` (line 123).

## 3. Workflow `.github/workflows/libs.yml`

```yaml
on:
  push:
    branches:
      - master
      - stable

concurrency:
  group: libs
  cancel-in-progress: false
```

Jobs:

| Job | Needs | Condition |
|---|---|---|
| `check` | — | — |
| `build-linux` | `check` | `needs.check.outputs.build == 'true'` |
| `build-macos` | `check` | `needs.check.outputs.build == 'true'` |
| `build-windows` | `check` | `needs.check.outputs.build == 'true'` |
| `release` | `check`, `build-linux`, `build-macos`, `build-windows` | all builds succeeded |

### 3.1. `check`

```
runner     ubuntu-latest
version    sed -n 's/^version: *//p' simplex-chat.cabal
build      true when `gh release view core-<version> --repo simplex-chat/simplex-chat-libs` fails
token      secrets.NODEJS_REPO_TOKEN
outputs    version, build
```

### 3.2. `build-linux`

Matrix:

```
arch      runner             image hash                                                                 backend
x86_64    ubuntu-22.04       sha256:5c8b2c0a6c745bc177669abfaa716b4bc57d58e2ea3882fb5da67f4d59e3dda5    sqlite
x86_64    ubuntu-22.04       sha256:5c8b2c0a6c745bc177669abfaa716b4bc57d58e2ea3882fb5da67f4d59e3dda5    postgres
aarch64   ubuntu-22.04-arm   sha256:6a62a4157b8775eaf4959cb629e757d32d39d1f4c8ac1b0ddc2510b555cf72f3    sqlite
```

Steps, as `build-linux` in `build.yml` (lines 153-230):

1. Checkout, free disk space, cache restore (key with `libs`, arch and backend).
2. Docker image from `Dockerfile.build`: `TAG=22.04`, `HASH`, `GHC=9.6.3`, user UID and GID.
3. Container start with `~/.cabal` and the workspace mounted.
4. `cabal.project.local`: `ignore-project: False`, `direct-sqlcipher` with `flags: +openssl`.
5. In the container: `cabal update`, then `SKIP_VLC=1 scripts/desktop/build-lib-linux.sh <backend>`.
6. Zip and artifact upload.
7. Cache permissions fix, as `build.yml` lines 356-361.

### 3.3. `build-macos`

Matrix:

```
arch      runner           openssl_dir
aarch64   macos-latest     /opt/homebrew/opt
x86_64    macos-15-intel   /usr/local/opt
```

Steps, as `build-macos` in `build.yml` (lines 510-537):

1. Checkout, `./.github/actions/prepare-build`.
2. `brew install openssl@3.0`.
3. `cabal.project.local`: `simplexmq` and `direct-sqlcipher` with `extra-include-dirs: <openssl_dir>/openssl@3.0/include`, `extra-lib-dirs: <openssl_dir>/openssl@3.0/lib`; `direct-sqlcipher` with `flags: +openssl`.
4. `SKIP_VLC=1 scripts/desktop/build-lib-mac.sh`.
5. Zip of `libs/mac-<arch>/` as `simplex-chat-libs-macos-<arch>.zip`, artifact upload.

### 3.4. `build-windows`

Steps, as `build-windows` in `build.yml` (lines 621-697):

1. Checkout, `./.github/actions/prepare-build` with `cache_path: "C:/cabal"`.
2. Pagefile 16GB.
3. MSYS2 `ucrt64` with `git`, `perl`, `make`, `toolchain:p`, `cmake:p`.
4. `export PATH=$PATH:/c/ghcup/bin:$(echo /c/tools/ghc-*/bin || echo)`, then `SKIP_VLC=1 scripts/desktop/build-lib-windows.sh`.
5. `dlltool -d libsimplex.dll.def -l libsimplex.lib -D libsimplex.dll`.
6. Zip of `libs/windows-x86_64/` and `libsimplex.lib`, artifact upload.

### 3.5. `release`

1. Artifact download.
2. `sha256sum simplex-chat-libs-*.zip > SHA256SUMS`.
3. `softprops/action-gh-release@v2`:

```
repository   simplex-chat/simplex-chat-libs
tag_name     core-<version>
files        simplex-chat-libs-*.zip, SHA256SUMS
token        secrets.NODEJS_REPO_TOKEN
```

## 4. Scripts: `SKIP_VLC`

With `SKIP_VLC` set, the VLC download is skipped.
Unset: unchanged.

| File | Line | After |
|---|---|---|
| `scripts/desktop/build-lib-linux.sh` | 56 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-linux.sh` |
| `scripts/desktop/build-lib-mac.sh` | 154 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-mac.sh` |
| `scripts/desktop/build-lib-windows.sh` | 64 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-windows.sh` |

## 5. `build.yml`

Removed jobs:

- `build-linux-postgres`, with its header comment.
- `release-nodejs-libs`, with its header comment.

## 6. Node.js package

```
download-libs.js   RELEASE_TAG = 'core-<version>'
package.json       version: Node.js release cycle
release commits    RELEASE_TAG removed from their edits (author build@simplex.chat)
```

Platforms: `linux-aarch64` added by the assets in section 2; `download-libs.js` maps `arm64` to `aarch64` (lines 46-50).
Postgres: Linux x86_64 only (lines 19-22).

## 7. Precompiled add-on (`ep/node-add-on`)

Add-on releases: separate from library releases, triggered by changes of the Node.js package version, designed in `ep/node-add-on`.

Effects of this plan on `ep/node-add-on`:

```
release-nodejs-libs   removed by section 5; its add-on upload edits move to the add-on release
RELEASE_TAG           library release core-<version>; add-on assets in a separate release
SHA256                library values from SHA256SUMS of release core-<version>
```

## 8. `build.yml` fix

Line 531: `matrix.opnessl_dir` → `matrix.openssl_dir`.

## 9. Verification

1. Branch run of `libs.yml` with a temporary `push` trigger on the branch, tag `core-test-<version>`.
2. `npm install` of the Node.js package with `RELEASE_TAG = 'core-test-<version>'` on Linux x86_64, Linux aarch64, macOS aarch64, macOS x86_64, Windows x86_64.
3. Postgres: `SIMPLEX_BACKEND=postgres npm install` on Linux x86_64.
4. Release `core-test-<version>` deleted after verification.
