# Core library releases: implementation plan

Libraries for the Node.js package are released per core version, from the commit on `master` or `stable` that changes the version.

## 1. Release identity

```
repository     simplex-chat/simplex-chat-libs
tag            core-<version>
version        version: field of simplex-chat.cabal, e.g. 7.1.0.10
build          push to master or stable, head commit changes version: compared with its first parent
source         the head commit of that push
```

A version change that is not the head commit of its push is not released.

Rule: a core version is final once a published package depends on it.
Its migrations and chat protocol versions stay unchanged.
A further change requires a new cabal version.

## 2. Assets

| Asset | Job | Backend |
|---|---|---|
| `simplex-chat-libs-linux-x86_64.zip` | `build-linux`, 22.04 x86_64 | sqlite |
| `simplex-chat-libs-linux-x86_64-postgres.zip` | `build-linux`, 22.04 x86_64 | postgres |
| `simplex-chat-libs-linux-aarch64.zip` | `build-linux`, 22.04 aarch64 | sqlite |
| `simplex-chat-libs-macos-aarch64.zip` | `build-macos`, `macos-latest` | sqlite |
| `simplex-chat-libs-macos-x86_64.zip` | `build-macos`, `macos-15-intel` | sqlite |
| `simplex-chat-libs-windows-x86_64.zip` | `build-windows` | sqlite |
| `SHA256SUMS` | `release-nodejs-libs` | — |

Zip content: directory `libs/` with the top-level files of `apps/multiplatform/common/src/commonMain/cpp/desktop/libs/<os>-<arch>/`.
Windows zip: also `libs/libsimplex.lib`.
Asset names: as built by `download-libs.js` (line 123).

## 3. `build.yml`

### 3.1. `check-version`

New job, after `maybe-release`.

```
runner     ubuntu-latest
checkout   fetch-depth: 2
version    sed -n 's/^version: *//p' simplex-chat.cabal
bump       push event, ref master or stable, version differs from HEAD^1:simplex-chat.cabal
outputs    version, bump
```

### 3.2. Concurrency

Workflow-level `concurrency`: removed.

Job-level `concurrency` in `build-linux`, `build-macos`, `build-windows`:

```
group                ${{ github.workflow }}-${{ github.ref }}-<job>-<matrix entry>, and -${{ github.sha }} when bump
cancel-in-progress   not a v* tag and not bump
```

Bump runs are never cancelled; other runs are cancelled by the next push to the same ref.

### 3.3. Library steps

`needs` of the three build jobs: adds `check-version`.

| Job | Entries | Position | Steps |
|---|---|---|---|
| `build-linux` | 22.04 x86_64 and 22.04 aarch64, GHC 9.6.3 (matrix key `libs: true`) | after "Build CLI" | `SKIP_VLC=1 scripts/desktop/build-lib-linux.sh`; x86_64 also `postgres`; upload |
| `build-macos` | both (matrix key `arch`) | after "Run tests", `!cancelled()`, CLI build succeeded | `SKIP_VLC=1 scripts/desktop/build-lib-mac.sh`; upload |
| `build-windows` | `windows-latest` | after "Build CLI" | `SKIP_VLC=1 scripts/desktop/build-lib-windows.sh`; `dlltool -d libsimplex.dll.def -l libsimplex.lib -D libsimplex.dll`; upload |

Condition of all library steps: `needs.check-version.outputs.bump == 'true'`.
Linux steps run in the build container.
Artifacts: `libs-<platform>-<arch>`, each with directories `<asset name>/libs/`.

### 3.4. `release-nodejs-libs`

Replaces the job body of the same name; header comment and secrets comment kept.

```
needs        check-version, build-linux, build-macos, build-windows
condition    !cancelled() and bump
steps        download artifacts libs-*, merged
             zip of each of the six asset names; missing asset fails the job
             sha256sum simplex-chat-libs-*.zip > SHA256SUMS
             checkout of simplex-chat-libs
             commit and tag in simplex-chat-libs, pushed to master
             softprops/action-gh-release@v2
repository   simplex-chat/simplex-chat-libs
tag_name     core-<version>
files        simplex-chat-libs-*.zip, SHA256SUMS
token        secrets.NODEJS_REPO_TOKEN
```

Commit in simplex-chat-libs:

```
file         core-release
content      version: <version>
             commit: <simplex-chat commit sha>
author       SimpleX Chat <build@simplex.chat>
message      core-<version>
tag          core-<version>, on this commit
```

`NODEJS_REPO_TOKEN`: a token of the build account; releases are attributed to the token owner.

The release follows the build jobs, after their tests; test results do not affect it.

## 4. Scripts: `SKIP_VLC`

With `SKIP_VLC` set, the VLC download is skipped.
Unset: unchanged.

| File | Line | After |
|---|---|---|
| `scripts/desktop/build-lib-linux.sh` | 56 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-linux.sh` |
| `scripts/desktop/build-lib-mac.sh` | 154 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-mac.sh` |
| `scripts/desktop/build-lib-windows.sh` | 64 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-windows.sh` |

## 5. `build.yml`: removed job

`build-linux-postgres`, with its header comment.

## 6. Node.js package

```
download-libs.js   RELEASE_TAG = 'core-<version>', set in the Node.js release commit, once release core-<version> exists
package.json       version: Node.js release cycle
release commits    RELEASE_TAG removed from their edits (author build@simplex.chat)
```

This change: `RELEASE_TAG` unchanged, `'v7.1.0-beta.6'`.

Platforms: `linux-aarch64` added by the assets in section 2; `download-libs.js` maps `arm64` to `aarch64` (lines 46-50).
Postgres: Linux x86_64 only (lines 19-22).

## 7. Precompiled add-on (`ep/node-add-on`)

Add-on releases: separate from library releases, triggered by changes of the Node.js package version, designed in `ep/node-add-on`.

Effects of this plan on `ep/node-add-on`:

```
release-nodejs-libs   replaced by section 3.4; its add-on upload edits move to the add-on release
RELEASE_TAG           library release core-<version>; add-on assets in a separate release
SHA256                library values from SHA256SUMS of release core-<version>
```

## 8. `build.yml` fix

Line 531: `matrix.opnessl_dir` → `matrix.openssl_dir`.

## 9. Verification

1. Branch run of `build.yml` with temporary edits: `push` trigger and bump condition with the branch, tag `core-test-<version>`, cabal version increment in the head commit of the push.
2. `npm install` of the Node.js package with `RELEASE_TAG = 'core-test-<version>'` on Linux x86_64, Linux aarch64, macOS aarch64, macOS x86_64, Windows x86_64.
3. Postgres: `SIMPLEX_BACKEND=postgres npm install` on Linux x86_64.
4. Release `core-test-<version>` and its tag deleted after verification.
