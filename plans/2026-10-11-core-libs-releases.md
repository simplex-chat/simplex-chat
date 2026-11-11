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
| `simplex-chat-libs-linux-x86_64-postgres.zip` | `build-linux-postgres` | postgres |
| `simplex-chat-libs-linux-aarch64.zip` | `build-linux`, 22.04 aarch64 | sqlite |
| `simplex-chat-libs-macos-aarch64.zip` | `build-macos`, `macos-latest` | sqlite |
| `simplex-chat-libs-macos-x86_64.zip` | `build-macos`, `macos-15-intel` | sqlite |
| `simplex-chat-libs-windows-x86_64.zip` | `build-windows` | sqlite |

Zip content: directory `libs/` with the top-level files of `apps/multiplatform/common/src/commonMain/cpp/desktop/libs/<os>-<arch>/`.
Windows zip: also `libs/libsimplex.lib`.
Asset names: as built by `download-libs.js` (line 123).
Checksums: `digest` (sha256) of each asset in the GitHub release API.

## 3. `build.yml`

### 3.1. `maybe-release`

The release is created before the build jobs; each build job uploads its assets, as for `v*` tags.

```
outputs        libs_tag: tag of the created library release, empty otherwise
Clone project  condition: push event; fetch-depth: 2
```

Steps after "Create release":

| Step | Condition | Action |
|---|---|---|
| Check version change | ref `master` or `stable` | output `version` when `version:` of `simplex-chat.cabal` differs from `HEAD^1:simplex-chat.cabal` |
| Clone libs repo | `version` set | checkout of simplex-chat-libs to `libs-repo`, token `NODEJS_REPO_TOKEN` |
| Commit and tag libs release | `version` set | commit and tag below; `git push --atomic origin HEAD <tag>`; output `tag` |
| Create libs release | `version` set | `softprops/action-gh-release@v2`, repository simplex-chat-libs, `tag_name` = `tag`, without files |

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
Secrets comment: above `maybe-release`.

A failure of `maybe-release` skips the build jobs, as for `v*` tags.

### 3.2. Concurrency

Workflow-level `concurrency`: removed.

Job-level `concurrency` in `build-linux`, `build-macos`, `build-windows`:

```
group                ${{ github.workflow }}-${{ github.ref }}-<job>-<matrix entry>-${{ needs.maybe-release.outputs.libs_tag }}
cancel-in-progress   not a v* tag and libs_tag empty
```

Library release runs are never cancelled; other runs are cancelled by the next push to the same ref.

### 3.3. Library steps

| Job | Entries | Position | Build |
|---|---|---|---|
| `build-linux` | 22.04 x86_64 and 22.04 aarch64, GHC 9.6.3 (matrix key `libs: true`) | after "Build CLI" | `SKIP_VLC=1 scripts/desktop/build-lib-linux.sh` |
| `build-macos` | both (matrix key `arch`) | after "Run tests" | `SKIP_VLC=1 scripts/desktop/build-lib-mac.sh` |
| `build-windows` | `windows-latest` | after "Build CLI" | `SKIP_VLC=1 scripts/desktop/build-lib-windows.sh`; `dlltool -d libsimplex.dll.def -l libsimplex.lib -D libsimplex.dll` |

Each build step: staging in `release-libs/<asset name>/libs/`, then `zip -r` to `release-libs/<asset name>.zip`.
Linux steps run in the build container.
Windows: `zip` added to the MSYS2 `install` list.

Upload step, after the build steps of the job:

```
action       softprops/action-gh-release@v2
repository   ${{ github.repository }}-libs
tag_name     needs.maybe-release.outputs.libs_tag
files        release-libs/*.zip
token        secrets.NODEJS_REPO_TOKEN
```

Conditions:

| Job | Build | Upload |
|---|---|---|
| `build-linux` | `libs_tag` set, `matrix.libs` | `libs_tag` set, `matrix.libs` |
| `build-macos` | `!cancelled()`, `libs_tag` set, CLI build succeeded | `!cancelled()`, library build succeeded |
| `build-windows` | `libs_tag` set | `libs_tag` set |

A failed job leaves its assets absent; the other jobs upload theirs.
A re-run of a failed job uploads its assets to the same release; an asset of the same name is replaced.

### 3.4. `build-linux-postgres`

Edits of the existing job:

```
if                    needs.maybe-release.outputs.libs_tag, replaces the v* tag condition
Copy libs from container
                      files copied to postgres-libs/libs/
                      zip -r simplex-chat-libs-linux-x86_64-postgres.zip libs, in postgres-libs
Upload postgres library
                      replaces the artifact upload
                      softprops/action-gh-release@v2, files postgres-libs/*.zip, other inputs as in 3.3
```

## 4. Scripts: `SKIP_VLC`

With `SKIP_VLC` set, the VLC download is skipped.
Unset: unchanged.

| File | Line | After |
|---|---|---|
| `scripts/desktop/build-lib-linux.sh` | 56 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-linux.sh` |
| `scripts/desktop/build-lib-mac.sh` | 154 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-mac.sh` |
| `scripts/desktop/build-lib-windows.sh` | 64 | `[ -n "$SKIP_VLC" ] \|\| scripts/desktop/prepare-vlc-windows.sh` |

## 5. `build.yml`: removed job

`release-nodejs-libs`, with its header comment.

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
release-nodejs-libs   removed; its add-on upload edits move to the add-on release
RELEASE_TAG           library release core-<version>; add-on assets in a separate release
SHA256                library values from the asset digests of release core-<version>
```

## 8. `build.yml` fix

Line 531: `matrix.opnessl_dir` → `matrix.openssl_dir`.

## 9. Verification

1. Branch run of `build.yml` with temporary edits: `push` trigger and "Check version change" condition with the branch, tag `core-test-<version>`, cabal version increment in the head commit of the push.
2. `npm install` of the Node.js package with `RELEASE_TAG = 'core-test-<version>'` on Linux x86_64, Linux aarch64, macOS aarch64, macOS x86_64, Windows x86_64.
3. Postgres: `SIMPLEX_BACKEND=postgres npm install` on Linux x86_64.
4. Release `core-test-<version>` and its tag deleted after verification.
