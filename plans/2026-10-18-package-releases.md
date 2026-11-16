# Package releases

Repository: simplex-chat/simplex-chat.
Prerequisite: PR #7750 merged; master runs are not cancelled.
Scope: releases in simplex-chat-libs, npm and PyPI.
App releases (`v*` tags in simplex-chat): unchanged.

## Releases

| Release | Repository | Name | Released when | Assets |
|---|---|---|---|---|
| libsimplex | simplex-chat-libs | `core-<version of simplex-chat.cabal>` | push to master or stable claims the version for its branch | `LIBS_ASSETS` |
| `@simplex-chat/types` | npm | version of its `package.json` | library release push, version absent from npm | |
| add-on | simplex-chat-libs | `nodejs-addon-<7 characters of the pushed commit sha>` | push to master or stable, published release of the add-on hash absent | `ADDON_ASSETS` |
| `simplex-chat` (npm) | npm | version of its `package.json` | push to master or stable, version absent from npm | |
| `simplex-chat` (PyPI) | PyPI | `__version__` of `_version.py` | push to master or stable, version absent from PyPI | |

Release states in simplex-chat-libs:

```
absent
draft       created before the build jobs; assets uploaded by the build jobs
published   every asset present; immutable
```

Add-on hash: output of the current `addon_tag` function without its prefix.
Add-on release notes: `sources: <add-on hash>`.

Types source: `packages/simplex-chat-client/types/typescript/src`.
Python types source: `packages/simplex-chat-python/src/simplex_chat/types`.

## Library claim

Claim: entry `core.<branch>.<version>` = simplex-chat commit in `releases.toml` of simplex-chat-libs.
Branches: master, stable.
Written through the contents API with the blob sha; a concurrent writer re-reads after 409.

Rows in order of evaluation:

| `core.<branch>.<version>` | `claim-core` result |
|---|---|
| version lower than the highest version recorded for the branch | exit 1 |
| version recorded on any branch for this commit | `resume` |
| version recorded on any branch for another commit | `skip` |
| absent, types check failed | exit 1 |
| absent, types check passed | `new`: entry written |

Previous core release: commit of the highest version recorded for the branch.

Types check:

```
types changed   types source differs from the previous core release
passed          types unchanged
                or types version unrecorded, absent from npm, simplexChatCore = <version>
```

## Types versions

```
scheme      <app version>.<n>, as simplex-chat (npm)
first       7.1.0-beta.6.1, from core-release --types-version
next        bump_npm_version of the types version at the previous core release
npm tag     dist_tag: beta for a prerelease version, latest otherwise
```

## Manual settings

1. simplex-chat-libs: immutable releases enabled.
2. npm trusted publisher of `simplex-chat`: workflow `build.yml`.
3. npm trusted publisher of `@simplex-chat/types`: workflow `build.yml`.
4. PyPI trusted publisher of `simplex-chat`: workflow `build.yml`.
5. Secret `RELEASE_PR_TOKEN`: deleted.
6. PR #7732: closed; branch `packages-release/master`: deleted.
7. `releases.toml` entry added: `core.master."7.1.0.11"` = `9711dcfc9c39ba125c1fb1674a3e039b1b9c0e81`.
8. Notes of release `nodejs-addon-308c0960c707`: `sources: 308c0960c707`.

## Files

- `scripts/ci/packages_release.py`
- `scripts/ci/test_packages_release.py`
- `scripts/ci/package-tests.sh`: new.
- `.github/workflows/build.yml`
- `.github/workflows/packages.yml`
- `packages/simplex-chat-client/types/typescript/package.json`: field `simplexChatCore`, `7.1.0.11`.
- `packages/simplex-chat-client/types/typescript/README.md`: line `SimpleX Chat core: 7.1.0.11`.

## packages_release.py

Asset set by tag prefix:

```
core-           LIBS_ASSETS
nodejs-addon-   ADDON_ASSETS
```

Commands:

```
core-release <version> [--branch <branch>] [--types-version <version>]
    run locally
    branch: the checked out branch by default
    version not higher than the highest version recorded for the branch: exit 1
    version: of simplex-chat.cabal written
    types changed or --types-version: types version written, as in Types versions
    types version written: simplexChatCore and the README line written
claim-core <version> <commit> <branch>
    run in maybe-release
    Library claim; prints claim=new, claim=resume or claim=skip
    new: also prints commit=<simplex-chat-libs commit of the entry>
release-status <tag>
    prints status=published, status=draft or status=absent
    drafts: from the authenticated release list
addon-release
    releases nodejs-addon-* with notes "sources: <add-on hash of the checkout>"
    published one: prints tag=<tag> and status=published
    draft: prints tag=<tag> and status=draft
    neither: prints status=absent
latest-libs [--branch <branch>]
    prints tag=<newest published core-* release of a version recorded for the branch, with every LIBS_ASSETS digest>
    order: numeric components of the version
publish-release <tag>
    draft with every asset of its set: published
    missing assets: listed, exit 1
check-pins
    pins of release.ts and _version.py compared with the digests of published releases
    mismatch, missing asset or unpublished release: listed, exit 1
download <tag> <asset> <dir>
    asset saved to <dir>/<asset>
    SHA-256 compared with the API digest; mismatch: exit 1
download-pinned <asset> <dir>
    tag and SHA-256 from release.ts
    asset saved to <dir>/<asset>; mismatch: exit 1
prepare [nodejs] [python] [--branch <branch>]
    run locally
    branch: the checked out branch by default
    nodejs: simplex-chat (npm); python: simplex-chat (PyPI)
    packages: the listed ones; all when none is listed
    other package: pins and version unchanged
    library pin: latest-libs of the branch
    warning: core-<version of simplex-chat.cabal> newer than the pinned release
    nodejs add-on pin: addon-release; exit 1 unless published with every asset
    nodejs types dependency: ^<types version of the branch>; exit 1 unless published on npm
    python: exit 1 when the Python types source differs from the commit recorded for the pinned core version
    every pinned asset: download, SHA-256 compared with the API digest
    changed package: directory without tests differs from the commit that set its version
    versions: bumped as now; --nodejs-version, --python-version override
    written: release.ts, _version.py, package.json of simplex-chat (npm)
    summary: printed; --summary and --types-version removed
build-plan
    pull requests only
    --event and --release-missing removed
```

Commands removed: `addon-tag`.
`record`: option `--branch` of `--core`, default master; replaced by `claim-core` in maybe-release.
Unchanged: `changes`, `publish-plan`, `test-names`.

`build_plan(core_changed, addon_changed)`:

```
core changed, add-on changed     platforms: all; test: false
core changed, add-on unchanged   platforms: none; test: false
core unchanged                   platforms: all; test: true
```

## package-tests.sh

Commands from the build steps of `packages.yml` (lines 147-166).

```
build <platform>
    types, npm package and add-on built
    add-on copied to packages/simplex-chat-nodejs/build/simplex-chat-nodejs-<platform>.node
test
    jest, as in packages.yml; SIMPLEX_LIBS_DIR and SIMPLEX_ADDON_PATH from the environment
    pytest
addon <platform>
    build <platform>
    simplex-chat-libs-<platform>.zip of $LIBS_RELEASE: packages_release.py download, unpack to $RUNNER_TEMP/libs
    jest with SIMPLEX_LIBS_DIR=$RUNNER_TEMP/libs/libs
    add-on uploaded to draft $ADDON_TAG of $LIBS_REPOSITORY: gh release upload --clobber, token $GH_TOKEN
publish <platform> [nodejs] [python]
    nodejs: types and npm package built
            download-pinned simplex-chat-nodejs-<platform>.node to $RUNNER_TEMP/addon
            add-on copied to packages/simplex-chat-nodejs/build/Release/simplex.node, the path of the tests
            jest
    python: pytest
    run in CI and locally
```

## build.yml

### maybe-release

Outputs:

```
libs_tag         core-<version>, when the claim is new or resume
addon_tag        tag of the add-on draft
addon_draft      true when the add-on release is a draft after this job
libs_latest      output tag of latest-libs
publish_types    libs_tag set, types version printed by publish-plan
publish_nodejs   nodejs version printed by publish-plan
publish_python   python version printed by publish-plan
publish          publish_nodejs or publish_python
```

Steps after "Create release":

| Step | Condition | Action |
|---|---|---|
| Library release | push to master or stable | `claim-core <version> <github.sha> <github.ref_name>`; new → `gh release create core-<version> --draft --target <commit>`; resume → existing draft, `libs_tag` empty when published; skip → notice annotation |
| Add-on release | push to master or stable | `addon-release`: absent → `gh release create nodejs-addon-<7 characters of github.sha> --draft --notes "sources: <add-on hash>"`; draft → reused; output `addon_draft` true when absent or draft |
| Latest library | `addon_draft` | `latest-libs --branch <github.ref_name>` |
| Versions to publish | push to master or stable | `publish-plan`; environment `NPM_SCOPE`, `PYPI_PROJECT`, `PYPI_URL` as in `packages.yml` |

Steps removed: "Check version change", "Record libs release", "Create libs release".
"Clone project": `fetch-depth: 2`, unchanged; the types check fetches the commit of the recorded types version.
Token of the release steps: `NODEJS_REPO_TOKEN`.
Library release jobs: concurrency groups with `libs_tag`, unchanged.

### Library uploads

Steps "Upload library" (build-linux, build-macos, build-windows) and "Upload postgres library":

```
action      gh release upload <libs_tag> <zip files> --clobber --repo <repository>-libs
token       NODEJS_REPO_TOKEN
conditions  unchanged
```

### Package steps in build jobs

Platform jobs:

```
build-linux        matrix.libs entry       linux-x86_64
build-linux-push   matrix.libs entry       linux-aarch64
build-macos        both entries            macos-<arch>
build-windows      windows-latest          windows-x86_64
```

Steps, after "Run tests" (build-linux, build-macos) and after "Upload library" (build-windows):

| Step | Condition |
|---|---|
| Setup Node.js 22 | `!cancelled()`; core pull request condition, `addon_draft` or `publish` |
| Setup Python 3.12 | `!cancelled()`, Setup Node.js succeeded |
| Package tests (pull request) | build-linux only, as now; `package-tests.sh build linux-x86_64`, then `test` with SIMPLEX_LIBS_DIR |
| Add-on | `!cancelled()`, `addon_draft`, Setup Python succeeded; `package-tests.sh addon <platform>` |
| Package tests | `!cancelled()`, `publish`, Setup Python succeeded; `package-tests.sh publish <platform>`, with `nodejs` when `publish_nodejs`, with `python` when `publish_python` |
| Package tests result | Package tests succeeded; artifact `package-tests-<platform>` |

Linux steps: on the host.

Environment of "Add-on":

```
LIBS_RELEASE      libs_latest
ADDON_TAG         addon_tag
LIBS_REPOSITORY   <repository>-libs
GH_TOKEN          NODEJS_REPO_TOKEN
```

### Final jobs

```
releases
    needs        maybe-release, build-linux, build-linux-push, build-linux-postgres, build-macos, build-windows
    if           !cancelled(), libs_tag or addon_draft
    step 1       publish-release <libs_tag>; libs_tag
    step 2       publish-release <addon_tag>; !cancelled(), addon_draft
    token        NODEJS_REPO_TOKEN
publish
    needs        maybe-release, build-linux, build-linux-push, build-macos, build-windows, releases
    if           !cancelled(), publish or publish_types
    permissions  contents: read; id-token: write
    env          as in packages.yml: NODE_AUTH_TOKEN, NPM_SCOPE, PYPI_PROJECT, PYPI_URL
    step 1       when publish: artifacts package-tests-*, 5 required
    step 2       when publish: check-pins
    steps        "publish" job of packages.yml, from "Versions to publish"
    types step   publish_types and needs.releases.result == 'success'
record
    as in packages.yml; needs publish
```

Comments of the `publish` job of `packages.yml`: moved with it.
Comment of the `record` job: removed.
Trusted publishing comment (`packages.yml` lines 12-15): moved above `publish`.

Job removed: `packages`, with its header comment.

## packages.yml

```
triggers       pull_request, paths unchanged
               push, workflow_dispatch, workflow_call: removed
addon job      steps "Add-on release tag", "Release branch", "Check the add-on release": removed
               outputs tag, release_branch, missing: removed
               Build plan: --event and --release-missing removed
build job      Checkout, Setup Node.js, Setup Python
               package-tests.sh build <platform>
               package-tests.sh test, when planned
               "Upload add-on artifact": removed
jobs removed   publish, record, release
header         lines 3-10 and 17-28: removed
```

## Release procedure

Library and types:

1. `python3 scripts/ci/packages_release.py core-release <version>` on master or stable, with `--types-version` for a new app version.
2. Commit of the written files; push to the same branch.
3. `build.yml`: claim, draft, uploads by the build jobs, `releases`, types publication.
4. Failed claim: fix commit; the next push claims the version.
5. Incomplete draft: re-run of the failed build jobs, then of `releases` and `publish`.

Add-on:

1. Push to master or stable changes the add-on sources.
2. Incomplete draft: completed by a re-run or by the next push.

Packages, published by GitHub:

1. `git pull` on master or stable.
2. `python3 scripts/ci/packages_release.py prepare`, with package names and version overrides when needed.
3. Commit of the written files; push to the same branch.
4. Versions absent from the registries: published by `build.yml`.

Packages, published locally:

1. Steps 1 and 2 above.
2. `scripts/ci/package-tests.sh publish <local platform> [nodejs] [python]`.
3. `npm publish` of `simplex-chat`; `python -m build` and `twine upload` of the Python package.
4. Step 3 above; `build.yml` skips the published versions.

Published npm package: `files` of `package.json` and the pins; add-on and libsimplex downloaded at run time.

## Verification

1. Unit tests:
   - `claim-core`: every row of the Library claim table
   - types check: changed and unchanged types, recorded and unrecorded types version
   - `core-release` with and without types changes
   - `release-status`
   - `addon-release` notes lookup
   - `prepare` with one package listed
   - `prepare python` with changed Python types
   - `latest-libs` order
   - `publish-release` with missing assets
   - `check-pins` mismatch
   - `download` digest mismatch
   - `build_plan` cases
2. `gh release upload` to a draft by tag: `shared.FetchRelease` of gh looks up the draft by tag (`fetchDraftRelease`).
3. First library release after the merge: assets uploaded to the draft, release published by `publish-release`.
4. First master push after merge: claim `skip`, as `core-7.1.0.11` is recorded for `9711dcfc`; `addon-release` finds `nodejs-addon-308c0960c707`; package steps skipped.
5. Local `prepare`: pins `core-7.1.0.11` and `nodejs-addon-308c0960c707`; every download matches.
6. `core-release 7.1.0.12 --types-version 7.1.0-beta.6.1`, commit, push: claim `new`, draft, 6 assets uploaded, release published, types 7.1.0-beta.6.1 published with tag beta.
7. Master push with an add-on source change: draft created, 5 assets uploaded, release published.
8. Master push with versions from `prepare`: 5 artifacts `package-tests-*`, pins checked, versions published, versions recorded.
9. Second push during the run of step 6: claim `skip`.
10. Stable push after master is merged into stable, with a stable core version: claim `new` for `core.stable`.
