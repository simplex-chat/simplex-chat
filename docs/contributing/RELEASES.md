# Library and package releases

## Releases

| Release | Location | Version | Published on |
|---|---|---|---|
| libsimplex | simplex-chat-libs release `core-<version>` | `version` of `simplex-chat.cabal` | push to master or stable with a new core version |
| Node.js add-on | simplex-chat-libs release `nodejs-addon-<7 characters of the commit>` | | push to master or stable with unreleased add-on sources |
| `@simplex-chat/types` | npm | `version` of `packages/simplex-chat-client/types/typescript/package.json` | library release with an unpublished types version |
| `simplex-chat` | npm | `version` of `packages/simplex-chat-nodejs/package.json` | push to master or stable with an unpublished version |
| `simplex-chat` | PyPI | `__version__` of `packages/simplex-chat-python/src/simplex_chat/_version.py` | push to master or stable with an unpublished version |

Add-on sources:
- `packages/simplex-chat-nodejs/binding.gyp`
- `packages/simplex-chat-nodejs/cpp/`
- `node-addon-api` version in `devDependencies` of `packages/simplex-chat-nodejs/package.json`

Release states in simplex-chat-libs:

```
draft       created by maybe-release; assets uploaded by the build jobs
published   every asset present; published by the releases job
```

npm tag: `beta` for a version with a prerelease part, `latest` otherwise.

Releases and publications are independent of the Haskell test results.

`releases.toml` of simplex-chat-libs, written by `build.yml`:

```
core.<branch>.<version>     commit of the library release; branch: master or stable
npm.<package>.<version>     commit of the publication
pypi.<package>.<version>    commit of the publication
```

## Library release

1. On master or stable, after `git pull`:

   ```
   python3 scripts/ci/packages_release.py core-release <version>
   ```

   Checked: version higher than the highest version recorded for the branch.

   Written:
   - `version` of `simplex-chat.cabal`
   - with types changed since the previous library release of the branch: types `version` (bumped), `simplexChatCore`, line `SimpleX Chat core:` of the types README

   `--types-version` sets the types version, for the first release of an app version, e.g. `--types-version 7.1.0-beta.7.1`.

2. Commit, push to the same branch.

3. Run of `build.yml`:
   - `maybe-release`: version claimed for the commit in `releases.toml`, draft `core-<version>` created
   - build jobs: 6 library zips uploaded; macOS libraries signed with the Developer ID and notarized
   - `releases`: release published
   - `publish`: `@simplex-chat/types` published, when its version is unpublished
   - `record`: types version recorded

Failed claim, with every build job skipped:
- version lower than the highest version recorded for the branch
- types changed, types version published
- types changed, `simplexChatCore` different from the core version

After a failed claim, a fix commit is pushed; the version is claimed by the next push.

Incomplete draft: completed by a re-run of the failed jobs in the run of the claim. The version is skipped by later pushes, as it is recorded for another commit.

## Add-on release

Run of `build.yml` on a push to master or stable with unreleased add-on sources:
- `maybe-release`: draft `nodejs-addon-<7 characters of the commit>` created, notes `sources: <hash of the add-on sources>`
- build jobs, 5 platforms: add-on built, tested with the latest library release of the branch, uploaded
- `releases`: release published

Incomplete draft: completed by a re-run or by the next push with the same add-on sources.

## Package release

1. On master or stable, after `git pull`:

   ```
   python3 scripts/ci/packages_release.py prepare [nodejs] [python]
   ```

   Both packages when none is listed.

   Checked:
   - published library release recorded for the branch
   - add-on release published with every asset (Node.js)
   - types version published on npm (Node.js)
   - Python types unchanged since the commit of the pinned library release (Python)
   - pinned releases published with the pinned SHA-256
   - every pinned asset downloaded with a matching SHA-256

   Written:
   - pins in `packages/simplex-chat-nodejs/src/release.ts` and `_version.py`: latest library release of the branch, add-on release of the current sources (Node.js), SHA-256 of every asset
   - `@simplex-chat/types` dependency: `^<types version>` (Node.js)
   - package versions: a published version bumped when the pins or the package files changed

   `--nodejs-version` and `--python-version` set the versions, for the first release of an app version, e.g. `--nodejs-version 7.1.0-beta.7.1 --python-version 7.1.0b7`.

2. Commit, push to the same branch.

3. Run of `build.yml`:
   - build jobs, 5 platforms: packages tested, Node.js with the pinned add-on
   - `publish`: package tests of 5 platforms and pins checked, unpublished versions published with trusted publishing
   - `record`: published versions recorded

Versions unpublished after a failure: published by the next push to the branch.

## Local publication

1. Step 1 of the package release.

2. Package tests in a Python virtual environment, on the local platform (`linux-x86_64`, `linux-aarch64`, `macos-aarch64`, `macos-x86_64` or `windows-x86_64`):

   ```
   scripts/ci/package-tests.sh publish <platform> nodejs python
   ```

3. In `packages/simplex-chat-nodejs`, with the npm tag of the version:

   ```
   npm publish --access public --tag beta
   ```

4. In `packages/simplex-chat-python`:

   ```
   python -m pip install build twine
   python -m build
   twine upload dist/simplex_chat-<version>*
   ```

5. Commit, push to the same branch. Locally published versions are skipped by `build.yml` and are not recorded in `releases.toml`.

## Settings

- simplex-chat-libs: release immutability enabled
- secret `NODEJS_REPO_TOKEN`: contents of simplex-chat-libs, read and write
- secrets `APPLE_SIMPLEX_SIGNING_KEYCHAIN`, `APPLE_SIMPLEX_NOTARIZATION_APPLE_ID` and `APPLE_SIMPLEX_NOTARIZATION_PASSWORD`: signing and notarization of the macOS libraries, shared with the desktop app build
- trusted publisher of `simplex-chat` and `@simplex-chat/types` on npm and of `simplex-chat` on PyPI: workflow `build.yml`
- secrets `NPM_TOKEN` and `PYPI_API_TOKEN`: used instead of trusted publishing when set
- variables `NPM_SCOPE`, `PYPI_PROJECT`, `PYPI_URL` and `PYPI_UPLOAD_URL`: publication under other names, for test repositories

## Pull requests

- `build.yml`, for every pull request it runs for: add-on built and packages tested on 4 platforms (Linux aarch64 excluded); Linux x86_64 uses the library built from the pull request when the core changed, other platforms the pinned library
- `packages.yml`, for package changes in a pull request without `build.yml` paths: add-on built and packages tested on 5 platforms
