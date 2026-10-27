# CI on GitHub's runners

Three workflows build the apps from a branch's own code and test them, only when asked: `ios-sim` (the iOS app in a simulator on a macOS runner), `desktop-linux` (the desktop app on a Linux runner) and `android-emu` (the Android app in an emulator on a Linux runner). Each runs a scripted smoke test and, on request, a Claude session that follows a test plan, and a test backend (relay, directory, names server, peers). Details: [iOS](ios-sim/README.md), [desktop](desktop-linux/README.md), [Android](android/README.md).

## Reduced build

GitHub runs the reduced build of `build.yml` on a dispatch (workflow_dispatch), on each push to a draft pull request and on a push to master or stable in a fork. Once the pull request is marked ready for review, GitHub runs the full matrix, as on a push to master or stable in simplex-chat/simplex-chat.

On a push to master or stable in a fork, the release steps and tests are skipped; build-linux saves the -O0 build caches, and on master the Docker image cache, that the fork's dispatches restore.

On a draft pull request, the tests are selected by a fenced block with the info string `ci-tests` in the description, one hspec match pattern per line; the block is read at each push. `/` runs the whole suite; without a block, the reduced build runs no tests.

### Dispatch

```sh
curl -fsS -X POST \
  -H "Authorization: Bearer $GH_TOKEN" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/repos/<owner>/simplex-chat/actions/workflows/build.yml/dispatches \
  -d '{"ref": "<branch>", "inputs": {"tests": "/SimpleX chat client/group tests/\n/Message batching/"}}'
```

Endpoint: https://docs.github.com/en/rest/actions/workflows#create-a-workflow-dispatch-event. Inputs:

| Input | Default | Value |
|---|---|---|
| `tests` | empty | hspec match patterns, one per line (`\n` in JSON); `/` runs the whole suite, and an empty value builds without tests |
| `base` | `master` | the branch that the changes are compared with:<br>`stable` for a branch cut from stable<br>the parent branch for a stacked branch |

When a newer dispatch on the same branch starts build-linux or build-ios, GitHub cancels the unfinished job of the older one (`cancel-in-progress`).

Results:
- Run and jobs: `GET /repos/<owner>/simplex-chat/actions/runs?head_sha=<commit>&event=workflow_dispatch` returns the run id; `GET /repos/<owner>/simplex-chat/actions/runs/<run id>/jobs` returns each step with its start, end and conclusion.
- Job log: `GET /repos/<owner>/simplex-chat/actions/jobs/<job id>/logs` returns a redirect to a text file, valid for 1 minute (https://docs.github.com/en/rest/actions/workflow-jobs#download-job-logs-for-a-workflow-run). The test summary is printed as "Finished in N seconds" and "N examples, N failures".

### Scopes

The reduced build runs one Linux job, build-linux (ubuntu-22.04 x86_64), for every scope except ios, and one macOS job, build-ios (macos-latest), for ios.

#### core

- Globs: `BUILD_CHANGES["haskell"]` in packages_release.py.
- CI, in build-linux:
  - Docker image (step "Build and cache Docker image");
  - caches (the steps "Restore ... from cache");
  - `cabal build -j --enable-tests --ghc-options=-O0` (step "Build CLI");
  - simplex-chat-test with the selected tests (step "Run tests").
- Local:
  ```sh
  cp scripts/cabal.project.local.linux cabal.project.local
  cabal build --enable-tests --ghc-options=-O0
  mkdir -p tests/tmp
  cabal test simplex-chat-test --ghc-options=-O0 --test-option=-m --test-option='/SimpleX chat client/group tests/'
  ```
  The selection runs as in CI with scripts/ci/run-tests.sh, which appends the patterns of `TESTS` to the test binary:
  ```sh
  TESTS='/SimpleX chat client/group tests/' scripts/ci/run-tests.sh "$(cabal list-bin simplex-chat-test --ghc-options=-O0)"
  ```
  `--ghc-options=-O0` matches the reduced build; the library and test-suite stanzas of simplex-chat.cabal set -O2. Test directories are created under tests/tmp (`tmpBracket` in tests/Test.hs).

#### kotlin

- Globs: `BUILD_CHANGES["kotlin"]` in packages_release.py.
- CI: Gradle in the Docker container of build-linux (step "Build and test Kotlin apps", matrix field `kotlin`).
- Local:
  ```sh
  cd apps/multiplatform
  ./gradlew --no-daemon --max-workers=1 :common:desktopTest :desktop:compileKotlinJvm -x :desktop:cmakeBuildAndCopy :android:compileGoogleDebugKotlin :android:testFossDebugUnitTest
  ```
  JDK 17 and an Android SDK in ANDROID_HOME are required, as installed by Dockerfile.build.

#### ios

- Globs:
  - apps/ios/**
  - scripts/ci/ios-stub-libs.sh
- CI: build-ios: stub libraries, then xcodebuild for the iOS Simulator without signing.
- Local, on macOS: `scripts/ci/ios-stub-libs.sh`, then the xcodebuild command of the step "Build iOS app" of build-ios.

#### packages

- Globs:
  - packages/simplex-chat-nodejs/**
  - packages/simplex-chat-client/types/typescript/**
  - packages/simplex-chat-python/**
  - scripts/ci/package-tests.sh
- CI, in build-linux:
  - Node 22 and Python 3.12 (steps "Setup Node.js" and "Setup Python");
  - libsimplex, when `CORE_SOURCES` of packages_release.py also changed (step "Build library");
  - `package-tests.sh build linux-x86_64` and `package-tests.sh test` (step "Package tests (pull request)").
- With libsimplex built, SIMPLEX_LIBS_DIR is set to it for the nodejs tests; the python tests run with it unset (package-tests.sh). Without SIMPLEX_LIBS_DIR, the pinned release libraries are downloaded (packages/simplex-chat-nodejs/src/libs.ts).
- Local:
  ```sh
  scripts/ci/package-tests.sh build linux-x86_64
  scripts/ci/package-tests.sh test
  ```

#### badge-web

- Globs: apps/simplex-badge-service/web/**. These paths are also in the core globs (apps/simplex-badge-service/**), and web/src/catalog.ts is read by core tests (tests/Bots/BadgeService/CatalogTests.hs, WebTests.hs).
- CI, in build-linux: core build and tests, then `scope-tests.sh badge-web` (step "Test changed scopes").
- Selected core tests: `/SimpleX badge service/` and `/SimpleX badge service e2e/`.
- Local:
  ```sh
  cd apps/simplex-badge-service/web
  npm ci
  npm test
  ```

#### ci-scripts

- Globs:
  - scripts/ci/packages_release.py
  - scripts/ci/test_packages_release.py
- These paths are also in the core globs (scripts/ci/**).
- CI, in build-linux: `scope-tests.sh ci-scripts` (step "Test changed scopes").
- Local: `python3 scripts/ci/test_packages_release.py`; Python 3.11 or newer is required for tomllib (packages_release.py).

#### Scopes lacking CI tests

| Scope | Paths | CI |
|---|---|---|
| ios | apps/ios/** | compile only |
| types package | packages/simplex-chat-client/types/typescript/** | compile only |
| website | website/**<br>images/**<br>blog/**<br>docs/**<br>PRIVACY.md | build and deploy on master pushes (web.yml) |
| ts-client | packages/simplex-chat-client/typescript/** | - |
| webrtc | packages/simplex-chat-webrtc/** | - |
| mcp | packages/simplex-chat-mcp/** | - |
| calculator-bot | apps/simplex-calculator-bot/** | - |
| support-bot | apps/simplex-support-bot/** | - |
| support-bot-light | apps/simplex-support-bot-light/** | - |
| prices | scripts/prices/** | - |
| nft | eth/nft/** | - |

CI coverage of such a scope is added in the pull request that changes it (Adding CI coverage).

### Adding CI coverage

For a scope lacking CI tests, in the pull request that changes it:
1. scripts/ci/packages_release.py: an entry `"<scope>": ["<glob>", ...]` in SCOPE_CHANGES.
2. build.yml, `on.pull_request.paths`: the same globs; the two are compared in `test_build_yml_runs_for_the_paths_of_its_builds` of test_packages_release.py.
3. scripts/ci/scope-tests.sh: a `<scope>)` case with the build and test commands, and the scope name in the usage line.
4. scripts/ci/test_packages_release.py, ChangesTest: a case asserting that the scope is printed in `scopes` for a change to one of its files.
5. This file, "Scopes": a subsection for the scope, in place of its row in "Scopes lacking CI tests".
6. Checks: `python3 scripts/ci/test_packages_release.py`, the new case of scope-tests.sh locally, then a dispatch of the reduced build.

The scope commands run in build-linux after the core tests, with Node 22 and Python 3.12 (steps "Setup Node.js" and "Setup Python"). For a scope that needs other versions, a setup step is added before "Test changed scopes", conditioned on the scope name in `needs.maybe-release.outputs.scopes`. A scope that needs macOS is a BUILD_CHANGES entry with a separate maybe-release output, as ios is, and a step in build-ios; the `if` of build-ios includes that output.

## Starting a run

Pushes start none of the app workflows: a run is started by hand, on any branch, with inputs that are off by default and combine.

| Input | iOS | Desktop | Android |
|---|---|---|---|
| no input | scripted test, the app uploaded | scripted test, the app uploaded | scripted test, the app uploaded |
| `claude` | scripted test, then Claude | scripted test, then Claude | scripted test, then Claude |
| `backend` | also the test backend | also the test backend | also the test backend |
| `release` | — | also the latest release's onboarding, to tell a branch's failure from the runner's | also the latest release's onboarding, to tell a branch's failure from the runner's |

In the Actions tab: `ios-sim`, `desktop-linux` or `android-emu` → Run workflow, choose the branch and tick the inputs. With the GitHub CLI, each input is a field:

```sh
gh workflow run ios-sim.yml --ref <branch>
gh workflow run ios-sim.yml --ref <branch> -f claude=true
gh workflow run desktop-linux.yml --ref <branch> -f claude=true -f backend=true -f release=true
gh workflow run android-emu.yml --ref <branch> -f claude=true
```

A newer run of a workflow on the same branch cancels the older one.

Each run uploads the app it built, for manual tests, as an artifact kept 14 days: `ios-sim-app` (SimpleX.app.tar.gz, for the iOS simulator on an Apple-silicon Mac), `desktop-app` (desktop-app.tar.gz, for Linux x86-64) and `android-app` (the debug APK, arm64-v8a only). The run's page lists it under Artifacts; `gh run download <run id> -n <artifact>` downloads it.

## How long a run takes

| Run | iOS | Desktop | Android |
|---|---|---|---|
| Scripted test, builds reused | about 25 min, plus the wait for a macOS runner, which the team's builds share (minutes to an hour) | about 10 min | about 15 min |
| The core rebuilt, after Haskell changes | 30–45 min more | about 25 min more | about 27 min more |
| With the test backend | 20–45 min more when it is rebuilt, then about 10 min to start | about 30 min more when it is rebuilt, then a few minutes to start | as desktop, whose backend it shares |
| With Claude on the smoke plan | 10–15 min more | about 4 min more | about 5 min more |

## The Claude job

It needs the GitHub environment `app-sim-claude` with the secret `CLAUDE_CODE_OAUTH_TOKEN`, a Claude subscription token. The environment's branch rule alone decides which branches may use it: on a branch it doesn't allow, a run that asks for Claude fails at the Claude job's environment check, and the scripted test runs as usual. Each run spends the subscription's usage limits; the job summary gives its tokens and cost estimated at API prices.
