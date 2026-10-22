# UI tests on GitHub's runners

`.github/workflows/ui-tests.yml` runs the Compose UI tests on GitHub runners (Ubuntu 24.04): the desktop app's `:common:desktopUiTest`, and the Android app's instrumented tests in an emulator. It uses the same runners as local runs, `apps/multiplatform/scripts/uitest.sh` and `apps/multiplatform/scripts/uitest-android.sh`. How the tests are written: `apps/multiplatform/common/src/uiTestShared/README.md`.

## Running it

The workflow runs only when started by hand, on any branch. In the Actions tab, `ui-tests` → Run workflow, then choose the branch and the inputs. With the GitHub CLI:

```sh
gh workflow run ui-tests.yml --ref <branch>                                  # all desktop tests
gh workflow run ui-tests.yml --ref <branch> -f platform=both
gh workflow run ui-tests.yml --ref <branch> -f platform=android -f tests=chat.simplex.app.ui.main.ChatListRowsTest
gh workflow run ui-tests.yml --ref <branch> -f tests='*ChatListRowsTest, chat.simplex.app.ui.chat.ComposeTest#sendText_sendButton_peerReceives'
```

| Input | Default | Does |
|---|---|---|
| `platform` | `desktop` | `desktop`, `android` or `both` |
| `tests` | all tests | comma- or space-separated test classes (`chat.simplex.app.ui.main.ChatListRowsTest`) or methods (`Class#method`). Desktop also takes Gradle patterns with `*`; Android doesn't, and the plan job fails when it gets one |

GitHub offers Run workflow, and accepts `gh workflow run`, only for a workflow whose file is on the default branch. Until `ui-tests.yml` is there, a branch can't be tested with it; once it is, the run uses the workflow file of the branch it is started on. A newer run on the same branch cancels the older one.

Every run builds what the tests need from the branch's own code, and reuses an earlier build only when its sources are unchanged, as an artifact of the same branch named after a hash of its sources (30 days). The core libraries have the names and sources of `desktop-linux` and `android-emu`, so the three workflows reuse each other's builds on a branch. The CLI and `uitest-servers` (`ui-tests-bin-<hash>`) are built from `src/**`, `apps/simplex-chat/**`, the files under `tests/` that `uitest-servers` compiles (`UITestServers.hs`, `ChatClient.hs`, `NameResolver.hs`, `ChatTests/DBUtils.hs`, `ChatTests/DBUtils/**`), `cabal.project`, `simplex-chat.cabal`, `PRIVACY.md` and `build-bin.sh`. The workflow saves no Actions caches.

## Jobs

Without `tests`, each platform's tests run in four jobs at once; with `tests`, in one job per platform, named `selected`.

| Job | Does | Time |
|---|---|---|
| `plan` | `plan.sh`: the platforms and the shards of each; finds earlier builds | 10 s |
| `desktop-core` | desktop-linux's `build-core.sh`, when the desktop tests run | skipped when reused, about 24 min not |
| `android-core` | android-emu's `build-core.sh`, when the Android tests run | skipped when reused, about 27 min not |
| `bin` | `build-bin.sh`: `exe:simplex-chat` and `exe:uitest-servers` with `-O0 -f uitest`, as `uitest.sh` builds them locally | skipped when reused; to be measured |
| `desktop` | one job per shard: `uitest.sh` with the prebuilt CLI and servers (`UITEST_CLI`, `UITEST_SERVERS`) | to be measured |
| `android-apk` | the debug APK and the test APK, once per run | to be measured |
| `android` | one job per shard: the host-side bridge, the emulator, `uitest-android.sh` | to be measured |

### Desktop shards

Without `tests`, the desktop tests run in four jobs, one per test package: `chat`, `groups`, `main`, and `settings-desktop` (`settings`, the desktop-only `desktop` tests and the harness's own tests). Fixed packages keep a shard's name telling which area failed, need no timings, and don't move tests between shards as tests are added. The plan job fails when a test package that no shard names appears, so a new package can't go untested; add it to `plan.sh`. In each job `uitest.sh` runs as many test JVMs as memory allows (at most 4).

`uitest.sh` runs the tests in their own user, PID and network namespaces (`unshare -rn --pid`), which Ubuntu 24.04 restricts: the job turns `kernel.apparmor_restrict_unprivileged_userns` off first, as android-emu's core build does for Nix, and checks that `unshare` works.

The desktop app's system theme detector runs `gsettings monitor`, which exits at once without a session bus and then crashes its thread (plan §15.5 item 24), which the harness reports as an app crash. The desktop jobs start a session bus on a filesystem socket (`unix:dir=`), which the tests' network namespace can reach, unlike an abstract socket, and install the GNOME schemas.

### Android

An emulator runs one test at a time, and the Android tests (about 450) run under ARM translation, likely too long for one job's 6 hours: without `tests` they are split by AndroidJUnitRunner's own sharding (`-e numShards 4 -e shardIndex <i>`), which assigns each test method to a shard by a hash of its name, the same in every pass of `uitest-android.sh`. The APKs are built once, in `android-apk`; each shard job builds only the host-side bridge, whose classpath names files in the job's Gradle cache, with the Kotlin compiler in-process (building the app and the bridge in one Gradle run ran out of memory).

The emulator is android-emu's (`install-sdk.sh`, `emulator.sh`): Google APIs x86_64, API 34, with KVM, running the app's arm64-v8a code under ARM translation. The APKs are built as android-emu builds its APK (`-Pandroid.injected.build.abi=arm64-v8a`, `:android:assembleFossDebug`), with `:android:assembleFossDebugAndroidTest`; each shard builds `:common:uitestBridgeClasspath` before `install-sdk.sh` points `ANDROID_HOME` at an SDK that has only the emulator. Without `local.properties` the debug app's id is `chat.simplex.app` (`UITEST_APP_ID`). `uitest.sh --prepare-only` writes the peer template and `build/uitest/env.properties` that the host-side bridge reads, without the desktop build; `uitest-android.sh` then runs with `UITEST_SERIAL` set to the emulator and `ADB` to the SDK's adb, and forwards the bridge's and the servers' ports with `adb reverse`.

## Results

The job summaries list the results: per test class for desktop (`desktop-linux/test-summary.py` on the JUnit XML; known-issue tests are counted as skipped), and per class with the failed and crashed tests for Android (`android-summary.py` on `results.txt`). A job fails when a test fails or, on Android, crashes. The artifacts (7 days):

- `ui-tests-desktop-<shard>`: the HTML report and the JUnit XML (`common/build/{reports,test-results}/desktopUiTest`), and from the run directory the failure screenshots (`failures/`), the transcripts of the failed tests' CLI peers (`jvm-*/t<n>/peers/<name>/transcript.log`: a test's directory is kept only when it failed) and JVM crash logs;
- `ui-tests-android-<shard>`: the run directory of `uitest-android.sh` (`results.txt`, `instrument-<pass>.txt`, `logcat.txt`, `timings.txt`, `failures/`, `bridge.log`), `emulator/emulator.log`, and the transcripts of the bridge's peers (all tests).

## Diagnosing a failure

Start from the failure message in the summary or the test report: a timed-out wait names what it waited for, the API calls still in flight and the first visible texts. Its screenshot in `failures/` is named after the JVM and the wait. The peers' transcripts show what each CLI peer printed, with timestamps, to tell an app failure from a peer or server one. The step log has each test's standard output. On Android, `instrument-<pass>.txt` has the stack traces, `logcat.txt` the app's log and the in-process watchdog's thread dumps, and `timings.txt` each test's phases; a test reported as `crashed` was running when the app process died, and the run resumed without it.

## Known limits

- **ARM translation on Android**, as android-emu's README describes: the app runs slower than on a phone and can crash in the translator. `uitest-android.sh` resumes after a crash (at most 4 passes), and force-stops the app when the instrumentation prints nothing for 420 s (`UITEST_STALL_S`).
- **Memory**: `uitest.sh` needs 3000 MB free and `uitest-android.sh` 4500 MB besides the emulator; the runners have 16 GB.
- The test JVMs per desktop job follow free memory, not the runner's 4 CPUs.

## Scripts

- `plan.sh`: the plan job's choices and the shards of both platforms.
- `build-bin.sh`, `install-bin.sh`: the CLI and `uitest-servers`, built and installed for `uitest.sh` (`UITEST_CLI`, `UITEST_SERVERS`).
- `android-summary.py`: the Android job summary.
