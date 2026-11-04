# App builds for signing

`.github/workflows/build-apps.yml` builds the Android and iOS apps of a commit without signing them and uploads them as artifacts, to be signed outside CI. No signing key or certificate reaches GitHub.

Each job runs `scripts/simplex-chat-build.py build` (its code is the `scripts/simplex_build` package, tested with `python3 -m unittest discover -s scripts/simplex_build -t scripts`), the script that also reproduces releases ([docs/REPRODUCE.md](../../../docs/REPRODUCE.md)), so a reproduce run rebuilds an APK exactly as this workflow built it. The same commands build locally: `build apk <arch> --out <dir>`, `build aab --libs <dir> --out <dir>` with Docker, and `build xcarchive --art <simplex-chat-art> --out <dir>` on a Mac with Xcode and Nix.

## Smoke tests

After each build the job runs what it built as users do, with `scripts/simplex-chat-build.py test`: each CLI prints its version; the Linux debs and AppImage, the macOS dmg and the Windows msi are installed and started, and pass when the app keeps running and its screenshot shows a window (on Linux the test also waits for the window); the APKs, signed with a throwaway key, start in an Android emulator, aarch64 on Android 14 and armv7a on Android 11, the last x86_64 image that runs 32-bit ARM code. The iOS archive's Nix core, relabelled for the simulator, runs in the iOS simulator app (job ios-test). Each app leaves a screenshot, in the artifacts `screens-<platform>`.

The job old-database makes an encrypted database with the previous release's CLI (v7.0.3, pinned by SHA-256), and every build must open it with its key: each CLI opens and migrates it and refuses a wrong key; each desktop app opens and migrates it, which the new CLI then checks; the Android apps open it from the passphrase screen, and the iOS app in the simulator too. Each desktop package is also installed over the v7.0.3 package that the old app started first, and the new app must open the database the old app made (`test desktop --upgrade-from-previous`).

Two tests cover the OpenSSL of the mobile apps' core beyond opening a database:

- **Upgrade** (`test apk --upgrade-from-previous`): the v7.0.3 APK for the same ABI, pinned by its SHA-256 and signed with the same throwaway key as the new APK, installs, makes a profile through onboarding and keeps its database key in the Android keystore; the new APK installs over it (`adb install -r`) and must show the chat list, not the passphrase or database error screen. Screenshots `<apk>.v7.0.3.png` and `<apk>.upgraded.png`.
- **Quantum resistant contact** (`test apk --cli`, `scripts/ci/ios-sim/pq-contact.sh`): the upgraded Android app, and the iOS simulator app with the archive's core, take a one-time link of the new CLI from the desktop jobs, both on the preset servers (Android through a VIEW intent, iOS pasted, as `simctl openurl` delivered a link the core rejected), and exchange a message each way. The CLI's `/i` must show "quantum resistant end-to-end encryption"; the Android chat must show "quantum resistant e2e encryption", and the iOS contact info "E2E encryption: Quantum resistant", since the iOS accessibility tree has no message texts. sntrup761, the KEM of these connections, hashes with OpenSSL's SHA-512. Screenshots `<apk>.upgraded.pq.png`, `pq-messages` and `pq-contact-info`, with the peer's log and contact info.

## Running it

The workflow runs only when started by hand, on any branch: in the Actions tab, `build-apps` → Run workflow, or `gh workflow run build-apps.yml --ref <branch>`. A newer run on the same branch cancels the older one. Start it on master or a branch of master: armv7a is built from the same commit, which `stable` only supports once it carries the armv7a build of master.

It needs the repository secret `SIMPLEX_ART_SSH_KEY`, the private half of a read-only deploy key of simplex-chat/simplex-chat-art, for the iOS brand assets; without it, as on forks, the archive has the default icons and the build warns.

## Jobs and artifacts

| Job | Runner | Builds | Artifact, kept 30 days |
|---|---|---|---|
| Android aarch64 APK, Android armv7a APK | ubuntu-24.04 | `build apk <arch>`: `scripts/android/build-android.sh` in the `Dockerfile.build` container | `android-<arch>-unsigned`: `simplex-<arch>.apk`, `pkg-<arch>-android-libsimplex.zip`, `pkg-<arch>-android-libsupport.zip`, `SHA256SUMS` |
| Android Play bundle | ubuntu-24.04 | `build aab`: `:android:bundleGoogleRelease` in the same container, with both arches' core libraries from the APK jobs and release-mobile.py's `local.properties` | `android-bundle-unsigned`: `simplex.aab`, `SHA256SUMS` |
| iOS archive | macos-26 | `build xcarchive` with Xcode 26.6: the core library `aarch64-darwin-ios:lib:simplex-chat` with Nix, the brand assets of simplex-chat-art, then `xcodebuild archive` of the Release configuration for devices, unsigned, and each bundle signed ad hoc with its entitlements | `ios-archive-unsigned`: `SimpleX.xcarchive.zip`, `SHA256SUMS` |

`gh run download <run id> -n <artifact>` downloads one. The version and build numbers are the commit's: bump them before the run.

## Android: signing reproducibly

The APKs are the foss flavor, as `scripts/android/build-android.sh` builds them: AGP's unsigned output, which is deterministic. Two runs on different runners built byte-identical APKs, core libraries and bundles.

Releases are signed by Gradle, with the release signing config that `apps/multiplatform/android/build.gradle.kts` reads from `ANDROID_KEYSTORE`, `ANDROID_KEYSTORE_PASSWORD`, `ANDROID_KEY_ALIAS` and `ANDROID_KEY_PASSWORD`, or with the `android.injected.signing.*` properties that release-mobile.py passes. With the variables set, `build android-apk` and `build aab` mount the keystore read-only and pass the passwords to Docker by name, so that they are in no command line; without them the script and Gradle warn that the build is unsigned. CI never has the key. An APK of this artifact signed outside Gradle stays verifiable when its layout is kept: `apksigner sign --alignment-preserved`.

The reproduce script rebuilds the APK, copies the release's signature onto it with `apksigcopier` and compares the two. Releases signed before Gradle signed them, by apksigner after a repack, are checked by aligning the rebuilt APK to 16 KB pages with `repro-apk zipalign` first, padded as the released APK is (apksigner 35 and later pad with their `0xd935` extra field, older ones with zeros), and restoring the ZIP flags of the v1 signature files that apksigcopier 1.1.1 drops.

The bundle is the google flavor for Google Play; nothing checks its reproducibility.

## iOS: signing at export

The export of an archive keeps only the entitlements that each bundle's signature carries: an unsigned archive exports without app groups, keychain groups or push. Xcode refuses ad-hoc signing for iOS devices ("Ad Hoc code signing is not allowed with SDK"), so the script archives with `CODE_SIGNING_ALLOWED=NO`, then signs the extensions and the app ad hoc with `codesign`, each with its `.entitlements` file and `$(AppIdentifierPrefix)` replaced by the team id, and fails unless each signature carries exactly those entitlements. It records the team in the archive's Info.plist, as Xcode does when it signs.

Compared with the 7.1-beta.7 release archive, which Xcode signed with an Apple Development identity, the bundles have the same files except the development profiles (`embedded.mobileprovision`), and the same entitlements except `application-identifier`, `com.apple.developer.team-identifier` and `get-task-allow`, which the export takes from the distribution profile.

To sign for the App Store: on a Mac with the distribution certificate, unzip the archive, open `SimpleX.xcarchive` in Xcode's Organizer and use Distribute App, or run `xcodebuild -exportArchive`.

It also fails when an object of the core library is marked for a newer iOS than the app's deployment target: mac2ios relabels the library's macOS objects as iOS and keeps their minimum version.

## macOS and iOS: reproducibility

The unsigned macOS files (the dmg, the CLI and the libraries zip of `build desktop macos` and `build libs macos`) and the iOS archive and core package of `build xcarchive` don't depend on the Mac that builds them, its paths or its user; signed macOS releases, whose Developer ID signatures and notarization tickets differ on each signing, are compared with such a build after their signatures are removed. The builds remove what the machine would add:

- **Xcode**: they fail unless the selected Xcode is 26.6 (17F113), `XCODE` in `scripts/simplex_build/apple.py`, whose compilers, linker and SDKs write the binaries. Of GitHub's images only macos-26 and macos-26-intel have it, so every macOS job runs on one of them and selects it with the action `.github/actions/xcode`, which moves it to `/Applications/Xcode.app`: the iOS build needs it there, as the dSYMs name Xcode's paths in the Swift modules they embed and in the API notes of Clang's modules, which `-debug-prefix-map` doesn't change. The macOS builds compile and link with its macOS SDK (`SDKROOT`), and fail unless `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk`, which GHC's libraries list as a library directory, is missing or links to it: the jobs link it.
- **Minimum macOS**: without `MACOSX_DEPLOYMENT_TARGET` the CLI and libraries required the macOS of the machine that built them, 26.0 on aarch64 and 15.0 on x86_64 in the releases until v7.1.0-beta.8. The builds set it to 11.0 (`MACOS_DEPLOYMENT_TARGET` in `scripts/simplex_build/macos.py`), the minimum of the bundled launcher and JDK runtime, the newest of the parts not built here (VLC's, skiko's and GHC's libraries need 10.8 to 11.0), and fail when the CLI, a library or the app's JNI library needs a newer macOS, or when the app's Info.plist declares another minimum (`LSMinimumSystemVersion`, which `minimumSystemVersion` in `apps/multiplatform/desktop/build.gradle.kts` sets; it was Compose's default, 10.13).
- **Paths**: they copy the checkout to `/private/tmp/simplex-chat-build/simplex-chat`, with the modes of a checkout made with umask 022, and build there, with cabal's directory (`CABAL_DIR`), the temporary directory (`TMPDIR`), DerivedData and the other build files in `/private/tmp/simplex-chat-build`, which they remove before and after. The binaries contain the paths of the checkout, of OpenSSL's prefix and of cabal's package store; the dSYMs those of the checkout, DerivedData and Swift's macro expansions in `TMPDIR`. `build libs macos` copies the libraries back into the checkout for its Gradle build.
- **Settings**: umask 022, `LC_ALL=en_US.UTF-8` and `TZ=UTC`, as on GitHub's runners. The iOS bundles' `BuildMachineOSBuild`, the build of the macOS that Xcode runs on, is pinned to 25G83.
- **Libraries**: Cabal and GHC link each library with an absolute rpath to the directory of each library it depends on, GHC's own and the SDK's in Xcode among them, and their room in the load commands moved the libraries' code with where GHC and Xcode are. The packages link with `-fno-use-rpaths` and `-pgml` of `scripts/simplex_build/link_without_rpaths.py`, which drops absolute rpaths from the linker's arguments and GHC's response files; the build finds the libraries it bundles in cabal's store and GHC's directory, and fails when one of them, or the app's JNI library, has an rpath not relative to it (`@loader_path`, `@executable_path`): dyld searches an absolute rpath, which another user may create, such as one in `/private/tmp`, before `@loader_path`, and the app's entitlements disable library validation. The JNI library's CMake build links it with `@loader_path` only.
- **dmg**: libdmg-hfsplus is linked with zlib 1.3.1, built from its pinned source, rather than the SDK's zlib that CMake finds, and the dmg's UDIF segment id, which libdmg took from the unseeded `rand()`, is pinned to the value of the dmgs built before.
- **Swift**: the Swift compiler orders some of its output by memory addresses: with ASLR, SwiftyGif's `calculateFrameDelay` compiled differently in 1.5 to 10% of compilations. `build xcarchive` starts `xcodebuild` with `_POSIX_SPAWN_DISABLE_ASLR`, which the compilers it starts inherit, and fails unless two programs started that way load at the same address.
- **Check**: each build fails, after writing its files, when they contain a path of the machine: the original checkout, the home directory, the user's temporary and cache directories, or Xcode's when it is not in `/Applications/Xcode.app`. VLC's prebuilt libraries, which contain the paths of the runner that built them, are not checked.

Run [41369223439](https://github.com/sh-agents/simplex-chat/actions/runs/41369223439) built the desktop files twice on each architecture (macos-26 for aarch64, macos-26-intel for x86_64), and run [41331759753](https://github.com/sh-agents/simplex-chat/actions/runs/41331759753) the iOS archive twice: build b ran from another checkout path, with another home, temporary directory, user name, locale, time zone, umask (077) and, for the desktop, Xcode path, and gave byte-identical files. Only macos-26 images have Xcode 26.6, so both builds ran on the same macOS; the Nix iOS core package was also the same on macos-15 and macos-26. With ASLR off the SwiftyGif sources compiled identically 1379 times, against 19 differences in 1261 compilations with ASLR (run [41326351453](https://github.com/sh-agents/simplex-chat/actions/runs/41326351453)).

## Known limits

- **The core libraries rely on cache.zw3rk.com** for the cross-compiling toolchains and dependencies, as android-emu does: without it they take 12 to 24 hours, longer than a job may run. On a recent master commit only simplexmq and simplex-chat are built. armv7a uses GHC 8.10.7, whose Template Haskell runs under qemu, so it takes longer than aarch64.
- Not tested yet: the export and App Store upload of an archive from this workflow; check the exported app with `codesign -d --entitlements - --xml Payload/SimpleX.app` and the same for `PlugIns/*.appex`.
