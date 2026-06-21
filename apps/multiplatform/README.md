# Android App Development

This is a guide to contributing to the develop of the SimpleX android and desktop apps.

## Project Overview

This is the **Kotlin Multiplatform (KMP)** mobile and desktop client for SimpleX Chat, sharing code between Android and Desktop (JVM) platforms using Compose Multiplatform for UI.

## Build Commands

```bash
# Android debug APK
./gradlew assembleDebug

# Android release APK
./gradlew assembleRelease

# Desktop distribution (current OS)
./gradlew :desktop:packageDistributionForCurrentOS

# Run desktop/JVM tests
./gradlew desktopTest

# Run Android instrumented tests (requires connected device/emulator)
./gradlew connectedAndroidTest

# Build native libraries for all platforms
./gradlew common:cmakeBuild -PcrossCompile

# Clean build
./gradlew clean
```

## Architecture

### Module Structure

- **`common/`** - Shared code (Compose UI, models, business logic)
  - `src/commonMain/` - Cross-platform code
  - `src/androidMain/` - Android-specific implementations
  - `src/desktopMain/` - Desktop-specific implementations
- **`android/`** - Android app container
- **`desktop/`** - Desktop JVM app container

### Key Components (`common/src/commonMain/kotlin/chat/simplex/common/`)

- **`model/ChatModel.kt`** - Main state container with reactive properties (MutableState, MutableStateFlow)
- **`model/SimpleXAPI.kt`** - API bindings to Haskell core library via FFI
- **`platform/Core.kt`** - FFI interface to native `libapp` library
- **`platform/`** - Platform abstraction layer (expect/actual pattern for Android/Desktop specifics)
- **`views/`** - Compose UI screens organized by feature (chat, chatlist, call, usersettings, etc.)
- **`ui/theme/`** - Design system (colors, typography, shapes)

### Native Integration

The app calls into a Haskell core library via JNI/FFI:
- CMake builds in `common/src/commonMain/cpp/android/` and `cpp/desktop/`
- Cross-compilation toolchains in `cpp/toolchains/`
- Built libraries go to `cpp/desktop/libs/` (organized by platform)

## Configuration

### `local.properties` (create from `local.properties.example`)

```properties
compression.level=0          # APK compression (0-9)
enable_debuggable=true       # Debug mode
application_id.suffix=.debug # Multiple app instances on same device
app.name=SimpleX Debug       # App name for debug builds
```

### `gradle.properties`

Contains versions (Kotlin, Compose, AGP) and app version info. Key settings:
- `kotlin.jvm.target=11`
- `database.backend=sqlite` (or `postgres`)

## Testing

Tests are in:
- `common/src/commonTest/kotlin/` - Cross-platform tests
- `common/src/desktopTest/kotlin/` - Desktop-specific tests (run with `./gradlew desktopTest`)
- `android/src/androidTest/` - Android instrumented tests

## Resources & Localization

- String resources: `common/src/commonMain/resources/MR/base/strings.xml` + 21 language variants
- Uses Moko Resources (`dev.icerock.moko:resources`) for cross-platform resource management
- The `adjustFormatting` gradle task validates string resources during build

## Platform-Specific Notes

### Android
- Min SDK 26, Target SDK 35
- NDK 23.1.7779620
- Supports ABI splits: `arm64-v8a`, `armeabi-v7a`
- Deep linking requires SHA certificate fingerprint in `assetlinks.json` (see README.md)

### Desktop
- Distributions: DMG (macOS), MSI/EXE (Windows), DEB (Linux)
- Mac signing/notarization configured via `local.properties`
- Video playback uses VLCJ

## Gotchas

#### SHA Signature for verification for app links/deep links

In order for the SimpleX app to be automatically adopted for opening links from https://simplex.chat the SHA certificate fingerprint for the App installed on the phone must be in the hosted [assetlinks.json](https://simplex.chat/.well-known/assetlinks.json) file on simplex.chat.

The accepted fingerprints are in the `sha256_cert_fingerprints` list.

To find your SHA certificate fingerprint perform the following steps.

1. Build and install your development version of the app as usual
2. From the terminal in Android studio run `adb shell pm get-app-links chat.simplex.app`
3. Copy the signature listed in `signatures` in the result
4. Add your signature to [assetlinks.json](https://github.com/simplex-chat/website/blob/master/.well-known/assetlinks.json) in the [website repo](https://github.com/simplex-chat/website) and make a PR. On approval, wait a few minutes for the changes to propagate to the public website and then you should be able to verify SimpleX.

More information is available [here](https://developer.android.com/training/app-links/verify-site-associations#manual-verification). If there is no response when running the `pm get-app-links` command, the intents in `AndroidManifest.xml` are likely misspecified. A verification attempt can be triggered using `adb shell pm verify-app-links --re-verify chat.simplex.app`. 

Note that this is not an issue for the app store build of the app as this is signed with our app store credentials and thus there is a stable signature over users. Developers do not have general access to these credentials for development and testing.

## Adding icons

1. Find a [Material symbol](https://fonts.google.com/icons?icon.style=Rounded) in Rounded category.

2. Set weight to 400, grade to -25 and size to 48px.

3. Click on the icon, choose Android and download XML file.

4. Update the color to black (#FF000000) and the size to "24.dp", as in other icons.

For example, this is [add reaction icon](https://fonts.google.com/icons?selected=Material+Symbols+Rounded:add_reaction:FILL@0;wght@300;GRAD@-25;opsz@24&icon.style=Rounded).
