# Desktop: crash at startup on Windows when WMI does not answer

Fixes #7580.

## Problem

The desktop app exits at startup on some Windows 10 machines:

```
java.lang.ExceptionInInitializerError
    at com.jthemedetecor.OsThemeDetector.getDetector(OsThemeDetector.java:43)
    at chat.simplex.common.platform.Resources_desktopKt.<clinit>(Resources.desktop.kt:29)
    ...
Caused by: java.lang.NullPointerException: Cannot invoke "String.compareTo(String)" because "buildNumber" is null
    at oshi.software.os.OperatingSystem$OSVersionInfo.<init>(OperatingSystem.java:510)
    at oshi.software.os.windows.WindowsOperatingSystem.queryFamilyVersionInfo(WindowsOperatingSystem.java:172)
    ...
    at com.jthemedetecor.util.OsInfo.<clinit>(OsInfo.java:27)
```

## Cause

`jSystemThemeDetector:3.8` depends on `oshi-core:5.8.6`. Its `OsInfo` static initialiser calls `OperatingSystem.getVersionInfo()`.

In oshi 5.8.6, `WindowsOperatingSystem.queryFamilyVersionInfo()` starts with `buildNumber = null` and sets it only if the WMI query for `Win32_OperatingSystem` returns a row. `WmiQueryHandler.queryWMI` returns an empty result on any `COMException` (invalid namespace/class, access denied, broken WMI repository or service) or on a timeout. The `OSVersionInfo` constructor then runs:

```java
if ("10".equals(version) && buildNumber.compareTo("22000") >= 0) {
```

It throws when `os.name` is "Windows 10" and WMI returned nothing. So only machines with a failing WMI query crash; a healthy WMI returns the build number.

The app calls `OsThemeDetector.getDetector()` in top-level `val` initialisers (`Resources.desktop.kt`, `Theme.desktop.kt`). The failure is an `Error` raised while the class loads, so nothing catches it. `settings` lives in the same file, so the first preference read in `runMigrations` fails and the app exits.

## Fix

Add `com.github.oshi:oshi-core:6.4.13` to `desktopMain`. Gradle conflict resolution replaces the transitive 5.8.6 with it.

- The `buildNumber.compareTo` check is in every oshi release up to 6.3.0 and was removed in 6.3.1. There the constructor just stores the null build number.
- 6.4.13 is built against JNA 5.14.0, the version the app already pins (`jna`, `jna-platform`). 6.3.1 used 5.12.1 and 6.6.x uses 5.16.0.
- jSystemThemeDetector 3.8 uses only `SystemInfo.<init>`, `getOperatingSystem`, `getCurrentPlatform`, `OperatingSystem.getFamily`, `getVersionInfo`, `OSVersionInfo.getVersion` and `PlatformEnum.{WINDOWS,MACOS,LINUX}`. All of them exist unchanged in 6.4.13.

## Verification

- `new OSVersionInfo("10", "", null)` throws the reported NPE with oshi 5.8.6 and returns `10` with 6.4.13.
- With 6.4.13, jSystemThemeDetector 3.8 and JNA 5.14.0 on the classpath, `OsThemeDetector.getDetector()` loads and returns a detector.
- `:common:dependencyInsight --configuration desktopRuntimeClasspath --dependency oshi-core` shows `5.8.6 -> 6.4.13`.
