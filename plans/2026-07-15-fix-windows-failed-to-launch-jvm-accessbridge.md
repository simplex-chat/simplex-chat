# Fix Windows "Failed to launch JVM" when assistive technologies are enabled (#4146)

## Symptom

On some Windows machines the desktop app fails on every launch with the jpackage
launcher's message box "Failed to launch JVM" and no log output anywhere. Reported
since v5.4 in #4146; #6547 shows the same dialog (cause not established).

## Root cause

1. Every JVM reads `%USERPROFILE%\.accessibility.properties` during AWT toolkit
   initialization (JDK 17 `Toolkit.initAssistiveTechnologies`, Toolkit.java:407-422).
   The file is created by enabling the Java Access Bridge: `jabswitch -enable`
   (ships in Windows JREs/JDKs, including JRE 8), the "Enable Java Access Bridge"
   checkbox in Ease of Access Center, or screen reader installers. It sets
   `assistive_technologies=com.sun.java.accessibility.AccessBridge`.

2. That provider lives in the `jdk.accessibility` module. Its Windows
   implementation keeps the legacy name `com.sun.java.accessibility.AccessBridge`
   specifically for compatibility with properties files written by pre-Java-9
   installers (JDK `ProviderImpl.java:38-43`).

3. Our jlinked runtime did not include `jdk.accessibility`: the Compose gradle
   plugin's (1.8.2) default modules are `java.base, java.desktop, java.logging,
   jdk.crypto.ec`, plus our `modules("jdk.zipfs", "jdk.unsupported")` in
   `desktop/build.gradle.kts`.

4. With the property set and the module absent, `Toolkit.loadAssistiveTechnologies`
   throws `AWTError: Assistive Technology not found:
   com.sun.java.accessibility.AccessBridge` (Toolkit.java:491) at the first AWT
   touch - before any window or logging exists. The launcher runs the JVM
   in-process and reports any nonzero exit as "Failed to launch JVM"
   (jpackage `JvmLauncher.cpp:212-214`), so the actual error is never seen.

This also matches the user reports: a user in #4146 traced the exact `AWTError`
and could start the app after neutralizing the property in `SimpleX.cfg`; the
bug report that triggered this investigation had installed JRE 8 (whose Access
Bridge tooling creates the properties file when enabled).

## Fix

Add `jdk.accessibility` to the jlink modules in `desktop/build.gradle.kts`,
conditionally on the build host being Windows (desktop packages are always
built natively on the target OS, so the host OS is the package OS). The
conditional applies to both the release and the `debugJava` configurations.

## Why this fix and not alternatives

- Forcing `-Djavax.accessibility.assistive_technologies=` via jvmArgs would stop
  the crash but permanently disable Java Access Bridge, breaking the app for
  screen reader users (an affected screen reader user is asking for a solution
  in #4146).
- Bundling the module makes the configuration the JDK explicitly supports work:
  the app starts, and assistive technologies can attach. Screen reader support
  additionally requires `WindowsAccessBridge-64.dll` on the machine, which real
  assistive-technology users already have.

## Impact assessment

- Runtime size: +~112 KB (measured with jlink on Linux; same module set).
  No new transitive modules are pulled in.
  The size increase applies to the Windows package only.
- Machines without assistive technologies enabled: the provider-loading path is
  gated on the `javax.accessibility.assistive_technologies` property being
  non-blank (Toolkit.java:518), so the module stays dormant - no classes loaded,
  no behavior change.
- Linux/macOS: unaffected - the module is only bundled when building on
  Windows. (Its provider is Windows-only anyway, so bundling it elsewhere
  would have had no functional effect, only the size cost.)

## Verification

- Reproduced on Windows 10 with the current release MSI: creating
  `%USERPROFILE%\.accessibility.properties` with
  `assistive_technologies=com.sun.java.accessibility.AccessBridge` makes every
  launch fail with "Failed to launch JVM"; deleting the file restores launch.
- Test MSI with this change built via CI; confirmation that it launches on the
  repro machine with the properties file present is pending.

## Out of scope (follow-up)

"Failed to launch JVM" is a generic dialog for any nonzero exit during startup;
at least one other cause exists in the wild (#4146 has a reporter unaffected by
the assistive technology workaround; #5237/#5238 was a DLL name mismatch). A
separate change adds a copyable startup error window so the remaining causes
become diagnosable from user reports.
