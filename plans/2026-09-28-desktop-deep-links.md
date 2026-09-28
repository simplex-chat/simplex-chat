# Desktop deep links: `simplexchat:` on Linux, Windows, macOS

## Context

The badge page (`badges.simplex.chat`) ends a purchase opened from the app with *Return to SimpleX*, which navigates to `simplexchat:/badge/code/<code>`. Android and iOS handle it since #7592 (`isAppLink` → `openAppLink` → `openBadgeLink` → `BadgesRedeemLinkView`, which confirms before redeeming). Desktop registers no scheme, so it opens the page with `app=desktop` and relies on the user pasting the code (D1). Goal: every packaged desktop build receives the link, cold or running, and sends `app=true` only when this installation really owns the scheme. D1 stays as the fallback.

Decisions made: build on #7592; forward to a running instance through the existing signal file with a payload; register `simplexchat:` only; self-register at runtime on Windows (HKCU) and AppImage.

## Research summary

- **macOS**: `CFBundleURLTypes` in Info.plist; LaunchServices registers on copy to /Applications. URL arrives only as an Apple Event through `Desktop.setOpenURIHandler`; JDK 17 queues it until the handler is set (`_OpenURIDispatcher`), so cold start is safe for AWT/Compose. Running app receives it in-process; no second process.
- **Windows**: `HKCU\Software\Classes\simplexchat` with `URL Protocol`, `shell\open\command = "<exe>" "%1"`. Compose 1.8.2 exposes no `--resource-dir`, so installer registration needs a WiX fork; runtime HKCU is what Signal, Element, Telegram do. URL arrives as argv of a new process. `jpackage.app-path` is set by the jpackage launcher (JDK 16+).
- **Linux**: `.desktop` with `MimeType=x-scheme-handler/simplexchat;` and `Exec=... %u`, visible once `mimeinfo.cache` is rebuilt. The v7.0.3 deb ships `/opt/simplex/lib/simplex-simplex.desktop` with `Exec=/opt/simplex/bin/simplex` and empty `MimeType=`; its postinst runs `xdg-desktop-menu install`, which runs `update-desktop-database`. AppImage has no install step, so it self-registers in `~/.local/share/applications`. Flatpak exports the `.desktop` MimeType (file is in `scripts/flatpak/`).
- **Browsers**: all prompt before launching; Chrome needs a user gesture (the page's button provides it). No reliable way for the page to know the app opened, so *Show code* stays.
- **Security**: link is a bearer token visible in argv of the short-lived second process; the lane already confirms before redeeming. Nothing may log the URI.

## Flow

```
browser ─ simplexchat:/badge/code/X
  macOS:          Apple Event → setOpenURIHandler → openDesktopAppLink
  Windows/Linux:  new process main(args) → appLinkFromArgs
                    lock acquired  → chatModel.appOpenUrl (cold start)
                    lock taken     → simplex.show payload → primary watcher → openDesktopAppLink
openDesktopAppLink: showWindow(); chatModel.appOpenUrl = remoteHostId() to uri
App.kt LaunchedEffect (existing) → connectIfOpenedViaUri → openAppLink (lane)
```

## Changes

1. **`common/src/desktopMain/kotlin/chat/simplex/common/AppLinks.kt`** (new, beside `SingleInstance.kt`)
   - `isAcceptedAppLink(s)`: `isAppLink` (`ChatListView.kt`) within `MAX_APP_LINK_LENGTH`.
   - `appLinkFromArgs(args)`: the only argument, when it is an accepted app link; anything else is ignored.
   - `openDesktopAppLink(uri)`: on the EDT, `showWindow()` (`DesktopApp.kt:258`) then `chatModel.appOpenUrl.value = chatModel.remoteHostId() to uri`. `remoteHostId()` matches the redeem screen (`BadgesRedeemCodeView.kt:98`).
   - `installOpenUriHandler()`: macOS only (so Windows and Linux do not start AWT early); sets a handler that forwards accepted links to `openDesktopAppLink`.

2. **`desktop/.../Main.kt`**: `main(args)`. Compute the link before `acquireSingleInstance(appLink)`. After `initApp()`: start registration (step 4), `installOpenUriHandler()`, and put a cold-start link into `chatModel.appOpenUrl` (`null` host). Keep everything inside the existing try.

3. **`SingleInstance.kt`**: the signal carries an optional payload.
   - Second process: `Files.createTempFile(dataDir, "simplex.show", ".tmp")` (owner-only on POSIX), write the link or nothing, then `Files.move(tmp, showPath, ATOMIC_MOVE)`, which replaces an untaken signal on Linux and Windows. The rename arrives as `ENTRY_CREATE` on inotify and Windows. Existing 1 s wait and hung-primary alert stay; on "start anyway" the payload file is deleted and `main` handles the link itself.
   - Primary watcher: read the capped content, delete the file, then `invokeLater { showWindow(); link?.let(::openDesktopAppLink) }`.
   - Primary start also deletes stale `simplex.show*.tmp`.
   - Windows: call `AllowSetForegroundWindow(ASFW_ANY)` before signalling so the primary can come forward (best effort). jna-platform 5.14 does not bind it, so it is looked up through `NativeLibrary`.
   - Take the directory as a parameter internally so tests can use a temp dir.
   - Log fixed strings only.

4. **`common/src/desktopMain/.../platform/AppLinkScheme.desktop.kt`** (new): `registerAppLinkScheme()` runs once on a background thread and sets a `@Volatile` result read by the UI. `unixDataHome` is exposed from `Platform.desktop.kt` and `appLinkScheme` from `ChatListView.kt` for reuse.
   - macOS: registered when `jpackage.app-path` is set (packaged bundle).
   - Windows: when `jpackage.app-path` is set, read the HKCU command via `Advapi32Util`. If it differs from `"<path>" "%1"`, write the default value `URL:SimpleX Chat`, `URL Protocol`, `DefaultIcon`, and `shell\open\command`. Registered if the key now matches.
   - Linux:
     - `FLATPAK_ID` set → registered.
     - `APPIMAGE` set → write `$XDG_DATA_HOME/applications/chat.simplex.app-links.desktop`: `NoDisplay=true`, `Exec="<APPIMAGE>" %u` quoted per the desktop-entry spec, and the MimeType line. Rewrite only when the content differs, then run `xdg-mime default` for the scheme.
     - Otherwise (deb): registered when `xdg-mime query default x-scheme-handler/simplexchat` is non-empty. Processes are run without a shell and with a timeout.
   - Unpackaged runs register nothing and report false.

5. **Common UI**
   - `expect fun appLinkSchemeRegistered(): Boolean` in `platform/AppCommon.kt`, with Android actual `true` and desktop actual returning step 4's result.
   - `BadgeStore.kt`: `badgePageUrl` becomes a function returning `app=true` when registered, `app=desktop` otherwise.
   - `BadgesSupportSimplexView.kt` `BuyInBrowserButton`: open the Redeem code screen beside the browser only when not registered (replaces `appPlatform.isDesktop`), and update its comment.

6. **Packaging**
   - `desktop/build.gradle.kts`: add `CFBundleURLTypes` (`CFBundleURLName` `chat.simplex.app`, scheme `simplexchat`) to the existing `extraKeysRawXml`.
   - `scripts/desktop/make-deb-linux.sh`: after `dpkg-deb -R`, sed `extracted/opt/*imple*/lib/*.desktop` so `Exec` ends in ` %u` and `MimeType=x-scheme-handler/simplexchat;`. This must run before the timestamp `touch`.
   - `desktop/src/jvmMain/resources/distribute/SimpleX.desktop` (AppImage only): add the MimeType line. In `scripts/desktop/make-appimage-linux.sh`, change the Exec sed to `Exec=simplex %u`.
   - `scripts/flatpak/chat.simplex.simplex.desktop`: add the MimeType line. Confirm the Flathub manifest takes this file.

7. **Docs**
   - `apps/multiplatform/spec/client/navigation.md`: replace "Desktop registers no URL scheme" with the desktop paths.
   - `plans/2026-09-24-badge-buy-in-browser.md`: mark the desktop deep links section as done.

## Commits

1. `desktop: receive app links from args and macOS`
2. `desktop: forward an app link to the running app`
3. `desktop: register simplexchat scheme at runtime`
4. `desktop: register simplexchat scheme in packages`
5. `ui: end the badge page with a link on desktop`
6. `docs: desktop deep links`

## Verification

- **Unit tests** (`common/src/desktopTest`, beside `SingleInstanceTest.kt`):
  - `appLinkFromArgs`: accepts `simplexchat:` in any case; rejects `simplex:`, `https:`, oversized and empty args, and a link that is not the only argument.
  - Signal payload: link and empty round trip, replace of an untaken signal, owner-only file on POSIX, rejected content, stale file sweep, delivery through a real `WatchService`.
  - The Windows command string and the AppImage desktop entry for paths containing spaces and quotes.
- **Commands**: `./gradlew :common:desktopTest`, `./gradlew :desktop:compileKotlinJvm` and `./gradlew :common:compileDebugKotlinAndroid`, with no new warnings.
- **Manual matrix** on packaged builds. States: app closed, running, hidden in tray, locked with passcode, no active profile.
  - Launch commands: `open 'simplexchat:/badge/code/<code>'`, `start simplexchat:/badge/code/<code>`, `xdg-open ...`.
  - End to end through the page mock (`web/mock/server.py`, `#/tier?app=true`, settle via `/control/settle`) in Chrome, Firefox and Safari.
  - Linux: deb on GNOME and KDE, snap Firefox, Flatpak, AppImage.
  - Check `lsregister -dump | grep simplexchat` on macOS and the HKCU key on Windows.
  - Confirm that no log contains the code.
- **Unpackaged `./gradlew run`**: reports not registered and keeps D1.

## Risks to confirm on hardware

- How the Windows launcher re-quotes argv when it restarts itself.
- macOS cold start with Compose 1.8.2.
- Snap Firefox on Ubuntu opening custom schemes.
- GNOME Wayland raising the window (best effort).
- Stale HKCU keys and AppImage entries after uninstall or deletion (accepted).
- The deb's existing `Name=simplex` and `Categories=Unknown` are out of scope; noted only.
