# Desktop deep links: `simplexchat:` on Linux, Windows, macOS

## Context

The badge page (`badges.simplex.chat`) ends a purchase opened from the app with *Return to SimpleX*, which navigates to `simplexchat:/badge/code/<code>`. Android and iOS handle it since #7592 (`isAppLink` → `openAppLink` → `openBadgeLink` → `BadgesRedeemLinkView`, which confirms before redeeming). Desktop registers no scheme, so it opens the page with `app=desktop` and relies on the user pasting the code (D1). Goal: every packaged desktop build receives the link, cold or running, and sends `app=true` only when a link is expected to reach this installation. D1 stays as the fallback.

The decisions were to build on #7592, forward to a running instance through the existing signal file with a payload, register `simplexchat:` only, and self-register at runtime on Windows (HKCU) and AppImage.

## Research summary

- **macOS**: `CFBundleURLTypes` in Info.plist; LaunchServices registers on copy to /Applications. URL arrives only as an Apple Event through `Desktop.setOpenURIHandler`; JDK 17 queues it until the handler is set (`_OpenURIDispatcher`), so cold start is safe for AWT/Compose. The running app receives it in-process, so no second process starts.
- **Windows**: `HKCU\Software\Classes\simplexchat` with `URL Protocol`, `shell\open\command = "<exe>" "%1"`. Compose 1.8.2 exposes no `--resource-dir`, so installer registration needs a WiX fork; runtime HKCU is what Signal, Element, Telegram do. URL arrives as argv of a new process. `jpackage.app-path` is set by the jpackage launcher (JDK 16+).
- **Linux**: `.desktop` with `MimeType=x-scheme-handler/simplexchat;` and `Exec=... %u`, visible once `mimeinfo.cache` is rebuilt. The v7.0.3 deb ships `/opt/simplex/lib/simplex-simplex.desktop` with `Exec=/opt/simplex/bin/simplex` and empty `MimeType=`; its postinst runs `xdg-desktop-menu install`, which runs `update-desktop-database`. AppImage has no install step, so it self-registers in `~/.local/share/applications`. Flatpak exports the `.desktop` MimeType (file is in `scripts/flatpak/`).
- **Browsers**: all prompt before launching; Chrome needs a user gesture (the page's button provides it). No reliable way for the page to know the app opened, so *Show code* stays.
- **Security**: link is a bearer token in argv, which on Linux other local users can read through `/proc` (for the whole session after a cold start); this risk is accepted. The lane already confirms before redeeming. Nothing may log the URI.

## Flow

```
browser ─ simplexchat:/badge/code/X
  macOS:          Apple Event → setOpenURIHandler → openDesktopAppLink
  Windows/Linux:  new process main(args) → appLinkFromArgs
                    lock acquired  → chatModel.appOpenUrl (cold start), then the watcher starts
                    lock taken     → simplex.show payload → primary watcher → openDesktopAppLink
openDesktopAppLink: chatModel.appOpenUrl = remoteHostId() to uri; showWindow()
App.kt LaunchedEffect (existing) → connectIfOpenedViaUri → openAppLink (lane)
```

## Changes

1. **`common/src/desktopMain/kotlin/chat/simplex/common/AppLinks.kt`** (new, beside `SingleInstance.kt`)
   - `isAcceptedAppLink(uri)`: `isAppLink` (`ChatListView.kt`) within `MAX_APP_LINK_BYTES` UTF-8 bytes.
   - `appLinkFromArgs(args)`: the only argument, when it is an accepted app link; anything else is ignored.
   - `openDesktopAppLink(uri)`: on the EDT, `chatModel.appOpenUrl.value = chatModel.remoteHostId() to uri`, then `showWindow()` (`DesktopApp.kt:258`), so a failure to show the window cannot lose the link. `remoteHostId()` matches the redeem screen (`BadgesRedeemCodeView.kt:98`).
   - `installOpenUriHandler()`: called on macOS only (so Windows and Linux do not start AWT early); sets a handler that forwards accepted links to `openDesktopAppLink`.

2. **`desktop/.../Main.kt`**: `main(args)`. Compute the link and pass it to `acquireSingleInstance(appLink)`; when this process runs, put it into `chatModel.appOpenUrl` (`null` host), then start the watcher if this process holds the lock, so a link the watcher forwards later is not overwritten. A short-lived second process never touches `chatModel`. After `initApp()`: start registration (step 4) and `installOpenUriHandler()`. Keep everything inside the existing try.

3. **`SingleInstance.kt`**: the signal carries an optional payload.
   - Second process: `Files.createTempFile(dataDir, "simplex.show", ".tmp")` (owner-only on POSIX), write the link or nothing, then `Files.move(tmp, showPath, ATOMIC_MOVE)`, which replaces an untaken signal on Linux and Windows. If that fails, fall back to an empty signal, which still brings the running instance forward. The rename arrives as `ENTRY_CREATE` on inotify and Windows. Existing 1 s wait and hung-primary alert stay; on "start anyway" the payload file is deleted and `main` handles the link itself.
   - Primary watcher: rename `simplex.show` to a private temp name, so a newer signal renamed over it is not deleted unread, then accept at most `MAX_APP_LINK_BYTES` bytes, delete it, and post `openDesktopAppLink(link)` or `showWindow()`.
   - Primary start deletes a taken file left by a crash, and any `simplex.show*` file older than its lock attempt minus a 2 s tolerance for file time granularity: such a signal or temp file was left by an earlier session, while a fresh temp file is a signal being written. `main` then starts the watcher, which registers the watch and handles a signal already present, which raised no event.
   - Windows: call `AllowSetForegroundWindow(ASFW_ANY)` before signalling so the primary can come forward (best effort). jna-platform 5.14 does not bind it, so it is looked up through `NativeLibrary`.
   - Take the directory as a parameter internally so tests can use a temp dir.
   - No log line includes the link.

4. **`common/src/desktopMain/.../platform/AppLinkScheme.desktop.kt`** (new): `registerAppLinkScheme()` runs once on a background thread and sets a `@Volatile` result read by the UI. `unixDataHome` is exposed from `Platform.desktop.kt` and `appLinkScheme` from `ChatListView.kt` for reuse.
   - macOS: registered when `jpackage.app-path` is set (packaged bundle).
   - Windows: when `jpackage.app-path` is set, read the HKCU command via `Advapi32Util`. If it differs from `"<path>" "%1"`, write the default value `URL:SimpleX Chat`, `URL Protocol`, `DefaultIcon`, and `shell\open\command`. Registered if the key now matches.
   - Linux:
     - `container=flatpak` set (the check `AppUpdater` uses) → registered.
     - AppImage, when `APPIMAGE` and `APPDIR` are set and the launcher runs from `APPDIR`, resolved through symlinks (the runtime exports `APPIMAGE` to every child process) → write `$XDG_DATA_HOME/applications/chat.simplex.app-links.desktop`: `NoDisplay=true`, `Exec=<APPIMAGE> %u` with the path quoted only when the desktop-entry spec requires it (`xdg-open` 1.1.3 cannot run a quoted program), and the MimeType line; a path with a line break or `%` is refused (GLib looks the program up before it expands `%%`), and so is an AppImage whose file or any directory above it another user could replace (owned by anyone but the user or root, writable by others, or group-writable by a group other than the user's own; POSIX ACLs are not inspected). Rewrite it and run `update-desktop-database` only when the content differs, then run `xdg-mime default` unless the entry is already the default.
     - Otherwise (deb): registered when `xdg-mime query default x-scheme-handler/simplexchat` names the deb's own `simplex-simplex.desktop`. Processes are run without a shell and with a timeout.
   - The decision is the function `desktopInstallation(platform, appPath, env)`, so it is unit tested.
   - `desktopPlatform` becomes lazy in its own commit: the new tests reference a `DesktopPlatform` constant first, and the eager value was then null through a class initialization cycle.
   - Unpackaged runs register nothing and report false.

5. **Common UI**
   - `expect fun appLinkSchemeRegistered(): Boolean` in `platform/AppCommon.kt`, with Android actual `true` and desktop actual returning step 4's result.
   - `BadgeStore.kt`: `badgePageUrl(linkReturns)` returns `app=true` or `app=desktop`; the button reads the flag once and passes it, so the URL and the fallback screen agree.
   - `BadgesSupportSimplexView.kt` `BuyInBrowserButton`: open the Redeem code screen beside the browser only when not registered (replaces `appPlatform.isDesktop`), and update its comment.

6. **Packaging**
   - `desktop/build.gradle.kts`: add `CFBundleURLTypes` (`CFBundleURLName` `chat.simplex.app`, scheme `simplexchat`) to the existing `extraKeysRawXml`.
   - `scripts/desktop/make-deb-linux.sh`: after `dpkg-deb -R`, sed `extracted/opt/*imple*/lib/*.desktop` so `Exec` ends in ` %u` and `MimeType=x-scheme-handler/simplexchat;`, and fail if the MimeType line is missing. This must run before the timestamp `touch`.
   - `desktop/src/jvmMain/resources/distribute/SimpleX.desktop` (AppImage only): add the MimeType line and `%u` to Exec. In `scripts/desktop/make-appimage-linux.sh`, make the Exec sed replace only the program, so `%u` is kept.
   - `scripts/flatpak/chat.simplex.simplex.desktop`: add the MimeType line. Confirm the Flathub manifest takes this file.

7. **Window activation** (`WindowActivation.kt`, `DesktopApp.kt showWindow`): `showWindow` skips a disposed window (during a crash restart), sets `isVisible` immediately, and on Linux also sends `_NET_ACTIVE_WINDOW` (pager source) through JNA after syncing AWT's connection, because AWT's `toFront()` sends only `XRaiseWindow`, which Sway drops.

8. **Docs**
   - `apps/multiplatform/spec/client/navigation.md`: replace "Desktop registers no URL scheme" with the desktop paths.
   - `apps/multiplatform/spec/architecture.md` and `spec/README.md`: the desktop `main()` steps, `showWindow()` and the line references they shift.
   - `spec/impact.md` and the `CODE.md` Document Map: rows for the new desktop files, with `Main.kt` and `SingleInstance.kt` mapped to `navigation.md` as well; the Source Files tables in `navigation.md` and `architecture.md`; `product/concepts.md`: a row for `AppLinks.kt`; `product/gaps.md`: GAP-08 for the known registration gaps, marked `[GAP]` in `navigation.md`.
   - `plans/2026-09-24-badge-buy-in-browser.md`: mark the desktop deep links section as done.

## Commits

1. `tests: close the walk stream in withTempDir`
2. `desktop: receive app links from args and macOS`
3. `desktop: forward an app link to the running app`
4. `desktop: initialize the desktop platform lazily`
5. `tests: share the temp directory helper`
6. `desktop: register simplexchat scheme in packages`
7. `desktop: register simplexchat scheme at runtime`
8. `ui: end the badge page with a link on desktop`
9. `desktop: raise the window over other apps`
10. `docs: describe desktop deep links in the spec`
11. `docs: plan desktop deep links`
12. `docs: report on desktop deep links`

## Verification

- **Unit tests** (`common/src/desktopTest`, beside `SingleInstanceTest.kt`):
  - `appLinkFromArgs`: accepts `simplexchat:` in any case; rejects `simplex:`, `https:`, empty args, a link that is not the only argument, and a link over the bound in bytes, including a multi-byte one within it in characters.
  - Signal payload: link and empty round trip, taken once, replace of an untaken signal and of a taken file left by a crash, owner-only file on POSIX, a link of exactly the byte bound, rejected content including a multi-byte link over the bound, a temp file created beside the signal, a sweep that removes old temp files and a taken file but keeps a fresh temp file and the old lock and database files, keeps a signal within the 2 s tolerance and drops an older one, a watcher that takes a signal written before its watch, and successive signals arriving while it watches, each once, through a real `WatchService`.
  - The Windows command string, the registry values and registration with a stand-in registry, the deb's registered check, the AppImage desktop entry for a path with spaces, Exec quoting that leaves a plain path bare and quotes and escapes reserved characters, its refusal of line breaks and `%`, `runProcess` output, discarded error output, an end of input and failure, and `desktopInstallation` precedence including Flatpak over AppImage, inherited AppImage variables, a mount path that only shares a prefix, and a mount reached through a symlink; the AppImage ownership check for world-writable and group-writable files and directories, a world-writable ancestor, another owner, a root-owned file, and a missing file; AppImage registration with a stand-in for `xdg-mime`: a new entry in a missing directory, an unchanged default, an unchanged entry that is no longer the default, a moved AppImage, a default that does not change, and a path the entry cannot run.
  - `badgePageUrl` (in `common/src/commonTest`): `app=true` when the link returns, `app=desktop` otherwise.
  - `activationEvent` and `sendActivation`: a `_NET_ACTIVE_WINDOW` client message for the window, with the pager source and `CurrentTime`, checked in native memory, sent to the root window through a stand-in for libX11 that also checks the display is closed.
- **Commands**: `./gradlew :common:desktopTest`, `./gradlew :desktop:compileKotlinJvm` and `./gradlew :common:compileDebugKotlinAndroid`, with no new warnings.
- **Manual matrix** on packaged builds. States: app closed, running, hidden in tray, locked with passcode, no active profile.
  - Launch commands: `open 'simplexchat:/badge/code/<code>'`, `start simplexchat:/badge/code/<code>`, `xdg-open ...`.
  - End to end through the page mock (`apps/simplex-badge-service/web/mock/server.py`, `#/tier?app=true`, settle via `/control/settle/<invoiceId>`) in Chrome, Firefox and Safari.
  - Linux: deb on GNOME and KDE, snap Firefox, Flatpak, AppImage.
  - Check `lsregister -dump | grep simplexchat` on macOS and the HKCU key on Windows.
  - Confirm that no log contains the code.
- **Unpackaged `./gradlew run`**: reports not registered and keeps D1.

## Risks to confirm on hardware

- How the Windows launcher re-quotes argv when it restarts itself.
- macOS cold start with Compose 1.8.2.
- Snap Firefox on Ubuntu opening custom schemes.
- GNOME Wayland raising the window (best effort).
- Stale HKCU keys and AppImage entries stay after uninstall or deletion; the plan accepts that.
- The deb's existing `Categories=Unknown`, which `desktop-file-validate` rejects, is out of scope and left as is.
