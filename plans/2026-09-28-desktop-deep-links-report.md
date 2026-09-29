# Desktop deep links: report

Packaged desktop builds on macOS, Windows and Linux receive `simplexchat:` links from the badge page, with the app closed, running or hidden in the tray. Unit tests and builds pass; no packaged build has been run on real machines yet, so the manual checklist below is open.

Plan: `plans/2026-09-28-desktop-deep-links.md`.

## Flow

```
page: Return to SimpleX → simplexchat:/badge/code/<code>
  macOS            Apple Event → installOpenUriHandler → openDesktopAppLink
  Windows, Linux   new process main(args) → appLinkFromArgs
                     lock acquired → chatModel.appOpenUrl (cold start)
                     lock taken    → simplex.show with the link → running app → openDesktopAppLink
openDesktopAppLink: showWindow(), chatModel.appOpenUrl = remoteHostId() to link
App.kt LaunchedEffect → connectIfOpenedViaUri → openAppLink → BadgesRedeemLinkView (asks before redeeming)
```

## Registration

| Build | Registered by | What is written | Registered when |
|---|---|---|---|
| macOS `.dmg` | Info.plist, at build | `CFBundleURLTypes` with scheme `simplexchat` in `desktop/build.gradle.kts` | copied to /Applications; LaunchServices picks it up |
| Windows `.msi`, `.exe` | the app, each start | `HKCU\Software\Classes\simplexchat`: `URL Protocol`, `DefaultIcon`, `shell\open\command = "<SimpleX.exe>" "%1"` | first launch per user; rewritten if the path changed |
| Linux `.deb` | `make-deb-linux.sh`, at build | jpackage's `simplex-simplex.desktop` gets `Exec=... %u` and `MimeType=x-scheme-handler/simplexchat;` | install; postinst `xdg-desktop-menu install` refreshes the MIME cache |
| AppImage | the app, each start | hidden `$XDG_DATA_HOME/applications/chat.simplex.app-links.desktop` with `Exec="<APPIMAGE>" %u`, then `xdg-mime default` | first launch; rewritten if the AppImage moved |
| Flatpak | `scripts/flatpak/chat.simplex.simplex.desktop` | `MimeType=x-scheme-handler/simplexchat;` (Exec already has `%U`) | install, once Flathub takes the file |

Registration code: `common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt`. It runs once on a background thread after `initApp()`. An unpackaged run (no `jpackage.app-path`, no `APPIMAGE`) registers nothing.

## Receiving the link

- **Closed, Windows and Linux:** the link is the only argument of `main`. `appLinkFromArgs` accepts it only when it is the single argument, starts with `simplexchat:` and is at most 1024 characters.
- **Running, Windows and Linux:** the second process writes the link to an owner-only temp file in `dataDir` and renames it to `simplex.show`. The running app reads it, deletes it, shows its window and opens the link. On Windows the second process first calls `AllowSetForegroundWindow` so the window can come to the front.
- **Hung running app:** the existing "not responding" alert stays. "Start anyway" deletes the signal and the new process opens the link itself.
- **macOS:** the link arrives as an Apple Event, closed or running. `installOpenUriHandler` sets the handler on macOS only; the JDK holds an event that arrives during startup until then.

## Badge page ending

`appLinkSchemeRegistered()` decides the page flag when *Buy in browser* is tapped:

| Result | Page URL | App side |
|---|---|---|
| registered | `#/tier?app=true` | page ends with *Return to SimpleX* and *Show code* |
| not registered | `#/tier?app=desktop` | Redeem code screen opens beside the browser (fallback) |

Per build: macOS is registered when packaged; Windows when the HKCU command matches this exe; Flatpak always; AppImage when `xdg-mime` reports its own entry as default; deb when `xdg-mime` reports any default handler. Android always reports registered.

## Security

- The link is a bearer token. Nothing logs it: log lines on this path are fixed strings.
- Any web page or program can send a link. The redeem screen from #7592 asks before redeeming and names the profile.
- On Windows and Linux the code is visible in the command line of the short-lived second process to processes of the same user. Accepted.
- The signal file holding the link is owner-only on POSIX and deleted as soon as it is read. It stays on disk only if the running app is hung.
- Another program can claim the same scheme (on Windows, a per-user key wins; on Linux, the last `xdg-mime default` wins). Accepted; *Show code* on the page remains.

## Automated verification (done)

- `./gradlew :common:desktopTest`: 79 tests pass, 23 new.
  - `AppLinksTest`: accepts `simplexchat:` in any case; rejects `simplex:`, `https:`, empty, oversized, and a link that is not the only argument.
  - `SingleInstanceTest`: link and empty signals round trip; a later signal replaces an untaken one; the file is owner-only; non-link content is dropped and still deleted; stale files are swept; a real `WatchService` sees the rename as `ENTRY_CREATE`.
  - `AppLinkSchemeTest`: Windows command quoting; desktop-entry escaping of spaces, `"`, `` ` ``, `$`, `\` and `%`; the full AppImage entry.
- `./gradlew :desktop:compileKotlinJvm` and `./gradlew :common:compileDebugKotlinAndroid`: succeed, no new warnings.
- The `make-deb-linux.sh` sed applied to the v7.0.3 deb's desktop file gives the intended lines, and `update-desktop-database` lists it for `x-scheme-handler/simplexchat`.
- The AppImage entry passes `desktop-file-validate` and reaches the MIME cache.

## Manual verification (to do)

Test links: `simplexchat:/badge/code/<code>` from a real purchase, or the page mock (`apps/simplex-badge-service/web/mock/server.py`, open `#/tier?app=true` in a private window, settle with `/control/settle`).

### macOS (signed `.dmg`, installed to /Applications)
- [ ] `lsregister -dump | grep simplexchat` lists the installed app
- [ ] app closed: `open 'simplexchat:/badge/code/<code>'` starts it and shows the redeem screen
- [ ] app running, window behind others: link raises it and shows the redeem screen
- [ ] app hidden in the tray: link shows the window
- [ ] dmg still mounted beside the installed copy: the installed copy opens
- [ ] Safari, Chrome, Firefox: *Return to SimpleX* prompts, then opens the app

### Windows (`.msi` install)
- [ ] after first launch, `HKCU\Software\Classes\simplexchat\shell\open\command` is `"C:\Program Files\SimpleX\SimpleX.exe" "%1"`
- [ ] app closed: `start simplexchat:/badge/code/<code>` starts it and shows the redeem screen
- [ ] app running: link reaches the running app, window comes to the front
- [ ] app hidden in the tray: link shows the window
- [ ] the argument arrives as one piece after the launcher restarts itself
- [ ] Edge, Chrome, Firefox: *Return to SimpleX* prompts, then opens the app
- [ ] after an MSI upgrade, the key still points at the installed exe

### Linux
- [ ] deb on GNOME and KDE: `xdg-mime query default x-scheme-handler/simplexchat` gives `simplex-simplex.desktop`
- [ ] deb, app closed and running: `xdg-open 'simplexchat:/badge/code/<code>'` shows the redeem screen
- [ ] AppImage: after first launch the hidden entry exists and is the default; moving the AppImage and relaunching updates it
- [ ] Flatpak: link opens the Flatpak app (needs the Flathub update)
- [ ] Firefox and Chrome from the distribution, snap Firefox on Ubuntu, Flatpak Firefox: *Return to SimpleX* opens the app
- [ ] GNOME on Wayland: window comes forward (best effort)

### All platforms
- [ ] app locked with a passcode: the redeem screen appears after unlock
- [ ] no active profile: the link waits until a profile is active
- [ ] desktop connected to a mobile: the badge goes to the active remote profile
- [ ] a second link while one redeems is ignored
- [ ] no log file or console output contains the code
- [ ] unpackaged `./gradlew run`: page gets `app=desktop`, Redeem code screen opens beside the browser

## Known gaps

- The Flathub manifest is in a separate repo and must take the updated desktop file.
- The HKCU key and the AppImage entry stay after uninstalling or deleting the app.
- jpackage's deb desktop file already has `Name=simplex` and `Categories=Unknown`, which `desktop-file-validate` rejects. The scheme still registers; not changed here.

## Commits

| Commit | Change |
|---|---|
| `desktop: receive app links from args and macOS` | `AppLinks.kt`, `main(args)`, macOS handler |
| `desktop: forward an app link to the running app` | signal file payload in `SingleInstance.kt` |
| `desktop: register simplexchat scheme at runtime` | `AppLinkScheme.desktop.kt` (Windows, AppImage, detection) |
| `desktop: register simplexchat scheme in packages` | Info.plist, deb, AppImage and Flatpak desktop entries |
| `ui: end the badge page with a link on desktop` | `appLinkSchemeRegistered()`, `badgePageUrl()`, fallback |
| `docs: desktop deep links` | spec, badge plan, plan |
