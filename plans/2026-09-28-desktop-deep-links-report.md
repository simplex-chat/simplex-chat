# Desktop deep links: report

Packaged desktop builds on macOS, Windows and Linux receive `simplexchat:` links from the badge page, and `simplex:` connection links, with the app closed, running or hidden in the tray. Unit tests and builds pass; no packaged build has been run on real machines yet, so the manual checklist below is open.

The plan is `plans/2026-09-28-desktop-deep-links.md`.

## Flow

```
page: Return to SimpleX → simplexchat:/badge/code/<code>
  macOS            Apple Event → installOpenUriHandler → openDesktopLink
  Windows, Linux   new process main(args) → linkFromArgs
                     lock acquired → chatModel.appOpenUrl (cold start), then the watcher starts
                     lock taken    → simplex.show with the link → running app → openDesktopLink
openDesktopLink: chatModel.appOpenUrl = remoteHostId() to link, then showWindow()
App.kt LaunchedEffect → connectIfOpenedViaUri → openAppLink → BadgesRedeemLinkView (asks before redeeming)
```

## Registration

| Build | Registered by | What is written | Registered when |
|---|---|---|---|
| macOS `.dmg` | Info.plist, at build | `CFBundleURLTypes` with schemes `simplexchat` and `simplex` in `desktop/build.gradle.kts` | copied to /Applications; LaunchServices picks it up |
| Windows `.msi`, `.exe` | the app, each start | `HKCU\Software\Classes\simplexchat` and `\simplex`: `URL Protocol`, `DefaultIcon`, `shell\open\command = "<SimpleX.exe>" "%1"` | first launch per user; rewritten if the path changed |
| Linux `.deb` | `make-deb-linux.sh`, at build | jpackage's `simplex-simplex.desktop` gets `Exec=... %u` and `MimeType=x-scheme-handler/simplexchat;x-scheme-handler/simplex;` | install; postinst `xdg-desktop-menu install` refreshes the MIME cache |
| AppImage | the app, each start; also the bundled entry | hidden `$XDG_DATA_HOME/applications/chat.simplex.app-links.desktop` with `Exec=<APPIMAGE> %u`, the path quoted only when the desktop-entry spec requires it, then `xdg-mime default` for each scheme; the bundled `chat.simplex.app.desktop` declares the MimeType for AppImage integration tools | first launch; rewritten if the AppImage moved |
| Flatpak | `scripts/flatpak/chat.simplex.simplex.desktop` | `MimeType=x-scheme-handler/simplexchat;x-scheme-handler/simplex;` (Exec already has `%U`) | install of a Flathub release whose pinned commit includes this file |

The registration code is in `common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt`. It runs once on a background thread after `initApp()`. The installation kind comes from `desktopInstallation(platform, appPath, env)`. An unpackaged run (no `jpackage.app-path`) registers nothing, and `APPIMAGE` counts only when the launcher runs from `APPDIR`, resolved through symlinks, because the AppImage runtime exports it to every child process. The AppImage entry is rewritten, followed by `update-desktop-database`, only when its content changed. An AppImage is not registered when its file or any directory above it belongs to a user other than this user or root, or has permission bits that let others write it, since the entry would run that file for every link; POSIX ACLs are not inspected. A path with a line break or `%` is not registered either, since GLib looks the program up before it expands `%%`. The path in `Exec` is quoted only when the desktop-entry spec requires it, because `xdg-open` 1.1.3 (Debian 12, Ubuntu 24.04) cannot run a quoted program in its generic mode, which Sway and other desktops it does not recognise use.

## Receiving the link

- **Closed, Windows and Linux:** the link is the only argument of `main`. `linkFromArgs` accepts it only when it is the single argument, starts with `simplexchat:` or `simplex:` and is at most 8192 bytes in UTF-8, enough for a one-time link with its post-quantum key (about 2 KB) and for a second such key.
- **Running, Windows and Linux:** the second process writes the link to an owner-only temp file in `dataDir` and renames it to `simplex.show`; if that fails, it writes an empty signal, which still raises the running app. The running app renames it to a private name, accepts at most 8192 bytes, deletes it, opens the link and shows its window. A signal present when the watcher starts is handled too, unless it predates the lock attempt by more than a 2 s tolerance for file time granularity; such a signal is left by an earlier session and is deleted instead, as are old temp files and a taken file left by a crash. On Windows the second process first calls `AllowSetForegroundWindow` so the window can come to the front, and logs a refusal.
- **Hung running app:** the existing "not responding" alert stays. "Start anyway" deletes the signal and the new process opens the link itself.
- **macOS:** the link arrives as an Apple Event, closed or running. `installOpenUriHandler` sets the handler on macOS only; the JDK holds an event that arrives during startup until then.
- **Bringing the window forward on Linux:** AWT's `toFront()` sends only `XRaiseWindow`, which Sway drops and focus stealing prevention refuses. `showWindow` makes the window visible at once and also sends `_NET_ACTIVE_WINDOW` (pager source) through JNA, after syncing AWT's connection so the map request comes first. On Sway the result follows `focus_on_window_activation`: the default `urgent` marks the window urgent, `smart` or `focus` focuses it. This was verified on headless Sway 1.9 with XWayland.

## Badge page ending

`appLinkSchemeRegistered()` decides the page flag when *Buy in browser* is tapped:

| Result | Page URL | App side |
|---|---|---|
| registered | `#/tier?app=true` | page ends with *Return to SimpleX* and *Show code* |
| not registered | `#/tier?app=desktop` | Redeem code screen opens beside the browser (fallback) |

Only `simplexchat:` counts here; `simplex:` is registered alongside it without being checked. By build, macOS is registered when packaged; Windows when the HKCU `simplexchat` command matches this exe; Flatpak always; AppImage and deb when `xdg-mime` reports their own entry as default. Android always reports registered. The button reads the flag once, so the URL and the fallback screen agree.

## Security

- The link is a bearer token. No log line on this path includes it.
- Any web page or program can send a link. The redeem screen from #7592 asks before redeeming and names the profile.
- The code arrives as a command-line argument. On Linux any local user can read it through `/proc/<pid>/cmdline` unless `/proc` is mounted with `hidepid`; on a cold start the argument belongs to the app itself and stays readable until it exits, otherwise to the second process for about a second. On Windows only the same user and administrators can read it. This is an accepted risk; removing it needs the page to encrypt the code to a key the app passes in the URL fragment.
- The signal file holding the link is owner-only on POSIX and deleted as soon as it is read. It stays on disk only if the running app is hung. A later start that takes the lock deletes it without opening the link once it is more than 2 s older than the lock attempt; while the hung app still holds the lock, a later launch replaces the file and shows the alert again.
- Another program can claim the same scheme (on Windows, a per-user key wins; on Linux, the last `xdg-mime default` wins). That risk is accepted, and *Show code* on the page remains.

## Automated verification (done)

- `./gradlew :common:desktopTest`: all tests pass; this work adds `AppLinksTest`, `AppLinkSchemeTest`, `BadgePageUrlTest`, `WindowActivationTest` and thirteen `SingleInstanceTest` cases.
  - `AppLinksTest`: accepts `simplexchat:` and `simplex:` in any case, including a one-time link with its post-quantum key; rejects `https:`, another scheme sharing the prefix, a path, empty, a link that is not the only argument, and a link over 8192 bytes, including a multi-byte one shorter than that in characters.
  - `SingleInstanceTest`: link and empty signals round trip and are taken once; a later signal replaces an untaken one, and a taken file left by a crash is replaced; the file is owner-only (on POSIX); a link of exactly 8192 bytes and a one-time link with its post-quantum key are accepted; non-link content, an oversized link and a multi-byte link over the byte bound are dropped and still removed; the temp file is created beside the signal (skipped where the watch service polls, as on macOS); the startup sweep removes old temp files and a taken file but keeps a fresh temp file and the old lock and database files, and keeps a signal within the 2 s tolerance but drops an older one; the watcher takes a signal written before its watch, and successive signals arriving while it watches (skipped where the watch service polls, as on macOS), through a real `WatchService`, each once.
  - `AppLinkSchemeTest`: Windows command quoting, the registry values, the deb counting as registered only when its own entry is the default, and registration with a stand-in registry (writing every value of both schemes when the commands point elsewhere, leaving matching commands alone, failing when the `simplexchat` command is not stored, succeeding when only the `simplex` one is not); desktop-entry quoting that leaves a plain path bare, quotes every reserved character, keeps spaces inside the quotes and escapes `"`, `` ` ``, `$` and `\`; the full AppImage entry and its refusal of line breaks and `%`; `desktopInstallation` precedence, including Flatpak over a matching AppImage, inherited AppImage variables, a mount path that only shares a prefix, and a mount reached through a symlink; the AppImage ownership check for world-writable and group-writable files and directories, a world-writable ancestor, another owner, a root-owned file, and a missing file; AppImage registration writing the entry into a missing directory and setting both defaults, leaving unchanged defaults alone, setting the defaults again for an unchanged entry, setting only the default that points elsewhere, rewriting the entry of a moved AppImage, failing when another handler keeps the `simplexchat:` default, succeeding when it keeps only the `simplex:` one, and refusing a path the entry cannot run; `runProcess` output, discarded error output, an end of input and failure.
    The ownership, AppImage registration and `runProcess` cases need POSIX permissions and `sh`, and the symlinked-mount case needs symlink rights, so they are skipped on Windows.
  - `BadgePageUrlTest`: `app=true` when the link returns, `app=desktop` otherwise.
  - `WindowActivationTest`: the `_NET_ACTIVE_WINDOW` client message targets the window with the pager source and `CurrentTime`, checked in native memory; it is sent to the root window with the window manager's event mask, and the display is closed even when sending fails (skipped where libX11 cannot load, as on Windows and macOS).
- `./gradlew :desktop:compileKotlinJvm` and `./gradlew :common:compileDebugKotlinAndroid`: succeed, no new warnings.
- The `make-deb-linux.sh` sed applied to the v7.0.3 deb's desktop file gives the intended lines, and `update-desktop-database` lists it for `x-scheme-handler/simplexchat`.
- The AppImage entry passes `desktop-file-validate` and reaches the MIME cache.

## Manual verification (to do)

Test with `simplexchat:/badge/code/<code>` from a real purchase, or with the page mock (`apps/simplex-badge-service/web/mock/server.py`, open `#/tier?app=true` in a private window, settle with `/control/settle/<invoiceId>`).

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
- [ ] GNOME on Wayland: window comes forward
- [ ] Sway: default config marks SimpleX urgent; with `focus_on_window_activation focus` the link focuses it
- [ ] deb and AppImage: `xdg-mime query default x-scheme-handler/simplex` names the app's entry

### All platforms
- [ ] app locked with a passcode: the redeem screen appears after unlock
- [ ] no active profile: the link waits until a profile is active
- [ ] desktop connected to a mobile: the badge goes to the active remote profile
- [ ] a second link while one redeems is ignored
- [ ] no log file or console output contains the code
- [ ] a `simplex:` contact or one-time link from a browser, app closed and running: the connect dialog opens and asks before connecting
- [ ] unpackaged `./gradlew run`: page gets `app=desktop`, Redeem code screen opens beside the browser
- [ ] `./gradlew runDistributable` points the Windows HKCU key at the build output until the installed app starts again; restart the installed app after such a run

## Known gaps

The registration gaps below are also recorded as GAP-08 in `apps/multiplatform/product/gaps.md`.

- A Flatpak always counts as registered, since the sandbox cannot query the host's default handler. If another installation holds the default, the page sends `app=true` and the link opens that installation; *Show code* still works.
- On Sway the window only becomes urgent unless the user sets `focus_on_window_activation smart` or `focus`. No client-side request can do more: Sway accepts activation tokens only for native Wayland windows.
- The Flathub release bump must pin a simplex-chat commit that includes the desktop file's MimeType line, and the Flathub wrapper must pass `"$@"` (done on its `update` branch).
- The HKCU key and the AppImage entry stay after uninstalling or deleting the app. On Windows, after an install into a folder other users can write to (such as a custom folder at the root of `C:\`), another user could place a program at the registered path; this accepted risk does not apply to the default Program Files install.
- On macOS, if LaunchServices starts a second copy of the app while one runs, that copy forwards no link: the URL arrives as an Apple Event the second copy never handles.
- jpackage's deb desktop file already has `Categories=Unknown`, which `desktop-file-validate` rejects, and a generic `Name=simplex`. The scheme still registers, and this work leaves both as they are.

## Commits

| Commit | Change |
|---|---|
| `tests: close the walk stream in withTempDir` | `Files.walk` closed in the existing `SingleInstanceTest` helper |
| `desktop: receive app links from args and macOS` | `AppLinks.kt`, `main(args)`, macOS handler |
| `desktop: forward an app link to the running app` | signal file payload in `SingleInstance.kt` |
| `desktop: initialize the desktop platform lazily` | `desktopPlatform` class initialization cycle |
| `tests: share the temp directory helper` | `withTempDir` moved to `TempDir.kt` for the scheme tests |
| `desktop: register simplexchat scheme in packages` | Info.plist, deb, AppImage and Flatpak desktop entries |
| `desktop: register simplexchat scheme at runtime` | `AppLinkScheme.desktop.kt` (Windows, AppImage, detection) |
| `ui: end the badge page with a link on desktop` | `appLinkSchemeRegistered()`, `badgePageUrl(linkReturns)`, fallback |
| `desktop: raise the window over other apps` | `isVisible` in `showWindow` and its skip of a disposed window during a crash restart, `_NET_ACTIVE_WINDOW` in `WindowActivation.kt`, `AllowSetForegroundWindow` on Windows |
| `docs: describe desktop deep links in the spec` | `navigation.md`, `architecture.md`, spec `README.md`, `impact.md`, `CODE.md` Document Map, `concepts.md`, `gaps.md` (GAP-08) |
| `docs: plan desktop deep links` | badge plan, plan |
| `docs: report on desktop deep links` | this report |
| `desktop: open simplex connection links` | `simplex:` accepted and registered beside `simplexchat:`, one 8192-byte bound, desktop link names without `App` |
| `docs: describe simplex links on desktop` | spec, `gaps.md`, plan and this report |
