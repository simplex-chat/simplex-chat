# Desktop: peer text injected into notification commands and protocols

## Problem

On desktop, a contact's display name and message text can control how the app's notification is shown.

- **Linux.** Notifications go through two-slices 0.9.1 `NotifyToaster`, which runs `notify-send -i ICON -t 10000 TITLE BODY` with no `--` separator. GLib parses options anywhere among the arguments (`notify-send Title --version` prints the version). So a message text, or a contact's display name, that starts with `-` becomes notify-send options:
  - `--app-name=…`: spoofs the sending application.
  - `--urgency=critical`: a critical notification that never times out.
  - `--icon=/any/local/file`: shows any local image as the icon.
  - `--wait`: blocks the app's single background executor (`withBGApi`) while the notification is open. Each such message delays every later notification, and all other background work, by up to 10 s. Three messages delayed the next notification by 30 s.
  - `--version`, or a malformed option: the notification is suppressed.
- **Linux, markup.** notify-send decodes C escapes (`\074` → `<`) in the body, and notification servers render the body as markup. So a message can show styled text, links (KDE Plasma), or local images (Plasma `<img>`). Plain `<`/`&` text is mangled too: `a < b & c > d` shows as `a  d`.
- **Any desktop OS with a GNTP listener** (Snarl/Growl on port 23053). two-slices tries `GNTPToaster` first. It writes the title and body into GNTP headers without escaping, so a CR/LF in a message adds headers of the sender's choosing.
- **macOS, if the native Notification Center backend fails to load.** two-slices falls back to `OsXToaster`. It builds an AppleScript string escaping only `"`, so `\"` ends the string.

Android is not affected (no process launch). Windows uses the AWT tray balloon (plain strings), except with a GNTP listener.

## Evidence (master a932c82, before the fix)

Reproduced in the desktop GUI with a scripted contact, a private D-Bus session and dunst as the notification server. The screenshots are in the PR.

| Message text | Result |
|---|---|
| `--app-name=SPOOFED` | notification from "SPOOFED", empty body |
| `--urgency=critical` | critical notification |
| `--icon=…/dialog-warning.png` | the icon replaced |
| `<span color="red" size="xx-large">FAKE ALERT</span>` | rendered red and large |
| `a < b & c > d` | shown as `a  d` |
| `--wait` ×3, then `after flood` | `after flood` notified 30 s late. Thread dump: `NotifyToaster.toast` blocked reading the notify-send pipe on the `withBGApi` thread |
| `gntp test\r\nX-Injected-Header: yes`, with a GNTP listener | the GNTP NOTIFY request contained `X-Injected-Header: yes` as its own header line |

## Fix

One file, `NtfManager.desktop.kt`. The app now chooses the two-slices backend itself, and gives it to each toast with `ToastBuilder.toaster(...)`.

```
Linux  → NotifySendToaster (our subclass of two-slices' NotifyToaster)
macOS  → NotificationCenterToaster (JNA, NSString arguments)
other  → AWTToaster (Windows tray balloon, plain strings)
if that fails → AWTToaster, then SysOutToaster (the old last resort)
```

`NotifySendToaster` keeps `NotifyToaster`'s availability check (its constructor runs `notify-send --help`) and overrides only `toast()`. The arguments are the same as before (`-i <icon or dialog-information> -t 10000`) with two changes:
- `--` before the title and body, so they are never parsed as options;
- the body is escaped: backslashes are doubled (notify-send runs the body through `g_strcompress`), then `&`, `<`, `>` become entities (freedesktop body markup).
  - The title is plain text by the spec, and notify-send passes it through unchanged, so it is not escaped.

GNTP, Growl, OsX (osascript) and the other backends two-slices would probe are never constructed.

### Alternatives considered

- **Strip or prefix a leading `-` before handing text to two-slices.** That transforms visible text, misses option clusters and markup, and keeps the GNTP and osascript paths. Rejected.
- **`ToasterSettings.setPreferredToasterClassName`.** It is only a preference: the factory still constructs every service in order, GNTP included, and falls back to the first that worked. Rejected.
- **D-Bus `Notify` directly instead of notify-send.** It would avoid argv and escape decoding, but it needs a new dependency (dbus-java) and much more code. The body markup escaping is needed either way. Rejected for this fix.

## Behaviour changes besides the fix

- Snarl/Growl (GNTP) users get the OS-native notification instead of GNTP.
- macOS or Windows with a `notify-send` binary on PATH (for example Homebrew libnotify) now get the native backend; before, two-slices picked notify-send.
- The `twoslices.preferred` system property is ignored.
- Bodies show backslash sequences literally: `C:\new` was shown as `C:` + newline + `ew`.
- Notification servers that don't support body markup show `&amp;`, `&lt;`, `&gt;` literally. GNOME Shell, KDE Plasma, xfce4-notifyd and dunst (default) support it.
- On a notify-send failure the log no longer contains the sender's name: two-slices put the title into its exception message.

## Testing

- Before and after, with the same scripted checks (desktop GUI, scripted contacts, private D-Bus session with dunst as the notification server). The screenshots are in the PR.
- Regression of every area the change touches: message, contact request, contact connected and call notifications, preview modes, muted chats, background profiles, unicode and multi-line text.
- Android: no APK. The change is in `desktopMain` only, so an APK runs no changed code.
- macOS and Windows: not tested here (no machines). The macOS and Windows paths construct the same backend classes that two-slices selected in normal setups.

## Open questions

- The file still contains the old, unreachable `displayNotification(title, text, icon)` with `linuxDisplayNotification` and `macDisplayNotification`. They have the same injection patterns but no callers. Should they be removed in a separate PR?
- A hung notification server can still block the `withBGApi` thread until notify-send's D-Bus timeout (~25 s). A peer can no longer trigger it. Should notify-send get a time limit?
- The Windows click plan (#7743) planned its own AWT-only fallback. With this fix it can reuse this backend selection.
