# Show desktop startup errors in a copyable window (#4146)

## Problem

When any exception escapes `main()` before the app window appears - a missing
DLL, a failed migration, broken AWT initialization - Windows users see only the
jpackage launcher's "Failed to launch JVM" box. The launcher runs without a
console, so stderr is lost, and no log file exists yet at that point. Every
report in #4146 stalled on exactly this ("no console output or log-files
whatsoever"); the same class of failure shipped once before as #5237 (wrong
OpenSSL DLL name, fixed by #5238) and was only diagnosable by rebuilding.

The launcher reports any nonzero exit as "Failed to launch JVM" (jpackage
`JvmLauncher.cpp`, `JP_THROW` on nonzero `JLI_Launch` status), so the dialog is
generic for the whole class: bad `java-options` in `SimpleX.cfg`, poisoned
`_JAVA_OPTIONS`, DLL load failures, or any uncaught startup exception.

## Change

Catch `Throwable` around the startup portion of `main()`, show the error in a
native Win32 window, and on Windows exit cleanly afterwards. The window is laid
out like a message box: a system error icon and a message at the top, then two
clickable report links (the GitHub issue tracker and the support email
chat@simplex.chat) above a read-only, selectable, scrolling box holding the stack
trace (`EDIT`, `ES_READONLY | ES_MULTILINE | WS_VSCROLL`, sunken border), and an OK
button. The links are `SS_NOTIFY` static controls drawn blue via
`WM_CTLCOLORSTATIC`; clicking one sends `WM_COMMAND` (`STN_CLICKED`) and is opened
with `ShellExecute` - the URL in a browser, the email in the mail client (with a
`mailto:` scheme). Built with jna-platform's typed `User32` plus raw calls for the
stock icon/font, the link colouring and the link launch. Nothing is written to disk.

Line endings are normalised to a single CRLF (`stackTraceToString().lines()
.joinToString("\r\n")`) because an `EDIT` control breaks lines only on CRLF - a
lone LF, or the CR-CR-LF produced by naively replacing LF with CRLF over the
JVM's already-CRLF Windows output, renders as merged lines.

A plain `MessageBoxW` was tried first but rejected: its text cannot be selected
and it has no scrollbar, truncating the trace. An all-in-one read-only rich-edit
control was tried next - it made the message, links and trace all selectable with
`EM_AUTOURLDETECT` link detection - but its `EN_LINK` click notification proved
unreliable in the packaged runtime (links highlighted but did not open), and it
placed the links inside the trace box. Dedicated `SS_NOTIFY` link statics above a
plain edit control are reliably clickable and give the cleaner requested layout;
the trade-off is that the link text itself is not selectable (the addresses are
fixed and well-known, and the full trace remains selectable/copyable).

Key placement detail: `showApp()` is inside the try block. Its first statements
(the `SystemTray.isSupported()` probe in `DesktopTray.kt`, then Compose setup)
are the process's first AWT initialization, which is itself a known startup
failure cause (#4146, fixed separately by bundling `jdk.accessibility`).
Verified empirically: a first build with `showApp()` outside the try showed no
dialog on a machine reproducing the AWT failure; moving it inside is required.

## Why this design

- Native window, not Swing: broken AWT initialization is one of the failure
  causes, so a Swing dialog would crash the same way the app did. Win32 windowing
  goes straight to `user32.dll` via JNA and is unaffected by the JVM's AWT state.
- The WndProc handles WM_COMMAND (OK -> close, or a link static -> ShellExecute
  its URL/email by control id), WM_CTLCOLORSTATIC (white static backgrounds, plus
  blue text for the two link statics) and WM_DESTROY (end the loop). The
  `WindowProc` callback is held in a local for the loop's lifetime so it is not
  garbage-collected while alive.
- `SS_NOTIFY` static links instead of a rich-edit control: no dependency on
  Msftedit.dll or comctl32 v6, no `EN_LINK` reliability issue, and `STN_CLICKED`
  is unambiguous. Simpler and it removes the rich-edit machinery entirely.
- On Windows, after the dialog is dismissed the process exits with `exitProcess(0)`
  instead of rethrowing, so the launcher does not also show its own generic
  "Failed to launch JVM" box on top of ours (it shows that box on any nonzero
  exit). If the native window itself fails, we fall through to the caller's
  `throw e`, and the launcher's box becomes the fallback.
- Windows-only dialog: on Linux/macOS the rethrown exception reaches stderr in
  the terminal; the launcher-hides-everything problem is Windows-specific.
- The window icon is set to the app's own exe icon (falling back to the system
  error icon) so the title bar is not the default "unknown" icon.
- try/catch in `main()`, not `Thread.setDefaultUncaughtExceptionHandler`: a
  default handler would change crash handling for every thread for the app's
  whole lifetime; the try/catch is scoped to startup only.
- Crashes after the window appears are out of scope: the existing
  `WindowExceptionHandler` in `showApp()` (DesktopApp.kt) already surfaces
  those in-app with a shareable stack trace and does not propagate to `main()`.

## Impact

- No behavior change when startup succeeds.
- On startup failure, Windows users get one error window with clickable report
  links (issue tracker, email) above a selectable/scrollable stack trace - and no
  longer the launcher's generic "Failed to launch JVM" box (suppressed via the
  clean exit).
- The message text is hardcoded English: translated resources may not be
  loadable in the failed state this code reports on.

## Verification

- Compiles on Linux; the Linux/macOS path (rethrow to stderr) is default JVM
  behavior.
- Negative result that shaped the change: CI test MSI v1 (catch not covering
  `showApp()`) showed no dialog on a Windows 10 machine reproducing the
  assistive-technology startup failure - the crash fires at the SystemTray
  probe inside `showApp()`.
- CI test MSI v2 (this change) built; confirmation on the same repro machine
  that the dialog appears with the `AWTError` stack trace is pending.
