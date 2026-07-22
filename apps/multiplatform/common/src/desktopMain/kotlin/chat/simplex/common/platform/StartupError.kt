package chat.simplex.common.platform

import com.sun.jna.*
import com.sun.jna.platform.win32.*
import com.sun.jna.platform.win32.WinDef.*
import com.sun.jna.platform.win32.WinUser.*
import kotlin.system.exitProcess

// Windows-only: shows a startup error the jpackage launcher otherwise hides behind "Failed to launch
// JVM" (#4146); on Linux/Mac the error rethrown by the caller reaches stderr. Uses a native window,
// not Swing, because broken AWT initialization can be the cause. Laid out like a message box: an error
// icon and a message at the top, two clickable report links (the GitHub issue tracker and the support
// email) above a read-only, selectable, scrolling box with the stack trace, and an OK button.
fun showStartupError(e: Throwable) {
  if (!desktopPlatform.isWindows()) return
  try {
    // Win32 edit controls only break lines on CRLF; normalize every line ending to one CRLF so the
    // trace does not render as merged lines (the JVM's own separator is CRLF on Windows, LF elsewhere)
    showWindowsErrorWindow("SimpleX failed to start", e.stackTraceToString().lines().joinToString("\r\n"))
  } catch (_: Throwable) {
    return // dialog failed to show; let the caller rethrow so the launcher's own box is the fallback
  }
  // The dialog was shown and dismissed. Exit cleanly so the jpackage launcher does not also show its
  // own generic "Failed to launch JVM" box on top of ours (it shows that box on any nonzero exit).
  exitProcess(0)
}

private const val ISSUES_URL = "https://github.com/simplex-chat/simplex-chat/issues"
private const val SUPPORT_EMAIL = "chat@simplex.chat"

// Win32 constants not defined in jna-platform
private const val ES_MULTILINE = 0x0004
private const val ES_READONLY = 0x0800
private const val ES_AUTOVSCROLL = 0x0040
private const val WS_EX_CLIENTEDGE = 0x0200
private const val CW_USEDEFAULT = 0x80000000.toInt()
private const val WM_SETTEXT = 0x000C
private const val WM_SETFONT = 0x0030
private const val WM_COMMAND = 0x0111
private const val WM_CTLCOLORSTATIC = 0x0138
private const val EM_SETLIMITTEXT = 0x00C5
private const val SS_ICON = 0x0003
private const val SS_NOTIFY = 0x0100
private const val STM_SETICON = 0x0170
private const val COLOR_WINDOW = 5
private const val WHITE_BRUSH = 0
private const val LINK_COLOR = 0x00EE0000 // COLORREF 0x00BBGGRR = link blue RGB(0,0,238)
private const val IDI_ERROR = 32513
private const val APP_ICON_ID = 1 // the app icon jpackage embeds in the launcher exe
private const val DEFAULT_GUI_FONT = 17
private const val OK_BUTTON_ID = 1
private const val URL_LINK_ID = 2
private const val EMAIL_LINK_ID = 3

// Shows a modal-style native error dialog: error icon and message, two clickable report-link statics,
// a read-only selectable edit control with the stack trace, and an OK button. Blocks on its own
// message loop until the window is closed.
private fun showWindowsErrorWindow(title: String, trace: String) {
  val user32 = User32.INSTANCE
  val user32native = NativeLibrary.getInstance("user32")
  val gdi32 = NativeLibrary.getInstance("gdi32")
  val shellExecute = NativeLibrary.getInstance("shell32").getFunction("ShellExecuteW")
  val sendMessageW = user32native.getFunction("SendMessageW")
  val hInstance = Kernel32.INSTANCE.GetModuleHandle(null)
  val className = "SimpleXStartupError"

  // Paint static backgrounds white to match the window, and draw the two link statics in blue
  val whiteBrush = gdi32.getFunction("GetStockObject").invokePointer(arrayOf<Any?>(Integer.valueOf(WHITE_BRUSH)))
  val setTextColor = gdi32.getFunction("SetTextColor")
  // The link statics' HWNDs, set once created and read back in the paint callback
  var urlLinkHwnd = 0L
  var emailLinkHwnd = 0L

  fun open(target: String) = shellExecute.invokePointer(arrayOf<Any?>(
    Pointer.NULL, WString("open"), WString(target), Pointer.NULL, Pointer.NULL, Integer.valueOf(SW_SHOWNORMAL)))

  val wndProc = WindowProc { hwnd, uMsg, wParam, lParam ->
    when {
      // A click on the OK button or a link static arrives as WM_COMMAND with the control id in LOWORD
      uMsg == WM_COMMAND -> {
        when (wParam.toInt() and 0xFFFF) {
          OK_BUTTON_ID -> user32.DestroyWindow(hwnd)
          URL_LINK_ID -> open(ISSUES_URL)                 // open the issue tracker in a browser
          EMAIL_LINK_ID -> open("mailto:$SUPPORT_EMAIL")  // open the mail client
        }
        LRESULT(0)
      }
      uMsg == WM_CTLCOLORSTATIC -> {
        if (lParam.toLong() == urlLinkHwnd || lParam.toLong() == emailLinkHwnd) {
          setTextColor.invokeInt(arrayOf<Any?>(Pointer.createConstant(wParam.toLong()), Integer.valueOf(LINK_COLOR)))
        }
        LRESULT(Pointer.nativeValue(whiteBrush))
      }
      uMsg == WM_DESTROY -> { user32.PostQuitMessage(0); LRESULT(0) }
      else -> user32.DefWindowProc(hwnd, uMsg, wParam, lParam)
    }
  }

  // Prefer the app's own icon so the title bar/taskbar is not the default "unknown" icon; fall back
  // to the system error icon (also shown in the window below).
  val hErrorIcon = user32native.getFunction("LoadIconW").invokePointer(arrayOf<Any?>(Pointer.NULL, Pointer.createConstant(IDI_ERROR)))
  val hAppIcon = user32native.getFunction("LoadIconW").invokePointer(arrayOf<Any?>(hInstance, Pointer.createConstant(APP_ICON_ID)))
  val windowIcon = hAppIcon ?: hErrorIcon // invokePointer returns null when the exe has no such icon

  val windowClass = WNDCLASSEX()
  windowClass.cbSize = windowClass.size()
  windowClass.lpfnWndProc = wndProc
  windowClass.hInstance = hInstance
  windowClass.lpszClassName = className
  windowClass.hbrBackground = HBRUSH(Pointer.createConstant(COLOR_WINDOW + 1)) // system window (light) color
  windowClass.hIcon = HICON(windowIcon)
  windowClass.hIconSm = HICON(windowIcon)
  user32.RegisterClassEx(windowClass)

  // If the window cannot be created, throw rather than enter the message loop: with no window,
  // GetMessage would block forever. The caller's catch then rethrows and the launcher box is shown.
  val window = user32.CreateWindowEx(
    0, className, title, WS_CAPTION or WS_SYSMENU or WS_VISIBLE,
    CW_USEDEFAULT, CW_USEDEFAULT, 760, 520,
    null, null, hInstance, null
  ) ?: throw IllegalStateException("failed to create startup error window")
  val client = RECT()
  user32.GetClientRect(window, client)
  val cw = client.right
  val ch = client.bottom

  val iconView = user32.CreateWindowEx(0, "STATIC", null, WS_CHILD or WS_VISIBLE or SS_ICON,
    18, 18, 32, 32, window, null, hInstance, null)
  user32.SendMessage(iconView, STM_SETICON, WPARAM(Pointer.nativeValue(hErrorIcon)), LPARAM(0))

  user32.CreateWindowEx(0, "STATIC", "SimpleX could not start. Copy the error below and report it via:",
    WS_CHILD or WS_VISIBLE, 60, 20, cw - 80, 20, window, null, hInstance, null)
  val urlLink = user32.CreateWindowEx(0, "STATIC", ISSUES_URL, WS_CHILD or WS_VISIBLE or SS_NOTIFY,
    60, 46, cw - 80, 18, window, HMENU(Pointer.createConstant(URL_LINK_ID)), hInstance, null)
  urlLinkHwnd = Pointer.nativeValue(urlLink.pointer)
  val emailLink = user32.CreateWindowEx(0, "STATIC", SUPPORT_EMAIL, WS_CHILD or WS_VISIBLE or SS_NOTIFY,
    60, 68, cw - 80, 18, window, HMENU(Pointer.createConstant(EMAIL_LINK_ID)), hInstance, null)
  emailLinkHwnd = Pointer.nativeValue(emailLink.pointer)

  val traceView = user32.CreateWindowEx(WS_EX_CLIENTEDGE, "EDIT", null,
    WS_CHILD or WS_VISIBLE or WS_VSCROLL or ES_MULTILINE or ES_READONLY or ES_AUTOVSCROLL,
    18, 96, cw - 36, ch - 96 - 54, window, null, hInstance, null)
  user32.SendMessage(traceView, EM_SETLIMITTEXT, WPARAM(0), LPARAM(0)) // do not truncate long traces
  sendMessageW.invokePointer(arrayOf<Any?>(traceView, Integer.valueOf(WM_SETTEXT), Pointer.NULL, WString(trace)))

  val okButton = user32.CreateWindowEx(0, "BUTTON", "OK", WS_CHILD or WS_VISIBLE or BS_DEFPUSHBUTTON,
    cw - 98, ch - 42, 80, 30, window, HMENU(Pointer.createConstant(OK_BUTTON_ID)), hInstance, null)

  // The default control font is the dated bitmap "System" font; use the standard dialog font
  val hFont = gdi32.getFunction("GetStockObject").invokePointer(arrayOf<Any?>(Integer.valueOf(DEFAULT_GUI_FONT)))
  for (view in listOf(urlLink, emailLink, traceView, okButton)) {
    user32.SendMessage(view, WM_SETFONT, WPARAM(Pointer.nativeValue(hFont)), LPARAM(1))
  }

  user32.ShowWindow(window, SW_SHOWNORMAL)
  user32.SetForegroundWindow(window)
  user32.SetFocus(traceView) // so Ctrl+A / Ctrl+C act on the trace immediately

  val msg = MSG()
  while (user32.GetMessage(msg, null, 0, 0) > 0) {
    user32.TranslateMessage(msg)
    user32.DispatchMessage(msg)
  }
}
