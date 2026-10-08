package chat.simplex.common

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import com.sun.jna.Native
import com.sun.jna.NativeLong
import com.sun.jna.platform.unix.X11
import java.awt.Toolkit
import java.awt.Window

// EWMH source indication of a pager, which window managers exempt from focus stealing prevention
private const val NET_ACTIVE_WINDOW_SOURCE_PAGER = 2L
private const val CLIENT_MESSAGE_FORMAT_BITS = 32

// AWT's toFront() sends only XRaiseWindow, which wlroots compositors (Sway) drop and focus stealing
// prevention refuses; _NET_ACTIVE_WINDOW is the request X11 and XWayland window managers act on.
internal fun requestX11Activation(window: Window) {
  val x11: X11
  val windowId: Long
  try {
    x11 = X11.INSTANCE
    windowId = Native.getWindowID(window)
  } catch (e: LinkageError) {
    Log.w(TAG, "window activation: native library unavailable: ${e.message}")
    return
  } catch (e: IllegalStateException) {
    Log.w(TAG, "window activation: no native window: ${e.message}")
    return
  }
  // The map request goes out on AWT's connection; flushing it first lets the window manager map the window before activating it.
  Toolkit.getDefaultToolkit().sync()
  sendActivation(x11, windowId)
}

internal fun sendActivation(x11: X11, windowId: Long) {
  val display = x11.XOpenDisplay(null)
  if (display == null) {
    Log.w(TAG, "window activation: cannot open the X display")
    return
  }
  try {
    val event = activationEvent(X11.Window(windowId), x11.XInternAtom(display, "_NET_ACTIVE_WINDOW", false))
    val mask = NativeLong((X11.SubstructureRedirectMask or X11.SubstructureNotifyMask).toLong())
    x11.XSendEvent(display, x11.XDefaultRootWindow(display), 0, mask, event)
    x11.XFlush(display)
  } finally {
    x11.XCloseDisplay(display)
  }
}

internal fun activationEvent(window: X11.Window, activeWindowAtom: X11.Atom): X11.XEvent {
  val event = X11.XEvent()
  event.setType(X11.XClientMessageEvent::class.java)
  event.xclient.type = X11.ClientMessage
  event.xclient.window = window
  event.xclient.message_type = activeWindowAtom
  event.xclient.format = CLIENT_MESSAGE_FORMAT_BITS
  event.xclient.data.setType(Array<NativeLong>::class.java)
  event.xclient.data.l[0] = NativeLong(NET_ACTIVE_WINDOW_SOURCE_PAGER)
  // the request carries no user interaction timestamp
  event.xclient.data.l[1] = NativeLong(X11.CurrentTime.toLong())
  return event
}
