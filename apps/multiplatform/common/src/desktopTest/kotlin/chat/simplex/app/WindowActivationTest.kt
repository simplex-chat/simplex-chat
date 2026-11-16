package chat.simplex.app

import chat.simplex.common.activationEvent
import chat.simplex.common.sendActivation
import com.sun.jna.NativeLong
import com.sun.jna.platform.unix.X11
import org.junit.Assume.assumeNoException
import java.lang.reflect.Proxy
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertNull
import kotlin.test.assertSame

private const val WINDOW_ID = 42L
private const val ATOM_ID = 7L

class WindowActivationTest {
  @Test
  fun activationEventAsksTheWindowManagerToActivateTheWindowAsAPager() {
    assumeLibX11()
    val atom = X11.Atom(ATOM_ID)
    val event = activationEvent(X11.Window(WINDOW_ID), atom)
    val message = event.xclient
    assertEquals(WINDOW_ID, message.window.toLong(), "target window")
    assertEquals(atom, message.message_type, "_NET_ACTIVE_WINDOW atom")
    assertEquals(32, message.format, "data format in bits")
    // JNA writes only the selected member of a union, so the bytes XSendEvent receives are checked too
    event.write()
    assertEquals(X11.ClientMessage, event.pointer.getInt(0), "event type in native memory")
    assertEquals(2L, message.data.pointer.getNativeLong(0).toLong(), "source indication in native memory: a pager, which focus stealing prevention lets through")
    assertEquals(0L, message.data.pointer.getNativeLong(NativeLong.SIZE.toLong()).toLong(), "timestamp in native memory: CurrentTime")
  }

  @Test
  fun activationIsSentToTheRootWindowAndTheDisplayIsClosed() {
    assumeLibX11()
    val x11 = FakeX11()
    sendActivation(x11.proxy, WINDOW_ID)
    assertEquals(listOf("XOpenDisplay", "XInternAtom", "XDefaultRootWindow", "XSendEvent", "XFlush", "XCloseDisplay"), x11.calls)
    assertNull(x11.displayName, "the display named by DISPLAY")
    assertEquals("_NET_ACTIVE_WINDOW", x11.atomName, "atom of the request")
    val send = x11.sendArgs
    assertSame(x11.display, send[0], "display")
    assertEquals(x11.root, send[1], "the request goes to the root window, where the window manager listens")
    assertEquals(NativeLong((X11.SubstructureRedirectMask or X11.SubstructureNotifyMask).toLong()), send[3], "event mask the window manager selects")
    val event = (send[4] as X11.XEvent).xclient
    assertEquals(WINDOW_ID, event.window.toLong(), "window to activate")
    assertEquals(x11.atom, event.message_type, "the interned atom is the request type")
  }

  @Test
  fun displayIsClosedWhenSendingFails() {
    assumeLibX11()
    val x11 = FakeX11(sendFails = true)
    assertFailsWith<IllegalStateException> { sendActivation(x11.proxy, WINDOW_ID) }
    assertEquals("XCloseDisplay", x11.calls.last(), "the display must be closed after a failure")
  }

  private class FakeX11(private val sendFails: Boolean = false) {
    val display = X11.Display()
    val root = X11.Window(1)
    val atom = X11.Atom(ATOM_ID)
    val calls = mutableListOf<String>()
    var displayName: Any? = null
    var atomName: Any? = null
    var sendArgs: Array<Any?> = emptyArray()

    val proxy = Proxy.newProxyInstance(X11::class.java.classLoader, arrayOf(X11::class.java)) { _, method, args ->
      calls += method.name
      when (method.name) {
        "XOpenDisplay" -> { displayName = args[0]; display }
        "XInternAtom" -> { atomName = args[1]; atom }
        "XDefaultRootWindow" -> root
        "XSendEvent" -> { sendArgs = args.copyOf(); if (sendFails) throw IllegalStateException("send failed"); 1 }
        "XFlush", "XCloseDisplay" -> 0
        else -> throw UnsupportedOperationException(method.name)
      }
    } as X11
  }

  // JNA loads libX11 to lay out any X11 structure, and only X11 desktops have it
  private fun assumeLibX11() {
    try { X11.INSTANCE } catch (e: LinkageError) { assumeNoException("libX11 is not available", e) }
  }
}
