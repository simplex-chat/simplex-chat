package chat.simplex.common

import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chatlist.isAppLink
import chat.simplex.common.views.chatlist.isConnectionLink
import java.awt.Desktop

// A one-time link with its post-quantum key is about 2 KB, near 4 KB with a second such key;
// the bound caps what the app accepts from outside.
internal const val MAX_LINK_BYTES = 8192

internal fun isAcceptedLink(uri: String): Boolean =
  uri.toByteArray(Charsets.UTF_8).size <= MAX_LINK_BYTES && (isAppLink(uri) || isConnectionLink(uri))

fun linkFromArgs(args: Array<String>): String? =
  args.singleOrNull()?.takeIf(::isAcceptedLink)

// The link is stored before showWindow, so a failure to show the window cannot lose it.
internal fun openDesktopLink(uri: String) {
  chatModel.appOpenUrl.value = chatModel.remoteHostId() to uri
  showWindow()
}

// macOS delivers a link as an Apple Event, never in args; the JDK queues it until a handler is set
fun installOpenUriHandler() {
  if (!Desktop.isDesktopSupported()) return
  val desktop = Desktop.getDesktop()
  if (!desktop.isSupported(Desktop.Action.APP_OPEN_URI)) return
  desktop.setOpenURIHandler { event ->
    val uri = event.uri.toString()
    if (isAcceptedLink(uri)) openDesktopLink(uri)
  }
}
