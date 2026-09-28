package chat.simplex.common

import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chatlist.isAppLink
import java.awt.Desktop

// A link this build handles is far shorter; the bound caps what the app accepts from outside.
internal const val MAX_APP_LINK_BYTES = 1024

internal fun isAcceptedAppLink(uri: String): Boolean =
  uri.toByteArray(Charsets.UTF_8).size <= MAX_APP_LINK_BYTES && isAppLink(uri)

fun appLinkFromArgs(args: Array<String>): String? =
  args.singleOrNull()?.takeIf(::isAcceptedAppLink)

// The link is stored before showWindow, so a failure to show the window cannot lose it.
internal fun openDesktopAppLink(uri: String) {
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
    if (isAcceptedAppLink(uri)) openDesktopAppLink(uri)
  }
}
