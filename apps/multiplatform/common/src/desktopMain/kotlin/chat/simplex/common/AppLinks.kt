package chat.simplex.common

import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.desktopPlatform
import chat.simplex.common.views.chatlist.isAppLink
import java.awt.Desktop

// far above any app link this build handles, and bounds what a second process can hand to this one
const val MAX_APP_LINK_LENGTH = 1024

fun isAcceptedAppLink(s: String): Boolean =
  s.length <= MAX_APP_LINK_LENGTH && isAppLink(s)

// the scheme handlers pass the link as the only argument, so anything else is not a link
fun appLinkFromArgs(args: Array<String>): String? =
  args.singleOrNull()?.takeIf(::isAcceptedAppLink)

fun openDesktopAppLink(uri: String) {
  showWindow()
  chatModel.appOpenUrl.value = chatModel.remoteHostId() to uri
}

// macOS delivers a link as an Apple Event, never in args; the JDK queues it until a handler is set
fun installOpenUriHandler() {
  if (!desktopPlatform.isMac() || !Desktop.isDesktopSupported()) return
  val desktop = Desktop.getDesktop()
  if (!desktop.isSupported(Desktop.Action.APP_OPEN_URI)) return
  desktop.setOpenURIHandler { event ->
    val uri = event.uri.toString()
    if (isAcceptedAppLink(uri)) openDesktopAppLink(uri)
  }
}
