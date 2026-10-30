package chat.simplex.app.ui.harness

import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking

class RemoteCtrlOffer(val ctrlName: String, val sessionCode: String)

fun CliPeer.setDeviceName(name: String) {
  this sends "/set device name $name"
  eventually("ok")
}

fun CliPeer.connectRemoteCtrl(invitation: String): RemoteCtrlOffer {
  this sends "/connect remote ctrl $invitation"
  val ctrlName = eventuallyMatching(CONNECTING_NEW_CTRL, CONNECT_TIMEOUT_MS).groupValues[1]
  eventually("new remote controller connected", CONNECT_TIMEOUT_MS)
  this expects "Compare session code with controller and use:"
  val verify = nextLine()
  if (!verify.startsWith(VERIFY_CTRL)) throw AssertionError("$name: expected `$VERIFY_CTRL<session code>`, got `$verify`; next: ${pending()}")
  return RemoteCtrlOffer(ctrlName, verify.removePrefix(VERIFY_CTRL))
}

fun CliPeer.verifyRemoteCtrl(sessionCode: String): String {
  this sends "$VERIFY_CTRL$sessionCode"
  return eventuallyMatching(CTRL_SESSION_STARTED, CONNECT_TIMEOUT_MS).groupValues[1]
}

fun CliPeer.stopRemoteCtrl() {
  this sends "/stop remote ctrl"
  eventually("ok")
}

fun CliPeer.startRemoteHost(port: Int): String {
  this sends "/start remote host new addr=127.0.0.1 iface=\"lo\" port=$port"
  eventually("new remote host started on 127.0.0.1:$port", CONNECT_TIMEOUT_MS)
  eventually("Remote session invitation:")
  return nextLine()
}

fun CliPeer.awaitRemoteHostCode(): String {
  eventually("new remote host connecting", CONNECT_TIMEOUT_MS)
  this expects "Compare session code with host:"
  return nextLine()
}

fun CliPeer.awaitNewRemoteHost(): String {
  val added = eventuallyMatching(NEW_HOST_ADDED, CONNECT_TIMEOUT_MS)
  eventuallyMatching(Regex("remote host ${added.groupValues[1]} connected \\((with|no) compression\\)"), CONNECT_TIMEOUT_MS)
  return added.groupValues[2]
}

fun UiScope.linkMobile(app: App): LinkMobileScreen {
  app.userPicker()
  click(Ui.UserPicker.LinkMobile)
  waitFor(Ui.Remote.AddMobile)
  return LinkMobileScreen(this)
}

class LinkMobileScreen(private val s: UiScope) {
  fun deviceName(): String? {
    s.waitFor(Ui.Remote.DeviceName)
    return s.editableText(Ui.Remote.DeviceName)
  }

  fun addMobile(): String {
    s.click(Ui.Remote.AddMobile)
    s.poll(timeoutMs = 30_000, what = "the invitation QR code with `Waiting for mobile to connect:` and `Share link`") {
      s.seesDescription("QR Code") && s.seesText("Waiting for mobile to connect:") && s.exists(Ui.Remote.ShareLink)
    }
    s.click(Ui.Remote.ShareLink)
    var invitation: String? = null
    s.poll(what = "the invitation on the clipboard") {
      invitation = s.sharedText()?.takeIf { it.startsWith("xrcp:/") }
      invitation != null
    }
    return invitation!!
  }
}

fun UiScope.awaitMobileSession(hostDeviceName: String): Long {
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the desktop to switch to the mobile `$hostDeviceName` and close the pairing screen") {
    chatModel.currentRemoteHost.value?.hostDeviceName == hostDeviceName && !chatModel.switchingUsersAndHosts.value &&
      !exists(Ui.Remote.AddMobile) && !exists(Ui.Remote.DeviceName)
  }
  quiesce()
  return chatModel.currentRemoteHost.value!!.remoteHostId
}

fun UiScope.linkMobileHost(app: App, mobile: CliPeer, hostDeviceName: String): Long {
  val offer = mobile.connectRemoteCtrl(linkMobile(app).addMobile())
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the session code `${offer.sessionCode.take(23)}` on the pairing screen") { seesText(offer.sessionCode.take(23)) }
  mobile.verifyRemoteCtrl(offer.sessionCode)
  return awaitMobileSession(hostDeviceName)
}

fun UiScope.endMobileSessionViaApi(mobile: CliPeer) {
  val host = chatModel.currentRemoteHost.value ?: error("the desktop has no mobile session to end")
  chatController.stopRemoteHostAndReloadHosts(host, switchToLocal = true)
  mobile.eventually("remote controller stopped", CONNECT_TIMEOUT_MS)
  poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the desktop back on its own profile") {
    chatModel.currentRemoteHost.value == null && !chatModel.switchingUsersAndHosts.value
  }
  quiesce()
}

fun UiScope.useFromDesktop(app: App): ConnectDesktopScreen {
  app.userPicker()
  click(Ui.UserPicker.UseFromDesktop)
  waitFor(Ui.Remote.DeviceName)
  return ConnectDesktopScreen(this)
}

class ConnectDesktopScreen(private val s: UiScope) {
  fun deviceName(): String? {
    s.waitFor(Ui.Remote.DeviceName)
    return s.editableText(Ui.Remote.DeviceName)
  }

  fun paste(invitation: String) {
    s.click(Ui.Remote.PasteAddress)
    s.poll(what = "the pasted desktop address in the address field") { s.seesText(invitation) }
  }

  fun connect() = s.click(Ui.Remote.Connect)

  fun confirm(sessionCode: String) {
    s.poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Verify connection` with the session code `${sessionCode.take(23)}`") {
      s.seesText("Verify connection") && s.seesText(sessionCode.take(23)) && s.exists(Ui.Remote.Confirm)
    }
    s.click(Ui.Remote.Confirm)
  }

  fun awaitConnected(desktopName: String, sessionCode: String) =
    s.poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Connected to desktop` with `$desktopName` and the session code `${sessionCode.take(23)}`") {
      s.seesText("Connected to desktop") && s.seesText("Connected desktop") && s.seesText(desktopName) && s.seesText(sessionCode.take(23)) && s.exists(Ui.Remote.Disconnect)
    }
}

fun UiScope.linkToDesktop(app: App, controller: CliPeer, desktopName: String, port: Int): ConnectDesktopScreen {
  val invitation = controller.startRemoteHost(port)
  setClipboardText(invitation)
  val screen = useFromDesktop(app)
  screen.paste(invitation)
  screen.connect()
  val code = controller.awaitRemoteHostCode()
  screen.confirm(code)
  controller.awaitNewRemoteHost()
  screen.awaitConnected(desktopName, code)
  return screen
}

fun UiScope.endDesktopSessionViaApi(controller: CliPeer) {
  check(runBlocking { chatController.stopRemoteCtrl() }) { "stopRemoteCtrl failed" }
  runBlocking { chatController.switchToLocalSession() }
  controller.eventuallyMatching(Regex("remote host \\d+ stopped"), CONNECT_TIMEOUT_MS)
  quiesce()
}

private const val VERIFY_CTRL = "/verify remote ctrl "
private val CONNECTING_NEW_CTRL = Regex("connecting new remote controller: (.+), v\\d\\S*.*")
private val CTRL_SESSION_STARTED = Regex("remote controller \\d+ session started with (.+) \\((with|no) compression\\)")
private val NEW_HOST_ADDED = Regex("new remote host (\\d+) added: (.+)")
