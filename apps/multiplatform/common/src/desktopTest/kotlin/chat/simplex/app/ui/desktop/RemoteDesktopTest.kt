package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.ChatScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitMobileSession
import chat.simplex.app.ui.harness.connectPeers
import chat.simplex.app.ui.harness.connectRemoteCtrl
import chat.simplex.app.ui.harness.endMobileSessionViaApi
import chat.simplex.app.ui.harness.linkMobile
import chat.simplex.app.ui.harness.linkMobileHost
import chat.simplex.app.ui.harness.setDeviceName
import chat.simplex.app.ui.harness.stopRemoteCtrl
import chat.simplex.app.ui.harness.verifyRemoteCtrl
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

class RemoteDesktopTest : UiTest() {
  @Test fun linkMobile_newPairing_verifyAndConnect() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val mobile = cli("mobile")
    mobile.setDeviceName(MOBILE)
    val link = linkMobile(alice)
    val desktopName = link.deviceName()
    val invitation = link.addMobile()
    check(!seesText("Verify connection") && !seesText("New mobile device")) { "the pairing screen shows a mobile before one connected" }
    val offer = mobile.connectRemoteCtrl(invitation)
    assertEquals("the controller name the mobile sees", desktopName, offer.ctrlName)
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Verify connection` with `New mobile device` and the mobile's session code `${offer.sessionCode.take(23)}`") {
      seesText("Verify connection") && seesText("New mobile device") && seesText(offer.sessionCode.take(23)) && !seesDescription("QR Code")
    }
    check(chatModel.currentRemoteHost.value == null) { "the desktop switched to the mobile before the mobile verified the session code" }
    assertEquals("the controller name in the mobile's session", desktopName, mobile.verifyRemoteCtrl(offer.sessionCode))
    val rh = awaitMobileSession(MOBILE)
    alice.userPicker()
    poll(what = "the user picker with the `$MOBILE` and `This device` pills and the mobile's profile") {
      MOBILE in textsOf(Ui.UserPicker.Device(rh)) && "This device" in textsOf(Ui.UserPicker.Device(null)) &&
        "mobile" in textsOf(Ui.UserPicker.User(chatModel.currentUser.value!!.userId))
    }
    escape()
    poll(what = "the user picker to close") { !exists(Ui.UserPicker.Sheet) }
    endMobileSessionViaApi(mobile)
  }

  @Test fun linkMobile_remoteChatsVisible_sendMessage() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val mobile = cli("mobile")
    mobile.setDeviceName(MOBILE)
    val bob = cli("bob")
    connectPeers(mobile, bob)
    check(chatModel.chats.value.none { it.chatInfo.chatViewName == "bob" }) { "alice has a chat with bob on the desktop" }
    val rh = linkMobileHost(alice, mobile, MOBILE)
    val bobId = alice.chatList.chatId("bob")
    val row = Ui.ChatList.Row(bobId, rh)
    poll(what = "the mobile's chat with bob in the desktop's chat list") { exists(row) && "bob" in textsOf(row) }
    click(row)
    poll(what = "the mobile's chat with bob to open") { chatModel.chatId.value == bobId }
    ChatScreen(this).send("hi from desktop")
    bob.eventuallyMessage("mobile> hi from desktop", CONNECT_TIMEOUT_MS)
    bob sends "@mobile hello desktop"
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "bob's reply in the open chat on the desktop") { chatModel.chatsContext.chatItems.value.any { it.content.text == "hello desktop" } }
    ChatScreen(this).itemId("hello desktop")
    endMobileSessionViaApi(mobile)
  }

  @Test fun linkMobile_disconnectFromDesktop() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val mobile = cli("mobile")
    mobile.setDeviceName(MOBILE)
    val rh = linkMobileHost(alice, mobile, MOBILE)
    alice.userPicker()
    click(Ui.UserPicker.Device(rh).part("disconnect"))
    mobile.eventually("remote controller stopped", CONNECT_TIMEOUT_MS)
    backOnThisDevice(alice, rh)
  }

  @Test fun linkMobile_mobileStopsSession() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val mobile = cli("mobile")
    mobile.setDeviceName(MOBILE)
    val rh = linkMobileHost(alice, mobile, MOBILE)
    check(disconnectedToasts() == 0) { "a `was disconnected` toast before the mobile stopped" }
    mobile.stopRemoteCtrl()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the `Mobile $MOBILE was disconnected` toast") { disconnectedToasts() == 1 }
    backOnThisDevice(alice, rh)
  }

  private fun UiScope.backOnThisDevice(alice: App, rh: Long) {
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "alice's own profile back on the desktop") {
      chatModel.currentRemoteHost.value == null && chatModel.currentUser.value?.displayName == "alice" && !chatModel.switchingUsersAndHosts.value
    }
    alice.userPicker()
    poll(what = "the user picker with only the `This device` pill and alice's profile") {
      exists(Ui.UserPicker.Device(null)) && !exists(Ui.UserPicker.Device(rh)) && "alice" in textsOf(Ui.UserPicker.User(chatModel.currentUser.value!!.userId))
    }
  }

  private fun enableDeveloperToolsViaApi() = appPrefs.developerTools.set(true)

  private fun disconnectedToasts(): Int = simplexWindowState.toasts.toList().count { (text, _) -> MOBILE in text && "was disconnected" in text }

  private companion object {
    const val MOBILE = "Pixel test"
  }
}
