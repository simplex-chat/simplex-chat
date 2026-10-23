package chat.simplex.app.ui.android

import chat.simplex.app.ui.harness.Bridge
import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.awaitNewRemoteHost
import chat.simplex.app.ui.harness.awaitRemoteHostCode
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.endDesktopSessionViaApi
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.linkToDesktop
import chat.simplex.app.ui.harness.setDeviceName
import chat.simplex.app.ui.harness.startRemoteHost
import chat.simplex.app.ui.harness.useFromDesktop
import chat.simplex.common.model.CIDirection
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.UIRemoteCtrlSessionState
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

class UseFromDesktopAndroidTest : UiTest() {
  @KnownAppIssue("15.5 item 4", "CameraUnavailableException: Device reporting less cameras than anticipated", intermittent = true)
  @Test fun useFromDesktop_pasteVerifyConfirm_connected() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val desktop = cli("desktop")
    desktop.setDeviceName(DESKTOP)
    val invitation = desktop.startRemoteHost(Bridge.remoteCtrlPort())
    setClipboardText(invitation)
    val screen = useFromDesktop(alice)
    val androidName = screen.deviceName()
    check(!seesText(invitation)) { "the desktop address is shown before pasting it" }
    screen.paste(invitation)
    screen.connect()
    val code = desktop.awaitRemoteHostCode()
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "`Verify connection` with `$DESKTOP (new)` and the controller's session code `${code.take(23)}`") {
      seesText("Verify connection") && seesText("$DESKTOP (new)") && seesText(code.take(23)) && exists(Ui.Remote.Confirm)
    }
    check(chatModel.remoteCtrlSession.value?.sessionState !is UIRemoteCtrlSessionState.Connected) { "the session is connected before Confirm" }
    click(Ui.Remote.Confirm)
    assertEquals("the host name the controller shows", androidName, desktop.awaitNewRemoteHost())
    screen.awaitConnected(DESKTOP, code)
    endDesktopSessionViaApi(desktop)
  }

  @KnownAppIssue("15.5 item 4", "CameraUnavailableException: Device reporting less cameras than anticipated", intermittent = true)
  @Test fun useFromDesktop_controllerSendsAsAndroidUser() = uiTest {
    val alice = app("alice")
    enableDeveloperToolsViaApi()
    val bob = cli("bob")
    connect(bob)
    val desktop = cli("desktop")
    desktop.setDeviceName(DESKTOP)
    linkToDesktop(alice, desktop, DESKTOP, Bridge.remoteCtrlPort())
    desktop sends "/contacts"
    desktop.eventually("bob")
    desktop sends "@bob hi from desktop"
    bob.eventuallyMessage("alice> hi from desktop", CONNECT_TIMEOUT_MS)
    endDesktopSessionViaApi(desktop)
    goBack()
    poll(what = "`Connect to desktop` to close over alice's chat list") { !exists(Ui.Remote.DeviceName) && exists(Ui.ChatList.Search) }
    alice.chatList.open("bob")
    poll(what = "the message sent from the desktop as alice's sent item in the chat with bob") {
      seesText("hi from desktop", substring = true) && chatModel.chatsContext.chatItems.value.any { it.content.text == "hi from desktop" && it.chatDir is CIDirection.DirectSnd }
    }
  }

  private fun enableDeveloperToolsViaApi() = appPrefs.developerTools.set(true)

  private companion object {
    const val DESKTOP = "My desktop"
  }
}
