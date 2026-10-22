package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.chatData
import chat.simplex.app.ui.harness.chatRunningInCore
import chat.simplex.app.ui.harness.connect
import chat.simplex.app.ui.harness.described
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DBMigrationResult
import org.junit.Test

class ChatDataDesktopTest : UiTest() {
  @Test fun dbMgmt_stoppedStatePersistsRestart() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    chatData(alice).management()
    click(Ui.Database.RunChat)
    poll(what = "the Stop chat? alert with Stop") { seesText("Stop chat?") && "Stop" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the chat stopped on screen, on disk and in the core") { seesText("Chat is stopped") && storedSetting("ChatStopped") == "true" && !chatRunningInCore() }
    bob sends "@alice $WHILE_STOPPED"
    restart(ready = { startChatAsked() }, what = "the Start chat? alert")
    poll(what = "the Start chat? alert") { seesText("Start chat?") && seesText(START_CHAT_TEXT) }
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the start to finish with the chat stopped") {
      !AlertManager.shared.hasAlertsShown() && !chatModel.ctrlInitInProgress.value && chatModel.chatRunning.value == false
    }
    check(!chatRunningInCore() && storedSetting("ChatStopped") == "true") { "Cancel started the chat: ChatStopped=${storedSetting("ChatStopped")}" }
    click(described("Settings"))
    poll(what = "Chat data marked as stopped in settings") { "Chat is stopped" in descriptionsOf(Ui.Settings.Database) }
    val current = restart(ready = { startChatAsked() }, what = "the Start chat? alert after the second restart")
    poll(what = "the Start chat? alert after the second restart") { seesText("Start chat?") && "OK" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared)) }
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(timeoutMs = CONNECT_TIMEOUT_MS, what = "the chat running with bob's message sent while it was stopped") {
      chatRunningInCore() && WHILE_STOPPED in current.chatList.rowTexts("bob")
    }
    check(storedSetting("ChatStopped") == "false") { "ChatStopped is still stored after the start" }
    current.chatList.open("bob").send(STARTED_AGAIN)
    bob.expectsMessage("alice> $STARTED_AGAIN", CONNECT_TIMEOUT_MS)
  }

  private fun startChatAsked(): Boolean = chatModel.chatDbStatus.value == DBMigrationResult.OK && AlertManager.shared.hasAlertsShown()

  private companion object {
    const val WHILE_STOPPED = "sent before the restart"
    const val STARTED_AGAIN = "started again"
    const val START_CHAT_TEXT = "Chat is stopped. If you already used this database on another device, you should transfer it back before starting chat."
  }
}
