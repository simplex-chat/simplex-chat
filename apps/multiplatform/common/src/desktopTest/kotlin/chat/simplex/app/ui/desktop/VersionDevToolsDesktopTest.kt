package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.PasscodeScreen
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.consoleShows
import chat.simplex.app.ui.harness.developerTools
import chat.simplex.app.ui.harness.yourPrivacy
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.TerminalItem
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Assert.assertEquals
import org.junit.Test

class VersionDevToolsDesktopTest : UiTest() {
  @Test fun console_sqlRunsOnlyWithLockAndDevTools() = uiTest {
    val alice = app("alice")
    yourPrivacy(alice).lock().apply { enable(CODE); close() }
    developerTools(alice)
    check(!appPrefs.developerTools.get()) { "developer options are on before the switch" }
    openConsoleWithPasscode()
    check(!consoleShows("> $SQL_CMD")) { "the /sql command is in the console before it was sent" }
    sendSql()
    poll(what = "the /sql command and the error response in the console") { consoleShows("> $SQL_CMD") && consoleShows("< error chat") }
    assertEquals("the response to $SQL_CMD with SimpleX Lock on and developer options off", SQL_BLOCKED, responseTo(SQL_CMD)?.details)

    escape()
    poll(what = "the chat console to close") { !exists(Ui.Chat.Message) && exists(Ui.DevTools.Console) }
    click(Ui.DevTools.ShowOptions)
    poll(what = "developer options turned on") { appPrefs.developerTools.get() }
    openConsoleWithPasscode()
    check(!consoleShows(SQL_RESULT)) { "the core's SQL result is in the console before /sql ran" }
    sendSql()
    poll(what = "the core's SQL result after the second /sql command, shown in the console") {
      responseTo(SQL_CMD)?.label == SQL_RESULT && consoleShows(SQL_RESULT)
    }
  }

  private fun UiScope.openConsoleWithPasscode() {
    val passcode = PasscodeScreen(this)
    click(Ui.DevTools.Console)
    passcode.await(OPEN_CONSOLE)
    passcode.enter(CODE)
    passcode.submit()
    poll(what = "the chat console after the passcode") { passcode.gone() && !exists(Ui.Lock.Unlock) && exists(Ui.Chat.Message) }
  }

  private fun UiScope.sendSql() {
    typeInto(Ui.Chat.Message, SQL_CMD)
    click(Ui.Chat.Send)
  }

  private fun responseTo(cmd: String): TerminalItem? {
    val items = chatModel.terminalItems.value
    val i = items.indexOfLast { it.label == "> $cmd" }
    return if (i < 0) null else items.getOrNull(i + 1)
  }

  private companion object {
    const val CODE = "1234"
    const val OPEN_CONSOLE = "Open chat console"
    const val SQL_CMD = "/sql chat select 1"
    const val SQL_BLOCKED = "error chat commandError Failed reading: empty"
    const val SQL_RESULT = "< * sQLResult"
  }
}
