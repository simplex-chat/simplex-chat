package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.chatConsole
import chat.simplex.app.ui.harness.closeDeveloperTools
import chat.simplex.app.ui.harness.consoleShows
import chat.simplex.app.ui.harness.developerTools
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.appPlatform
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Test

private const val DEVELOPER_TOOLS = "DeveloperTools"
private const val SHOW_FOOTER = "Show: Database IDs and Transport isolation option."
private const val HIDE_FOOTER = "Hide: Database IDs and Transport isolation option."
private const val CONTACTS_CMD = "/contacts"
private const val SQL_CMD = "/sql chat select 1"
private const val SQL_BLOCKED = "error chat commandError Failed reading: empty"
private const val ONE_HAND_CARD_SHOWN = "OneHandUICardShown"
private const val TALK_TO_SOMEONE = "Talk to someone"

class VersionDevToolsTest : UiTest() {
  @Test fun version_screenShowsVersions() = uiTest {
    val alice = app("alice")
    val (name, code) =
      if (appPlatform.isDesktop) BuildConfigCommon.DESKTOP_VERSION_NAME to BuildConfigCommon.DESKTOP_VERSION_CODE
      else BuildConfigCommon.ANDROID_VERSION_NAME to BuildConfigCommon.ANDROID_VERSION_CODE
    val core = runBlocking { chatController.apiGetVersion() } ?: error("the core did not return its version")
    alice.settings()
    assertEquals("$name ($code)", textOf(Ui.Settings.Version))
    check(!seesText("App build: $code")) { "the build is shown before the version screen opened" }
    click(Ui.Settings.Version)
    poll(what = "the App version screen with the app build and the core and simplexmq versions") {
      seesText("App version") && seesText("App version: v$name") && seesText("App build: $code") &&
        seesText("Core version: v${core.version}") && seesText("simplexmq: v${core.simplexmqVersion} (${core.simplexmqCommit.take(7).padStart(2)})")
    }
  }

  @Test fun dev_showDeveloperOptionsSection() = uiTest {
    val alice = app("alice")
    developerTools(alice)
    check(storedSetting(DEVELOPER_TOOLS) != "true" && seesText(HIDE_FOOTER) && !seesText(SHOW_FOOTER) && !seesText("Enable logs")) {
      "developer options before the switch: stored ${storedSetting(DEVELOPER_TOOLS)}, texts ${texts().take(40)}"
    }
    click(Ui.DevTools.ShowOptions)
    poll(what = "the Developer options section with the Show footer, stored as on") {
      storedSetting(DEVELOPER_TOOLS) == "true" && seesText(SHOW_FOOTER) && !seesText(HIDE_FOOTER) && seesText("Enable logs") && seesText("Show internal errors")
    }
    click(Ui.DevTools.ShowOptions)
    poll(what = "the Developer options section hidden with the Hide footer, stored as off") {
      storedSetting(DEVELOPER_TOOLS) == "false" && seesText(HIDE_FOOTER) && !seesText(SHOW_FOOTER) && !seesText("Enable logs") && !seesText("Show internal errors")
    }
  }

  @Test fun console_sendCommand() = uiTest {
    val alice = app("alice")
    chatConsole(alice)
    check(!consoleShows("> $CONTACTS_CMD") && !consoleShows("> $SQL_CMD")) { "the commands are in the console before they were sent" }
    typeInto(Ui.Chat.Message, CONTACTS_CMD)
    click(Ui.Chat.Send)
    poll(what = "the command and the core's contactsList response in the console, and the field cleared") {
      consoleShows("> $CONTACTS_CMD") && consoleShows("< * contactsList") && editableText(Ui.Chat.Message).isNullOrEmpty()
    }
    typeInto(Ui.Chat.Message, SQL_CMD)
    click(Ui.Chat.Send)
    poll(what = "the /sql command and the error response in the console") { consoleShows("> $SQL_CMD") && consoleShows("< error chat") }
    val items = chatModel.terminalItems.value
    val cmd = items.indexOfLast { it.label == "> $SQL_CMD" }
    assertEquals("the response to $SQL_CMD without SimpleX Lock and developer tools", SQL_BLOCKED, items.getOrNull(cmd + 1)?.details)
  }

  @Test fun dev_resetAllHints() = uiTest {
    val alice = app("alice")
    waitFor(Ui.ChatList.Search)
    check(appPrefs.oneHandUICardShown.get() && !appPrefs.hintPreferences.all { it.isUnchanged() } && !exists(Ui.ChatList.OneHandCard) && !seesText(TALK_TO_SOMEONE)) {
      "the hints are already reset before Reset all hints: card shown ${exists(Ui.ChatList.OneHandCard)}, onboarding shown ${seesText(TALK_TO_SOMEONE)}"
    }
    developerTools(alice)
    click(Ui.DevTools.ResetHints)
    poll(what = "every hint preference back to its default, the one-hand card hint stored as not shown") {
      appPrefs.hintPreferences.all { it.isUnchanged() } && storedSetting(ONE_HAND_CARD_SHOWN) == "false"
    }
    closeDeveloperTools("the connect onboarding in place of the chat list") { seesText(TALK_TO_SOMEONE) && !exists(Ui.ChatList.Search) }
  }
}
