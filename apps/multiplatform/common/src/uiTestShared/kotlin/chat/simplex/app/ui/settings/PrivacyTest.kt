package chat.simplex.app.ui.settings

import chat.simplex.app.ui.harness.App
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.connect
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import org.junit.Test

private const val SHOW_PREVIEWS = "PrivacyShowChatPreviews"
private const val MESSAGE = "hello from bob"

class PrivacyTest : UiTest() {
  @Test fun morePrivacy_showLastMessages() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    connect(bob)
    bob sends "@alice $MESSAGE"
    poll(what = "bob's message on the row") { MESSAGE in alice.chatList.rowTexts("bob") }
    check(storedSetting(SHOW_PREVIEWS) != "false") { "`Show last messages` is stored as off before it is turned off" }
    toggleShowLastMessages(alice)
    poll(what = "`Show last messages` stored as off") { storedSetting(SHOW_PREVIEWS) == "false" && !chatModel.showChatPreviews.value }
    closeMorePrivacy()
    poll(what = "bob's row without the message text") { alice.chatList.rowTexts("bob").let { "bob" in it && MESSAGE !in it } }
    val after = restart()
    check(storedSetting(SHOW_PREVIEWS) == "false") { "`Show last messages` is not stored as off after restart" }
    poll(what = "bob's row still without the message text after restart") { after.chatList.rowTexts("bob").let { "bob" in it && MESSAGE !in it } }
    toggleShowLastMessages(after)
    poll(what = "`Show last messages` stored as on") { storedSetting(SHOW_PREVIEWS) == "true" && chatModel.showChatPreviews.value }
    closeMorePrivacy()
    poll(what = "bob's message on the row again") { MESSAGE in after.chatList.rowTexts("bob") }
  }

  private fun UiScope.toggleShowLastMessages(app: App) {
    app.settings()
    click(Ui.Settings.Privacy)
    click(Ui.Privacy.More)
    click(Ui.Privacy.ShowLastMessages)
  }

  private fun UiScope.closeMorePrivacy() {
    escape()
    poll(what = "More privacy to close") { !exists(Ui.Privacy.ShowLastMessages) && exists(Ui.Privacy.More) }
    escape()
    poll(what = "Your privacy to close") { !exists(Ui.Privacy.More) && exists(Ui.Settings.Privacy) }
    escape()
    poll(what = "Your settings to close") { !exists(Ui.Settings.Privacy) && exists(Ui.ChatList.Search) }
  }
}
