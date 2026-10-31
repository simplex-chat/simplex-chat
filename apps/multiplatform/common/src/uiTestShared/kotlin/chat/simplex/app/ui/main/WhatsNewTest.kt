package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.WhatsNewScreen
import chat.simplex.app.ui.harness.enableOperator
import chat.simplex.app.ui.harness.whatsNewFromSettings
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.OperatorTag
import chat.simplex.common.platform.chatController
import chat.simplex.common.ui.Ui
import kotlinx.coroutines.runBlocking
import org.junit.Test

class WhatsNewTest : UiTest() {
  @Test fun whatsNewFromSettings() = uiTest {
    val alice = app("alice")
    val whatsNew = whatsNewFromSettings(alice)
    poll(what = "What's New from Help & support, without an OK button") { whatsNew.shown() && !exists(Ui.WhatsNew.Ok) }
    escape()
    poll(what = "Help & support again") { !whatsNew.shown() && exists(Ui.Help.WhatsNew) }
  }

  @Test fun whatsNewShownAfterVersionUpgrade() = uiTest {
    app("alice")
    appPrefs.whatsNewVersion.set(WhatsNewScreen.OLDER_VERSION)
    check(storedSetting("WhatsNewVersion") == WhatsNewScreen.OLDER_VERSION) { "stored What's New version: ${storedSetting("WhatsNewVersion")}" }
    restart()
    val whatsNew = WhatsNewScreen(this)
    poll(what = "What's New to open by itself") { whatsNew.shown() && exists(Ui.WhatsNew.Ok) }
    poll(what = "the stored What's New version to be the current one") { storedSetting("WhatsNewVersion") == WhatsNewScreen.VERSION }
    whatsNew.ok()
    restart()
    waitFor(Ui.ChatList.NewChat)
    repeat(AUTO_OPEN_DELAY_SETTLES) { settle() }
    check(!whatsNew.shown() && !exists(Ui.WhatsNew.Ok)) { "What's New opened again after a restart" }
  }

  @Test fun whatsNewShowsViewUpdatedConditions() = uiTest {
    app("alice")
    enableOperator(OperatorTag.SimpleX)
    restart()
    val whatsNew = WhatsNewScreen(this)
    poll(what = "What's New with `View updated conditions`") {
      whatsNew.shown() && exists(Ui.WhatsNew.ViewConditions) && textOf(Ui.WhatsNew.ViewConditions) == "View updated conditions"
    }
    click(Ui.WhatsNew.ViewConditions)
    poll(what = "the `Updated conditions` screen") { seesText("Updated conditions") && seesText("Accept conditions") }
    poll(what = "the conditions to be marked as notified in the core") {
      runBlocking { chatController.getServerOperators(null) }?.currentConditions?.notifiedAt != null
    }
  }

  private companion object {
    const val AUTO_OPEN_DELAY_SETTLES = 5
  }
}
