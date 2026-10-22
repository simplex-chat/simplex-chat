package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.helpAndSupport
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.helpers.AlertManager
import org.junit.Test

private const val MAILTO = "mailto:chat@simplex.chat"
private const val CONTRIBUTE = "https://github.com/simplex-chat/simplex-chat#contribute"
private const val STAR = "https://github.com/simplex-chat/simplex-chat"

class HelpDesktopTest : UiTest() {
  @Test fun help_contributeAndStar() = uiTest {
    val alice = app("alice")
    helpAndSupport(alice)
    check(opened().isEmpty()) { "links opened before any tap: ${opened()}" }
    click(Ui.Help.Email)
    poll(what = "the mailto link opened without a confirmation") { opened() == listOf(MAILTO) && !AlertManager.shared.hasAlertsShown() }
    click(Ui.Help.Contribute)
    awaitExternalLinkAlert(CONTRIBUTE)
    click(Ui.Alert.Dismiss(Ui.AlertSource.Shared))
    poll(what = "the alert closed by Cancel") { !AlertManager.shared.hasAlertsShown() }
    check(opened() == listOf(MAILTO)) { "Cancel opened the link: ${opened()}" }
    click(Ui.Help.Contribute)
    awaitExternalLinkAlert(CONTRIBUTE)
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the Contribute link opened after Open") { opened() == listOf(MAILTO, CONTRIBUTE) && !AlertManager.shared.hasAlertsShown() }
    click(Ui.Help.Star)
    awaitExternalLinkAlert(STAR)
    click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    poll(what = "the Star on GitHub link opened after Open") { opened() == listOf(MAILTO, CONTRIBUTE, STAR) }
  }

  private fun UiScope.opened(): List<String> = synchronized(uriHandler.opened) { uriHandler.opened.toList() }

  private fun UiScope.awaitExternalLinkAlert(url: String) =
    poll(what = "the Open external link? alert for $url with Open") {
      seesText("Open external link?") && seesText(url) && "Open" in textsOf(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    }
}
