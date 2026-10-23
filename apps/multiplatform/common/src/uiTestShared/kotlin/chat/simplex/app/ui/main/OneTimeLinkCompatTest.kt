package chat.simplex.app.ui.main

import chat.simplex.app.ui.harness.CONNECT_TIMEOUT_MS
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.Ui
import org.junit.Test

class OneTimeLinkCompatTest : UiTest() {
  @Test fun oneTimeLinkCliConnectsViaShortLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val link = alice.newChat().createOneTimeLink()
    bob sends "/c $link"; bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.row("bob")
  }

  @Test fun oneTimeLinkCliConnectsViaFullLink() = uiTest {
    val alice = app("alice"); val bob = cli("bob")
    val short = alice.newChat().createOneTimeLink()
    poll(what = "the 1-time link without the `Full link` toggle while developer tools are off") {
      seesText("OR SHOW THIS CODE") && !exists(Ui.Link.ShortLinkToggle)
    }
    check(FULL_INVITATION !in short) { "the 1-time link shown by default is the full link `$short`" }
    enableDeveloperToolsViaApi()
    poll(what = "the `Full link` toggle with developer tools on") { exists(Ui.Link.ShortLinkToggle) && "Full link" in textsOf(Ui.Link.ShortLinkToggle) }
    click(Ui.Link.ShortLinkToggle)
    var full: String? = null
    poll(what = "the full link in place of the short one, and the toggle reading `Short link`") {
      full = editableText(Ui.Link.Text)?.takeIf { FULL_INVITATION in it }
      full != null && "Short link" in textsOf(Ui.Link.ShortLinkToggle)
    }
    bob sends "/c $full"; bob expects "confirmation sent!"
    bob.eventually("alice: contact is connected", CONNECT_TIMEOUT_MS)
    alice.chatList.row("bob")
  }

  private fun enableDeveloperToolsViaApi() = appPrefs.developerTools.set(true)

  private companion object {
    const val FULL_INVITATION = "/invitation#/?v="
  }
}
