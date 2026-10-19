package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.WhatsNewScreen
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.common.ui.Ui
import org.junit.Test

class WhatsNewDesktopTest : UiTest() {
  @Test fun whatsNewNotShownForNewUser() = uiTest {
    app(null)
    check(storedSetting("WhatsNewVersion") == null) { "a What's New version is stored before onboarding" }
    click(Ui.Onboarding.GetStarted)
    typeInto(Ui.Onboarding.ProfileName, "alice")
    poll(what = "the stored What's New version to be the current one on the profile screen") { storedSetting("WhatsNewVersion") == WhatsNewScreen.VERSION }
    finishOnboarding()
    repeat(NO_MODAL_SETTLES) { settle() }
    check(!WhatsNewScreen(this).shown() && !exists(Ui.WhatsNew.Ok)) { "What's New opened for a new user" }
  }

  private fun UiScope.finishOnboarding() {
    val o = onboarding()
    click(Ui.Onboarding.CreateProfile)
    o.useRandomPassphrase()
    click(Ui.Onboarding.Continue)
    click(Ui.Onboarding.Accept)
    o.waitForChatList()
  }

  private companion object {
    const val NO_MODAL_SETTLES = 5
  }
}
