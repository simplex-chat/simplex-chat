package chat.simplex.app.ui.desktop

import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.UiScope
import chat.simplex.app.ui.harness.WhatsNewScreen
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.onboarding.OnboardingStage
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
    poll(what = "Create profile to be enabled") { isEnabled(Ui.Onboarding.CreateProfile) }
    click(Ui.Onboarding.CreateProfile)
    onboarding().useRandomPassphrase()
    click(Ui.Onboarding.Continue)
    click(Ui.Onboarding.Accept)
    poll(timeoutMs = 30_000, what = "the chat list after onboarding") {
      appPrefs.onboardingStage.get() == OnboardingStage.OnboardingComplete && exists(Ui.ChatList.UserPicker)
    }
  }

  private companion object {
    const val NO_MODAL_SETTLES = 5
  }
}
