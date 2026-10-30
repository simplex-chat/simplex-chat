package chat.simplex.app.ui.android

import chat.simplex.app.ui.harness.KnownAppIssue
import chat.simplex.app.ui.harness.UiTest
import chat.simplex.app.ui.harness.goBack
import chat.simplex.app.ui.harness.onboarding
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.common.views.onboarding.OnboardingStage
import org.junit.Test

class OnboardingProfileAndroidTest : UiTest() {
  @KnownAppIssue("15.5 item 4", "CameraUnavailableException: Device reporting less cameras than anticipated", intermittent = true)
  @Test fun androidOnboardingCreateProfileMobileLayout() = uiTest {
    app(null)
    waitFor(Ui.Onboarding.GetStarted)
    check(!keyboardShown()) { "the soft keyboard is open on the welcome screen" }
    click(Ui.Onboarding.GetStarted)
    poll(what = "Your profile with the name field focused and the soft keyboard open") {
      onboarding().stage == OnboardingStage.Step2_CreateProfile && exists(Ui.Onboarding.ProfileName) && keyboardShown()
    }
    check(seesText("Your profile") && seesText("On your phone, not on servers.") && seesText(NO_ACCOUNT, substring = true) && seesText("Enter profile name…")) {
      "the mobile profile screen: ${texts().take(30)}"
    }
    check(textsOf(Ui.Onboarding.Migrate).toSet() == setOf("Migrate")) { "migrate button: ${textsOf(Ui.Onboarding.Migrate)}" }
    check("Create profile" in textsOf(Ui.Onboarding.CreateProfile) && !isEnabled(Ui.Onboarding.CreateProfile)) { "Create profile is enabled with an empty name" }
    typeInto(Ui.Onboarding.ProfileName, "alice")
    poll(what = "Create profile enabled for `alice`") { isEnabled(Ui.Onboarding.CreateProfile) }

    click(Ui.Onboarding.Migrate)
    poll(what = "Migrate here leading with the camera section, then the archive link and file") {
      chatModel.migrationState.value is MigrationToState.PasteOrScanLink && seesText("Scan QR code") && seesText("Or paste archive link") && seesText("Or import archive file")
    }
    check(!seesText("Paste archive link") && chatModel.currentUser.value == null) { "the desktop paste label is shown, or a profile was created: ${texts().take(30)}" }

    goBack()
    poll(what = "Migrate here closed over Your profile") { !seesText("Scan QR code") && !seesText("Or import archive file") && exists(Ui.Onboarding.CreateProfile) }
  }

  private companion object {
    const val NO_ACCOUNT = "No account. No phone. No email. No ID."
  }
}
