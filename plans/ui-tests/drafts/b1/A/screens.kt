package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.Ui
import chat.simplex.common.views.onboarding.OnboardingStage

class OnboardingScreen(private val s: UiScope) {
  val stage: OnboardingStage get() = appPrefs.onboardingStage.get()

  fun createProfile(name: String) {
    s.click(Ui.Onboarding.GetStarted)
    s.typeInto(Ui.Onboarding.ProfileName, name)
    s.click(Ui.Onboarding.CreateProfile)
    s.poll(timeoutMs = 30_000, what = "profile `$name` to be created and the profile step to close") {
      chatModel.currentUser.value?.displayName == name && stage != OnboardingStage.Step2_CreateProfile
    }
  }

  fun useRandomPassphrase() {
    s.click(Ui.Onboarding.RandomPassphrase)
    s.click(Ui.Alert.Confirm(Ui.AlertSource.Shared))
    s.poll(what = "the passphrase step to close") { stage != OnboardingStage.Step2_5_SetupDatabasePassphrase }
  }

  fun toYourNetwork(name: String) {
    createProfile(name)
    if (stage == OnboardingStage.Step2_5_SetupDatabasePassphrase) useRandomPassphrase()
    s.poll(timeoutMs = 30_000, what = "the Your network step") { stage == OnboardingStage.Step3_ChooseServerOperators && s.exists(Ui.Onboarding.Continue) }
  }

  fun openOperators() {
    s.click(Ui.Onboarding.SetupRouters)
    s.waitFor(Ui.Onboarding.OperatorsOk)
  }

  fun enabledOperatorIds(): Set<Long> =
    chatModel.conditions.value.serverOperators.filter { it.enabled }.map { it.operatorId }.toSet()

  fun toConditions(name: String) {
    toYourNetwork(name)
    s.click(Ui.Onboarding.Continue)
    s.poll(what = "the Network commitments step") { stage == OnboardingStage.Step4_NetworkCommitments && s.exists(Ui.Onboarding.Accept) }
  }

  fun waitForChatList() {
    s.poll(timeoutMs = 30_000, what = "onboarding to complete and the chat list to show") {
      stage == OnboardingStage.OnboardingComplete && s.exists(Ui.ChatList.NewChat)
    }
  }

  fun complete(name: String) {
    toConditions(name)
    s.click(Ui.Onboarding.Accept)
    waitForChatList()
  }
}

fun UiScope.onboarding() = OnboardingScreen(this)
