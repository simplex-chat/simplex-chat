package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Row
import androidx.compose.runtime.Composable
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.model.User
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource

@Composable
actual fun OnboardingActionButton(user: User?, onboardingStage: SharedPreference<OnboardingStage>, onclick: (() -> Unit)?) {
  if (user == null) {
    Row(horizontalArrangement = Arrangement.spacedBy(DEFAULT_PADDING * 2.5f)) {
      OnboardingActionButton(MR.strings.link_a_mobile, onboarding = if (controller.appPrefs.initialRandomDBPassphrase.get() && !chatModel.desktopOnboardingRandomPassword.value) OnboardingStage.Step2_5_SetupDatabasePassphrase else OnboardingStage.LinkAMobile, true, icon = painterResource(MR.images.ic_smartphone_300), onclick = onclick)
      OnboardingActionButton(MR.strings.create_your_profile, onboarding = OnboardingStage.Step2_CreateProfile, true, icon = painterResource(MR.images.ic_desktop), onclick = onclick)
    }
  } else {
    OnboardingActionButton(MR.strings.make_private_connection, onboarding = OnboardingStage.OnboardingComplete, true, onclick = onclick)
  }
}
