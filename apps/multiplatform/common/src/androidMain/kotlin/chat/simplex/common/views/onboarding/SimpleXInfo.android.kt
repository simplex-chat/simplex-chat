package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.model.User
import chat.simplex.common.ui.Ui
import chat.simplex.common.ui.ui
import chat.simplex.res.*

@Composable
actual fun OnboardingActionButton(user: User?, onboardingStage: SharedPreference<OnboardingStage>, onclick: (() -> Unit)?) {
  if (user == null) {
    OnboardingActionButton(Modifier.fillMaxWidth().ui(Ui.Onboarding.GetStarted), labelId = MR.strings.get_started, onboarding = OnboardingStage.Step2_CreateProfile, onclick = onclick)
  } else {
    OnboardingActionButton(Modifier.fillMaxWidth(), labelId = MR.strings.make_private_connection, onboarding = OnboardingStage.OnboardingComplete, onclick = onclick)
  }
}
