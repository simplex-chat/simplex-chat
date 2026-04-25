package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.widthIn
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.model.User
import chat.simplex.res.MR

@Composable
actual fun OnboardingActionButton(user: User?, onboardingStage: SharedPreference<OnboardingStage>, onclick: (() -> Unit)?) {
  if (user == null) {
    OnboardingActionButton(Modifier.widthIn(min = 300.dp), labelId = MR.strings.get_started, onboarding = OnboardingStage.Step2_CreateProfile, onclick = onclick)
  } else {
    OnboardingActionButton(Modifier.widthIn(min = 300.dp), labelId = MR.strings.make_private_connection, onboarding = OnboardingStage.OnboardingComplete, onclick = onclick)
  }
}
