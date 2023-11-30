package chat.simplex.common.views.onboarding

import androidx.compose.runtime.Composable
import chat.simplex.common.model.SharedPreference
import chat.simplex.common.model.User
import chat.simplex.res.MR

@Composable
actual fun OnboardingActionButton(user: User?, onboardingStage: SharedPreference<OnboardingStage>, onclick: (() -> Unit)?) {
  if (user == null) {
    OnboardingActionButton(MR.strings.create_your_profile, onboarding = OnboardingStage.Step2_CreateProfile, true, onclick = onclick)
  } else {
    OnboardingActionButton(MR.strings.make_private_connection, onboarding = OnboardingStage.OnboardingComplete, true, onclick = onclick)
  }
}
