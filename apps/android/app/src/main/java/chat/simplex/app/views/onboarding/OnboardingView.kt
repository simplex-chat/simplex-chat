package chat.simplex.app.views.onboarding

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material.MaterialTheme
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.ChatModel
import chat.simplex.app.views.CreateProfilePanel
import chat.simplex.app.views.helpers.getKeyboardState
import com.google.accompanist.insets.ProvideWindowInsets
import kotlinx.coroutines.launch

enum class OnboardingStage {
  Step1_SimpleXInfo,
  Step2_CreateProfile,
  Step3_MakeConnection,
  OnboardingComplete
}

@Composable fun OnboardingView(chatModel: ChatModel, onboarding: OnboardingStage) {
  when (onboarding) {
    OnboardingStage.Step1_SimpleXInfo -> SimpleXInfo(chatModel, onboarding = true)
    OnboardingStage.Step2_CreateProfile -> CreateProfile(chatModel)
    OnboardingStage.Step3_MakeConnection -> MakeConnection(chatModel)
  }
}

@Composable
fun CreateProfile(chatModel: ChatModel) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }

  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Box(
      modifier = Modifier
        .fillMaxSize()
        .background(color = MaterialTheme.colors.background)
        .padding(20.dp)
    ) {
      CreateProfilePanel(chatModel)
      if (savedKeyboardState != keyboardState) {
        LaunchedEffect(keyboardState) {
          scope.launch {
            savedKeyboardState = keyboardState
            scrollState.animateScrollTo(scrollState.maxValue)
          }
        }
      }
    }
  }
}
