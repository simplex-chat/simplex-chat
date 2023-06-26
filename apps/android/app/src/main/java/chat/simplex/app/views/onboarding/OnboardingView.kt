package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.ChatModel
import chat.simplex.common.platform.getKeyboardState
import chat.simplex.common.views.CreateProfilePanel
import kotlinx.coroutines.launch

enum class OnboardingStage {
  Step1_SimpleXInfo,
  Step2_CreateProfile,
  Step3_CreateSimpleXAddress,
  Step4_SetNotificationsMode,
  OnboardingComplete
}

@Composable
fun CreateProfile(chatModel: ChatModel, close: () -> Unit) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }

  Box(
    modifier = Modifier
      .fillMaxSize()
      .padding(top = 20.dp)
  ) {
    CreateProfilePanel(chatModel, close)
    LaunchedEffect(Unit) {
      setLastVersionDefault(chatModel)
    }
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
