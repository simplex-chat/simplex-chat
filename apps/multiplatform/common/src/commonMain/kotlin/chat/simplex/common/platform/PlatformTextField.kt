package chat.simplex.common.platform

import androidx.compose.runtime.Composable
import androidx.compose.runtime.MutableState
import androidx.compose.ui.text.TextStyle
import chat.simplex.common.views.chat.ComposeState

@Composable
expect fun PlatformTextField(
  composeState: MutableState<ComposeState>,
  sendMsgEnabled: Boolean,
  textStyle: MutableState<TextStyle>,
  showDeleteTextButton: MutableState<Boolean>,
  userIsObserver: Boolean,
  onMessageChange: (String) -> Unit,
  onUpArrow: () -> Unit,
  onDone: () -> Unit,
)
