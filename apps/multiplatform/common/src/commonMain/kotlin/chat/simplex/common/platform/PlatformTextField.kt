package chat.simplex.common.platform

import androidx.compose.runtime.*
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.TextStyle
import chat.simplex.common.views.chat.ComposeMessage
import chat.simplex.common.views.chat.ComposeState
import java.net.URI

@Composable
expect fun PlatformTextField(
  composeState: MutableState<ComposeState>,
  sendMsgEnabled: Boolean,
  sendMsgButtonDisabled: Boolean,
  textStyle: MutableState<TextStyle>,
  showDeleteTextButton: MutableState<Boolean>,
  userIsObserver: Boolean,
  placeholder: String,
  showVoiceButton: Boolean,
  onMessageChange: (ComposeMessage) -> Unit,
  onUpArrow: () -> Unit,
  onFilesPasted: (List<URI>) -> Unit,
  focusRequester: FocusRequester? = null,
  onDone: () -> Unit,
)
