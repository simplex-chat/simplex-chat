package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.runtime.*
import chat.simplex.common.model.AgentErrorType
import chat.simplex.common.model.ChatController
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chatlist.ChatHelpView
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.common.views.helpers.withBGApi
import chat.simplex.res.MR
import kotlinx.coroutines.delay

@Composable
fun HelpView(userDisplayName: String) {
  HelpLayout(userDisplayName)
}

@Composable
fun HelpLayout(userDisplayName: String) {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
      .padding(horizontal = DEFAULT_PADDING),
  ){
    AppBarTitle(String.format(stringResource(MR.strings.personal_welcome), userDisplayName), withPadding = false)
    ChatHelpView()
    LaunchedEffect(Unit) {
      ChatController.chatModel.processedCriticalError.newError(AgentErrorType.CRITICAL(false, "OOPS"), false)
      withBGApi {
        delay(5000)
        ChatController.chatModel.processedCriticalError.newError(AgentErrorType.CRITICAL(false, "OOPS2"), false)
        delay(20000)
        ChatController.chatModel.processedCriticalError.newError(AgentErrorType.CRITICAL(false, "OOPS3"), true)
      }
    }
  }
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewHelpView() {
  SimpleXTheme {
    HelpLayout("Alice")
  }
}
