package chat.simplex.common.views.usersettings

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.desktop.ui.tooling.preview.Preview
import chat.simplex.common.platform.ColumnWithScrollBar
import chat.simplex.common.ui.theme.DEFAULT_PADDING
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chatlist.ChatHelpView
import chat.simplex.common.views.helpers.AppBarTitle
import chat.simplex.res.MR

@Composable
fun HelpView(userDisplayName: String) {
  HelpLayout(userDisplayName)
}

@Composable
fun HelpLayout(userDisplayName: String) {
  ColumnWithScrollBar(
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING),
  ){
    AppBarTitle(String.format(stringResource(MR.strings.personal_welcome), userDisplayName), withPadding = false)
    ChatHelpView()
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
