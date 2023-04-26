package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.R
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ChatHelpView
import chat.simplex.app.views.helpers.AppBarTitle

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
    AppBarTitle(String.format(stringResource(R.string.personal_welcome), userDisplayName), false)
    ChatHelpView()
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewHelpView() {
  SimpleXTheme {
    HelpLayout("Alice")
  }
}
