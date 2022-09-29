package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import chat.simplex.app.ui.theme.DEFAULT_PADDING
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ChatHelpView

@Composable
fun HelpView() {
  HelpLayout()
}

@Composable
fun HelpLayout() {
  Column(
    Modifier
      .fillMaxWidth()
      .verticalScroll(rememberScrollState())
      .padding(horizontal = DEFAULT_PADDING),
    horizontalAlignment = Alignment.Start
  ){
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
    HelpLayout()
  }
}
