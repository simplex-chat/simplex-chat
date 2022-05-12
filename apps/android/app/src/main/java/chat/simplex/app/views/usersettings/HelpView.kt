package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ChatHelpView

@Composable
fun HelpView(chatModel: ChatModel) {
  val user = chatModel.currentUser.value
  if (user != null) {
    HelpLayout(displayName = user.profile.displayName)
  }
}

@Composable
fun HelpLayout(displayName: String) {
  Column(
    Modifier
      .verticalScroll(rememberScrollState())
      .padding(bottom = 16.dp),
    horizontalAlignment = Alignment.Start
  ){
    Text(
      String.format(stringResource(R.string.personal_welcome), displayName),
      Modifier.padding(bottom = 24.dp),
      style = MaterialTheme.typography.h1,
    )
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
    HelpLayout(displayName = "Alice")
  }
}
