package chat.simplex.app.views.usersettings

import android.content.res.Configuration
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chat.ChatHelpView
import chat.simplex.app.views.helpers.CloseSheetBar

@Composable
fun HelpView(chatModel: ChatModel) {
  val user = chatModel.currentUser.value
  if (user != null) {
    HelpLayout(displayName = user.profile.displayName)
  }
}

@Composable
fun HelpLayout(displayName: String) {
  Column(horizontalAlignment = Alignment.Start) {
    Text(
      "Welcome $displayName!",
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
