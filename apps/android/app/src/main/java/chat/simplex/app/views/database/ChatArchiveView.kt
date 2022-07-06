package chat.simplex.app.views.database

import android.content.res.Configuration
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun ChatArchiveView() {
  ChatArchiveLayout()
}

@Composable
fun ChatArchiveLayout() {
  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    @Composable fun divider() = Divider(Modifier.padding(horizontal = 8.dp))
    Text(
      "Chat archive",
      // stringResource(R.string.your_chat_database),
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
  }
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewChatHelpLayout() {
  SimpleXTheme {
    ChatArchiveLayout()
  }
}
