package chat.simplex.app.views.helpers

import android.graphics.BitmapFactory
import android.util.Base64
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.AccountCircle
import androidx.compose.material.icons.filled.SupervisedUserCircle
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.Dp
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.Chat
import chat.simplex.app.model.ChatInfo
import chat.simplex.app.ui.theme.SimpleXTheme

@Composable
fun ChatInfoImage(chat: Chat, size: Dp) {
  val icon =
    if (chat.chatInfo is ChatInfo.Group) Icons.Filled.SupervisedUserCircle
                                    else Icons.Filled.AccountCircle
  Box(Modifier.size(size)) {
    if (chat.chatInfo.displayImage == null) {
      Icon(
        icon,
        contentDescription = "Avatar Placeholder",
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.fillMaxSize()
      )
    }
    else {
      val imageBytes = Base64.decode(chat.chatInfo.displayImage, Base64.DEFAULT)
      val bitmap = BitmapFactory.decodeByteArray(imageBytes, 0, imageBytes.size).asImageBitmap()
      Image(
        bitmap,
        "display image for profile",
        modifier = Modifier.fillMaxSize()
      )
    }
  }
}

@Preview
@Composable
fun PreviewChatInfoImage() {
  SimpleXTheme {
    ChatInfoImage(
      chat = Chat(chatInfo = ChatInfo.Direct.sampleData, chatItems = arrayListOf()),
      size = 55.dp
    )
  }
}
