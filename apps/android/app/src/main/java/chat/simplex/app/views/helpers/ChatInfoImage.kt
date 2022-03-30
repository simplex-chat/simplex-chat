package chat.simplex.app.views.helpers

import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.Icon
import androidx.compose.material.MaterialTheme
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.AccountCircle
import androidx.compose.material.icons.filled.SupervisedUserCircle
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.asImageBitmap
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.layout.ContentScale
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
  ProfileImage(size, chat.chatInfo.image, icon)
}

@Composable
fun ProfileImage(
  size: Dp,
  image: String? = null,
  icon: ImageVector = Icons.Filled.AccountCircle
) {
  Box(Modifier.size(size)) {
    if (image == null) {
      Icon(
        icon,
        contentDescription = "profile image placeholder",
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier.fillMaxSize()
      )
    } else {
      val imageBitmap = base64ToBitmap(image).asImageBitmap()
      Image(
        imageBitmap,
        "profile image",
        contentScale = ContentScale.Crop,
        modifier = Modifier.size(size).padding(size / 12).clip(CircleShape)
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
