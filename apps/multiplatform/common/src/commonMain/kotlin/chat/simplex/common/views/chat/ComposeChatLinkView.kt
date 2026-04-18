package chat.simplex.common.views.chat

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.MsgChatLink
import chat.simplex.common.ui.theme.appColors
import chat.simplex.common.views.helpers.ProfileImage
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.res.MR

@Composable
fun ComposeChatLinkView(
  chatLink: MsgChatLink,
  cancelEnabled: Boolean,
  cancelPreview: () -> Unit
) {
  val sentColor = MaterialTheme.appColors.sentMessage
  Row(
    Modifier
      .fillMaxWidth()
      .padding(top = 8.dp)
      .background(sentColor),
    verticalAlignment = Alignment.CenterVertically
  ) {
    ProfileImage(size = 60.dp, image = chatLink.image, icon = chatLink.iconRes)
    Column(
      Modifier.fillMaxWidth().weight(1f).padding(horizontal = 8.dp)
    ) {
      Text(chatLink.displayName, maxLines = 1, overflow = TextOverflow.Ellipsis)
      chatLink.shortDescription?.let { descr ->
        Text(
          descr,
          maxLines = 1,
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.secondary,
        )
      }
    }
    if (cancelEnabled) {
      IconButton(onClick = cancelPreview) {
        Icon(painterResource(MR.images.ic_close), null, tint = MaterialTheme.colors.primary)
      }
    }
  }
}
