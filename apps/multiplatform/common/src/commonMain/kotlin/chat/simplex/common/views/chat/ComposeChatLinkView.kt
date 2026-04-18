package chat.simplex.common.views.chat

import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.MsgChatLink
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.ProfileImage
import dev.icerock.moko.resources.compose.painterResource
import chat.simplex.res.MR

@Composable
fun ComposeChatLinkView(
  chatLink: MsgChatLink,
  cancelEnabled: Boolean,
  cancelPreview: () -> Unit
) {
  Row(
    Modifier
      .fillMaxWidth()
      .heightIn(min = 54.dp)
      .padding(vertical = 1.dp)
      .padding(start = 12.dp, end = 12.dp),
    verticalAlignment = Alignment.CenterVertically,
    horizontalArrangement = Arrangement.spacedBy(8.dp)
  ) {
    ProfileImage(size = 44.dp, image = chatLink.image, icon = chatLink.iconRes)
    Column(
      Modifier.weight(1f).padding(vertical = 5.dp),
      verticalArrangement = Arrangement.spacedBy(2.dp)
    ) {
      Text(
        chatLink.displayName,
        style = MaterialTheme.typography.subtitle1,
        maxLines = 1,
      )
      chatLink.shortDescription?.let { descr ->
        Text(
          descr,
          style = MaterialTheme.typography.caption,
          color = MaterialTheme.colors.secondary,
          maxLines = 1,
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
