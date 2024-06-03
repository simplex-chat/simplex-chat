package chat.simplex.common.views.contacts

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_SPACE_AFTER_ICON
import chat.simplex.res.MR

@Composable
fun ContactPreviewView(
  chat: Chat,
  disabled: Boolean,
) {
  val cInfo = chat.chatInfo

  @Composable
  fun inactiveIcon() {
    Icon(
      painterResource(MR.images.ic_cancel_filled),
      stringResource(MR.strings.icon_descr_group_inactive),
      Modifier.size(18.dp).background(MaterialTheme.colors.background, CircleShape),
      tint = MaterialTheme.colors.secondary
    )
  }

  @Composable
  fun chatPreviewImageOverlayIcon() {
    when (cInfo) {
      is ChatInfo.Direct ->
        if (!cInfo.contact.active) {
          inactiveIcon()
        }

      else -> {}
    }
  }

  @Composable
  fun VerifiedIcon() {
    Icon(painterResource(MR.images.ic_verified_user), null, Modifier.size(19.dp).padding(end = 3.dp, top = 1.dp), tint = MaterialTheme.colors.secondary)
  }

  @Composable
  fun chatPreviewTitle() {
    val deleting by remember(disabled, chat.id) { mutableStateOf(chatModel.deletedChats.value.contains(chat.remoteHostId to chat.chatInfo.id)) }
    when (cInfo) {
      is ChatInfo.Direct ->
        Row(verticalAlignment = Alignment.CenterVertically) {
          if (cInfo.contact.verified) {
            VerifiedIcon()
          }
          Text(
            cInfo.chatViewName,
            maxLines = 1,
            overflow = TextOverflow.Ellipsis,
            color = if (deleting) MaterialTheme.colors.secondary else Color.Unspecified
          )
        }

      else -> {}
    }
  }

  Row(
    verticalAlignment = Alignment.CenterVertically
  ) {
    Box(contentAlignment = Alignment.BottomEnd) {
      ChatInfoImage(cInfo, size = 42.dp)
      Box(Modifier.padding(end = 2.dp, bottom = 2.dp)) {
        chatPreviewImageOverlayIcon()
      }
    }

    Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))

    Box(modifier = Modifier.weight(10f, fill = true)) {
      chatPreviewTitle()
    }

    Spacer(Modifier.fillMaxWidth().weight(1f))

    if (chat.chatInfo.chatSettings?.favorite == true) {
      Icon(
        painterResource(MR.images.ic_star_filled),
        contentDescription = generalGetString(MR.strings.favorite_chat),
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier
          .size(17.dp)
      )
      if (chat.chatInfo.incognito) {
        Spacer(Modifier.width(DEFAULT_SPACE_AFTER_ICON))
      }
    }

    if (chat.chatInfo.incognito) {
      Icon(
        painterResource(MR.images.ic_theater_comedy),
        contentDescription = null,
        tint = MaterialTheme.colors.secondary,
        modifier = Modifier
          .size(21.dp)
      )
    }
  }
}
