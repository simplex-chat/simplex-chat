package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.views.helpers.ProfileImage
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.res.MR

@Composable
fun ShareListNavLinkView(chat: Chat, chatModel: ChatModel) {
  val stopped = chatModel.chatRunning.value == false
  when (chat.chatInfo) {
    is ChatInfo.Direct ->
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat) },
        click = { directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel) },
        stopped
      )
    is ChatInfo.Group ->
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat) },
        click = { groupChatAction(chat.remoteHostId, chat.chatInfo.groupInfo, chatModel) },
        stopped
      )
    is ChatInfo.Local ->
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat) },
        click = { noteFolderChatAction(chat.remoteHostId, chat.chatInfo.noteFolder) },
        stopped
      )
    is ChatInfo.ContactRequest, is ChatInfo.ContactConnection, is ChatInfo.InvalidJSON -> {}
  }
}

@Composable
private fun ShareListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  stopped: Boolean
) {
  SectionItemView(minHeight = 50.dp, click = click, disabled = stopped) {
    chatLinkPreview()
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}

@Composable
private fun SharePreviewView(chat: Chat) {
  Row(
    Modifier.fillMaxSize(),
    horizontalArrangement = Arrangement.SpaceBetween,
    verticalAlignment = Alignment.CenterVertically
  ) {
    Row(
      verticalAlignment = Alignment.CenterVertically,
      horizontalArrangement = Arrangement.spacedBy(4.dp)
    ) {
      if (chat.chatInfo is ChatInfo.Local) {
        ProfileImage(size = 46.dp, null, icon = MR.images.ic_folder_filled, color = NoteFolderIconColor)
      } else {
        ProfileImage(size = 46.dp, chat.chatInfo.image)
      }
      Text(
        chat.chatInfo.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis,
        color = if (chat.chatInfo.incognito) Indigo else Color.Unspecified
      )
    }
  }
}
