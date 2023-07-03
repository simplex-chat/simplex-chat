package chat.simplex.app.views.chatlist

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.Indigo
import chat.simplex.app.views.helpers.ProfileImage

@Composable
fun ShareListNavLinkView(chat: Chat, chatModel: ChatModel) {
  val stopped = chatModel.chatRunning.value == false
  when (chat.chatInfo) {
    is ChatInfo.Direct ->
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat) },
        click = { directChatAction(chat.chatInfo, chatModel) },
        stopped
      )
    is ChatInfo.Group ->
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat) },
        click = { groupChatAction(chat.chatInfo.groupInfo, chatModel) },
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
      ProfileImage(size = 46.dp, chat.chatInfo.image)
      Text(
        chat.chatInfo.chatViewName, maxLines = 1, overflow = TextOverflow.Ellipsis,
        color = if (chat.chatInfo.incognito) Indigo else Color.Unspecified
      )
    }
  }
}
