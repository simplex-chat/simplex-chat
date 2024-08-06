package chat.simplex.common.views.chatlist

import SectionItemView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.scale
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.launch

@Composable
fun ShareListNavLinkView(
  chat: Chat,
  chatModel: ChatModel,
  isMediaOrFileAttachment: Boolean,
  isVoice: Boolean,
  hasSimplexLink: Boolean,
  reachableChatToolbar: State<Boolean>
) {
  val stopped = chatModel.chatRunning.value == false
  val scope = rememberCoroutineScope()
  when (chat.chatInfo) {
    is ChatInfo.Direct -> {
      val voiceProhibited = isVoice && !chat.chatInfo.featureEnabled(ChatFeature.Voice)
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat, disabled = voiceProhibited, reachableChatToolbar = reachableChatToolbar) },
        click = {
          if (voiceProhibited) {
            showForwardProhibitedByPrefAlert()
          } else {
            scope.launch { directChatAction(chat.remoteHostId, chat.chatInfo.contact, chatModel) }
          }
        },
        stopped
      )
    }
    is ChatInfo.Group -> {
      val simplexLinkProhibited = hasSimplexLink && !chat.groupFeatureEnabled(GroupFeature.SimplexLinks)
      val fileProhibited = isMediaOrFileAttachment && !chat.groupFeatureEnabled(GroupFeature.Files)
      val voiceProhibited = isVoice && !chat.chatInfo.featureEnabled(ChatFeature.Voice)
      val prohibitedByPref = simplexLinkProhibited || fileProhibited || voiceProhibited
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat, disabled = prohibitedByPref, reachableChatToolbar = reachableChatToolbar) },
        click = {
          if (prohibitedByPref) {
            showForwardProhibitedByPrefAlert()
          } else {
            scope.launch { groupChatAction(chat.remoteHostId, chat.chatInfo.groupInfo, chatModel) }
          }
        },
        stopped
      )
    }
    is ChatInfo.Local ->
      ShareListNavLinkLayout(
        chatLinkPreview = { SharePreviewView(chat, disabled = false, reachableChatToolbar = reachableChatToolbar) },
        click = { scope.launch { noteFolderChatAction(chat.remoteHostId, chat.chatInfo.noteFolder) } },
        stopped
      )
    is ChatInfo.ContactRequest, is ChatInfo.ContactConnection, is ChatInfo.InvalidJSON -> {}
  }
}

private fun showForwardProhibitedByPrefAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.cannot_share_message_alert_title),
    text = generalGetString(MR.strings.cannot_share_message_alert_text),
  )
}

@Composable
private fun ShareListNavLinkLayout(
  chatLinkPreview: @Composable () -> Unit,
  click: () -> Unit,
  stopped: Boolean,
) {
  SectionItemView(minHeight = 50.dp, click = click, disabled = stopped) {
    chatLinkPreview()
  }
  Divider(Modifier.padding(horizontal = 8.dp))
}

@Composable
private fun SharePreviewView(chat: Chat, disabled: Boolean, reachableChatToolbar: State<Boolean>) {
  var modifier = Modifier.fillMaxSize()

  if (reachableChatToolbar.value) {
    modifier = modifier.scale(scaleX = 1f, scaleY = -1f)
  }

  Row(
    modifier,
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
        color = if (disabled) MaterialTheme.colors.secondary else if (chat.chatInfo.incognito) Indigo else Color.Unspecified
      )
    }
  }
}
