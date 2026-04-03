package chat.simplex.common.views.invitation_redesign

import SectionBottomSpacer
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.Image
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.style.TextOverflow
import androidx.compose.ui.unit.dp
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.*
import chat.simplex.res.MR
import kotlinx.coroutines.*

@Composable
fun OneTimeLinkView(rhId: Long?, close: () -> Unit) {
  val contactConnection: MutableState<PendingContactConnection?> = rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(chatModel.showingInvitation.value?.conn) }
  val connLinkInvitation by remember { derivedStateOf { chatModel.showingInvitation.value?.connLink ?: CreatedConnLink("", null) } }
  val creatingConnReq = rememberSaveable { mutableStateOf(false) }

  LaunchedEffect(Unit) {
    if (
      connLinkInvitation.connFullLink.isEmpty()
      && contactConnection.value == null
      && !creatingConnReq.value
    ) {
      creatingConnReq.value = true
      withBGApi {
        val (r, alert) = controller.apiAddContact(rhId, incognito = controller.appPrefs.incognito.get())
        if (r != null) {
          withContext(Dispatchers.Main) {
            chatModel.chatsContext.updateContactConnection(rhId, r.second)
            chatModel.showingInvitation.value = ShowingInvitation(connId = r.second.id, connLink = r.first, connChatUsed = false, conn = r.second)
            contactConnection.value = r.second
          }
        } else {
          creatingConnReq.value = false
          if (alert != null) {
            alert()
          }
        }
      }
    }
  }

  DisposableEffect(Unit) {
    onDispose {
      if (chatModel.showingInvitation.value != null && ModalManager.start.openModalCount() <= 1) {
        val conn = contactConnection.value
        if (chatModel.showingInvitation.value?.connChatUsed == false && conn != null) {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.keep_unused_invitation_question),
            text = generalGetString(MR.strings.you_can_view_invitation_link_again),
            confirmText = generalGetString(MR.strings.delete_verb),
            dismissText = generalGetString(MR.strings.keep_invitation_link),
            destructive = true,
            onConfirm = {
              withBGApi {
                val chatInfo = ChatInfo.ContactConnection(conn)
                controller.deleteChat(Chat(remoteHostId = rhId, chatInfo = chatInfo, chatItems = listOf()))
                if (chatModel.chatId.value == chatInfo.id) {
                  chatModel.chatId.value = null
                  ModalManager.start.closeModals()
                }
              }
            }
          )
        }
        chatModel.showingInvitation.value = null
      }
    }
  }

  ModalView(close) {
    OneTimeLinkContent(connLinkInvitation)
  }
}

@Composable
fun OneTimeLinkContent(connLinkInvitation: CreatedConnLink) {
  val showShortLink = remember { mutableStateOf(true) }

  ColumnWithScrollBar(
    Modifier.fillMaxSize(),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Spacer(Modifier.height(DEFAULT_PADDING))

    Image(
      painterResource(MR.images.ic_invitation_one_time_link),
      contentDescription = null,
      modifier = Modifier
        .fillMaxWidth()
        .padding(horizontal = DEFAULT_PADDING * 3)
    )

    Spacer(Modifier.height(DEFAULT_PADDING))

    Text(
      stringResource(MR.strings.send_1_time_link_description),
      style = MaterialTheme.typography.body1,
      color = MaterialTheme.colors.secondary,
      modifier = Modifier.padding(horizontal = DEFAULT_PADDING)
    )

    Spacer(Modifier.height(DEFAULT_PADDING))

    if (connLinkInvitation.connFullLink.isNotEmpty()) {
      OneTimeLinkBar(connLinkInvitation, showShortLink.value)

      Spacer(Modifier.height(DEFAULT_PADDING))

      Text(
        stringResource(MR.strings.or_show_qr_code_in_person_or_video_call),
        style = MaterialTheme.typography.body1,
        color = MaterialTheme.colors.secondary,
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING)
      )

      Spacer(Modifier.height(DEFAULT_PADDING_HALF))

      Surface(
        shape = RoundedCornerShape(18.dp),
        color = MaterialTheme.colors.background,
        modifier = Modifier
          .fillMaxWidth()
          .padding(horizontal = DEFAULT_PADDING)
      ) {
        SimpleXCreatedLinkQRCode(
          connLinkInvitation,
          short = showShortLink.value,
          onShare = { chatModel.markShowingInvitationUsed() }
        )
      }
    } else {
      Box(Modifier.fillMaxWidth(), contentAlignment = Alignment.Center) {
        CircularProgressIndicator(
          Modifier.size(36.dp).padding(4.dp),
          color = MaterialTheme.colors.secondary,
          strokeWidth = 3.dp
        )
      }
    }

    SectionBottomSpacer()
  }
}

@Composable
private fun OneTimeLinkBar(connLinkInvitation: CreatedConnLink, short: Boolean) {
  val clipboard = LocalClipboardManager.current
  val link = connLinkInvitation.simplexChatUri(short)

  Surface(
    shape = RoundedCornerShape(24.dp),
    color = MaterialTheme.appColors.sentMessage,
    modifier = Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING)
  ) {
    Row(
      Modifier
        .padding(start = DEFAULT_PADDING, end = 4.dp)
        .heightIn(min = 48.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      Text(
        link,
        style = MaterialTheme.typography.body2,
        maxLines = 1,
        overflow = TextOverflow.Ellipsis,
        modifier = Modifier.weight(1f)
      )
      IconButton(onClick = {
        chatModel.markShowingInvitationUsed()
        clipboard.setText(AnnotatedString(simplexChatLink(link)))
      }) {
        Icon(
          painterResource(MR.images.ic_content_copy),
          contentDescription = stringResource(MR.strings.copy_verb),
          tint = MaterialTheme.colors.primary
        )
      }
      IconButton(onClick = {
        chatModel.markShowingInvitationUsed()
        clipboard.shareText(simplexChatLink(link))
      }) {
        Icon(
          painterResource(MR.images.ic_share),
          contentDescription = stringResource(MR.strings.share_verb),
          tint = MaterialTheme.colors.primary
        )
      }
    }
  }
}

@Preview
@Composable
fun PreviewOneTimeLinkView() {
  SimpleXTheme {
    OneTimeLinkContent(
      connLinkInvitation = CreatedConnLink("https://smp16.simplex.im/i#pT0CA_nnqmLA", null)
    )
  }
}
