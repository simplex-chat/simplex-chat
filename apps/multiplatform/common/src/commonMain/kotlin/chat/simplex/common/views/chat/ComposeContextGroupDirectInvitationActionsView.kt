package chat.simplex.common.views.chat

import TextIconSpaced
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.alpha
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.DEFAULT_PADDING_HALF
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*

@Composable
fun ComposeContextMemberContactActionsView(
  rhId: Long?,
  contact: Contact,
  groupDirectInv: GroupDirectInvitation
) {
  val inProgress = rememberSaveable { mutableStateOf(false) }
  var progressByTimeout by rememberSaveable { mutableStateOf(false) }

  KeyChangeEffect(chatModel.chatId.value) {
    if (inProgress.value) {
      inProgress.value = false
      progressByTimeout = false
    }
  }

  LaunchedEffect(inProgress.value) {
    progressByTimeout = if (inProgress.value) {
      delay(1000)
      inProgress.value
    } else {
      false
    }
  }

  Box(
    Modifier.height(60.dp),
    contentAlignment = Alignment.Center
  ) {
    Column(
      Modifier
        .background(MaterialTheme.colors.surface)
        .alpha(if (progressByTimeout) 0.6f else 1f)
    ) {
      Divider()

      if (groupDirectInv.memberRemoved) {
        Row(
          Modifier
            .fillMaxSize()
            .padding(horizontal = DEFAULT_PADDING_HALF),
          verticalAlignment = Alignment.CenterVertically,
          horizontalArrangement = Arrangement.spacedBy(8.dp, Alignment.CenterHorizontally)
        ) {
          Icon(painterResource(MR.images.ic_info), contentDescription = null, tint = MaterialTheme.colors.secondary)
          Text(generalGetString(MR.strings.member_is_deleted_cant_accept_request), color = MaterialTheme.colors.secondary)
        }
      } else {
        Row(
          Modifier
            .fillMaxWidth(),
          horizontalArrangement = Arrangement.SpaceEvenly,
        ) {
          var rejectButtonModifier = Modifier.fillMaxWidth().fillMaxHeight().weight(1F)
          rejectButtonModifier =
            if (inProgress.value) rejectButtonModifier
            else rejectButtonModifier.clickable { showRejectMemberContactRequestAlert(rhId, contact) }
          Row(
            rejectButtonModifier,
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center
          ) {
            Icon(
              painterResource(MR.images.ic_close),
              contentDescription = null,
              tint = if (inProgress.value) MaterialTheme.colors.secondary else Color.Red,
            )
            TextIconSpaced(false)
            Text(
              stringResource(MR.strings.reject_contact_button),
              color = if (inProgress.value) MaterialTheme.colors.secondary else Color.Red
            )
          }
          var acceptButtonModifier = Modifier.fillMaxWidth().fillMaxHeight().weight(1F)
          acceptButtonModifier =
            if (inProgress.value) acceptButtonModifier
            else acceptButtonModifier.clickable { acceptMemberContact(rhId, contact.contactId, inProgress = inProgress) }
          Row(
            acceptButtonModifier,
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.Center
          ) {
            Icon(
              painterResource(MR.images.ic_check),
              contentDescription = null,
              tint = if (inProgress.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
            )
            TextIconSpaced(false)
            Text(
              stringResource(MR.strings.accept_contact_button),
              color = if (inProgress.value) MaterialTheme.colors.secondary else MaterialTheme.colors.primary
            )
          }
        }
      }
    }

    if (progressByTimeout) {
      ComposeProgressIndicator()
    }
  }
}

fun showRejectMemberContactRequestAlert(rhId: Long?, contact: Contact) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.reject_contact_request),
    text = generalGetString(MR.strings.the_sender_will_not_be_notified),
    confirmText = generalGetString(MR.strings.reject_contact_button),
    onConfirm = {
      AlertManager.shared.hideAlert()
      deleteMemberContact(rhId, contact)
    },
    destructive = true,
    hostDevice = hostDevice(rhId),
  )
}

private fun deleteMemberContact(rhId: Long?, contact: Contact) {
  withBGApi {
    chatModel.controller.apiDeleteContact(rhId, contact.contactId, chatDeleteMode = ContactDeleteMode.Full().toChatDeleteMode(notify = false))
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.removeChat(rhId, contact.id)
      chatModel.chatId.value = null
    }
  }
}

fun acceptMemberContact(
  rhId: Long?,
  contactId: Long,
  close: ((chat: Chat) -> Unit)? = null,
  inProgress: MutableState<Boolean>? = null
) {
  withBGApi {
    inProgress?.value = true
    val contact = chatModel.controller.apiAcceptMemberContact(rhId, contactId)
    if (contact != null) {
      withContext(Dispatchers.Main) {
        chatModel.chatsContext.updateContact(rhId, contact)
        inProgress?.value = false
      }
      chatModel.setContactNetworkStatus(contact, NetworkStatus.Connected())
      val chat = Chat(remoteHostId = rhId, ChatInfo.Direct(contact), listOf())
      close?.invoke(chat)
    } else {
      inProgress?.value = false
    }
  }
}
