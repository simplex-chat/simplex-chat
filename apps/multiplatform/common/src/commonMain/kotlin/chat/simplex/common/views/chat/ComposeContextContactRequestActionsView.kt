package chat.simplex.common.views.chat

import SectionItemView
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
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.platform.chatModel
import chat.simplex.common.views.chatlist.acceptContactRequest
import chat.simplex.common.views.chatlist.rejectContactRequest
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.delay

@Composable
fun ComposeContextContactRequestActionsView(
  rhId: Long?,
  contactRequestId: Long
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

      Row(
        Modifier
          .fillMaxWidth(),
        horizontalArrangement = Arrangement.SpaceEvenly,
      ) {
        var rejectButtonModifier = Modifier.fillMaxWidth().fillMaxHeight().weight(1F)
        rejectButtonModifier =
          if (inProgress.value) rejectButtonModifier
          else rejectButtonModifier.clickable { showRejectRequestAlert(rhId, contactRequestId) }
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
          else
            acceptButtonModifier.clickable {
              if (chatModel.addressShortLinkDataSet()) {
                acceptContactRequest(rhId, incognito = false, contactRequestId, isCurrentUser = true, chatModel = chatModel, close = null, inProgress = inProgress)
              } else {
                showAcceptRequestAlert(rhId, contactRequestId, inProgress = inProgress)
              }
            }
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

    if (progressByTimeout) {
      ComposeProgressIndicator()
    }
  }
}

fun showRejectRequestAlert(rhId: Long?, contactRequestId: Long) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.reject_contact_request),
    text = generalGetString(MR.strings.the_sender_will_not_be_notified),
    confirmText = generalGetString(MR.strings.reject_contact_button),
    onConfirm = {
      AlertManager.shared.hideAlert()
      rejectContactRequest(rhId, contactRequestId, chatModel, dismissToChatList = true)
    },
    destructive = true,
    hostDevice = hostDevice(rhId),
  )
}

fun showAcceptRequestAlert(rhId: Long?, contactRequestId: Long, inProgress: MutableState<Boolean>) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.accept_contact_request),
    buttons = {
      Column {
        // Accept
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(rhId, incognito = false, contactRequestId, isCurrentUser = true, chatModel = chatModel, close = null, inProgress = inProgress)
        }) {
          Text(generalGetString(MR.strings.accept_contact_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        // Accept incognito
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(rhId, incognito = true, contactRequestId, isCurrentUser = true, chatModel = chatModel, close = null, inProgress = inProgress)
        }) {
          Text(generalGetString(MR.strings.accept_contact_incognito_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        // Cancel
        SectionItemView({
          AlertManager.shared.hideAlert()
        }) {
          Text(stringResource(MR.strings.cancel_verb), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
      }
    },
    hostDevice = hostDevice(rhId),
  )
}
