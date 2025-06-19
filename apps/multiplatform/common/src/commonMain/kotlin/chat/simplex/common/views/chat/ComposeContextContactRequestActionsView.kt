package chat.simplex.common.views.chat

import SectionItemView
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatModel
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.chatlist.acceptContactRequest
import chat.simplex.common.views.chatlist.rejectContactRequest
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.stringResource

@Composable
fun ComposeContextContactRequestActionsView(
  rhId: Long?,
  contactRequestId: Long
) {
  Column(
    Modifier
      .height(60.dp)
      .background(MaterialTheme.colors.surface)
  ) {
    Divider()

    Row(
      Modifier
        .fillMaxWidth(),
      horizontalArrangement = Arrangement.SpaceEvenly,
    ) {
      Column(
        Modifier
          .fillMaxWidth()
          .fillMaxHeight()
          .weight(1F)
          .clickable {
            showRejectRequestAlert(rhId, contactRequestId)
          },
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        Text(stringResource(MR.strings.reject_contact_button), color = Color.Red)
      }

      Column(
        Modifier
          .fillMaxWidth()
          .fillMaxHeight()
          .weight(1F)
          .clickable {
            showAcceptRequestAlert(rhId, contactRequestId)
          },
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
      ) {
        Text(stringResource(MR.strings.accept_contact_button), color = MaterialTheme.colors.primary)
      }
    }
  }
}

fun showRejectRequestAlert(rhId: Long?, contactRequestId: Long) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.reject_contact_request),
    text = generalGetString(MR.strings.the_sender_will_not_be_notified),
    confirmText = generalGetString(MR.strings.reject_contact_button),
    onConfirm = {
      rejectContactRequest(rhId, contactRequestId, chatModel, dismissToChatList = true)
    },
    destructive = true,
  )
}

fun showAcceptRequestAlert(rhId: Long?, contactRequestId: Long) {
  AlertManager.shared.showAlertDialogButtonsColumn(
    title = generalGetString(MR.strings.accept_contact_request),
    buttons = {
      Column {
        // Accept
        SectionItemView({
          AlertManager.shared.hideAlert()
          acceptContactRequest(rhId, incognito = false, contactRequestId, isCurrentUser = true, chatModel)
        }) {
          Text(generalGetString(MR.strings.accept_contact_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
        }
        if (!chatModel.addressShortLinkDataSet) {
          // Accept incognito
          SectionItemView({
            AlertManager.shared.hideAlert()
            acceptContactRequest(rhId, incognito = true, contactRequestId, isCurrentUser = true, chatModel)
          }) {
            Text(generalGetString(MR.strings.accept_contact_incognito_button), Modifier.fillMaxWidth(), textAlign = TextAlign.Center, color = MaterialTheme.colors.primary)
          }
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
