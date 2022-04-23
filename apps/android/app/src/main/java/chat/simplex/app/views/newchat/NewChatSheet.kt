package chat.simplex.app.views.newchat

import android.Manifest
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ScaffoldController
import chat.simplex.app.views.helpers.*
import com.google.accompanist.permissions.rememberPermissionState

@Composable
fun NewChatSheet(chatModel: ChatModel, newChatCtrl: ScaffoldController) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  NewChatSheetLayout(
    addContact = {
      withApi {
        //        show spinner
        chatModel.connReqInvitation = chatModel.controller.apiAddContact()
        //        hide spinner
        if (chatModel.connReqInvitation != null) {
          newChatCtrl.collapse()
          ModalManager.shared.showModal { AddContactView(chatModel) }
        }
      }
    },
    scanCode = {
      newChatCtrl.collapse()
      ModalManager.shared.showCustomModal { close -> ScanToConnectView(chatModel, close) }
      cameraPermissionState.launchPermissionRequest()
    },
    pasteLink = {
      newChatCtrl.collapse()
      ModalManager.shared.showModal { PasteToConnectView(chatModel) }
    }
  )
}

@Composable
fun NewChatSheetLayout(addContact: () -> Unit, scanCode: () -> Unit, pasteLink: () -> Unit) {
  Row(
    Modifier
      .fillMaxWidth()
      .padding(horizontal = 8.dp, vertical = 48.dp),
    horizontalArrangement = Arrangement.SpaceEvenly
  ) {
    Box(
      Modifier
        .weight(1F)
        .fillMaxWidth()) {
      ActionButton(
        generalGetString(R.string.add_contact),
        generalGetString(R.string.create_QR_code_or_link__bracketed__multiline),
        Icons.Outlined.PersonAdd,
        click = addContact
      )
    }
    Box(
      Modifier
        .weight(1F)
        .fillMaxWidth()) {
      ActionButton(
        generalGetString(R.string.scan_QR_code),
        generalGetString(R.string.in_person_or_in_video_call__bracketed),
        Icons.Outlined.QrCode,
        click = scanCode
      )
    }
    Box(
      Modifier
        .weight(1F)
        .fillMaxWidth()) {
      ActionButton(
        generalGetString(R.string.paste_connection_link),
        generalGetString(R.string.paste_received_link_from_clipboard),
        Icons.Outlined.ContentPaste,
        click = pasteLink
      )
    }
  }
}

@Composable
fun ActionButton(text: String?, comment: String?, icon: ImageVector, disabled: Boolean = false,
                 click: () -> Unit = {}) {
  Column(
    Modifier
      .clickable(onClick = click)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    val tint = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
    Icon(icon, text,
      tint = tint,
      modifier = Modifier
        .size(40.dp)
        .padding(bottom = 8.dp))
    if (text != null) {
      Text(
        text,
        textAlign = TextAlign.Center,
        fontWeight = FontWeight.Bold,
        color = tint,
        modifier = Modifier.padding(bottom = 4.dp)
      )
    }
    if (comment != null) {
      Text(
        comment,
        textAlign = TextAlign.Center,
        style = MaterialTheme.typography.body2
      )
    }
  }
}

@Preview
@Composable
fun PreviewNewChatSheet() {
  SimpleXTheme {
    NewChatSheetLayout(
      addContact = {},
      scanCode = {},
      pasteLink = {}
    )
  }
}
