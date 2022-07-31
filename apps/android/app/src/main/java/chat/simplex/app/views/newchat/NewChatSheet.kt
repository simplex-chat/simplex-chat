package chat.simplex.app.views.newchat

import android.Manifest
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.Composable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.vector.ImageVector
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ScaffoldController
import chat.simplex.app.views.helpers.ModalManager
import chat.simplex.app.views.helpers.withApi
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
      ModalManager.shared.showCustomModal { close -> PasteToConnectView(chatModel, close) }
    },
    createGroup = {
      newChatCtrl.collapse()
      ModalManager.shared.showCustomModal { close -> AddGroupView(chatModel, close) }
    }
  )
}

@Composable
fun NewChatSheetLayout(
  addContact: () -> Unit,
  scanCode: () -> Unit,
  pasteLink: () -> Unit,
  createGroup: () -> Unit
) {
  Column(
    Modifier.padding(bottom = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    Text(
      stringResource(R.string.add_contact_or_create_group),
      modifier = Modifier.padding(horizontal = 8.dp).padding(top = 20.dp, bottom = 10.dp)
    )
    val boxModifier = Modifier.fillMaxWidth().height(88.dp).padding(horizontal = 16.dp, vertical = 8.dp)

    Box(boxModifier) {
      ActionRowButton(
        stringResource(R.string.create_one_time_link),
        stringResource(R.string.to_share_with_your_contact),
        Icons.Outlined.AddLink,
        click = addContact
      )
    }

    Box(boxModifier) {
      ActionRowButton(
        stringResource(R.string.paste_received_link),
        stringResource(R.string.paste_received_link_from_clipboard),
        Icons.Outlined.Article,
        click = pasteLink
      )
    }

    Box(boxModifier) {
      ActionRowButton(
        stringResource(R.string.scan_QR_code),
        stringResource(R.string.in_person_or_in_video_call__bracketed),
        Icons.Outlined.QrCode,
        click = scanCode
      )
    }

    Box(boxModifier) {
      ActionRowButton(
        stringResource(R.string.create_group),
        stringResource(R.string.only_stored_on_members_devices),
        icon = Icons.Outlined.Group,
        click = createGroup
      )
    }
  }
}

@Composable
fun ActionRowButton(
  text: String, comment: String? = null, icon: ImageVector, disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(
    Modifier.fillMaxSize(),
    shape = RoundedCornerShape(18.dp),
    color = MaterialTheme.colors.secondary
  ) {
    Row(
      Modifier.clickable(onClick = click).size(48.dp).padding(8.dp),
      verticalAlignment = Alignment.CenterVertically
    ) {
      val tint = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
      Icon(icon, text, tint = tint, modifier = Modifier.size(48.dp).padding(horizontal = 10.dp))

      Column {
        Text(
          text,
          textAlign = TextAlign.Center,
          fontWeight = FontWeight.Bold,
          color = tint
        )

        if (comment != null) {
          Text(
            comment,
            textAlign = TextAlign.Center,
            style = MaterialTheme.typography.body2
          )
        }
      }
    }
  }
}

@Composable
fun ActionButton(
  text: String?, comment: String? = null, icon: ImageVector, disabled: Boolean = false,
  click: () -> Unit = {}
) {
  Surface(
    Modifier.fillMaxSize(),
    shape = RoundedCornerShape(18.dp),
    color = MaterialTheme.colors.secondary
  ) {
    Column(
      Modifier
        .clickable(onClick = click)
        .padding(8.dp),
      horizontalAlignment = Alignment.CenterHorizontally,
      verticalArrangement = Arrangement.Center
    ) {
      val tint = if (disabled) HighOrLowlight else MaterialTheme.colors.primary
      Icon(
        icon, text,
        tint = tint,
        modifier = Modifier
          .size(40.dp)
          .padding(bottom = 8.dp)
      )
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
}

@Preview
@Composable
fun PreviewNewChatSheet() {
  SimpleXTheme {
    NewChatSheetLayout(
      addContact = {},
      scanCode = {},
      pasteLink = {},
      createGroup = {}
    )
  }
}
