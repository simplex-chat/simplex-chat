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
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.ChatModel
import chat.simplex.app.ui.theme.DarkGray
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ScaffoldController
import chat.simplex.app.views.helpers.withApi
import com.google.accompanist.permissions.ExperimentalPermissionsApi
import com.google.accompanist.permissions.rememberPermissionState
import kotlinx.coroutines.DelicateCoroutinesApi

@DelicateCoroutinesApi
@ExperimentalPermissionsApi
@ExperimentalMaterialApi
@Composable
fun NewChatSheet(chatModel: ChatModel, newChatCtrl: ScaffoldController, nav: NavController) {
  val cameraPermissionState = rememberPermissionState(permission = Manifest.permission.CAMERA)
  NewChatSheetLayout(
    addContact = {
      withApi {
        //        show spinner
        chatModel.connReqInvitation = chatModel.controller.apiAddContact()
        //        hide spinner
        if (chatModel.connReqInvitation != null) {
          newChatCtrl.collapse()
          nav.navigate(Pages.AddContact.route)
        }
      }
    },
    scanCode = {
      newChatCtrl.collapse()
      nav.navigate(Pages.Connect.route)
      cameraPermissionState.launchPermissionRequest()
    },
    close = {
      newChatCtrl.collapse()
    }
  )
}

@Composable
fun NewChatSheetLayout(addContact: () -> Unit, scanCode: () -> Unit, close: () -> Unit) {
  Row(Modifier
      .fillMaxWidth()
      .padding(horizontal = 8.dp, vertical = 48.dp),
    horizontalArrangement = Arrangement.SpaceEvenly
  ) {
    Box(Modifier.weight(1F).fillMaxWidth()) {
      ActionButton(
        "Add contact", "(create QR code\nor link)",
        Icons.Outlined.PersonAdd, click = addContact
      )
    }
    Box(Modifier.weight(1F).fillMaxWidth()) {
      ActionButton(
        "Scan QR code", "(in person or in video call)",
        Icons.Outlined.QrCode, click = scanCode
      )
    }
    Box(Modifier.weight(1F).fillMaxWidth()) {
      ActionButton(
        "Create Group", "(coming soon!)",
        Icons.Outlined.GroupAdd, disabled = true
      )
    }
  }
}

@Composable
fun ActionButton(text: String, comment: String, icon: ImageVector, disabled: Boolean = false,
                 click: () -> Unit = {}) {
  Column(
    Modifier
      .clickable(onClick = click)
      .padding(horizontal = 8.dp),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    val tint = if (disabled) DarkGray else MaterialTheme.colors.primary
    Icon(icon, text,
      tint = tint,
      modifier = Modifier
        .size(40.dp)
        .padding(bottom = 8.dp))
    Text(text,
      textAlign = TextAlign.Center,
      fontWeight = FontWeight.Bold,
      color = tint,
      modifier = Modifier.padding(bottom = 4.dp)
    )
    Text(comment,
      textAlign = TextAlign.Center,
      style = MaterialTheme.typography.body2
    )
  }
}

@Preview
@Composable
fun PreviewNewChatSheet() {
  SimpleXTheme {
    NewChatSheetLayout(
      addContact = {},
      scanCode = {},
      close = {},
    )
  }
}
