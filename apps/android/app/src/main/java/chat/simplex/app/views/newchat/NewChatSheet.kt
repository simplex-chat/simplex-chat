package chat.simplex.app.views.newchat

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Button
import androidx.compose.material.ExperimentalMaterialApi
import androidx.compose.runtime.Composable
import androidx.compose.material.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.chatlist.ScaffoldController
import chat.simplex.app.views.helpers.withApi

@ExperimentalMaterialApi
@Composable
fun NewChatSheet(chatModel: ChatModel, newChatCtrl: ScaffoldController, nav: NavController) {
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
      nav.navigate(Pages.Connect.route)
    },
    close = {
      newChatCtrl.collapse()
    }
  )
}

@Composable
fun NewChatSheetLayout(addContact: () -> Unit, scanCode: () -> Unit, close: () -> Unit) {
  Column ( Modifier.padding(all = 8.dp) ) {
    Button(onClick = addContact) {
      Text("Add contact")
    }
    Button(onClick = scanCode) {
      Text("Scan QR code")
    }
    Button(onClick = close) {
      Text("Cancel")
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
      close = {},
    )
  }
}
