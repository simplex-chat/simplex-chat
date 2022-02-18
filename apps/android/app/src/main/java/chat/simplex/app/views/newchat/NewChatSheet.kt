package chat.simplex.app.views.newchat

import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.padding
import androidx.compose.material.Button
import androidx.compose.material.ExperimentalMaterialApi
import androidx.compose.runtime.Composable
import androidx.compose.material.Text
import androidx.compose.ui.Modifier
import androidx.compose.ui.modifier.modifierLocalConsumer
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.views.chatlist.ScaffoldController

@ExperimentalMaterialApi
@Composable
fun NewChatSheet(newChatCtrl: ScaffoldController, nav: NavController) {
  Column ( Modifier.padding(all = 8.dp) ) {
    Button(onClick = {
      newChatCtrl.collapse()
      nav.navigate(Pages.Chat.route)
    }) {
      Text("Add contact")
    }
    Button(onClick = {}) {
      Text("Scan QR code")
    }
    Button(onClick = {
      newChatCtrl.collapse()
    }) {
      Text("Cancel")
    }
  }
}
