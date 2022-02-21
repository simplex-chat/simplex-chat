package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.navigation.NavController
import chat.simplex.app.model.ChatModel

@Composable
fun UserProfileView(chatModel: ChatModel, nav: NavController) {
  val user = chatModel.currentUser.value
  val profile = user!!.profile
  Column() {
    Button(
      onClick = { nav.popBackStack() }
    ) {
      Text("Back")
    }
  }
}
