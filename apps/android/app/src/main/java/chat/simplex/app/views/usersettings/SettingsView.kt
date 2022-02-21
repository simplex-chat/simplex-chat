package chat.simplex.app.views.usersettings

import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.height
import androidx.compose.material.Button
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import chat.simplex.app.Pages
import chat.simplex.app.model.ChatModel

//@Preview(showBackground = true)
@Composable
fun SettingsView(chatModel: ChatModel, nav: NavController) {
  val user = chatModel.currentUser.value
  val profile = user!!.profile
  Column() {
    Button(
      onClick = { nav.popBackStack() }
    ) {
      Text("Back")
    }
    Text("Your Settings")
    Spacer(Modifier.height(4.dp))
    Text("YOU", style = MaterialTheme.typography.h4)
    Button(
      onClick = { nav.navigate(Pages.UserProfile.route) }
    ) {
      Text(profile.displayName)
    }
    Button(
      onClick = { println(profile.hashCode()) }
    ) {
      Text("Your SimpleX contact address", style = MaterialTheme.typography.body1)
    }
    Spacer(Modifier.height(10.dp))
    Text("HELP", style = MaterialTheme.typography.h4)
    Button(
      onClick = { println("navigate to help") }
    ) {
      Text("How to use SimpleX Chat")
    }
    Button(
      onClick = { println("start help chat") }
    ) {
      Text("Get help & advice via chat")
    }
    Button(
      onClick = { println("navigate to email") }
    ) {
      Text("Ask questions via email")
    }
    Spacer(Modifier.height(10.dp))
    Text("DEVELOP", style = MaterialTheme.typography.h4)
    Button(
      onClick = { println("navigate to console") }
    ) {
      Text("Chat console")
    }
    Button(
      onClick = { println("navigate to github") }
    ) {
      Text("Install SimpleX for terminal")
    }
  }
}

