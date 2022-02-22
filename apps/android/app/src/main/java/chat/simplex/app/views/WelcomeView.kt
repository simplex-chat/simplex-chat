package chat.simplex.app.views

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.unit.dp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.views.helpers.withApi
import kotlinx.coroutines.DelicateCoroutinesApi

@DelicateCoroutinesApi
@Composable
fun WelcomeView(chatModel: ChatModel, routeHome: () -> Unit) {
  Column(
    modifier = Modifier.verticalScroll(rememberScrollState())
  ) {
    Image(
      painter=painterResource(R.drawable.logo), contentDescription = "Simplex Logo",
    )
    Text("You control your chat!")
    Text("The messaging and application platform protecting your privacy and security.")
    Spacer(Modifier.height(8.dp))
    Text("We don't store any of your contacts or messages (once delivered) on the servers.")
    Spacer(Modifier.height(24.dp))
    CreateProfilePanel(chatModel, routeHome)
  }
}

@DelicateCoroutinesApi
@Composable
fun CreateProfilePanel(chatModel: ChatModel, routeHome: () -> Unit) {
  var displayName by remember { mutableStateOf("") }
  var fullName by remember { mutableStateOf("") }

  Column {
    Text("Create profile")
    Text("Your profile is stored on your device and shared only with your contacts.")
    Text("Display Name")
    TextField(value = displayName, onValueChange = { value -> displayName = value })
    Text("Full Name (Optional)")
    TextField(value = fullName, onValueChange = { fullName = it })
    Button(onClick={
      withApi {
        val user = chatModel.controller.apiCreateActiveUser(
          Profile(displayName, fullName)
        )
        chatModel.controller.startChat(user)
        routeHome()
      }
    },
    enabled = displayName.isNotEmpty()
    ) { Text("Create")}
  }
}