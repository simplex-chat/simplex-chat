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
    modifier = Modifier
      .verticalScroll(rememberScrollState())
      .fillMaxSize()
      .background(color = MaterialTheme.colors.background)
  ) {
    Image(
      painter = painterResource(R.drawable.logo), contentDescription = "Simplex Logo",
    )
    Text("You control your chat!", color = MaterialTheme.colors.onBackground)
    Text(
      "The messaging and application platform protecting your privacy and security.",
      color = MaterialTheme.colors.onBackground
    )
    Spacer(Modifier.height(8.dp))
    Text(
      "We don't store any of your contacts or messages (once delivered) on the servers.",
      color = MaterialTheme.colors.onBackground
    )
    Spacer(Modifier.height(24.dp))
    CreateProfilePanel(chatModel, routeHome)
  }
}

@DelicateCoroutinesApi
@Composable
fun CreateProfilePanel(chatModel: ChatModel, routeHome: () -> Unit) {
  var displayName by remember { mutableStateOf("") }
  var fullName by remember { mutableStateOf("") }

  Column(
    modifier=Modifier.fillMaxSize()
  ) {
    Text("Create profile", color = MaterialTheme.colors.onBackground)
    Text(
      "Your profile is stored on your device and shared only with your contacts.",
      color = MaterialTheme.colors.onBackground
    )
    Text("Display Name", color = MaterialTheme.colors.onBackground)
    TextField(value = displayName, onValueChange = { value -> displayName = value })
    Text("Full Name (Optional)", color = MaterialTheme.colors.onBackground)
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