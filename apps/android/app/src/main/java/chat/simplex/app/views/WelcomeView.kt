package chat.simplex.app.views

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.Image
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.ui.res.painterResource
import androidx.compose.runtime.*
import androidx.compose.runtime.Composable
import chat.simplex.app.R
import androidx.compose.material.Text
import androidx.compose.material.TextField
import androidx.compose.material.Button
import androidx.compose.ui.unit.dp
import androidx.compose.ui.Modifier
import chat.simplex.app.SimplexViewModel
import chat.simplex.app.model.Profile
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext

@Composable
fun WelcomeView(vm: SimplexViewModel, routeHome: () -> Unit) {
  Column(
    modifier=Modifier.verticalScroll(rememberScrollState())
  ) {
    Image(
      painter=painterResource(R.drawable.logo), contentDescription = "Simplex Logo",
    )
    Text("You control your chat!")
    Text("The messaging and application platform protecting your privacy and security.")
    Spacer(Modifier.height(8.dp))
    Text("We don't store any of your contacts or messages (once delivered) on the servers.")
    Spacer(Modifier.height(24.dp))
    CreateProfilePanel(vm, routeHome)
  }
}

@Composable
fun CreateProfilePanel(vm: SimplexViewModel, routeHome: () -> Unit) {
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
      GlobalScope.launch {
        withContext(Dispatchers.Main) {
          val user = vm.chatModel.controller.apiCreateActiveUser(
            Profile(displayName, fullName)
          )
          vm.chatModel.setCurrentUser(user)
          routeHome()
        }
      }
    },
    enabled=displayName.isNotEmpty()
    ) { Text("Create")}
  }
}