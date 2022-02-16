package chat.simplex.app.views

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.Image
import androidx.compose.ui.res.painterResource
import androidx.compose.runtime.*
import androidx.compose.runtime.Composable
import chat.simplex.app.R
import androidx.compose.material.Text
import androidx.compose.material.TextField
import androidx.compose.material.Button
import androidx.compose.ui.unit.dp
import androidx.compose.ui.Modifier
import androidx.navigation.NavController

@Composable
fun WelcomeView(createUser: (data: String) -> String, navController: NavController) {
  Column {
    Image(
      painter=painterResource(R.drawable.logo), contentDescription = "Simplex Logo",
    )
    Text("You control your chat!")
    Text("The messaging and application platform protecting your privacy and security.")
    Spacer(Modifier.height(8.dp))
    Text("We don't store any of your contacts or messages (once delivered) on the servers.")
    Spacer(Modifier.height(24.dp))
    CreateProfilePanel(createUser, navController)
  }
}

@Composable
fun CreateProfilePanel(createUser: (data: String) -> String, navController: NavController) {
  var displayName by remember { mutableStateOf("") }
  var fullName by remember { mutableStateOf("") }

  Column {
    Text("Create profile")
    Text("Your profile is stored on your device and shared only with your contacts.")
    Text("Display Name")
    TextField(value = displayName, onValueChange = { displayName = it }, modifier = Modifier.height(30.dp))
    Text("Full Name (Optional)")
    TextField(value = fullName, onValueChange = { fullName = it }, modifier = Modifier.height(30.dp))
    Button(onClick={
      createUser("{\"displayName\": $displayName, \"fullName\": $fullName}")
      navController.navigate("home") { popUpTo("home") { inclusive = true }}
    }) { Text("Create")}
  }
}