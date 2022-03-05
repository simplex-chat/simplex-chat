package chat.simplex.app.views

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.views.helpers.withApi
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding

@Composable
fun WelcomeView(chatModel: ChatModel) {
  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Box(
      modifier = Modifier
        .fillMaxSize()
        .background(color = MaterialTheme.colors.background)
    ) {
      Column(
        verticalArrangement = Arrangement.SpaceBetween,
        modifier = Modifier
          .verticalScroll(rememberScrollState())
          .fillMaxSize()
          .background(color = MaterialTheme.colors.background)
          .padding(12.dp)
      ) {
        Image(
          painter = painterResource(R.drawable.logo),
          contentDescription = "Simplex Logo",
          modifier = Modifier.padding(vertical = 15.dp)
        )
        Text(
          "You control your chat!",
          style = MaterialTheme.typography.h4,
          color = MaterialTheme.colors.onBackground
        )
        Text(
          "The messaging and application platform protecting your privacy and security.",
          style = MaterialTheme.typography.body1,
          color = MaterialTheme.colors.onBackground
        )
        Spacer(Modifier.height(8.dp))
        Text(
          "We don't store any of your contacts or messages (once delivered) on the servers.",
          style = MaterialTheme.typography.body1,
          color = MaterialTheme.colors.onBackground
        )
        Spacer(Modifier.height(24.dp))
        CreateProfilePanel(chatModel)
      }
    }
  }
}


fun isValidDisplayName(name: String) : Boolean {
  return (name.firstOrNull { it.isWhitespace() }) == null
}

@Composable
fun CreateProfilePanel(chatModel: ChatModel) {
  var displayName by remember { mutableStateOf("") }
  var fullName by remember { mutableStateOf("") }

  Column(
    modifier=Modifier.fillMaxSize()
  ) {
    Text(
      "Create profile",
      style = MaterialTheme.typography.h4,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(vertical = 5.dp)
    )
    Text(
      "Your profile is stored on your device and shared only with your contacts.",
      style = MaterialTheme.typography.body1,
      color = MaterialTheme.colors.onBackground
    )
    Spacer(Modifier.height(10.dp))
    Text(
      "Display Name",
      style = MaterialTheme.typography.h6,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(bottom = 3.dp)
    )
    BasicTextField(
      value = displayName,
      onValueChange = { displayName = it },
      modifier = Modifier
        .fillMaxWidth()
        .background(MaterialTheme.colors.secondary)
        .height(40.dp)
        .clip(RoundedCornerShape(5.dp))
        .padding(8.dp)
        .navigationBarsWithImePadding(),
      textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
      keyboardOptions = KeyboardOptions(
        capitalization = KeyboardCapitalization.None,
        autoCorrect = false
      ),
      singleLine = true
    )
    val errorText = if(!isValidDisplayName(displayName)) "Display name cannot contain whitespace." else ""

    Text(
      errorText,
      fontSize = 15.sp,
      color = MaterialTheme.colors.error
    )

    Spacer(Modifier.height(3.dp))
    Text(
      "Full Name (Optional)",
      style = MaterialTheme.typography.h6,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(bottom = 5.dp)
    )
    BasicTextField(
      value = fullName,
      onValueChange = { fullName = it },
      modifier = Modifier
        .fillMaxWidth()
        .background(MaterialTheme.colors.secondary)
        .height(40.dp)
        .clip(RoundedCornerShape(3.dp))
        .padding(8.dp)
        .navigationBarsWithImePadding(),
      textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
      keyboardOptions = KeyboardOptions(
        capitalization = KeyboardCapitalization.None,
        autoCorrect = false
      ),
      singleLine = true
    )
    Spacer(Modifier.height(20.dp))
    Button(onClick = {
      withApi {
        val user = chatModel.controller.apiCreateActiveUser(
          Profile(displayName, fullName)
        )
        chatModel.controller.startChat(user)
      }
    },
    enabled = (displayName.isNotEmpty() && isValidDisplayName(displayName))
    ) { Text("Create") }
  }
}
