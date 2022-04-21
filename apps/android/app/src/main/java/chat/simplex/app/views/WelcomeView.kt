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
import chat.simplex.app.SimplexService
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.views.helpers.*
import com.google.accompanist.insets.ProvideWindowInsets
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.launch

@Composable
fun WelcomeView(chatModel: ChatModel) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }

  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Box(
      modifier = Modifier
        .fillMaxSize()
        .background(color = MaterialTheme.colors.background)
    ) {
      Column(
        verticalArrangement = Arrangement.SpaceBetween,
        modifier = Modifier
          .verticalScroll(scrollState)
          .fillMaxSize()
          .background(color = MaterialTheme.colors.background)
          .padding(12.dp)
      ) {
        Image(
          painter = painterResource(R.drawable.logo),
          contentDescription = generalGetString(R.string.image_descr_simplex_logo),
          modifier = Modifier.padding(vertical = 15.dp)
        )
        Text(
          generalGetString(R.string.you_control_your_chat),
          style = MaterialTheme.typography.h4,
          color = MaterialTheme.colors.onBackground
        )
        Text(
          generalGetString(R.string.the_messaging_and_app_platform_protecting_your_privacy_and_security),
          style = MaterialTheme.typography.body1,
          color = MaterialTheme.colors.onBackground
        )
        Spacer(Modifier.height(8.dp))
        Text(
          generalGetString(R.string.we_do_not_store_contacts_or_messages_on_servers),
          style = MaterialTheme.typography.body1,
          color = MaterialTheme.colors.onBackground
        )
        Spacer(Modifier.height(24.dp))
        CreateProfilePanel(chatModel)
      }
      if (savedKeyboardState != keyboardState) {
        LaunchedEffect(keyboardState) {
          scope.launch {
            savedKeyboardState = keyboardState
            scrollState.animateScrollTo(scrollState.maxValue)
          }
        }
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
      generalGetString(R.string.create_profile),
      style = MaterialTheme.typography.h4,
      color = MaterialTheme.colors.onBackground,
      modifier = Modifier.padding(vertical = 5.dp)
    )
    Text(
      generalGetString(R.string.your_profile_is_stored_on_your_decide_and_shared_only_with_your_contacts),
      style = MaterialTheme.typography.body1,
      color = MaterialTheme.colors.onBackground
    )
    Spacer(Modifier.height(10.dp))
    Text(
      generalGetString(R.string.display_name),
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
    val errorText = if(!isValidDisplayName(displayName)) generalGetString(R.string.display_name_cannot_contain_whitespace) else ""

    Text(
      errorText,
      fontSize = 15.sp,
      color = MaterialTheme.colors.error
    )

    Spacer(Modifier.height(3.dp))
    Text(
      generalGetString(R.string.full_name_optional__prompt),
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
          Profile(displayName, fullName, null)
        )
        chatModel.controller.startChat(user)
        SimplexService.start(chatModel.controller.appContext)
        chatModel.controller.showBackgroundServiceNotice()
      }
    },
    enabled = (displayName.isNotEmpty() && isValidDisplayName(displayName))
    ) { Text(generalGetString(R.string.create_profile_button)) }
  }
}
