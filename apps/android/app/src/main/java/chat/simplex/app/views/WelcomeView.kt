package chat.simplex.app.views

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBackIosNew
import androidx.compose.material.icons.outlined.ArrowForwardIos
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.onboarding.OnboardingStage
import chat.simplex.app.views.onboarding.ReadableText
import com.google.accompanist.insets.navigationBarsWithImePadding
import kotlinx.coroutines.delay

fun isValidDisplayName(name: String) : Boolean {
  return (name.firstOrNull { it.isWhitespace() }) == null && !name.startsWith("@") && !name.startsWith("#")
}

@Composable
fun CreateProfilePanel(chatModel: ChatModel, close: () -> Unit) {
  val displayName = rememberSaveable { mutableStateOf("") }
  val fullName = rememberSaveable { mutableStateOf("") }
  val focusRequester = remember { FocusRequester() }

  Surface(Modifier.background(MaterialTheme.colors.onBackground)) {
    Column(
      modifier = Modifier.fillMaxSize().verticalScroll(rememberScrollState())
    ) {
      /*CloseSheetBar(close = {
        if (chatModel.users.isEmpty()) {
          chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo
        } else {
          close()
        }
      })*/
      Column(Modifier.padding(horizontal = DEFAULT_PADDING * 1f)) {
        Text(
          stringResource(R.string.create_profile),
          Modifier
            .padding(bottom = DEFAULT_PADDING * 1.5f)
            .align(Alignment.CenterHorizontally),
          overflow = TextOverflow.Ellipsis,
          style = MaterialTheme.typography.h1,
          color = colors.primary
        )
        ReadableText(R.string.your_profile_is_stored_on_your_device, TextAlign.Center, padding = PaddingValues())
        ReadableText(R.string.profile_is_only_shared_with_your_contacts, TextAlign.Center)
        Spacer(Modifier.height(DEFAULT_PADDING * 1.5f))
        Text(
          stringResource(R.string.display_name),
          fontSize = 16.sp,
          modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
        )
        ProfileNameField(displayName, generalGetString(R.string.enter_display_name), ::isValidDisplayName, focusRequester)
        val errorText = if (!isValidDisplayName(displayName.value)) stringResource(R.string.display_name_cannot_contain_whitespace) else ""
        if (errorText.isNotEmpty()) {
          Spacer(Modifier.size(DEFAULT_PADDING_HALF))
          Text(
            errorText,
            Modifier.fillMaxWidth(),
            fontSize = 15.sp,
            color = Color.Red,
            textAlign = TextAlign.Center
          )
        }
        Spacer(Modifier.height(DEFAULT_PADDING))
        Text(
          stringResource(R.string.full_name_optional__prompt),
          fontSize = 16.sp,
          modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
        )
        ProfileNameField(fullName, generalGetString(R.string.enter_full_name), ::isValidDisplayName)
      }
      Spacer(Modifier.fillMaxHeight().weight(1f))
      Row {
        if (chatModel.users.isEmpty()) {
          SimpleButtonDecorated(
            text = stringResource(R.string.about_simplex),
            icon = Icons.Outlined.ArrowBackIosNew,
            textDecoration = TextDecoration.None,
            fontWeight = FontWeight.Bold
          ) { chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo }
        }
        Spacer(Modifier.fillMaxWidth().weight(1f))
        val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
        val createModifier: Modifier
        val createColor: Color
        if (enabled) {
          createModifier = Modifier.clickable { createProfile(chatModel, displayName.value, fullName.value, close) }.padding(8.dp)
          createColor = MaterialTheme.colors.primary
        } else {
          createModifier = Modifier.padding(8.dp)
          createColor = HighOrLowlight
        }
        Surface(shape = RoundedCornerShape(20.dp)) {
          Row(verticalAlignment = Alignment.CenterVertically, modifier = createModifier) {
            Text(stringResource(R.string.create_profile_button), style = MaterialTheme.typography.caption, color = createColor, fontWeight = FontWeight.Bold)
            Icon(Icons.Outlined.ArrowForwardIos, stringResource(R.string.create_profile_button), tint = createColor)
          }
        }
      }

      LaunchedEffect(Unit) {
        delay(300)
        focusRequester.requestFocus()
      }
    }
  }
}

fun createProfile(chatModel: ChatModel, displayName: String, fullName: String, close: () -> Unit) {
  withApi {
    val user = chatModel.controller.apiCreateActiveUser(
      Profile(displayName, fullName, null)
    ) ?: return@withApi
    chatModel.currentUser.value = user
    if (chatModel.users.isEmpty()) {
      chatModel.controller.startChat(user)
      chatModel.onboardingStage.value = OnboardingStage.Step3_SetNotificationsMode
      SimplexApp.context.chatModel.controller.ntfManager.createNtfChannelsMaybeShowAlert()
    } else {
      val users = chatModel.controller.listUsers()
      chatModel.users.clear()
      chatModel.users.addAll(users)
      chatModel.controller.getUserChatData()
      close()
    }
  }
}

@Composable
fun ProfileNameField(name: MutableState<String>, placeholder: String = "", isValid: (String) -> Boolean = { true }, focusRequester: FocusRequester? = null) {
  var valid by rememberSaveable { mutableStateOf(true) }
  var strokeColor by remember { mutableStateOf(HighOrLowlight.copy(alpha = 0.3f)) }
  val modifier = Modifier
    .fillMaxWidth()
    .height(55.dp)
    .border(border = BorderStroke(1.dp, strokeColor), shape = RoundedCornerShape(50))
    .padding(horizontal = 8.dp)
    .navigationBarsWithImePadding()
    .onFocusChanged {
      strokeColor = if (valid) {
          if (it.isFocused) {
            HighOrLowlight.copy(alpha = 0.6f)
          } else {
            HighOrLowlight.copy(alpha = 0.3f)
          }
        } else Color.Red
    }
  TextField(
    value = name.value,
    onValueChange = { name.value = it; valid = isValid(it) },
    modifier = if (focusRequester == null) modifier else modifier.focusRequester(focusRequester),
    textStyle = TextStyle(fontSize = 18.sp, color = colors.onBackground),
    keyboardOptions = KeyboardOptions(
      capitalization = KeyboardCapitalization.None,
      autoCorrect = false
    ),
    singleLine = true,
    isError = !valid,
    placeholder = { Text(placeholder, fontSize = 18.sp, color = HighOrLowlight.copy(alpha = 0.3f)) },
    shape = RoundedCornerShape(50),
    colors = TextFieldDefaults.textFieldColors(
      backgroundColor = Color.Unspecified,
      textColor = MaterialTheme.colors.onBackground,
      focusedIndicatorColor = Color.Unspecified,
      unfocusedIndicatorColor = Color.Unspecified,
      cursorColor = HighOrLowlight,
      errorIndicatorColor = Color.Unspecified
    )
    )
}
