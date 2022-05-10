package chat.simplex.app.views

import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.outlined.ArrowBackIosNew
import androidx.compose.material.icons.outlined.ArrowForwardIos
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.SimplexService
import chat.simplex.app.model.ChatModel
import chat.simplex.app.model.Profile
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleButton
import chat.simplex.app.views.helpers.withApi
import chat.simplex.app.views.onboarding.OnboardingStage
import chat.simplex.app.views.onboarding.ReadableText
import com.google.accompanist.insets.navigationBarsWithImePadding

fun isValidDisplayName(name: String) : Boolean {
  return (name.firstOrNull { it.isWhitespace() }) == null
}

@Composable
fun CreateProfilePanel(chatModel: ChatModel) {
  val displayName = remember { mutableStateOf("") }
  val fullName = remember { mutableStateOf("") }
  val focusRequester = remember { FocusRequester() }

  Surface(Modifier.background(MaterialTheme.colors.onBackground)) {
    Column(
      modifier = Modifier.fillMaxSize()
    ) {
      Text(
        stringResource(R.string.create_profile),
        style = MaterialTheme.typography.h4,
        modifier = Modifier.padding(vertical = 5.dp)
      )
      ReadableText(R.string.your_profile_is_stored_on_your_device)
      ReadableText(R.string.profile_is_only_shared_with_your_contacts)
      Spacer(Modifier.height(10.dp))
      Text(
        stringResource(R.string.display_name),
        style = MaterialTheme.typography.h6,
        modifier = Modifier.padding(bottom = 3.dp)
      )
      ProfileNameField(displayName, focusRequester)
      val errorText = if (!isValidDisplayName(displayName.value)) stringResource(R.string.display_name_cannot_contain_whitespace) else ""
      Text(
        errorText,
        fontSize = 15.sp,
        color = MaterialTheme.colors.error
      )
      Spacer(Modifier.height(3.dp))
      Text(
        stringResource(R.string.full_name_optional__prompt),
        style = MaterialTheme.typography.h6,
        modifier = Modifier.padding(bottom = 5.dp)
      )
      ProfileNameField(fullName)
      Spacer(Modifier.fillMaxHeight().weight(1f))
      Row {
        SimpleButton(
          text = stringResource(R.string.about_simplex),
          icon = Icons.Outlined.ArrowBackIosNew
        ) { chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo }

        Spacer(Modifier.fillMaxWidth().weight(1f))

        val enabled = displayName.value.isNotEmpty() && isValidDisplayName(displayName.value)
        val createModifier: Modifier
        val createColor: Color
        if (enabled) {
          createModifier = Modifier.padding(8.dp).clickable { createProfile(chatModel, displayName.value, fullName.value) }
          createColor = MaterialTheme.colors.primary
        } else {
          createModifier = Modifier.padding(8.dp)
          createColor = HighOrLowlight
        }
        Surface(shape = RoundedCornerShape(20.dp)) {
          Row(verticalAlignment = Alignment.CenterVertically, modifier = createModifier) {
            Text(stringResource(R.string.create_profile_button), style = MaterialTheme.typography.caption, color = createColor)
            Icon(Icons.Outlined.ArrowForwardIos, stringResource(R.string.create_profile_button), tint = createColor)
          }
        }
      }

      LaunchedEffect(Unit) {
        focusRequester.requestFocus()
      }
    }
  }
}

fun createProfile(chatModel: ChatModel, displayName: String, fullName: String) {
  withApi {
    val user = chatModel.controller.apiCreateActiveUser(
      Profile(displayName, fullName, null)
    )
    chatModel.controller.startChat(user)
    SimplexService.start(chatModel.controller.appContext)
    // TODO show it later?
    chatModel.controller.showBackgroundServiceNotice()
    chatModel.onboardingStage.value = OnboardingStage.OnboardingComplete
  }
}

@Composable
fun ProfileNameField(name: MutableState<String>, focusRequester: FocusRequester? = null) {
  val modifier = Modifier
    .fillMaxWidth()
    .background(MaterialTheme.colors.secondary)
    .height(40.dp)
    .clip(RoundedCornerShape(5.dp))
    .padding(8.dp)
    .navigationBarsWithImePadding()
  BasicTextField(
    value = name.value,
    onValueChange = { name.value = it },
    modifier = if (focusRequester == null) modifier else modifier.focusRequester(focusRequester),
    textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.onBackground),
    keyboardOptions = KeyboardOptions(
      capitalization = KeyboardCapitalization.None,
      autoCorrect = false
    ),
    singleLine = true,
    cursorBrush = SolidColor(HighOrLowlight)
  )
}