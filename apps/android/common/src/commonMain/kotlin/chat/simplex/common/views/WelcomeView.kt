package chat.simplex.common.views

import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.KeyboardCapitalization
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import com.icerockdev.library.MR
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.Profile
import chat.simplex.common.platform.navigationBarsWithImePadding
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.onboarding.ReadableText
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.distinctUntilChanged

fun isValidDisplayName(name: String) : Boolean {
  return (name.firstOrNull { it.isWhitespace() }) == null && !name.startsWith("@") && !name.startsWith("#")
}

@Composable
fun CreateProfilePanel(chatModel: ChatModel, close: () -> Unit) {
  val displayName = rememberSaveable { mutableStateOf("") }
  val fullName = rememberSaveable { mutableStateOf("") }
  val focusRequester = remember { FocusRequester() }

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
    Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
      AppBarTitle(stringResource(MR.strings.create_profile), bottomPadding = DEFAULT_PADDING)
      ReadableText(MR.strings.your_profile_is_stored_on_your_device, TextAlign.Center, padding = PaddingValues(), style = MaterialTheme.typography.body1)
      ReadableText(MR.strings.profile_is_only_shared_with_your_contacts, TextAlign.Center, style = MaterialTheme.typography.body1)
      Spacer(Modifier.height(DEFAULT_PADDING))
      Row(Modifier.padding(bottom = DEFAULT_PADDING_HALF).fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
        Text(
          stringResource(MR.strings.display_name),
          fontSize = 16.sp
        )
        if (!isValidDisplayName(displayName.value)) {
          Text(
            stringResource(MR.strings.no_spaces),
            fontSize = 16.sp,
            color = Color.Red
          )
        }
      }
      ProfileNameField(displayName, "", ::isValidDisplayName, focusRequester)
      Spacer(Modifier.height(DEFAULT_PADDING))
      Text(
        stringResource(MR.strings.full_name_optional__prompt),
        fontSize = 16.sp,
        modifier = Modifier.padding(bottom = DEFAULT_PADDING_HALF)
      )
      ProfileNameField(fullName, "", ::isValidDisplayName)
    }
    Spacer(Modifier.fillMaxHeight().weight(1f))
    Row {
      if (chatModel.users.isEmpty()) {
        SimpleButtonDecorated(
          text = stringResource(MR.strings.about_simplex),
          icon = painterResource(MR.images.ic_arrow_back_ios_new),
          textDecoration = TextDecoration.None,
          fontWeight = FontWeight.Medium
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
        createColor = MaterialTheme.colors.secondary
      }
      Surface(shape = RoundedCornerShape(20.dp), color = Color.Transparent) {
        Row(verticalAlignment = Alignment.CenterVertically, modifier = createModifier) {
          Text(stringResource(MR.strings.create_profile_button), style = MaterialTheme.typography.caption, color = createColor, fontWeight = FontWeight.Medium)
          Icon(painterResource(MR.images.ic_arrow_forward_ios), stringResource(MR.strings.create_profile_button), tint = createColor)
        }
      }
    }

    LaunchedEffect(Unit) {
      delay(300)
      focusRequester.requestFocus()
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
      chatModel.controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_CreateSimpleXAddress)
      chatModel.onboardingStage.value = OnboardingStage.Step3_CreateSimpleXAddress
    } else {
      val users = chatModel.controller.listUsers()
      chatModel.users.clear()
      chatModel.users.addAll(users)
      chatModel.controller.getUserChatData()
      // the next two lines are only needed for failure case when because of the database error the app gets stuck on on-boarding screen,
      // this will get it unstuck.
      chatModel.controller.appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
      chatModel.onboardingStage.value = OnboardingStage.OnboardingComplete
      close()
    }
  }
}

@Composable
fun ProfileNameField(name: MutableState<String>, placeholder: String = "", isValid: (String) -> Boolean = { true }, focusRequester: FocusRequester? = null) {
  var valid by rememberSaveable { mutableStateOf(true) }
  var focused by rememberSaveable { mutableStateOf(false) }
  val strokeColor by remember {
    derivedStateOf {
      if (valid) {
        if (focused) {
          CurrentColors.value.colors.secondary.copy(alpha = 0.6f)
        } else {
          CurrentColors.value.colors.secondary.copy(alpha = 0.3f)
        }
      } else Color.Red
    }
  }
  val modifier = Modifier
    .fillMaxWidth()
    .padding(horizontal = DEFAULT_PADDING)
    .navigationBarsWithImePadding()
    .onFocusChanged { focused = it.isFocused }
  Box(
    Modifier
      .fillMaxWidth()
      .height(52.dp)
      .border(border = BorderStroke(1.dp, strokeColor), shape = RoundedCornerShape(50)),
    contentAlignment = Alignment.Center
  ) {
    BasicTextField(
      value = name.value,
      onValueChange = { name.value = it },
      modifier = if (focusRequester == null) modifier else modifier.focusRequester(focusRequester),
      textStyle = TextStyle(fontSize = 18.sp, color = colors.onBackground),
      keyboardOptions = KeyboardOptions(
        capitalization = KeyboardCapitalization.None,
        autoCorrect = false
      ),
      singleLine = true,
      cursorBrush = SolidColor(MaterialTheme.colors.secondary)
    )
  }
  LaunchedEffect(Unit) {
    snapshotFlow { name.value }
      .distinctUntilChanged()
      .collect {
        valid = isValid(it)
      }
  }
}
