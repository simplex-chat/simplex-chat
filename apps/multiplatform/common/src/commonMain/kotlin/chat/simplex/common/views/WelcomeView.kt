package chat.simplex.common.views

import SectionTextFooter
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.BasicTextField
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
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

@Composable
fun CreateProfile(chatModel: ChatModel, close: () -> Unit) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }

  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Box(
      modifier = Modifier
        .fillMaxSize()
        .padding(top = 20.dp)
    ) {
      val displayName = rememberSaveable { mutableStateOf("") }
      val focusRequester = remember { FocusRequester() }

      ColumnWithScrollBar(
        modifier = Modifier.fillMaxSize()
      ) {
        Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          AppBarTitle(stringResource(MR.strings.create_profile), bottomPadding = DEFAULT_PADDING)
          Row(Modifier.padding(bottom = DEFAULT_PADDING_HALF).fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Text(
              stringResource(MR.strings.display_name),
              fontSize = 16.sp
            )
            val name = displayName.value.trim()
            val validName = mkValidName(name)
            Spacer(Modifier.height(20.dp))
            if (name != validName) {
              IconButton({ showInvalidNameAlert(mkValidName(displayName.value), displayName) }, Modifier.size(20.dp)) {
                Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
              }
            }
          }
          ProfileNameField(displayName, "", { it.trim() == mkValidName(it) }, focusRequester)
        }
        SettingsActionItem(
          painterResource(MR.images.ic_check),
          stringResource(MR.strings.create_another_profile_button),
          disabled = !canCreateProfile(displayName.value),
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
          click = {
            if (chatModel.localUserCreated.value == true) {
              createProfileInProfiles(chatModel, displayName.value, close)
            } else {
              createProfileInNoProfileSetup(displayName.value, close)
            }
          },
        )
        SectionTextFooter(generalGetString(MR.strings.your_profile_is_stored_on_your_device))
        SectionTextFooter(generalGetString(MR.strings.profile_is_only_shared_with_your_contacts))

        LaunchedEffect(Unit) {
          delay(300)
          focusRequester.requestFocus()
        }
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

@Composable
fun CreateFirstProfile(chatModel: ChatModel, close: () -> Unit) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }

  ProvideWindowInsets(windowInsetsAnimationsEnabled = true) {
    Column(
      modifier = Modifier
        .fillMaxSize(),
      horizontalAlignment = Alignment.CenterHorizontally
    ) {
      CloseSheetBar(close = {
        if (chatModel.users.none { !it.user.hidden }) {
          appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
        } else {
          close()
        }
      })
      BackHandler(onBack = {
        appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
      })

      ColumnWithScrollBar(
        modifier = if (appPlatform.isAndroid) Modifier.fillMaxSize() else Modifier.widthIn(max = 600.dp).fillMaxHeight(),
      ) {
        val displayName = rememberSaveable { mutableStateOf("") }
        val focusRequester = remember { FocusRequester() }
        Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          AppBarTitle(stringResource(MR.strings.create_profile), bottomPadding = DEFAULT_PADDING)
          ProfileNameField(displayName, stringResource(MR.strings.display_name), { it.trim() == mkValidName(it) }, focusRequester)
          Spacer(Modifier.height(DEFAULT_PADDING))
          ReadableText(MR.strings.your_profile_is_stored_on_your_device, TextAlign.Start, padding = PaddingValues(), style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary))
          ReadableText(MR.strings.profile_is_only_shared_with_your_contacts, TextAlign.Start, style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary))
        }
        Spacer(Modifier.fillMaxHeight().weight(1f))
        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingActionButton(
            if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING * 2).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
            labelId = MR.strings.create_profile_button,
            onboarding = null,
            enabled = canCreateProfile(displayName.value),
            onclick = { createProfileOnboarding(chat.simplex.common.platform.chatModel, displayName.value, close) }
          )
          // Reserve space
          TextButtonBelowOnboardingButton("", null)
        }

        LaunchedEffect(Unit) {
          delay(300)
          focusRequester.requestFocus()
        }
      }
      LaunchedEffect(Unit) {
        setLastVersionDefault(chatModel)
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

fun createProfileInNoProfileSetup(displayName: String, close: () -> Unit) {
  withBGApi {
    val user = controller.apiCreateActiveUser(null, Profile(displayName.trim(), "", null)) ?: return@withBGApi
    if (!chatModel.connectedToRemote()) {
      chatModel.localUserCreated.value = true
    }
    controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_CreateSimpleXAddress)
    controller.startChat(user)
    controller.switchUIRemoteHost(null)
    close()
  }
}

fun createProfileInProfiles(chatModel: ChatModel, displayName: String, close: () -> Unit) {
  withBGApi {
    val rhId = chatModel.remoteHostId()
    val user = chatModel.controller.apiCreateActiveUser(
      rhId, Profile(displayName.trim(), "", null)
    ) ?: return@withBGApi
    chatModel.currentUser.value = user
    if (chatModel.users.isEmpty()) {
      chatModel.controller.startChat(user)
      chatModel.controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_CreateSimpleXAddress)
    } else {
      val users = chatModel.controller.listUsers(rhId)
      chatModel.users.clear()
      chatModel.users.addAll(users)
      chatModel.controller.getUserChatData(rhId)
      close()
    }
  }
}

fun createProfileOnboarding(chatModel: ChatModel, displayName: String, close: () -> Unit) {
  withBGApi {
    chatModel.currentUser.value = chatModel.controller.apiCreateActiveUser(
      null, Profile(displayName.trim(), "", null)
    ) ?: return@withBGApi
    chatModel.localUserCreated.value = true
    val onboardingStage = chatModel.controller.appPrefs.onboardingStage
    // No users or no visible users
    if (chatModel.users.none { u -> !u.user.hidden }) {
      onboardingStage.set(if (appPlatform.isDesktop && chatModel.controller.appPrefs.initialRandomDBPassphrase.get() && !chatModel.desktopOnboardingRandomPassword.value) {
        OnboardingStage.Step2_5_SetupDatabasePassphrase
      } else {
        OnboardingStage.Step3_CreateSimpleXAddress
      })
    } else {
      // the next two lines are only needed for failure case when because of the database error the app gets stuck on on-boarding screen,
      // this will get it unstuck.
      onboardingStage.set(OnboardingStage.OnboardingComplete)
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
    .heightIn(min = 50.dp)
    .navigationBarsWithImePadding()
    .onFocusChanged { focused = it.isFocused }
  Column(
    Modifier
      .fillMaxWidth(),
    horizontalAlignment = Alignment.CenterHorizontally
  ) {
    BasicTextField(
      value = name.value,
      onValueChange = { name.value = it },
      modifier = if (focusRequester == null) modifier else modifier.focusRequester(focusRequester),
      textStyle = TextStyle(fontSize = 18.sp, color = colors.onBackground),
      singleLine = true,
      cursorBrush = SolidColor(MaterialTheme.colors.secondary),
      decorationBox = @Composable { innerTextField ->
        TextFieldDefaults.TextFieldDecorationBox(
          value = name.value,
          innerTextField = innerTextField,
          placeholder = if (placeholder != "") {{ Text(placeholder, style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary, lineHeight = 22.sp)) }} else null,
          contentPadding = PaddingValues(),
          label = null,
          visualTransformation = VisualTransformation.None,
          leadingIcon = null,
          trailingIcon = if (!valid && placeholder != "") {
            {
              IconButton({ showInvalidNameAlert(mkValidName(name.value), name) }, Modifier.size(20.dp)) {
                Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
              }
            }
          } else null,
          singleLine = true,
          enabled = true,
          isError = false,
          interactionSource = remember { MutableInteractionSource() },
        )
      }
    )
    Divider(color = strokeColor)
  }
  LaunchedEffect(Unit) {
    snapshotFlow { name.value }
      .distinctUntilChanged()
      .collect {
        valid = isValid(it)
      }
  }
}

private fun canCreateProfile(displayName: String): Boolean {
  val name = displayName.trim()
  return name.isNotEmpty() && mkValidName(name) == name
}

fun showInvalidNameAlert(name: String, displayName: MutableState<String>) {
  if (name.isEmpty()) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_name),
    )
  } else {
    AlertManager.shared.showAlertDialog(
      title = generalGetString(MR.strings.invalid_name),
      text = generalGetString(MR.strings.correct_name_to).format(name),
      onConfirm = {
        displayName.value = name
      }
    )
  }
}

fun isValidDisplayName(name: String) : Boolean = mkValidName(name.trim()) == name

fun mkValidName(s: String): String = chatValidName(s)
