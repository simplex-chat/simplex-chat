package chat.simplex.common.views

import SectionTextFooter
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.foundation.text.BasicTextField
import androidx.compose.material.*
import androidx.compose.material.MaterialTheme.colors
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.draw.drawBehind
import androidx.compose.ui.focus.*
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.graphics.Brush
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.layout.ContentScale
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.TextStyle
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.text.style.*
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.BuildConfigCommon
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.migration.MigrateToDeviceView
import chat.simplex.common.views.migration.MigrationToState
import chat.simplex.common.views.newchat.darkStops
import chat.simplex.common.views.newchat.gradientPoints
import chat.simplex.common.views.newchat.lightStops
import chat.simplex.common.views.onboarding.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch

const val MAX_BIO_LENGTH_BYTES = 160

fun bioFitsLimit(bio: String): Boolean {
  return chatJsonLength(bio) <= MAX_BIO_LENGTH_BYTES
}

@Composable
fun CreateProfile(chatModel: ChatModel, close: () -> Unit) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }
    Box(
      modifier = Modifier
        .fillMaxSize()
        .padding(top = 20.dp)
    ) {
      val displayName = rememberSaveable { mutableStateOf("") }
      val shortDescr = rememberSaveable { mutableStateOf("") }
      val focusRequester = remember { FocusRequester() }

      ColumnWithScrollBar {
        Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
          AppBarTitle(stringResource(MR.strings.create_profile), withPadding = false, bottomPadding = DEFAULT_PADDING)
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

          Spacer(Modifier.height(DEFAULT_PADDING))

          Row(Modifier.padding(bottom = DEFAULT_PADDING_HALF).fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Text(
              stringResource(MR.strings.short_descr),
              fontSize = 16.sp
            )
            Spacer(Modifier.height(20.dp))
            if (!bioFitsLimit(shortDescr.value)) {
              IconButton(
                onClick = { AlertManager.shared.showAlertMsg(title = generalGetString(MR.strings.bio_too_large)) },
                Modifier.size(20.dp)) {
                Icon(painterResource(MR.images.ic_info), null, tint = MaterialTheme.colors.error)
              }
            }
          }
          ProfileNameField(shortDescr, "", isValid = { bioFitsLimit(it) })
        }
        SettingsActionItem(
          painterResource(MR.images.ic_check),
          stringResource(MR.strings.create_another_profile_button),
          disabled = !canCreateProfile(displayName.value) || !bioFitsLimit(shortDescr.value),
          textColor = MaterialTheme.colors.primary,
          iconColor = MaterialTheme.colors.primary,
          click = {
            if (chatModel.localUserCreated.value == true) {
              createProfileInProfiles(chatModel, displayName.value, shortDescr.value, close)
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

@Composable
fun CreateFirstProfile(chatModel: ChatModel, close: () -> Unit) {
  if (appPlatform.isDesktop) {
    CreateFirstProfileDesktop(chatModel, close)
  } else {
    CreateFirstProfileMobile(chatModel, close)
  }
}

@Composable
private fun RowScope.MigrateButton(refocusTrigger: MutableState<Int>) {
  val focusManager = LocalFocusManager.current
  TextButton(
    onClick = {
      focusManager.clearFocus()
      if (chatModel.migrationState.value == null) {
        chatModel.migrationState.value = MigrationToState.PasteOrScanLink
      }
      ModalManager.fullscreen.showCustomModal(animated = false) { close ->
        MigrateToDeviceView {
          close()
          refocusTrigger.value++
        }
      }
    },
    modifier = Modifier.padding(end = DEFAULT_PADDING_HALF)
  ) {
    Icon(painterResource(MR.images.ic_download), null, Modifier.size(22.dp), tint = MaterialTheme.colors.primary)
    Spacer(Modifier.width(4.dp))
    Text(
      stringResource(if (appPlatform.isDesktop) MR.strings.migrate_from_another_device else MR.strings.migrate),
      color = MaterialTheme.colors.primary, fontWeight = FontWeight.Medium
    )
  }
}

private fun onboardingBackAction(chatModel: ChatModel, close: () -> Unit) {
  if (chatModel.users.none { !it.user.hidden }) {
    appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
  } else {
    close()
  }
}

@Composable
private fun CreateFirstProfileMobile(chatModel: ChatModel, close: () -> Unit) {
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    val focusRequester = remember { FocusRequester() }
    val refocusTrigger = remember { mutableStateOf(0) }
    ModalView(
      close = { onboardingBackAction(chatModel, close) },
      endButtons = { MigrateButton(refocusTrigger) }
    ) {
      val displayName = rememberSaveable { mutableStateOf("") }
      val keyboardState by getKeyboardState()
      val imageHeightModifier = if (keyboardState == KeyboardState.Opened) {
        Modifier.heightIn(max = 100.dp)
      } else {
        Modifier
      }
      ColumnWithScrollBar(Modifier.padding(horizontal = DEFAULT_ONBOARDING_HORIZONTAL_PADDING), horizontalAlignment = Alignment.CenterHorizontally, maxIntrinsicSize = true) {
        Spacer(Modifier.weight(1f))

        if (BuildConfigCommon.SIMPLEX_ASSETS) {
          Image(
            painterResource(if (isInDarkTheme()) MR.images.your_profile_light else MR.images.your_profile),
            contentDescription = null,
            contentScale = ContentScale.Fit,
            modifier = Modifier.fillMaxWidth().then(imageHeightModifier)
          )
        } else {
          val isDark = isInDarkTheme()
          val stops = if (isDark) darkStops else lightStops
          val scale = if (isDark) 1.5f else 1.2f
          Box(
            Modifier
              .then(if (keyboardState != KeyboardState.Opened) Modifier.fillMaxWidth() else Modifier)
              .then(imageHeightModifier)
              .aspectRatio(1f)
              .clip(RoundedCornerShape(24.dp))
              .drawBehind {
                val gp = gradientPoints(size.height / size.width, scale)
                drawRect(
                  Brush.linearGradient(
                    colorStops = stops,
                    start = Offset(gp.startX * size.width, gp.startY * size.height),
                    end = Offset(gp.endX * size.width, gp.endY * size.height)
                  )
                )
              },
            contentAlignment = Alignment.Center
          ) {
            Icon(
              painterResource(MR.images.ic_person),
              contentDescription = null,
              modifier = Modifier.size(80.dp),
              tint = MaterialTheme.colors.primary
            )
          }
        }

        Text(
          stringResource(MR.strings.onboarding_your_profile),
          style = MaterialTheme.typography.h1,
          fontWeight = FontWeight.Bold,
          textAlign = TextAlign.Center,
          modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
        )
        Text(
          stringResource(MR.strings.onboarding_on_your_phone),
          style = MaterialTheme.typography.h3,
          fontWeight = FontWeight.Medium,
          color = MaterialTheme.colors.secondary,
          fontSize = 20.sp,
          lineHeight = 27.sp,
          textAlign = TextAlign.Center,
          modifier = Modifier.padding(top = 14.dp)
        )
        Text(
          stringResource(MR.strings.onboarding_no_account),
          style = MaterialTheme.typography.body2,
          color = MaterialTheme.colors.secondary,
          textAlign = TextAlign.Center,
          lineHeight = 20.sp,
          modifier = Modifier.padding(top = DEFAULT_PADDING_HALF)
        )
        Spacer(Modifier.height(DEFAULT_PADDING_HALF))
        ProfileNameField(displayName, stringResource(MR.strings.enter_profile_name), { it.trim() == mkValidName(it) }, focusRequester)

        Spacer(Modifier.weight(1f))

        Column(Modifier.widthIn(max = 450.dp).padding(bottom = DEFAULT_PADDING * 2).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          OnboardingActionButton(
            Modifier.fillMaxWidth(),
            labelId = MR.strings.create_profile,
            onboarding = null,
            enabled = canCreateProfile(displayName.value),
            onclick = { createProfileOnboarding(chatModel, displayName.value, close) }
          )
        }

        LaunchedEffect(refocusTrigger.value) {
          delay(300)
          focusRequester.requestFocus()
        }
      }
      LaunchedEffect(Unit) {
        setLastVersionDefault(chatModel)
      }
    }
  }
}

@Composable
private fun CreateFirstProfileDesktop(chatModel: ChatModel, close: () -> Unit) {
  val focusRequester = remember { FocusRequester() }
  val refocusTrigger = remember { mutableStateOf(0) }
  val displayName = rememberSaveable { mutableStateOf("") }
  Row(Modifier.fillMaxSize()) {
    // Left: image
    Box(Modifier.weight(0.382f).fillMaxHeight().padding(horizontal = DEFAULT_PADDING), contentAlignment = Alignment.Center) {
      if (BuildConfigCommon.SIMPLEX_ASSETS) {
        Image(
          painterResource(if (isInDarkTheme()) MR.images.your_profile_light else MR.images.your_profile),
          contentDescription = null,
          contentScale = ContentScale.Fit,
          modifier = Modifier.fillMaxWidth()
        )
      } else {
        val isDark = isInDarkTheme()
        val stops = if (isDark) darkStops else lightStops
        val scale = if (isDark) 1.5f else 1.2f
        Box(
          Modifier
            .fillMaxWidth()
            .aspectRatio(1f)
            .clip(RoundedCornerShape(24.dp))
            .drawBehind {
              val gp = gradientPoints(size.height / size.width, scale)
              drawRect(
                Brush.linearGradient(
                  colorStops = stops,
                  start = Offset(gp.startX * size.width, gp.startY * size.height),
                  end = Offset(gp.endX * size.width, gp.endY * size.height)
                )
              )
            },
          contentAlignment = Alignment.Center
        ) {
          Icon(
            painterResource(MR.images.ic_person),
            contentDescription = null,
            modifier = Modifier.size(80.dp),
            tint = MaterialTheme.colors.primary
          )
        }
      }
    }
    Divider(Modifier.fillMaxHeight().width(1.dp))
    // Right: old layout with bar
    Box(Modifier.weight(0.618f).fillMaxHeight()) {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        ModalView(
          close = { onboardingBackAction(chatModel, close) },
          background = MaterialTheme.colors.background.mixWith(MaterialTheme.colors.onBackground, 0.97f),
          endButtons = { MigrateButton(refocusTrigger) }
        ) {
          ColumnWithScrollBar(horizontalAlignment = Alignment.CenterHorizontally) {
            Column(Modifier.widthIn(max = 600.dp).fillMaxHeight().padding(horizontal = DEFAULT_PADDING).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
              Box(Modifier.align(Alignment.CenterHorizontally)) {
                AppBarTitle(stringResource(MR.strings.onboarding_your_profile), bottomPadding = DEFAULT_PADDING, withPadding = false)
              }
              ReadableText(MR.strings.onboarding_on_your_phone, TextAlign.Center, padding = PaddingValues(), style = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary))
              Spacer(Modifier.height(DEFAULT_PADDING))
              ReadableText(MR.strings.onboarding_no_account, TextAlign.Center, style = MaterialTheme.typography.body2.copy(color = MaterialTheme.colors.secondary))
              Spacer(Modifier.height(DEFAULT_PADDING))
              ProfileNameField(displayName, stringResource(MR.strings.enter_profile_name), { it.trim() == mkValidName(it) }, focusRequester)
            }
            Spacer(Modifier.fillMaxHeight().weight(1f))
            Column(Modifier.widthIn(max = 1000.dp).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
              OnboardingActionButton(
                Modifier.widthIn(min = 300.dp),
                labelId = MR.strings.create_profile,
                onboarding = null,
                enabled = canCreateProfile(displayName.value),
                onclick = { createProfileOnboarding(chatModel, displayName.value, close) }
              )
              TextButtonBelowOnboardingButton("", null)
            }
          }
          LaunchedEffect(Unit) {
            setLastVersionDefault(chatModel)
          }
        }
      }
    }
  }
  LaunchedEffect(refocusTrigger.value) {
    delay(300)
    focusRequester.requestFocus()
  }
}

fun createProfileInNoProfileSetup(displayName: String, close: () -> Unit) {
  withBGApi {
    val user = controller.apiCreateActiveUser(null, Profile(displayName.trim(), "", null, null)) ?: return@withBGApi
    if (!chatModel.connectedToRemote()) {
      chatModel.localUserCreated.value = true
    }
    controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_ChooseServerOperators)
    controller.startChat(user)
    controller.switchUIRemoteHost(null)
    close()
  }
}

fun createProfileInProfiles(chatModel: ChatModel, displayName: String, shortDescr: String, close: () -> Unit) {
  withBGApi {
    val rhId = chatModel.remoteHostId()
    val user = chatModel.controller.apiCreateActiveUser(
      rhId, Profile(displayName.trim(), "", shortDescr.trim().ifEmpty { null }, null)
    ) ?: return@withBGApi
    chatModel.currentUser.value = user
    if (chatModel.users.isEmpty()) {
      chatModel.controller.startChat(user)
      chatModel.controller.appPrefs.onboardingStage.set(OnboardingStage.Step4_NetworkCommitments)
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
      null, Profile(displayName.trim(), "", null, null)
    ) ?: return@withBGApi
    chatModel.localUserCreated.value = true
    val onboardingStage = chatModel.controller.appPrefs.onboardingStage
    // No users or no visible users
    if (chatModel.users.none { u -> !u.user.hidden }) {
      onboardingStage.set(if (appPlatform.isDesktop && chatModel.controller.appPrefs.initialRandomDBPassphrase.get() && !chatModel.desktopOnboardingRandomPassword.value) {
        OnboardingStage.Step2_5_SetupDatabasePassphrase
      } else {
        OnboardingStage.Step3_ChooseServerOperators
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
          colors = TextFieldDefaults.textFieldColors(backgroundColor = Color.Unspecified)
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
