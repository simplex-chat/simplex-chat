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
import chat.simplex.common.views.usersettings.DeleteImageButton
import chat.simplex.common.views.usersettings.EditImageButton
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.net.URI

const val MAX_BIO_LENGTH_BYTES = 160

fun bioFitsLimit(bio: String): Boolean {
  return chatJsonLength(bio) <= MAX_BIO_LENGTH_BYTES
}

@Composable
fun CreateProfile(onSubmit: (displayName: String, shortDescr: String, image: String?) -> Unit) {
  val scope = rememberCoroutineScope()
  val scrollState = rememberScrollState()
  val keyboardState by getKeyboardState()
  var savedKeyboardState by remember { mutableStateOf(keyboardState) }
  val bottomSheetModalState = rememberModalBottomSheetState(initialValue = ModalBottomSheetValue.Hidden)
  val displayName = rememberSaveable { mutableStateOf("") }
  val shortDescr = rememberSaveable { mutableStateOf("") }
  val chosenImage = rememberSaveable { mutableStateOf<URI?>(null) }
  val profileImage = rememberSaveable { mutableStateOf<String?>(null) }
  val focusRequester = remember { FocusRequester() }

  ModalBottomSheetLayout(
    scrimColor = Color.Black.copy(alpha = 0.12F),
    modifier = Modifier.imePadding(),
    sheetContent = {
      GetImageBottomSheet(
        chosenImage,
        onImageChange = { bitmap -> profileImage.value = resizeImageToStrSize(cropToSquare(bitmap), maxDataSize = 12500) },
        hideBottomSheet = {
          scope.launch { bottomSheetModalState.hide() }
        })
    },
    sheetState = bottomSheetModalState,
    sheetShape = RoundedCornerShape(topStart = 18.dp, topEnd = 18.dp)
  ) {
    Box(
      modifier = Modifier.fillMaxSize()
    ) {
      ColumnWithScrollBar {
        AppBarTitle(stringResource(MR.strings.create_profile), bottomPadding = DEFAULT_PADDING_HALF)
        Row(
          Modifier
            .fillMaxWidth()
            .padding(vertical = DEFAULT_PADDING_HALF),
          horizontalArrangement = Arrangement.Center,
          verticalAlignment = Alignment.CenterVertically
        ) {
          Box(
            modifier = if (BuildConfigCommon.SIMPLEX_ASSETS) Modifier.padding(horizontal = 3.dp) else Modifier,
            contentAlignment = Alignment.Center
          ) {
            Box(contentAlignment = Alignment.TopEnd) {
              Box(contentAlignment = Alignment.Center) {
                ProfileImage(128.dp, image = profileImage.value)
                EditImageButton { scope.launch { bottomSheetModalState.show() } }
              }
              if (profileImage.value != null) {
                DeleteImageButton { profileImage.value = null }
              }
            }
          }
          if (BuildConfigCommon.SIMPLEX_ASSETS) {
            Image(
              painterResource(if (isInDarkTheme()) MR.images.create_profile_light else MR.images.create_profile),
              contentDescription = null,
              contentScale = ContentScale.Fit,
              modifier = Modifier.height(140.dp)
            )
          }
        }
        Column(Modifier.padding(horizontal = DEFAULT_PADDING)) {
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
          click = { onSubmit(displayName.value, shortDescr.value, profileImage.value) },
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
      ModalManager.fullscreen.showCustomModal(animated = false, forceAnimated = appPlatform.isDesktop) { close ->
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

        OnboardingImage(
          MR.images.your_profile, MR.images.your_profile_light, MR.images.ic_person,
          modifier = Modifier
            .then(if (keyboardState != KeyboardState.Opened) Modifier.fillMaxWidth() else Modifier)
            .then(imageHeightModifier)
        )

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
          lineHeight = 25.sp,
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
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView(
      close = { onboardingBackAction(chatModel, close) },
      endButtons = { MigrateButton(refocusTrigger) }
    ) {
      ColumnWithScrollBar(horizontalAlignment = Alignment.CenterHorizontally) {
        Column(Modifier.widthIn(max = 600.dp).fillMaxHeight().padding(horizontal = DEFAULT_PADDING).align(Alignment.CenterHorizontally), horizontalAlignment = Alignment.CenterHorizontally) {
          Box(Modifier.align(Alignment.CenterHorizontally)) {
            AppBarTitle(stringResource(MR.strings.onboarding_your_profile), bottomPadding = DEFAULT_PADDING, withPadding = false, overrideTitleColor = MaterialTheme.colors.onBackground, textAlign = TextAlign.Center, lineHeight = 42.sp)
          }
          Text(stringResource(MR.strings.onboarding_on_your_phone), style = MaterialTheme.typography.h3, fontWeight = FontWeight.Medium, color = MaterialTheme.colors.secondary, lineHeight = 25.sp, textAlign = TextAlign.Center)
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
  LaunchedEffect(refocusTrigger.value) {
    delay(300)
    focusRequester.requestFocus()
  }
}

/** True while a profile is being created for an invitation and handed over to the picker
 * that asked for it. Deliberately not remembered in either picker: on Android the picker's
 * composition is disposed while the create-profile modal is on top of it, and comes back
 * with every remembered flag reset - a per-picker flag would be released the moment the
 * form opens, leaving the rows live during the reassignment. */
val creatingProfileForInvitation = mutableStateOf(false)

// Creates a profile for an invitation and hands it to onCreated, which moves the
// invitation onto it. The profile is created *without* becoming active: the reassignment
// APIs resolve the prepared chat or connection under the active user, so the profile that
// owns the invitation has to stay active until onCreated has run - which is also why
// onCreated is suspending, so the in-flight flag covers the reassignment and not just the
// creation.
fun createProfileForInvitation(rhId: Long?, modalManager: ModalManager, onCreated: suspend (User) -> Unit) {
  // Shown in the picker's own pane: ModalManager.center nulls chatId on desktop, which
  // closes the chat the prepared invitation is in - and for the picker that is itself a
  // start-pane modal, leaves the picker live beside the form.
  // Two taps before the modal renders would otherwise stack two modals sharing one id,
  // after which close() could dismiss the wrong one.
  if (modalManager.hasModalOpen(ModalViewId.CONTEXT_USER_PICKER_NEW_PROFILE)) return
  modalManager.showModalCloseable(id = ModalViewId.CONTEXT_USER_PICKER_NEW_PROFILE) { close ->
    CreateProfile { displayName, shortDescr, image ->
      if (creatingProfileForInvitation.value) return@CreateProfile
      creatingProfileForInvitation.value = true
      withBGApi {
        try {
          // The reassignment in onCreated resolves the invitation under whatever is active
          // then, so remember what owns it now and check nothing moved underneath us.
          val ownerUserId = chatModel.currentUser.value?.userId
          val profile = Profile(displayName.trim(), "", shortDescr.trim().ifEmpty { null }, image = image)
          val newUser = controller.apiCreateProfileKeepingActive(rhId, profile) ?: return@withBGApi
          if (newUser.activeUser) {
            // The core did not honour keepActiveUser and activated the profile - an older
            // remote host ignoring the unknown field. Reassigning would now fail, so resync
            // to what the host actually did and report it. Not switching_profile_error_message:
            // that says the invitation was moved, and on this path it was not.
            controller.changeActiveUser(newUser.remoteHostId, newUser.userId, null)
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_changing_user))
            return@withBGApi
          }
          // Keep chatModel.users current even if onCreated's reassignment fails - it only
          // refreshes when it actually switches. listUsers throws and withBGApi does not
          // catch, so this cosmetic refresh is guarded; and it is applied on the main
          // thread, where the receiver loop also updates this list.
          if (chatModel.remoteHostId() == rhId) {
            runCatching { controller.listUsers(rhId) }.getOrNull()?.let { updatedUsers ->
              withContext(Dispatchers.Main) {
                chatModel.users.clear()
                chatModel.users.addAll(updatedUsers)
              }
            }
          }
          // A notification tap or a remote host switch can change the active user or tear
          // the form down while the profile is being created. Reassigning after either
          // would resolve the invitation under the wrong profile, or move it under a
          // screen the user has already left, so stop with the profile created.
          if (
            chatModel.currentUser.value?.userId != ownerUserId ||
            chatModel.remoteHostId() != rhId ||
            !modalManager.isLastModalOpen(ModalViewId.CONTEXT_USER_PICKER_NEW_PROFILE)
          ) {
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_changing_user))
            return@withBGApi
          }
          close()
          onCreated(newUser)
        } finally {
          creatingProfileForInvitation.value = false
        }
      }
    }
  }
}

// The two ordinary "add a profile" paths, where the new profile becomes the active
// one. Creating one for an invitation takes neither, which is why the form itself
// no longer chooses.
fun createProfileFromForm(chatModel: ChatModel, displayName: String, shortDescr: String, image: String?, close: () -> Unit) {
  if (chatModel.localUserCreated.value == true) {
    createProfileInProfiles(chatModel, displayName, shortDescr, image, close)
  } else {
    createProfileInNoProfileSetup(displayName, image, close)
  }
}

fun createProfileInNoProfileSetup(displayName: String, image: String? = null, close: () -> Unit) {
  withBGApi {
    val user = controller.apiCreateActiveUser(null, Profile(displayName.trim(), "", null, image = image)) ?: return@withBGApi
    if (!chatModel.connectedToRemote()) {
      chatModel.localUserCreated.value = true
    }
    controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_ChooseServerOperators)
    controller.startChat(user)
    controller.switchUIRemoteHost(null)
    close()
  }
}

fun createProfileInProfiles(chatModel: ChatModel, displayName: String, shortDescr: String, image: String? = null, close: () -> Unit) {
  withBGApi {
    val rhId = chatModel.remoteHostId()
    val user = chatModel.controller.apiCreateActiveUser(
      rhId, Profile(displayName.trim(), "", shortDescr.trim().ifEmpty { null }, image = image)
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
    // new users don't need the local file encryption indicator (all files are encrypted); existing users keep it on
    chatModel.controller.appPrefs.privacyShowEncryption.set(false)
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
