package chat.simplex.common.views.onboarding

import SectionBottomSpacer
import SectionTextFooter
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.input.key.*
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.input.ImeAction
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.style.TextAlign
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.database.*
import chat.simplex.common.views.helpers.*
import chat.simplex.res.MR
import kotlinx.coroutines.delay

@Composable
fun SetupDatabasePassphrase(m: ChatModel) {
  val progressIndicator = remember { mutableStateOf(false) }
  val prefs = m.controller.appPrefs
  val saveInPreferences = remember { mutableStateOf(prefs.storeDBPassphrase.get()) }
  val initialRandomDBPassphrase = remember { mutableStateOf(prefs.initialRandomDBPassphrase.get()) }
  // Do not do rememberSaveable on current key to prevent saving it on disk in clear text
  val currentKey = remember { mutableStateOf(if (initialRandomDBPassphrase.value) DatabaseUtils.ksDatabasePassword.get() ?: "" else "") }
  val newKey = rememberSaveable { mutableStateOf("") }
  val confirmNewKey = rememberSaveable { mutableStateOf("") }
  fun nextStep() {
    if (appPlatform.isAndroid || chatModel.currentUser.value != null) {
      m.controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_CreateSimpleXAddress)
    } else {
      m.controller.appPrefs.onboardingStage.set(OnboardingStage.LinkAMobile)
    }
  }
  SetupDatabasePassphraseLayout(
    currentKey,
    newKey,
    confirmNewKey,
    progressIndicator,
    onConfirmEncrypt = {
      withApi {
        if (m.chatRunning.value == true) {
          // Stop chat if it's started before doing anything
          stopChatAsync(m)
        }
        prefs.storeDBPassphrase.set(false)

        val newKeyValue = newKey.value
        val success = encryptDatabase(currentKey, newKey, confirmNewKey, mutableStateOf(true), saveInPreferences, mutableStateOf(true), progressIndicator)
        if (success) {
          startChat(newKeyValue)
          nextStep()
        } else {
          // Rollback in case of it is finished with error in order to allow to repeat the process again
          prefs.storeDBPassphrase.set(true)
        }
      }
    },
    nextStep = ::nextStep,
  )

  if (progressIndicator.value) {
    ProgressIndicator()
  }

  DisposableEffect(Unit) {
    onDispose {
      if (m.chatRunning.value != true) {
        withBGApi {
          val user = chatController.apiGetActiveUser(null)
          if (user != null) {
            m.controller.startChat(user)
          }
        }
      }
    }
  }
}

@Composable
private fun SetupDatabasePassphraseLayout(
  currentKey: MutableState<String>,
  newKey: MutableState<String>,
  confirmNewKey: MutableState<String>,
  progressIndicator: MutableState<Boolean>,
  onConfirmEncrypt: () -> Unit,
  nextStep: () -> Unit,
) {
  Column(
    Modifier.fillMaxSize().verticalScroll(rememberScrollState()).padding(top = DEFAULT_PADDING),
    horizontalAlignment = Alignment.CenterHorizontally,
  ) {
    AppBarTitle(stringResource(MR.strings.setup_database_passphrase))

    Spacer(Modifier.weight(1f))

    Column(Modifier.width(600.dp)) {
      val focusRequester = remember { FocusRequester() }
      val focusManager = LocalFocusManager.current
      LaunchedEffect(Unit) {
        delay(100L)
        focusRequester.requestFocus()
      }
      PassphraseField(
        newKey,
        generalGetString(MR.strings.new_passphrase),
        modifier = Modifier
          .padding(horizontal = DEFAULT_PADDING)
          .focusRequester(focusRequester)
          .onPreviewKeyEvent {
            if ((it.key == Key.Enter || it.key == Key.NumPadEnter) && it.type == KeyEventType.KeyUp) {
              focusManager.moveFocus(FocusDirection.Down)
              true
            } else {
              false
            }
          },
        showStrength = true,
        isValid = ::validKey,
        keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
      )
      val onClickUpdate = {
        // Don't do things concurrently. Shouldn't be here concurrently, just in case
        if (!progressIndicator.value) {
          encryptDatabaseAlert(onConfirmEncrypt)
        }
      }
      val disabled = currentKey.value == newKey.value ||
          newKey.value != confirmNewKey.value ||
          newKey.value.isEmpty() ||
          !validKey(currentKey.value) ||
          !validKey(newKey.value) ||
          progressIndicator.value

      PassphraseField(
        confirmNewKey,
        generalGetString(MR.strings.confirm_new_passphrase),
        modifier = Modifier
          .padding(horizontal = DEFAULT_PADDING)
          .onPreviewKeyEvent {
            if (!disabled && (it.key == Key.Enter || it.key == Key.NumPadEnter) && it.type == KeyEventType.KeyUp) {
              onClickUpdate()
              true
            } else {
              false
            }
          },
        isValid = { confirmNewKey.value == "" || newKey.value == confirmNewKey.value },
        keyboardActions = KeyboardActions(onDone = { defaultKeyboardAction(ImeAction.Done) }),
      )

      Box(Modifier.align(Alignment.CenterHorizontally).padding(vertical = DEFAULT_PADDING)) {
        SetPassphraseButton(disabled, onClickUpdate)
      }

      Column {
        SectionTextFooter(generalGetString(MR.strings.you_have_to_enter_passphrase_every_time))
        SectionTextFooter(annotatedStringResource(MR.strings.impossible_to_recover_passphrase))
      }
    }

    Spacer(Modifier.weight(1f))
    SkipButton(progressIndicator.value) {
      chatModel.desktopOnboardingRandomPassword.value = true
      nextStep()
    }

    SectionBottomSpacer()
  }
}

@Composable
private fun SetPassphraseButton(disabled: Boolean, onClick: () -> Unit) {
  SimpleButtonIconEnded(
    stringResource(MR.strings.set_database_passphrase),
    painterResource(MR.images.ic_check),
    style = MaterialTheme.typography.h2,
    color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary,
    disabled = disabled,
    click = onClick
  )
}

@Composable
private fun SkipButton(disabled: Boolean, onClick: () -> Unit) {
  SimpleButtonIconEnded(stringResource(MR.strings.use_random_passphrase), painterResource(MR.images.ic_chevron_right), color =
  if (disabled) MaterialTheme.colors.secondary else WarningOrange, disabled = disabled, click = onClick)
  Text(
    stringResource(MR.strings.you_can_change_it_later),
    Modifier
      .fillMaxWidth()
      .padding(horizontal = DEFAULT_PADDING * 3),
    style = MaterialTheme.typography.subtitle1,
    color = MaterialTheme.colors.secondary,
    textAlign = TextAlign.Center,
  )
}

@Composable
private fun ProgressIndicator() {
  Box(
    Modifier.fillMaxSize(),
    contentAlignment = Alignment.Center
  ) {
    CircularProgressIndicator(
      Modifier
        .padding(horizontal = 2.dp)
        .size(30.dp),
      color = MaterialTheme.colors.secondary,
      strokeWidth = 3.dp
    )
  }
}

private suspend fun startChat(key: String?) {
  val m = ChatModel
  initChatController(key)
  m.chatDbChanged.value = false
  m.chatRunning.value = true
}
