package chat.simplex.common.views.onboarding

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.*
import androidx.compose.ui.input.key.*
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.input.ImeAction
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
  val initialRandomDBPassphrase = remember { mutableStateOf(prefs.initialRandomDBPassphrase.get()) }
  // Do not do rememberSaveable on current key to prevent saving it on disk in clear text
  val currentKey = remember { mutableStateOf(if (initialRandomDBPassphrase.value) DatabaseUtils.ksDatabasePassword.get() ?: "" else "") }
  val newKey = rememberSaveable { mutableStateOf("") }
  val confirmNewKey = rememberSaveable { mutableStateOf("") }
  fun nextStep() {
    if (appPlatform.isAndroid || chatModel.currentUser.value != null) {
      m.controller.appPrefs.onboardingStage.set(OnboardingStage.Step3_ChooseServerOperators)
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
      withLongRunningApi {
        if (m.chatRunning.value == true) {
          // Stop chat if it's started before doing anything
          stopChatAsync(m)
        }
        prefs.storeDBPassphrase.set(false)

        val newKeyValue = newKey.value
        val success = encryptDatabase(
          currentKey = currentKey,
          newKey = newKey,
          confirmNewKey = confirmNewKey,
          initialRandomDBPassphrase = mutableStateOf(true),
          useKeychain = mutableStateOf(false),
          storedKey = mutableStateOf(true),
          progressIndicator = progressIndicator,
          migration = false
        )
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
  CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
    ModalView({}, showClose = false) {
      ColumnWithScrollBar(
        Modifier.themedBackground(bgLayerSize = LocalAppBarHandler.current?.backgroundGraphicsLayerSize, bgLayer = LocalAppBarHandler.current?.backgroundGraphicsLayer).padding(horizontal = DEFAULT_PADDING),
        horizontalAlignment = Alignment.CenterHorizontally,
      ) {
        AppBarTitle(stringResource(MR.strings.setup_database_passphrase))

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

        Column(Modifier.width(600.dp), horizontalAlignment = Alignment.CenterHorizontally) {
          val textStyle = MaterialTheme.typography.body1.copy(color = MaterialTheme.colors.secondary)
          ReadableText(MR.strings.you_have_to_enter_passphrase_every_time, TextAlign.Center, padding = PaddingValues(), style = textStyle )
          Spacer(Modifier.height(DEFAULT_PADDING))
          ReadableText(MR.strings.impossible_to_recover_passphrase, TextAlign.Center, padding = PaddingValues(), style = textStyle)
          Spacer(Modifier.height(DEFAULT_PADDING))

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
        }
        Spacer(Modifier.weight(1f))

        Column(Modifier.widthIn(max = if (appPlatform.isAndroid) 450.dp else 1000.dp), horizontalAlignment = Alignment.CenterHorizontally) {
          SetPassphraseButton(disabled, onClickUpdate)
          SkipButton(progressIndicator.value) {
            randomPassphraseAlert {
              chatModel.desktopOnboardingRandomPassword.value = true
              nextStep()
            }
          }
        }
      }
    }
  }
}

@Composable
private fun SetPassphraseButton(disabled: Boolean, onClick: () -> Unit) {
  OnboardingActionButton(
    if (appPlatform.isAndroid) Modifier.padding(horizontal = DEFAULT_PADDING).fillMaxWidth() else Modifier.widthIn(min = 300.dp),
    labelId = MR.strings.set_database_passphrase,
    onboarding = null,
    onclick = onClick,
    enabled =  !disabled
  )
}

@Composable
private fun SkipButton(disabled: Boolean, onClick: () -> Unit) {
  TextButtonBelowOnboardingButton(stringResource(MR.strings.use_random_passphrase), onClick = if (disabled) null else onClick)
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

private fun randomPassphraseAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.use_random_passphrase),
    text = generalGetString(MR.strings.you_can_change_it_later),
    confirmText = generalGetString(MR.strings.ok),
    onConfirm = onConfirm,
  )
}
