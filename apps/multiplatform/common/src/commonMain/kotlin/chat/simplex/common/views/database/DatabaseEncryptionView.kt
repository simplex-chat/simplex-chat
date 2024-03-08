package chat.simplex.common.views.database

import SectionBottomSpacer
import SectionItemViewSpaceBetween
import SectionSpacer
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.shape.ZeroCornerSize
import androidx.compose.foundation.text.*
import androidx.compose.material.*
import androidx.compose.material.TextFieldDefaults.indicatorLine
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.*
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.SolidColor
import androidx.compose.ui.platform.LocalSoftwareKeyboardController
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.*
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.common.model.*
import chat.simplex.common.platform.appPreferences
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.SettingsActionItem
import chat.simplex.res.MR
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.datetime.Clock
import kotlin.math.log2

@Composable
fun DatabaseEncryptionView(m: ChatModel, migration: Boolean) {
  val progressIndicator = remember { mutableStateOf(false) }
  val prefs = m.controller.appPrefs
  val useKeychain = remember { mutableStateOf(prefs.storeDBPassphrase.get()) }
  val initialRandomDBPassphrase = remember { mutableStateOf(prefs.initialRandomDBPassphrase.get()) }
  val storedKey = remember { val key = DatabaseUtils.ksDatabasePassword.get(); mutableStateOf(key != null && key != "") }
  // Do not do rememberSaveable on current key to prevent saving it on disk in clear text
  val currentKey = remember { mutableStateOf(if (initialRandomDBPassphrase.value) DatabaseUtils.ksDatabasePassword.get() ?: "" else "") }
  val newKey = rememberSaveable { mutableStateOf("") }
  val confirmNewKey = rememberSaveable { mutableStateOf("") }

  Box(
    Modifier.fillMaxSize(),
  ) {
    DatabaseEncryptionLayout(
      useKeychain,
      prefs,
      m.chatDbEncrypted.value,
      currentKey,
      newKey,
      confirmNewKey,
      storedKey,
      initialRandomDBPassphrase,
      progressIndicator,
      migration,
      onConfirmEncrypt = {
        withLongRunningApi {
          encryptDatabase(currentKey, newKey, confirmNewKey, initialRandomDBPassphrase, useKeychain, storedKey, progressIndicator, migration)
        }
      }
    )
    if (progressIndicator.value) {
      Box(
        Modifier.fillMaxSize(),
        contentAlignment = Alignment.Center
      ) {
        CircularProgressIndicator(
          Modifier
            .padding(horizontal = 2.dp)
            .size(30.dp),
          color = MaterialTheme.colors.secondary,
          strokeWidth = 2.5.dp
        )
      }
    }
  }
}

@Composable
fun DatabaseEncryptionLayout(
  useKeychain: MutableState<Boolean>,
  prefs: AppPreferences,
  chatDbEncrypted: Boolean?,
  currentKey: MutableState<String>,
  newKey: MutableState<String>,
  confirmNewKey: MutableState<String>,
  storedKey: MutableState<Boolean>,
  initialRandomDBPassphrase: MutableState<Boolean>,
  progressIndicator: MutableState<Boolean>,
  migration: Boolean,
  onConfirmEncrypt: () -> Unit,
) {
  Column(
    if (!migration) Modifier.fillMaxWidth().verticalScroll(rememberScrollState()) else Modifier.fillMaxWidth(),
  ) {
    if (!migration) {
      AppBarTitle(stringResource(MR.strings.database_passphrase))
    } else {
      ChatStoppedView()
      SectionSpacer()
    }
    SectionView(if (migration) generalGetString(MR.strings.database_passphrase).uppercase() else null) {
      SavePassphraseSetting(
        useKeychain.value,
        initialRandomDBPassphrase.value,
        storedKey.value,
        enabled = (!initialRandomDBPassphrase.value && !progressIndicator.value) || migration
      ) { checked ->
        if (checked) {
          setUseKeychain(true, useKeychain, prefs, migration)
        } else if (storedKey.value && !migration) {
          // Don't show in migration process since it will remove the key after successful encryption
          removePassphraseAlert {
            removePassphraseFromKeyChain(useKeychain, prefs, storedKey, false)
          }
        } else {
          setUseKeychain(false, useKeychain, prefs, migration)
        }
      }

      if (!initialRandomDBPassphrase.value && chatDbEncrypted == true) {
        PassphraseField(
          currentKey,
          generalGetString(MR.strings.current_passphrase),
          modifier = Modifier.padding(horizontal = DEFAULT_PADDING),
          isValid = ::validKey,
          keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
        )
      }

      PassphraseField(
        newKey,
        generalGetString(MR.strings.new_passphrase),
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING),
        showStrength = true,
        isValid = ::validKey,
        keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
      )
      val onClickUpdate = {
        // Don't do things concurrently. Shouldn't be here concurrently, just in case
        if (!progressIndicator.value) {
          if (currentKey.value == "") {
            if (useKeychain.value)
              encryptDatabaseSavedAlert(onConfirmEncrypt)
            else
              encryptDatabaseAlert(onConfirmEncrypt)
          } else {
            if (useKeychain.value)
              changeDatabaseKeySavedAlert(onConfirmEncrypt)
            else
              changeDatabaseKeyAlert(onConfirmEncrypt)
          }
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
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING),
        isValid = { confirmNewKey.value == "" || newKey.value == confirmNewKey.value },
        keyboardActions = KeyboardActions(onDone = {
          if (!disabled) onClickUpdate()
          defaultKeyboardAction(ImeAction.Done)
        }),
      )

      SectionItemViewSpaceBetween(onClickUpdate, disabled = disabled, minHeight = TextFieldDefaults.MinHeight) {
        Text(generalGetString(if (migration) MR.strings.set_passphrase else MR.strings.update_database_passphrase), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
      }
    }

    Column {
      DatabaseEncryptionFooter(useKeychain, chatDbEncrypted, storedKey, initialRandomDBPassphrase, migration)
    }
    SectionBottomSpacer()
  }
}

expect fun encryptDatabaseSavedAlert(onConfirm: () -> Unit)

fun encryptDatabaseAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.encrypt_database_question),
    text = generalGetString(MR.strings.database_will_be_encrypted) +"\n" + storeSecurelyDanger(),
    confirmText = generalGetString(MR.strings.encrypt_database),
    onConfirm = onConfirm,
    destructive = true,
  )
}

expect fun changeDatabaseKeySavedAlert(onConfirm: () -> Unit)

fun changeDatabaseKeyAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.change_database_passphrase_question),
    text = generalGetString(MR.strings.database_passphrase_will_be_updated) + "\n" + storeSecurelyDanger(),
    confirmText = generalGetString(MR.strings.update_database),
    onConfirm = onConfirm,
    destructive = true,
  )
}

expect fun removePassphraseAlert(onConfirm: () -> Unit)

@Composable
expect fun SavePassphraseSetting(
  useKeychain: Boolean,
  initialRandomDBPassphrase: Boolean,
  storedKey: Boolean,
  minHeight: Dp = TextFieldDefaults.MinHeight,
  enabled: Boolean,
  onCheckedChange: (Boolean) -> Unit,
)

@Composable
expect fun DatabaseEncryptionFooter(
  useKeychain: MutableState<Boolean>,
  chatDbEncrypted: Boolean?,
  storedKey: MutableState<Boolean>,
  initialRandomDBPassphrase: MutableState<Boolean>,
  migration: Boolean,
)

@Composable
fun ChatStoppedView() {
  SettingsActionItem(
    icon = painterResource(MR.images.ic_report_filled),
    text = stringResource(MR.strings.chat_is_stopped),
    iconColor = Color.Red,
  )
}

fun resetFormAfterEncryption(
  m: ChatModel,
  initialRandomDBPassphrase: MutableState<Boolean>,
  currentKey: MutableState<String>,
  newKey: MutableState<String>,
  confirmNewKey: MutableState<String>,
  storedKey: MutableState<Boolean>,
  stored: Boolean = false,
) {
  currentKey.value = ""
  newKey.value = ""
  confirmNewKey.value = ""
  storedKey.value = stored
  m.chatDbEncrypted.value = true
  initialRandomDBPassphrase.value = false
  m.controller.appPrefs.initialRandomDBPassphrase.set(false)
}

fun setUseKeychain(value: Boolean, useKeychain: MutableState<Boolean>, prefs: AppPreferences, migration: Boolean) {
  useKeychain.value = value
  // Postpone it when migrating to the end of encryption process
  if (!migration) {
    prefs.storeDBPassphrase.set(value)
  }
}

private fun removePassphraseFromKeyChain(useKeychain: MutableState<Boolean>, prefs: AppPreferences, storedKey: MutableState<Boolean>, migration: Boolean) {
  DatabaseUtils.ksDatabasePassword.remove()
  setUseKeychain(false, useKeychain, prefs, migration)
  storedKey.value = false
}

fun storeSecurelySaved() = generalGetString(MR.strings.store_passphrase_securely)

fun storeSecurelyDanger() = generalGetString(MR.strings.store_passphrase_securely_without_recover)

private fun operationEnded(m: ChatModel, progressIndicator: MutableState<Boolean>, alert: () -> Unit) {
  m.chatDbChanged.value = true
  progressIndicator.value = false
  alert.invoke()
}

@OptIn(ExperimentalComposeUiApi::class)
@Composable
fun PassphraseField(
  key: MutableState<String>,
  placeholder: String,
  modifier: Modifier = Modifier,
  showStrength: Boolean = false,
  isValid: (String) -> Boolean,
  keyboardActions: KeyboardActions = KeyboardActions(),
  dependsOn: State<Any?>? = null,
) {
  var valid by remember { mutableStateOf(validKey(key.value)) }
  var showKey by remember { mutableStateOf(false) }
  val icon = if (valid) {
    if (showKey) painterResource(MR.images.ic_visibility_off_filled) else painterResource(MR.images.ic_visibility_filled)
  } else painterResource(MR.images.ic_error)
  val iconColor = if (valid) {
    if (showStrength && key.value.isNotEmpty()) PassphraseStrength.check(key.value).color else MaterialTheme.colors.secondary
  } else Color.Red
  val keyboard = LocalSoftwareKeyboardController.current
  val keyboardOptions = KeyboardOptions(
    imeAction = if (keyboardActions.onNext != null) ImeAction.Next else ImeAction.Done,
    autoCorrect = false,
    keyboardType = KeyboardType.Password
  )
  val state = remember {
    mutableStateOf(TextFieldValue(key.value))
  }
  val enabled = true
  val colors = TextFieldDefaults.textFieldColors(
    backgroundColor = Color.Unspecified,
    textColor = MaterialTheme.colors.onBackground,
    focusedIndicatorColor = Color.Unspecified,
    unfocusedIndicatorColor = Color.Unspecified,
  )
  val color = MaterialTheme.colors.onBackground
  val shape = MaterialTheme.shapes.small.copy(bottomEnd = ZeroCornerSize, bottomStart = ZeroCornerSize)
  val interactionSource = remember { MutableInteractionSource() }
  BasicTextField(
    value = state.value,
    modifier = modifier
      .fillMaxWidth()
      .background(colors.backgroundColor(enabled).value, shape)
      .indicatorLine(enabled, false, interactionSource, colors)
      .defaultMinSize(
        minWidth = TextFieldDefaults.MinWidth,
        minHeight = TextFieldDefaults.MinHeight
      ),
    onValueChange = {
      state.value = it
      key.value = it.text
      valid = isValid(it.text)
    },
    cursorBrush = SolidColor(colors.cursorColor(false).value),
    visualTransformation = if (showKey)
      VisualTransformation.None
    else
      VisualTransformation { TransformedText(AnnotatedString(it.text.map { "*" }.joinToString(separator = "")), OffsetMapping.Identity) },
    keyboardOptions = keyboardOptions,
    keyboardActions = KeyboardActions(onDone = {
      keyboard?.hide()
      keyboardActions.onDone?.invoke(this)
    }),
    singleLine = true,
    textStyle = TextStyle.Default.copy(
      color = color,
      fontWeight = FontWeight.Normal,
      fontSize = 16.sp
    ),
    interactionSource = interactionSource,
    decorationBox = @Composable { innerTextField ->
      TextFieldDefaults.TextFieldDecorationBox(
        value = state.value.text,
        innerTextField = innerTextField,
        placeholder = { Text(placeholder, color = MaterialTheme.colors.secondary) },
        singleLine = true,
        enabled = enabled,
        isError = !valid,
        trailingIcon = {
          IconButton({ showKey = !showKey }) {
            Icon(icon, null, tint = iconColor)
          }
        },
        interactionSource = interactionSource,
        contentPadding = TextFieldDefaults.textFieldWithLabelPadding(start = 0.dp, end = 0.dp),
        visualTransformation = VisualTransformation.None,
        colors = colors
      )
    }
  )
  LaunchedEffect(Unit) {
    snapshotFlow { dependsOn?.value }
      .distinctUntilChanged()
      .collect {
        valid = isValid(state.value.text)
      }
  }
}

suspend fun encryptDatabase(
  currentKey: MutableState<String>,
  newKey: MutableState<String>,
  confirmNewKey: MutableState<String>,
  initialRandomDBPassphrase: MutableState<Boolean>,
  useKeychain: MutableState<Boolean>,
  storedKey: MutableState<Boolean>,
  progressIndicator: MutableState<Boolean>,
  migration: Boolean,
): Boolean {
  val m = ChatModel
  val prefs = ChatController.appPrefs
  progressIndicator.value = true
  return try {
    prefs.encryptionStartedAt.set(Clock.System.now())
    m.controller.apiSaveAppSettings(AppSettings.current)
    val error = m.controller.apiStorageEncryption(currentKey.value, newKey.value)
    prefs.encryptionStartedAt.set(null)
    val sqliteError = ((error?.chatError as? ChatError.ChatErrorDatabase)?.databaseError as? DatabaseError.ErrorExport)?.sqliteError
    when {
      sqliteError is SQLiteError.ErrorNotADatabase -> {
        operationEnded(m, progressIndicator) {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.wrong_passphrase_title),
            generalGetString(MR.strings.enter_correct_current_passphrase)
          )
        }
        false
      }
      error != null -> {
        operationEnded(m, progressIndicator) {
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_encrypting_database),
            "failed to set storage encryption: ${error.responseType} ${error.details}"
          )
        }
        false
      }
      else -> {
        val new = newKey.value
        if (migration) {
          appPreferences.storeDBPassphrase.set(useKeychain.value)
        }
        resetFormAfterEncryption(m, initialRandomDBPassphrase, currentKey, newKey, confirmNewKey, storedKey, useKeychain.value)
        if (useKeychain.value) {
          DatabaseUtils.ksDatabasePassword.set(new)
        } else if (migration) {
          removePassphraseFromKeyChain(useKeychain, prefs, storedKey, true)
        }
        operationEnded(m, progressIndicator) {
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.database_encrypted))
        }
        true
      }
    }
  } catch (e: Exception) {
    operationEnded(m, progressIndicator) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_encrypting_database), e.stackTraceToString())
    }
    false
  }
}

// based on https://generatepasswords.org/how-to-calculate-entropy/
private fun passphraseEntropy(s: String): Double {
  var hasDigits = false
  var hasUppercase = false
  var hasLowercase = false
  var hasSymbols = false
  for (c in s) {
    if (c.isDigit()) {
      hasDigits = true
    } else if (c.isLetter()) {
      if (c.isUpperCase()) {
        hasUppercase = true
      } else {
        hasLowercase = true
      }
    } else if (c.isASCII()) {
      hasSymbols = true
    }
  }
  val poolSize = (if (hasDigits) 10 else 0) + (if (hasUppercase) 26 else 0) + (if (hasLowercase) 26 else 0) + (if (hasSymbols) 32 else 0)
  return s.length * log2(poolSize.toDouble())
}

enum class PassphraseStrength(val color: Color) {
  VERY_WEAK(Color.Red), WEAK(WarningOrange), REASONABLE(WarningYellow), STRONG(SimplexGreen);

  companion object {
    fun check(s: String) = with(passphraseEntropy(s)) {
      when {
        this > 100 -> STRONG
        this > 70 -> REASONABLE
        this > 40 -> WEAK
        else -> VERY_WEAK
      }
    }
  }
}

fun validKey(s: String): Boolean {
  for (c in s) {
    if (c.isWhitespace() || !c.isASCII()) {
      return false
    }
  }
  return true
}

private fun Char.isASCII() = code in 32..126

@Preview
@Composable
fun PreviewDatabaseEncryptionLayout() {
  SimpleXTheme {
    DatabaseEncryptionLayout(
      useKeychain = remember { mutableStateOf(true) },
      prefs = AppPreferences(),
      chatDbEncrypted = true,
      currentKey = remember { mutableStateOf("") },
      newKey = remember { mutableStateOf("") },
      confirmNewKey = remember { mutableStateOf("") },
      storedKey = remember { mutableStateOf(true) },
      initialRandomDBPassphrase = remember { mutableStateOf(true) },
      progressIndicator = remember { mutableStateOf(false) },
      migration = false,
      onConfirmEncrypt = {},
    )
  }
}
