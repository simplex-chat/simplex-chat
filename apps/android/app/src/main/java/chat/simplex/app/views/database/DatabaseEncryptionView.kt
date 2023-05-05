package chat.simplex.app.views.database

import SectionBottomSpacer
import SectionItemView
import SectionItemViewSpaceBetween
import SectionTextFooter
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
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.*
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.*
import chat.simplex.app.R
import chat.simplex.app.SimplexApp
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import kotlinx.coroutines.flow.distinctUntilChanged
import kotlinx.datetime.Clock
import kotlin.math.log2

@Composable
fun DatabaseEncryptionView(m: ChatModel) {
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
      onConfirmEncrypt = {
        progressIndicator.value = true
        withApi {
          try {
            prefs.encryptionStartedAt.set(Clock.System.now())
            val error = m.controller.apiStorageEncryption(currentKey.value, newKey.value)
            prefs.encryptionStartedAt.set(null)
            val sqliteError = ((error?.chatError as? ChatError.ChatErrorDatabase)?.databaseError as? DatabaseError.ErrorExport)?.sqliteError
            when {
              sqliteError is SQLiteError.ErrorNotADatabase -> {
                operationEnded(m, progressIndicator) {
                  AlertManager.shared.showAlertMsg(
                    generalGetString(R.string.wrong_passphrase_title),
                    generalGetString(R.string.enter_correct_current_passphrase)
                  )
                }
              }
              error != null -> {
                operationEnded(m, progressIndicator) {
                  AlertManager.shared.showAlertMsg(generalGetString(R.string.error_encrypting_database),
                    "failed to set storage encryption: ${error.responseType} ${error.details}"
                  )
                }
              }
              else -> {
                prefs.initialRandomDBPassphrase.set(false)
                initialRandomDBPassphrase.value = false
                if (useKeychain.value) {
                  DatabaseUtils.ksDatabasePassword.set(newKey.value)
                }
                resetFormAfterEncryption(m, initialRandomDBPassphrase, currentKey, newKey, confirmNewKey, storedKey, useKeychain.value)
                operationEnded(m, progressIndicator) {
                  AlertManager.shared.showAlertMsg(generalGetString(R.string.database_encrypted))
                }
              }
            }
          } catch (e: Exception) {
            operationEnded(m, progressIndicator) {
              AlertManager.shared.showAlertMsg(generalGetString(R.string.error_encrypting_database), e.stackTraceToString())
            }
          }
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
  onConfirmEncrypt: () -> Unit,
) {
  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.database_passphrase))
    SectionView(null) {
      SavePassphraseSetting(useKeychain.value, initialRandomDBPassphrase.value, storedKey.value, progressIndicator.value) { checked ->
        if (checked) {
          setUseKeychain(true, useKeychain, prefs)
        } else if (storedKey.value) {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(R.string.remove_passphrase_from_keychain),
            text = generalGetString(R.string.notifications_will_be_hidden) + "\n" + storeSecurelyDanger(),
            confirmText = generalGetString(R.string.remove_passphrase),
            onConfirm = {
              DatabaseUtils.ksDatabasePassword.remove()
              setUseKeychain(false, useKeychain, prefs)
              storedKey.value = false
            },
            destructive = true,
          )
        } else {
          setUseKeychain(false, useKeychain, prefs)
        }
      }

      if (!initialRandomDBPassphrase.value && chatDbEncrypted == true) {
        PassphraseField(
          currentKey,
          generalGetString(R.string.current_passphrase),
          modifier = Modifier.padding(horizontal = DEFAULT_PADDING),
          isValid = ::validKey,
          keyboardActions = KeyboardActions(onNext = { defaultKeyboardAction(ImeAction.Next) }),
        )
      }

      PassphraseField(
        newKey,
        generalGetString(R.string.new_passphrase),
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
        generalGetString(R.string.confirm_new_passphrase),
        modifier = Modifier.padding(horizontal = DEFAULT_PADDING),
        isValid = { confirmNewKey.value == "" || newKey.value == confirmNewKey.value },
        keyboardActions = KeyboardActions(onDone = {
          if (!disabled) onClickUpdate()
          defaultKeyboardAction(ImeAction.Done)
        }),
      )

      SectionItemViewSpaceBetween(onClickUpdate, disabled = disabled, minHeight = TextFieldDefaults.MinHeight) {
        Text(generalGetString(R.string.update_database_passphrase), color = if (disabled) MaterialTheme.colors.secondary else MaterialTheme.colors.primary)
      }
    }

    Column {
      if (chatDbEncrypted == false) {
        SectionTextFooter(generalGetString(R.string.database_is_not_encrypted))
      } else if (useKeychain.value) {
        if (storedKey.value) {
          SectionTextFooter(generalGetString(R.string.keychain_is_storing_securely))
          if (initialRandomDBPassphrase.value) {
            SectionTextFooter(generalGetString(R.string.encrypted_with_random_passphrase))
          } else {
            SectionTextFooter(generalGetString(R.string.impossible_to_recover_passphrase))
          }
        } else {
          SectionTextFooter(generalGetString(R.string.keychain_allows_to_receive_ntfs))
        }
      } else {
        SectionTextFooter(generalGetString(R.string.you_have_to_enter_passphrase_every_time))
        SectionTextFooter(generalGetString(R.string.impossible_to_recover_passphrase))
      }
    }
    SectionBottomSpacer()
  }
}

fun encryptDatabaseSavedAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.encrypt_database_question),
    text = generalGetString(R.string.database_will_be_encrypted_and_passphrase_stored) + "\n" + storeSecurelySaved(),
    confirmText = generalGetString(R.string.encrypt_database),
    onConfirm = onConfirm,
    destructive = true,
  )
}

fun encryptDatabaseAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.encrypt_database_question),
    text = generalGetString(R.string.database_will_be_encrypted) +"\n" + storeSecurelyDanger(),
    confirmText = generalGetString(R.string.encrypt_database),
    onConfirm = onConfirm,
    destructive = true,
  )
}

fun changeDatabaseKeySavedAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.change_database_passphrase_question),
    text = generalGetString(R.string.database_encryption_will_be_updated) + "\n" + storeSecurelySaved(),
    confirmText = generalGetString(R.string.update_database),
    onConfirm = onConfirm,
    destructive = false,
  )
}

fun changeDatabaseKeyAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.change_database_passphrase_question),
    text = generalGetString(R.string.database_passphrase_will_be_updated) + "\n" + storeSecurelyDanger(),
    confirmText = generalGetString(R.string.update_database),
    onConfirm = onConfirm,
    destructive = true,
  )
}

@Composable
fun SavePassphraseSetting(
  useKeychain: Boolean,
  initialRandomDBPassphrase: Boolean,
  storedKey: Boolean,
  progressIndicator: Boolean,
  minHeight: Dp = TextFieldDefaults.MinHeight,
  onCheckedChange: (Boolean) -> Unit,
) {
  SectionItemView(minHeight = minHeight) {
    Row(verticalAlignment = Alignment.CenterVertically) {
      Icon(
        if (storedKey) painterResource(R.drawable.ic_vpn_key_filled) else painterResource(R.drawable.ic_vpn_key_off_filled),
        stringResource(R.string.save_passphrase_in_keychain),
        tint = if (storedKey) SimplexGreen else MaterialTheme.colors.secondary
      )
      Spacer(Modifier.padding(horizontal = 4.dp))
      Text(
        stringResource(R.string.save_passphrase_in_keychain),
        Modifier.padding(end = 24.dp),
        color = Color.Unspecified
      )
      Spacer(Modifier.fillMaxWidth().weight(1f))
      DefaultSwitch(
        checked = useKeychain,
        onCheckedChange = onCheckedChange,
        enabled = !initialRandomDBPassphrase && !progressIndicator
      )
    }
  }
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
  m.chatDbEncrypted.value = true
  initialRandomDBPassphrase.value = false
  m.controller.appPrefs.initialRandomDBPassphrase.set(false)
  currentKey.value = ""
  newKey.value = ""
  confirmNewKey.value = ""
  storedKey.value = stored
}

fun setUseKeychain(value: Boolean, useKeychain: MutableState<Boolean>, prefs: AppPreferences) {
  useKeychain.value = value
  prefs.storeDBPassphrase.set(value)
}

fun storeSecurelySaved() = generalGetString(R.string.store_passphrase_securely)

fun storeSecurelyDanger() = generalGetString(R.string.store_passphrase_securely_without_recover)

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
    if (showKey) painterResource(R.drawable.ic_visibility_off_filled) else painterResource(R.drawable.ic_visibility_filled)
  } else painterResource(R.drawable.ic_error)
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
      prefs = AppPreferences(SimplexApp.context),
      chatDbEncrypted = true,
      currentKey = remember { mutableStateOf("") },
      newKey = remember { mutableStateOf("") },
      confirmNewKey = remember { mutableStateOf("") },
      storedKey = remember { mutableStateOf(true) },
      initialRandomDBPassphrase = remember { mutableStateOf(true) },
      progressIndicator = remember { mutableStateOf(false) },
      onConfirmEncrypt = {},
    )
  }
}
