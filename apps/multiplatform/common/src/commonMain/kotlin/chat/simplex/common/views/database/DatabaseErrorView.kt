package chat.simplex.common.views.database

import SectionBottomSpacer
import SectionSpacer
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.focus.focusRequester
import androidx.compose.ui.input.key.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.AppPreferences
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.AppVersionText
import chat.simplex.res.MR
import dev.icerock.moko.resources.StringResource
import kotlinx.coroutines.*
import kotlinx.datetime.Clock
import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import kotlin.io.path.Path

@Composable
fun DatabaseErrorView(
  chatDbStatus: State<DBMigrationResult?>,
  appPreferences: AppPreferences,
) {
  val progressIndicator = remember { mutableStateOf(false) }
  val dbKey = remember { mutableStateOf("") }
  var storedDBKey by remember { mutableStateOf(DatabaseUtils.ksDatabasePassword.get()) }
  var useKeychain by remember { mutableStateOf(appPreferences.storeDBPassphrase.get()) }
  val restoreDbFromBackup = remember { mutableStateOf(shouldShowRestoreDbButton(appPreferences)) }

  fun callRunChat(confirmMigrations: MigrationConfirmation? = null) {
    val useKey = if (useKeychain) null else dbKey.value
    runChat(useKey, confirmMigrations, chatDbStatus, progressIndicator, appPreferences)
  }

  fun saveAndRunChatOnClick() {
    DatabaseUtils.ksDatabasePassword.set(dbKey.value)
    storedDBKey = dbKey.value
    appPreferences.storeDBPassphrase.set(true)
    useKeychain = true
    appPreferences.initialRandomDBPassphrase.set(false)
    callRunChat()
  }

  @Composable
  fun DatabaseErrorDetails(title: StringResource, content: @Composable ColumnScope.() -> Unit) {
    Text(
      generalGetString(title),
      Modifier.padding(start = DEFAULT_PADDING, top = DEFAULT_PADDING, bottom = DEFAULT_PADDING),
      style = MaterialTheme.typography.h1
    )
    SectionView(null, padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF), content)
  }

  @Composable
  fun FileNameText(dbFile: String) {
    Text(String.format(generalGetString(MR.strings.file_with_path), dbFile.split("/").lastOrNull() ?: dbFile))
  }

  @Composable
  fun MigrationsText(ms: List<String>) {
    Text(String.format(generalGetString(MR.strings.database_migrations), ms.joinToString(", ")))
  }

  Column(
    Modifier.fillMaxSize().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.Center,
  ) {
    val buttonEnabled = validKey(dbKey.value) && !progressIndicator.value
    when (val status = chatDbStatus.value) {
      is DBMigrationResult.ErrorNotADatabase ->
        if (useKeychain && !storedDBKey.isNullOrEmpty()) {
          DatabaseErrorDetails(MR.strings.wrong_passphrase) {
            Text(generalGetString(MR.strings.passphrase_is_different))
            DatabaseKeyField(dbKey, buttonEnabled) {
              saveAndRunChatOnClick()
            }
            SaveAndOpenButton(buttonEnabled, ::saveAndRunChatOnClick)
            SectionSpacer()
            FileNameText(status.dbFile)
          }
        } else {
          DatabaseErrorDetails(MR.strings.encrypted_database) {
            Text(generalGetString(MR.strings.database_passphrase_is_required))
            if (useKeychain) {
              DatabaseKeyField(dbKey, buttonEnabled, ::saveAndRunChatOnClick)
              SaveAndOpenButton(buttonEnabled, ::saveAndRunChatOnClick)
            } else {
              DatabaseKeyField(dbKey, buttonEnabled) { callRunChat() }
              OpenChatButton(buttonEnabled) { callRunChat() }
            }
          }
        }
      is DBMigrationResult.ErrorMigration -> when (val err = status.migrationError) {
        is MigrationError.Upgrade ->
          DatabaseErrorDetails(MR.strings.database_upgrade) {
            TextButton({ callRunChat(confirmMigrations = MigrationConfirmation.YesUp) }, Modifier.align(Alignment.CenterHorizontally), enabled = !progressIndicator.value) {
              Text(generalGetString(MR.strings.upgrade_and_open_chat))
            }
            Spacer(Modifier.height(20.dp))
            FileNameText(status.dbFile)
            MigrationsText(err.upMigrations.map { it.upName })
            AppVersionText()
          }
        is MigrationError.Downgrade ->
          DatabaseErrorDetails(MR.strings.database_downgrade) {
            TextButton({ callRunChat(confirmMigrations = MigrationConfirmation.YesUpDown) }, Modifier.align(Alignment.CenterHorizontally), enabled = !progressIndicator.value) {
              Text(generalGetString(MR.strings.downgrade_and_open_chat))
            }
            Spacer(Modifier.height(20.dp))
            Text(generalGetString(MR.strings.database_downgrade_warning), fontWeight = FontWeight.Bold)
            FileNameText(status.dbFile)
            MigrationsText(err.downMigrations)
            AppVersionText()
          }
        is MigrationError.Error ->
          DatabaseErrorDetails(MR.strings.incompatible_database_version) {
            FileNameText(status.dbFile)
            Text(String.format(generalGetString(MR.strings.error_with_info), mtrErrorDescription(err.mtrError)))
          }
      }
      is DBMigrationResult.ErrorSQL ->
        DatabaseErrorDetails(MR.strings.database_error) {
          FileNameText(status.dbFile)
          Text(String.format(generalGetString(MR.strings.error_with_info), status.migrationSQLError))
        }
      is DBMigrationResult.ErrorKeychain ->
        DatabaseErrorDetails(MR.strings.keychain_error) {
          Text(generalGetString(MR.strings.cannot_access_keychain))
        }
      is DBMigrationResult.InvalidConfirmation ->
        DatabaseErrorDetails(MR.strings.invalid_migration_confirmation) {
          // this can only happen if incorrect parameter is passed
        }
      is DBMigrationResult.Unknown ->
        DatabaseErrorDetails(MR.strings.database_error) {
          Text(String.format(generalGetString(MR.strings.unknown_database_error_with_info), status.json))
        }
      is DBMigrationResult.OK -> {}
      null -> {}
    }
    if (restoreDbFromBackup.value) {
      SectionSpacer()
      Text(generalGetString(MR.strings.database_backup_can_be_restored))
      Spacer(Modifier.size(DEFAULT_PADDING))
      RestoreDbButton {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(MR.strings.restore_database_alert_title),
          text = generalGetString(MR.strings.restore_database_alert_desc),
          confirmText = generalGetString(MR.strings.restore_database_alert_confirm),
          onConfirm = { restoreDb(restoreDbFromBackup, appPreferences) },
          destructive = true,
        )
      }
    }
    SectionBottomSpacer()
  }
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

private fun runChat(
  dbKey: String? = null,
  confirmMigrations: MigrationConfirmation? = null,
  chatDbStatus: State<DBMigrationResult?>,
  progressIndicator: MutableState<Boolean>,
  prefs: AppPreferences
) = CoroutineScope(Dispatchers.Default).launch {
  // Don't do things concurrently. Shouldn't be here concurrently, just in case
  if (progressIndicator.value) return@launch
  progressIndicator.value = true
  try {
    initChatController(dbKey, confirmMigrations)
  } catch (e: Exception) {
    Log.d(TAG, "initializeChat ${e.stackTraceToString()}")
  }
  progressIndicator.value = false
  when (val status = chatDbStatus.value) {
    is DBMigrationResult.OK -> {
      platform.androidChatStartedAfterBeingOff()
    }
    is DBMigrationResult.ErrorNotADatabase ->
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.wrong_passphrase_title), generalGetString(MR.strings.enter_correct_passphrase))
    is DBMigrationResult.ErrorSQL ->
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.database_error), status.migrationSQLError)
    is DBMigrationResult.ErrorKeychain ->
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.keychain_error))
    is DBMigrationResult.Unknown ->
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.unknown_error), status.json)
    is DBMigrationResult.InvalidConfirmation ->
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.invalid_migration_confirmation))
    is DBMigrationResult.ErrorMigration -> {}
    null -> {}
  }
}

private fun shouldShowRestoreDbButton(prefs: AppPreferences): Boolean {
  val startedAt = prefs.encryptionStartedAt.get() ?: return false
  /** Just in case there is any small difference between reported Java's [Clock.System.now] and Linux's time on a file */
  val safeDiffInTime = 10_000L
  val filesChat = File(dataDir.absolutePath + File.separator + "${chatDatabaseFileName}.bak")
  val filesAgent = File(dataDir.absolutePath + File.separator + "${agentDatabaseFileName}.bak")
  return filesChat.exists() &&
      filesAgent.exists() &&
      startedAt.toEpochMilliseconds() - safeDiffInTime <= filesChat.lastModified() &&
      startedAt.toEpochMilliseconds() - safeDiffInTime <= filesAgent.lastModified()
}

private fun restoreDb(restoreDbFromBackup: MutableState<Boolean>, prefs: AppPreferences) {
  val filesChatBase = dataDir.absolutePath + File.separator + chatDatabaseFileName
  val filesAgentBase = dataDir.absolutePath + File.separator + agentDatabaseFileName
  try {
    Files.copy(Path("$filesChatBase.bak"), Path(filesChatBase), StandardCopyOption.REPLACE_EXISTING)
    Files.copy(Path("$filesAgentBase.bak"), Path(filesAgentBase), StandardCopyOption.REPLACE_EXISTING)
    restoreDbFromBackup.value = false
    prefs.encryptionStartedAt.set(null)
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.database_restore_error), e.stackTraceToString())
  }
}

private fun mtrErrorDescription(err: MTRError): String =
  when (err) {
    is MTRError.NoDown ->
      String.format(generalGetString(MR.strings.mtr_error_no_down_migration), err.dbMigrations.joinToString(", "))
    is MTRError.Different ->
      String.format(generalGetString(MR.strings.mtr_error_different), err.appMigration, err.dbMigration)
  }

@Composable
private fun DatabaseKeyField(text: MutableState<String>, enabled: Boolean, onClick: (() -> Unit)? = null) {
  val focusRequester = remember { FocusRequester() }
  LaunchedEffect(Unit) {
    delay(100L)
    focusRequester.requestFocus()
  }
  PassphraseField(
    text,
    generalGetString(MR.strings.enter_passphrase),
    isValid = ::validKey,
    // Don't enable this on desktop since it interfere with key event listener
    keyboardActions = KeyboardActions(onDone = if (enabled && appPlatform.isAndroid) {
      { onClick?.invoke() }
    } else null
    ),
    modifier = Modifier.focusRequester(focusRequester).onPreviewKeyEvent {
      if (onClick != null && (it.key == Key.Enter || it.key == Key.NumPadEnter) && it.type == KeyEventType.KeyUp) {
        onClick()
        true
      } else {
        false
      }
    }
  )
}

@Composable
private fun ColumnScope.SaveAndOpenButton(enabled: Boolean, onClick: () -> Unit) {
  TextButton(onClick, Modifier.align(Alignment.CenterHorizontally), enabled = enabled) {
    Text(generalGetString(MR.strings.save_passphrase_and_open_chat))
  }
}

@Composable
private fun ColumnScope.OpenChatButton(enabled: Boolean, onClick: () -> Unit) {
  TextButton(onClick, Modifier.align(Alignment.CenterHorizontally), enabled = enabled) {
    Text(generalGetString(MR.strings.open_chat))
  }
}

@Composable
private fun ColumnScope.RestoreDbButton(onClick: () -> Unit) {
  TextButton(onClick, Modifier.align(Alignment.CenterHorizontally)) {
    Text(generalGetString(MR.strings.restore_database), color = MaterialTheme.colors.error)
  }
}

@Preview
@Composable
fun PreviewChatInfoLayout() {
  SimpleXTheme {
    DatabaseErrorView(
      remember { mutableStateOf(DBMigrationResult.ErrorNotADatabase("simplex_v1_chat.db")) },
      AppPreferences()
    )
  }
}
