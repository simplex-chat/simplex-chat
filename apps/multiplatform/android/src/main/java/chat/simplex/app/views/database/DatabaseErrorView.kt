package chat.simplex.app.views.database

import SectionBottomSpacer
import SectionSpacer
import SectionView
import android.content.Context
import android.util.Log
import androidx.annotation.StringRes
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.AppVersionText
import chat.simplex.app.views.usersettings.NotificationsMode
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
  val context = LocalContext.current
  val restoreDbFromBackup = remember { mutableStateOf(shouldShowRestoreDbButton(appPreferences, context)) }

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
  fun DatabaseErrorDetails(@StringRes title: Int, content: @Composable ColumnScope.() -> Unit) {
    Text(
      generalGetString(title),
      Modifier.padding(start = DEFAULT_PADDING, top = DEFAULT_PADDING, bottom = DEFAULT_PADDING),
      style = MaterialTheme.typography.h1
    )
    SectionView(null, padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF), content)
  }

  @Composable
  fun FileNameText(dbFile: String) {
    Text(String.format(generalGetString(R.string.file_with_path), dbFile.split("/").lastOrNull() ?: dbFile))
  }

  @Composable
  fun MigrationsText(ms: List<String>) {
    Text(String.format(generalGetString(R.string.database_migrations), ms.joinToString(", ")))
  }

  Column(
    Modifier.fillMaxSize().verticalScroll(rememberScrollState()),
    verticalArrangement = Arrangement.Center,
  ) {
    val buttonEnabled = validKey(dbKey.value) && !progressIndicator.value
    when (val status = chatDbStatus.value) {
      is DBMigrationResult.ErrorNotADatabase ->
        if (useKeychain && !storedDBKey.isNullOrEmpty()) {
          DatabaseErrorDetails(R.string.wrong_passphrase) {
            Text(generalGetString(R.string.passphrase_is_different))
            DatabaseKeyField(dbKey, buttonEnabled) {
              saveAndRunChatOnClick()
            }
            SaveAndOpenButton(buttonEnabled, ::saveAndRunChatOnClick)
            SectionSpacer()
            FileNameText(status.dbFile)
          }
        } else {
          DatabaseErrorDetails(R.string.encrypted_database) {
            Text(generalGetString(R.string.database_passphrase_is_required))
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
          DatabaseErrorDetails(R.string.database_upgrade) {
            TextButton({ callRunChat(confirmMigrations = MigrationConfirmation.YesUp) }, Modifier.align(Alignment.CenterHorizontally), enabled = !progressIndicator.value) {
              Text(generalGetString(R.string.upgrade_and_open_chat))
            }
            Spacer(Modifier.height(20.dp))
            FileNameText(status.dbFile)
            MigrationsText(err.upMigrations.map { it.upName })
            AppVersionText()
          }
        is MigrationError.Downgrade ->
          DatabaseErrorDetails(R.string.database_downgrade) {
            TextButton({ callRunChat(confirmMigrations = MigrationConfirmation.YesUpDown) }, Modifier.align(Alignment.CenterHorizontally), enabled = !progressIndicator.value) {
              Text(generalGetString(R.string.downgrade_and_open_chat))
            }
            Spacer(Modifier.height(20.dp))
            Text(generalGetString(R.string.database_downgrade_warning), fontWeight = FontWeight.Bold)
            FileNameText(status.dbFile)
            MigrationsText(err.downMigrations)
            AppVersionText()
          }
        is MigrationError.Error ->
          DatabaseErrorDetails(R.string.incompatible_database_version) {
            FileNameText(status.dbFile)
            Text(String.format(generalGetString(R.string.error_with_info), mtrErrorDescription(err.mtrError)))
          }
      }
      is DBMigrationResult.ErrorSQL ->
        DatabaseErrorDetails(R.string.database_error) {
          FileNameText(status.dbFile)
          Text(String.format(generalGetString(R.string.error_with_info), status.migrationSQLError))
        }
      is DBMigrationResult.ErrorKeychain ->
        DatabaseErrorDetails(R.string.keychain_error) {
          Text(generalGetString(R.string.cannot_access_keychain))
        }
      is DBMigrationResult.InvalidConfirmation ->
        DatabaseErrorDetails(R.string.invalid_migration_confirmation) {
          // this can only happen if incorrect parameter is passed
        }
      is DBMigrationResult.Unknown ->
        DatabaseErrorDetails(R.string.database_error) {
          Text(String.format(generalGetString(R.string.unknown_database_error_with_info), status.json))
        }
      is DBMigrationResult.OK -> {}
      null -> {}
    }
    if (restoreDbFromBackup.value) {
      SectionSpacer()
      Text(generalGetString(R.string.database_backup_can_be_restored))
      Spacer(Modifier.size(DEFAULT_PADDING))
      RestoreDbButton {
        AlertManager.shared.showAlertDialog(
          title = generalGetString(R.string.restore_database_alert_title),
          text = generalGetString(R.string.restore_database_alert_desc),
          confirmText = generalGetString(R.string.restore_database_alert_confirm),
          onConfirm = { restoreDb(restoreDbFromBackup, appPreferences, context) },
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
    SimplexApp.context.initChatController(dbKey, confirmMigrations)
  } catch (e: Exception) {
    Log.d(TAG, "initializeChat ${e.stackTraceToString()}")
  }
  progressIndicator.value = false
  when (val status = chatDbStatus.value) {
    is DBMigrationResult.OK -> {
      SimplexService.cancelPassphraseNotification()
      when (prefs.notificationsMode.get()) {
        NotificationsMode.SERVICE.name -> CoroutineScope(Dispatchers.Default).launch { SimplexService.start(SimplexApp.context) }
        NotificationsMode.PERIODIC.name -> SimplexApp.context.schedulePeriodicWakeUp()
      }
    }
    is DBMigrationResult.ErrorNotADatabase ->
      AlertManager.shared.showAlertMsg(generalGetString(R.string.wrong_passphrase_title), generalGetString(R.string.enter_correct_passphrase))
    is DBMigrationResult.ErrorSQL ->
      AlertManager.shared.showAlertMsg(generalGetString(R.string.database_error), status.migrationSQLError)
    is DBMigrationResult.ErrorKeychain ->
      AlertManager.shared.showAlertMsg(generalGetString(R.string.keychain_error))
    is DBMigrationResult.Unknown ->
      AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), status.json)
    is DBMigrationResult.InvalidConfirmation ->
      AlertManager.shared.showAlertMsg(generalGetString(R.string.invalid_migration_confirmation))
    is DBMigrationResult.ErrorMigration -> {}
    null -> {}
  }
}

private fun shouldShowRestoreDbButton(prefs: AppPreferences, context: Context): Boolean {
  val startedAt = prefs.encryptionStartedAt.get() ?: return false
  /** Just in case there is any small difference between reported Java's [Clock.System.now] and Linux's time on a file */
  val safeDiffInTime = 10_000L
  val filesChat = File(context.dataDir.absolutePath + File.separator + "files_chat.db.bak")
  val filesAgent = File(context.dataDir.absolutePath + File.separator + "files_agent.db.bak")
  return filesChat.exists() &&
      filesAgent.exists() &&
      startedAt.toEpochMilliseconds() - safeDiffInTime <= filesChat.lastModified() &&
      startedAt.toEpochMilliseconds() - safeDiffInTime <= filesAgent.lastModified()
}

private fun restoreDb(restoreDbFromBackup: MutableState<Boolean>, prefs: AppPreferences, context: Context) {
  val filesChatBase = context.dataDir.absolutePath + File.separator + "files_chat.db"
  val filesAgentBase = context.dataDir.absolutePath + File.separator + "files_agent.db"
  try {
    Files.copy(Path("$filesChatBase.bak"), Path(filesChatBase), StandardCopyOption.REPLACE_EXISTING)
    Files.copy(Path("$filesAgentBase.bak"), Path(filesAgentBase), StandardCopyOption.REPLACE_EXISTING)
    restoreDbFromBackup.value = false
    prefs.encryptionStartedAt.set(null)
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(generalGetString(R.string.database_restore_error), e.stackTraceToString())
  }
}

private fun mtrErrorDescription(err: MTRError): String =
  when (err) {
    is MTRError.NoDown ->
      String.format(generalGetString(R.string.mtr_error_no_down_migration), err.dbMigrations.joinToString(", "))
    is MTRError.Different ->
      String.format(generalGetString(R.string.mtr_error_different), err.appMigration, err.dbMigration)
  }

@Composable
private fun DatabaseKeyField(text: MutableState<String>, enabled: Boolean, onClick: (() -> Unit)? = null) {
  PassphraseField(
    text,
    generalGetString(R.string.enter_passphrase),
    isValid = ::validKey,
    keyboardActions = KeyboardActions(onDone = if (enabled) {
      { onClick?.invoke() }
    } else null
    )
  )
}

@Composable
private fun ColumnScope.SaveAndOpenButton(enabled: Boolean, onClick: () -> Unit) {
  TextButton(onClick, Modifier.align(Alignment.CenterHorizontally), enabled = enabled) {
    Text(generalGetString(R.string.save_passphrase_and_open_chat))
  }
}

@Composable
private fun ColumnScope.OpenChatButton(enabled: Boolean, onClick: () -> Unit) {
  TextButton(onClick, Modifier.align(Alignment.CenterHorizontally), enabled = enabled) {
    Text(generalGetString(R.string.open_chat))
  }
}

@Composable
private fun ColumnScope.RestoreDbButton(onClick: () -> Unit) {
  TextButton(onClick, Modifier.align(Alignment.CenterHorizontally)) {
    Text(generalGetString(R.string.restore_database), color = MaterialTheme.colors.error)
  }
}

@Preview
@Composable
fun PreviewChatInfoLayout() {
  SimpleXTheme {
    DatabaseErrorView(
      remember { mutableStateOf(DBMigrationResult.ErrorNotADatabase("simplex_v1_chat.db")) },
      AppPreferences(SimplexApp.context)
    )
  }
}
