package chat.simplex.app.views.database

import SectionSpacer
import SectionView
import android.content.Context
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
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
  var storedDBKey by remember { mutableStateOf(DatabaseUtils.getDatabaseKey()) }
  var useKeychain by remember { mutableStateOf(appPreferences.storeDBPassphrase.get()) }
  val context = LocalContext.current
  val restoreDbFromBackup = remember { mutableStateOf(shouldShowRestoreDbButton(appPreferences, context)) }
  val saveAndRunChatOnClick: () -> Unit = {
    DatabaseUtils.setDatabaseKey(dbKey.value)
    storedDBKey = dbKey.value
    appPreferences.storeDBPassphrase.set(true)
    useKeychain = true
    appPreferences.initialRandomDBPassphrase.set(false)
    runChat(dbKey.value, chatDbStatus, progressIndicator, appPreferences)
  }
  val title = when (chatDbStatus.value) {
    is DBMigrationResult.OK -> ""
    is DBMigrationResult.ErrorNotADatabase -> if (useKeychain && !storedDBKey.isNullOrEmpty())
      generalGetString(R.string.wrong_passphrase)
    else
      generalGetString(R.string.encrypted_database)
    is DBMigrationResult.Error -> generalGetString(R.string.database_error)
    is DBMigrationResult.ErrorKeychain -> generalGetString(R.string.keychain_error)
    is DBMigrationResult.Unknown -> generalGetString(R.string.database_error)
    null -> "" // should never be here
  }

  Column(
    Modifier.fillMaxSize().verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.Center,
  ) {
    Text(
      title,
      Modifier.padding(start = 16.dp, top = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionView(null, padding = PaddingValues(horizontal = DEFAULT_PADDING, vertical = DEFAULT_PADDING_HALF)) {
      val buttonEnabled = validKey(dbKey.value) && !progressIndicator.value
      when (val status = chatDbStatus.value) {
        is DBMigrationResult.ErrorNotADatabase -> {
          if (useKeychain && !storedDBKey.isNullOrEmpty()) {
            Text(generalGetString(R.string.passphrase_is_different))
            DatabaseKeyField(dbKey, buttonEnabled) {
              saveAndRunChatOnClick()
            }
            SaveAndOpenButton(buttonEnabled, saveAndRunChatOnClick)
            SectionSpacer()
            Text(String.format(generalGetString(R.string.file_with_path), status.dbFile))
          } else {
            Text(generalGetString(R.string.database_passphrase_is_required))
            DatabaseKeyField(dbKey, buttonEnabled) {
              if (useKeychain) saveAndRunChatOnClick() else runChat(dbKey.value, chatDbStatus, progressIndicator, appPreferences)
            }
            if (useKeychain) {
              SaveAndOpenButton(buttonEnabled, saveAndRunChatOnClick)
            } else {
              OpenChatButton(buttonEnabled) { runChat(dbKey.value, chatDbStatus, progressIndicator, appPreferences) }
            }
          }
        }
        is DBMigrationResult.Error -> {
          Text(String.format(generalGetString(R.string.file_with_path), status.dbFile))
          Text(String.format(generalGetString(R.string.error_with_info), status.migrationError))
        }
        is DBMigrationResult.ErrorKeychain -> {
          Text(generalGetString(R.string.cannot_access_keychain))
        }
        is DBMigrationResult.Unknown -> {
          Text(String.format(generalGetString(R.string.unknown_database_error_with_info), status.json))
        }
        is DBMigrationResult.OK -> {
        }
        null -> {
        }
      }
      if (restoreDbFromBackup.value) {
        SectionSpacer()
        Text(generalGetString(R.string.database_backup_can_be_restored))
        Spacer(Modifier.size(16.dp))
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
    }
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
        color = HighOrLowlight,
        strokeWidth = 2.5.dp
      )
    }
  }
}

private fun runChat(
  dbKey: String,
  chatDbStatus: State<DBMigrationResult?>,
  progressIndicator: MutableState<Boolean>,
  prefs: AppPreferences
) = CoroutineScope(Dispatchers.Default).launch {
  // Don't do things concurrently. Shouldn't be here concurrently, just in case
  if (progressIndicator.value) return@launch
  progressIndicator.value = true
  try {
    SimplexApp.context.initChatController(dbKey)
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
    is DBMigrationResult.ErrorNotADatabase -> {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.wrong_passphrase_title), generalGetString(R.string.enter_correct_passphrase))
    }
    is DBMigrationResult.Error -> {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.database_error), status.migrationError)
    }
    is DBMigrationResult.ErrorKeychain -> {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.keychain_error))
    }
    is DBMigrationResult.Unknown -> {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.unknown_error), status.json)
    }
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

@Composable
private fun DatabaseKeyField(text: MutableState<String>, enabled: Boolean, onClick: (() -> Unit)? = null) {
  DatabaseKeyField(
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
