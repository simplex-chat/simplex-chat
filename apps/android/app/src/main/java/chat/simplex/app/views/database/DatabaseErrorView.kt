package chat.simplex.app.views.database

import SectionSpacer
import SectionView
import android.util.Log
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.KeyboardActions
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.AppPreferences
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.NotificationsMode
import kotlinx.coroutines.*

@Composable
fun DatabaseErrorView(
  chatDbStatus: State<DBMigrationResult?>,
  appPreferences: AppPreferences,
) {
  val dbKey = remember { mutableStateOf("") }
  var storedDBKey by remember { mutableStateOf(DatabaseUtils.getDatabaseKey()) }
  var useKeychain by remember { mutableStateOf(appPreferences.storeDBPassphrase.get()) }
  val saveAndRunChatOnClick: () -> Unit = {
    DatabaseUtils.setDatabaseKey(dbKey.value)
    storedDBKey = dbKey.value
    appPreferences.storeDBPassphrase.set(true)
    useKeychain = true
    appPreferences.initialRandomDBPassphrase.set(false)
    runChat(dbKey.value, chatDbStatus, appPreferences)
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
    Modifier.fillMaxWidth().fillMaxHeight().verticalScroll(rememberScrollState()),
    horizontalAlignment = Alignment.Start,
    verticalArrangement = Arrangement.Center,
  ) {
    Text(
      title,
      Modifier.padding(start = 16.dp, top = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SectionView(null) {
      Column(
        Modifier.padding(horizontal = 8.dp, vertical = 8.dp)
      ) {
        val buttonEnabled = validKey(dbKey.value)
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
                if (useKeychain) saveAndRunChatOnClick() else runChat(dbKey.value, chatDbStatus, appPreferences)
              }
              if (useKeychain) {
                SaveAndOpenButton(buttonEnabled, saveAndRunChatOnClick)
              } else {
                OpenChatButton(buttonEnabled) { runChat(dbKey.value, chatDbStatus, appPreferences) }
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
      }
    }
  }
}

private fun runChat(dbKey: String, chatDbStatus: State<DBMigrationResult?>, prefs: AppPreferences) {
  try {
    SimplexApp.context.initChatController(dbKey)
  } catch (e: Exception) {
    Log.d(TAG, "initializeChat ${e.stackTraceToString()}")
  }
  when (val status = chatDbStatus.value) {
    is DBMigrationResult.OK -> {
      SimplexService.cancelPassphraseNotification()
      when (prefs.notificationsMode.get()) {
        NotificationsMode.SERVICE.name -> CoroutineScope(Dispatchers.Default).launch { SimplexService.start(SimplexApp.context) }
        NotificationsMode.PERIODIC.name -> SimplexApp.context.schedulePeriodicWakeUp()
      }
    }
    is DBMigrationResult.ErrorNotADatabase -> {
      AlertManager.shared.showAlertMsg( generalGetString(R.string.wrong_passphrase_title),  generalGetString(R.string.enter_correct_passphrase))
    }
    is DBMigrationResult.Error -> {
      AlertManager.shared.showAlertMsg( generalGetString(R.string.database_error), status.migrationError)
    }
    is DBMigrationResult.ErrorKeychain -> {
      AlertManager.shared.showAlertMsg( generalGetString(R.string.keychain_error))
    }
    is DBMigrationResult.Unknown -> {
      AlertManager.shared.showAlertMsg( generalGetString(R.string.unknown_error), status.json)
    }
    null -> {}
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
