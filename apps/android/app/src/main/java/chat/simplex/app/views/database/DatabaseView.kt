package chat.simplex.app.views.database

import android.content.Context
import android.content.res.Configuration
import android.net.Uri
import android.os.FileUtils
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.material.icons.Icons
import androidx.compose.material.icons.filled.PlayArrow
import androidx.compose.material.icons.filled.Report
import androidx.compose.material.icons.outlined.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.*
import kotlinx.datetime.*
import java.io.*
import java.text.SimpleDateFormat
import java.util.*

@Composable
fun DatabaseView(
  m: ChatModel,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)
) {
  val context = LocalContext.current
  val progressIndicator = remember { mutableStateOf(false) }
  val runChat = remember { mutableStateOf(false) }
  val chatArchiveName = remember { mutableStateOf(m.controller.appPrefs.chatArchiveName.get()) }
  val chatArchiveTime = remember { mutableStateOf(m.controller.appPrefs.chatArchiveTime.get()) }
  val chatLastStart = remember { mutableStateOf(m.controller.appPrefs.chatLastStart.get()) }
  val chatArchiveFile = remember { mutableStateOf<String?>(null) }
  val saveArchiveLauncher = rememberSaveArchiveLauncher(cxt = context, chatArchiveFile)
  val importedArchiveUri = remember { mutableStateOf<Uri?>(null) }
  LaunchedEffect(m.chatRunning) {
    runChat.value = m.chatRunning.value ?: true
  }
  LaunchedEffect(chatArchiveFile.value) {
    val chatArchiveFileVal = chatArchiveFile.value
    if (chatArchiveFileVal != null) {
      saveArchiveLauncher.launch(chatArchiveFileVal.substringAfterLast("/"))
      progressIndicator.value = false
    }
  }
  LaunchedEffect(importedArchiveUri.value) {
    val importedArchiveUriVal = importedArchiveUri.value
    if (importedArchiveUriVal != null) {
      importArchiveAlert(m, context, importedArchiveUri, progressIndicator)
    }
  }
  Box(
    Modifier.fillMaxSize(),
  ) {
    DatabaseLayout(
      progressIndicator.value,
      runChat.value,
      m.chatDbChanged.value,
      importedArchiveUri,
      chatArchiveName,
      chatArchiveTime,
      chatLastStart,
      startChat = { startChat(m, runChat) },
      stopChatAlert = { stopChatAlert(m, runChat) },
      exportArchive = { exportArchive(context, m, progressIndicator, chatArchiveName, chatArchiveTime, chatArchiveFile) },
      deleteChatAlert = { deleteChatAlert(m, progressIndicator) },
      showSettingsModal
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
          color = HighOrLowlight,
          strokeWidth = 2.5.dp
        )
      }
    }
  }
}

@Composable
fun DatabaseLayout(
  progressIndicator: Boolean,
  runChat: Boolean,
  chatDbChanged: Boolean,
  importedArchiveUri: MutableState<Uri?>,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatLastStart: MutableState<Instant?>,
  startChat: () -> Unit,
  stopChatAlert: () -> Unit,
  exportArchive: () -> Unit,
  deleteChatAlert: () -> Unit,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)
) {
  val stopped = !runChat
  val importArchiveLauncher = rememberImportArchiveLauncher(importedArchiveUri)

  Column(
    Modifier.fillMaxWidth(),
    horizontalAlignment = Alignment.Start,
  ) {
    @Composable fun divider() = Divider(Modifier.padding(horizontal = 8.dp))
    Text(
      stringResource(R.string.your_chat_database),
      Modifier.padding(start = 16.dp, bottom = 24.dp),
      style = MaterialTheme.typography.h1
    )
    SettingsSectionView(stringResource(R.string.run_chat_section)) {
      Row(
        Modifier.padding(start = 10.dp).fillMaxWidth(),
        verticalAlignment = Alignment.CenterVertically
      ) {
        val chatRunningText = if (stopped) stringResource(R.string.chat_is_stopped) else stringResource(R.string.chat_is_running)
        Icon(
          if (stopped) Icons.Filled.Report else Icons.Filled.PlayArrow,
          chatRunningText,
          tint = if (chatDbChanged) HighOrLowlight else if (stopped) Color.Red else MaterialTheme.colors.primary
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          chatRunningText,
          Modifier.padding(end = 24.dp),
          color = if (chatDbChanged) HighOrLowlight else Color.Unspecified
        )
        Spacer(Modifier.fillMaxWidth().weight(1f))
        Switch(
          checked = runChat,
          onCheckedChange = { runChatSwitch ->
            if (runChatSwitch) {
              startChat()
            } else {
              stopChatAlert()
            }
          },
          colors = SwitchDefaults.colors(
            checkedThumbColor = MaterialTheme.colors.primary,
            uncheckedThumbColor = HighOrLowlight
          ),
          enabled = !chatDbChanged
        )
      }
    }
    Spacer(Modifier.height(30.dp))

    SettingsSectionView(stringResource(R.string.chat_database_section)) {
      SettingsActionItem(
        Icons.Outlined.IosShare,
        stringResource(R.string.export_database),
        { exportArchive() },
        textColor = MaterialTheme.colors.primary,
        disabled = !stopped || progressIndicator || chatDbChanged
      )
      divider()
      SettingsActionItem(
        Icons.Outlined.FileDownload,
        stringResource(R.string.import_database),
        { importArchiveLauncher.launch("application/zip") },
        textColor = Color.Red,
        disabled = !stopped || progressIndicator || chatDbChanged
      )
      divider()
      val chatArchiveNameVal = chatArchiveName.value
      val chatArchiveTimeVal = chatArchiveTime.value
      val chatLastStartVal = chatLastStart.value
      if (chatArchiveNameVal != null && chatArchiveTimeVal != null && chatLastStartVal != null) {
        val title = chatArchiveTitle(chatArchiveTimeVal, chatLastStartVal)
        SettingsActionItem(
          Icons.Outlined.Inventory2,
          title,
          click = showSettingsModal { ChatArchiveView(it, title, chatArchiveNameVal, chatArchiveTimeVal) },
          disabled = !stopped || progressIndicator || chatDbChanged
        )
        divider()
      }
      SettingsActionItem(
        Icons.Outlined.DeleteForever,
        stringResource(R.string.delete_database),
        deleteChatAlert,
        textColor = Color.Red,
        disabled = !stopped || progressIndicator || chatDbChanged
      )
    }
    SettingsSectionFooter(
      if (chatDbChanged) {
        stringResource(R.string.restart_the_app_to_use_new_chat_database)
      } else {
        if (stopped) {
          stringResource(R.string.you_must_use_the_most_recent_version_of_database)
        } else {
          stringResource(R.string.stop_chat_to_enable_database_actions)
        }
      }
    )
  }
}

@Composable
fun chatArchiveTitle(chatArchiveTime: Instant, chatLastStart: Instant): String {
  return stringResource(if (chatArchiveTime < chatLastStart) R.string.old_database_archive else R.string.new_database_archive)
}

@Composable
fun SettingsSectionFooter(text: String) {
  Text(text, color = HighOrLowlight, modifier = Modifier.padding(start = 16.dp, top = 5.dp).fillMaxWidth(0.9F), fontSize = 12.sp)
}

private fun startChat(m: ChatModel, runChat: MutableState<Boolean>) {
  if (m.chatDbChanged.value) {
    ModalManager.shared.closeModals()
    // TODO resetChatCtrl, initializeChat
  } else {
    withApi {
      try {
        m.controller.apiStartChat()
        runChat.value = true
        m.chatRunning.value = true
        // TODO start recvMspLoop
        m.controller.appPrefs.chatLastStart.set(Clock.System.now())
      } catch (e: Error) {
        runChat.value = false
        AlertManager.shared.showAlertMsg(generalGetString(R.string.error_starting_chat), e.toString())
      }
    }
  }
}

private fun stopChatAlert(m: ChatModel, runChat: MutableState<Boolean>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.stop_chat_question),
    text = generalGetString(R.string.stop_chat_to_export_import_or_delete_chat_database),
    confirmText = generalGetString(R.string.stop_chat_confirmation),
    onConfirm = { stopChat(m, runChat) },
    onDismiss = { runChat.value = true }
  )
}

private fun stopChat(m: ChatModel, runChat: MutableState<Boolean>) {
  withApi {
    try {
      m.controller.apiStopChat()
      // TODO stop recvMspLoop
      runChat.value = false
      m.chatRunning.value = false
    } catch (e: Error) {
      runChat.value = true
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_starting_chat), e.toString())
    }
  }
}

private fun exportArchive(
  context: Context,
  m: ChatModel,
  progressIndicator: MutableState<Boolean>,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatArchiveFile: MutableState<String?>
) {
  progressIndicator.value = true
  withApi {
    try {
      chatArchiveFile.value = exportChatArchive(m, context, chatArchiveName, chatArchiveTime, chatArchiveFile)
      progressIndicator.value = false
    } catch (e: Error) {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_exporting_chat_database), e.toString())
      progressIndicator.value = false
    }
  }
}

private suspend fun exportChatArchive(
  m: ChatModel,
  context: Context,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatArchiveFile: MutableState<String?>
): String {
  val archiveTime = Clock.System.now()
  val ts = SimpleDateFormat("yyyy-MM-dd'T'HHmmss", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
  val archiveName = "simplex-chat.$ts.zip"
  val archivePath = "${getFilesDirectory(context)}/$archiveName"
  val config = ArchiveConfig(archivePath, parentTempDirectory = context.cacheDir.toString())
  m.controller.apiExportArchive(config)
  deleteOldArchive(m, context)
  m.controller.appPrefs.chatArchiveName.set(archiveName)
  chatArchiveName.value = archiveName
  m.controller.appPrefs.chatArchiveTime.set(archiveTime)
  chatArchiveTime.value = archiveTime
  chatArchiveFile.value = archivePath
  return archivePath
}

private fun deleteOldArchive(m: ChatModel, context: Context) {
  val chatArchiveName = m.controller.appPrefs.chatArchiveName.get()
  if (chatArchiveName != null) {
    val file = File("${getFilesDirectory(context)}/$chatArchiveName")
    val fileDeleted = file.delete()
    if (fileDeleted) {
      m.controller.appPrefs.chatArchiveName.set(null)
      m.controller.appPrefs.chatArchiveTime.set(null)
    } else {
      Log.e(TAG, "deleteOldArchive file.delete() error")
    }
  }
}

@Composable
private fun rememberSaveArchiveLauncher(cxt: Context, chatArchiveFile: MutableState<String?>): ManagedActivityResultLauncher<String, Uri?> =
  rememberLauncherForActivityResult(
    contract = ActivityResultContracts.CreateDocument(),
    onResult = { destination ->
      try {
        destination?.let {
          val filePath = chatArchiveFile.value
          if (filePath != null) {
            val contentResolver = cxt.contentResolver
            contentResolver.openOutputStream(destination)?.let { stream ->
              val outputStream = BufferedOutputStream(stream)
              val file = File(filePath)
              outputStream.write(file.readBytes())
              outputStream.close()
              Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
            }
          } else {
            Toast.makeText(cxt, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
          }
        }
      } finally {
        chatArchiveFile.value = null
      }
    }
  )

@Composable
private fun rememberImportArchiveLauncher(importedArchiveUri: MutableState<Uri?>): ManagedActivityResultLauncher<String, Uri?> =
  rememberGetContentLauncher { uri: Uri? ->
    if (uri != null) {
      importedArchiveUri.value = uri
    }
  }

private fun importArchiveAlert(m: ChatModel, context: Context, importedArchiveUri: MutableState<Uri?>, progressIndicator: MutableState<Boolean>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.import_database_question),
    text = generalGetString(R.string.your_current_chat_database_will_be_deleted_and_replaced_with_the_imported_one),
    confirmText = generalGetString(R.string.import_database_confirmation),
    onConfirm = { importArchive(m, context, importedArchiveUri, progressIndicator) }
  )
}

private fun importArchive(m: ChatModel, context: Context, importedArchiveUri: MutableState<Uri?>, progressIndicator: MutableState<Boolean>) {
  progressIndicator.value = true
  val importedArchiveUriVal = importedArchiveUri.value
  if (importedArchiveUriVal != null) {
    val archivePath = saveArchiveFromUri(context, importedArchiveUriVal)
    if (archivePath != null) {
      withApi {
        try {
          m.controller.apiDeleteStorage()
          try {
            val config = ArchiveConfig(archivePath, parentTempDirectory = context.cacheDir.toString())
            m.controller.apiImportArchive(config)
            operationEnded(m, progressIndicator) {
              AlertManager.shared.showAlertMsg(generalGetString(R.string.chat_database_imported), generalGetString(R.string.restart_the_app_to_use_imported_chat_database))
            }
          } catch (e: Error) {
            operationEnded(m, progressIndicator) {
              AlertManager.shared.showAlertMsg(generalGetString(R.string.error_importing_database), e.toString())
            }
          }
        } catch (e: Error) {
          operationEnded(m, progressIndicator) {
            AlertManager.shared.showAlertMsg(generalGetString(R.string.error_deleting_database), e.toString())
          }
        } finally {
          importedArchiveUri.value = null
          File(archivePath).delete()
        }
      }
    }
  } else {
    AlertManager.shared.showAlertMsg(generalGetString(R.string.error_importing_database), generalGetString(R.string.imported_archive_not_found))
  }
  importedArchiveUri.value = null
}

private fun saveArchiveFromUri(context: Context, importedArchiveUri: Uri): String? {
  return try {
    val inputStream = context.contentResolver.openInputStream(importedArchiveUri)
    val archiveName = getFileName(context, importedArchiveUri)
    if (inputStream != null && archiveName != null) {
      val archivePath = "${context.cacheDir}/$archiveName"
      val destFile = File(archivePath)
      FileUtils.copy(inputStream, FileOutputStream(destFile))
      archivePath
    } else {
      Log.e(TAG, "saveArchiveFromUri null inputStream")
      null
    }
  } catch (e: Exception) {
    Log.e(TAG, "saveArchiveFromUri error: ${e.message}")
    null
  }
}

private fun deleteChatAlert(m: ChatModel, progressIndicator: MutableState<Boolean>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.delete_chat_profile_question),
    text = generalGetString(R.string.delete_chat_profile_action_cannot_be_undone_warning),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = { deleteChat(m, progressIndicator) }
  )
}

private fun deleteChat(m: ChatModel, progressIndicator: MutableState<Boolean>) {
  progressIndicator.value = true
  withApi {
    try {
      m.controller.apiDeleteStorage()
      operationEnded(m, progressIndicator) {
        AlertManager.shared.showAlertMsg(generalGetString(R.string.chat_database_deleted), generalGetString(R.string.restart_the_app_to_create_a_new_chat_profile))
      }
    } catch (e: Error) {
      operationEnded(m, progressIndicator) {
        AlertManager.shared.showAlertMsg(generalGetString(R.string.error_deleting_database), e.toString())
      }
    }
  }
}

private fun operationEnded(m: ChatModel, progressIndicator: MutableState<Boolean>, alert: () -> Unit) {
  m.chatDbChanged.value = true
  progressIndicator.value = false
  alert.invoke()
}

@Preview(showBackground = true)
@Preview(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)
@Composable
fun PreviewDatabaseLayout() {
  SimpleXTheme {
    DatabaseLayout(
      progressIndicator = false,
      runChat = true,
      chatDbChanged = false,
      importedArchiveUri = remember { mutableStateOf(null) },
      chatArchiveName = remember { mutableStateOf("dummy_archive") },
      chatArchiveTime = remember { mutableStateOf(Clock.System.now()) },
      chatLastStart = remember { mutableStateOf(Clock.System.now()) },
      startChat = {},
      stopChatAlert = {},
      exportArchive = {},
      deleteChatAlert = {},
      showSettingsModal = { {} }
    )
  }
}
