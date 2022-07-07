package chat.simplex.app.views.database

import android.content.Context
import android.content.res.Configuration
import android.net.Uri
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
import chat.simplex.app.R
import chat.simplex.app.TAG
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.HighOrLowlight
import chat.simplex.app.ui.theme.SimpleXTheme
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.*
import kotlinx.datetime.*
import java.io.BufferedOutputStream
import java.io.File
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.util.*

@Composable
fun DatabaseView(
  m: ChatModel,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)
) {
  val context = LocalContext.current
  val progressIndicator = remember { mutableStateOf(false) }
  val runChat = remember { mutableStateOf(false) }
  val chatArchiveFile = remember { mutableStateOf<String?>(null) }
  val saveFileLauncher = rememberSaveArchiveLauncher(cxt = context, chatArchiveFile)
  val showFileImporter = remember { mutableStateOf(false) } // TODO LaunchedEffect
  LaunchedEffect(m.chatRunning) {
    runChat.value = m.chatRunning.value ?: true
  }
  LaunchedEffect(chatArchiveFile.value) {
    val chatArchiveFileVal = chatArchiveFile.value
    if (chatArchiveFileVal != null) {
      saveFileLauncher.launch(chatArchiveFileVal.substringAfterLast("/"))
    }
  }
  DatabaseLayout(
    progressIndicator,
    runChat,
    showFileImporter,
    chatRunning = m.chatRunning,
    chatArchiveName = remember { mutableStateOf(m.controller.appPrefs.chatArchiveName.get()) },
    chatArchiveTime = remember { mutableStateOf(m.controller.appPrefs.chatArchiveTime.get()) },
    chatLastStart = remember { mutableStateOf(m.controller.appPrefs.chatLastStart.get()) },
    showSettingsModal,
    startChat = { startChat(m, runChat) },
    stopChatAlert = { stopChatAlert(m, runChat) },
    exportArchive = { exportArchive(context, m, progressIndicator, chatArchiveFile) }
  )
}

@Composable
fun rememberSaveArchiveLauncher(cxt: Context, chatArchiveFile: MutableState<String?>): ManagedActivityResultLauncher<String, Uri?> =
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
fun DatabaseLayout(
  progressIndicator: MutableState<Boolean>,
  runChat: MutableState<Boolean>,
  showFileImporter: MutableState<Boolean>,
  chatRunning: MutableState<Boolean?>,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatLastStart: MutableState<Instant?>,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  startChat: () -> Unit,
  stopChatAlert: () -> Unit,
  exportArchive: () -> Unit
) {
  Box(
    Modifier.fillMaxSize(),
  ) {
    ChatDatabaseView(
      runChat,
      showFileImporter,
      chatRunning,
      chatArchiveName,
      chatArchiveTime,
      chatLastStart,
      showSettingsModal,
      startChat,
      stopChatAlert,
      exportArchive
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
fun ChatDatabaseView(
  runChat: MutableState<Boolean>,
  showFileImporter: MutableState<Boolean>,
  chatRunning: MutableState<Boolean?>,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatLastStart: MutableState<Instant?>,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit),
  startChat: () -> Unit,
  stopChatAlert: () -> Unit,
  exportArchive: () -> Unit
) {
  val stopped = chatRunning.value == false

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
          tint = if (stopped) Color.Red else MaterialTheme.colors.primary
        )
        Spacer(Modifier.padding(horizontal = 4.dp))
        Text(
          chatRunningText,
          Modifier.padding(end = 24.dp)
        )
        Spacer(Modifier.fillMaxWidth().weight(1f))
        Switch(
          checked = runChat.value,
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
        disabled = !stopped
      )
      divider()
      SettingsActionItem(
        Icons.Outlined.FileDownload,
        stringResource(R.string.import_database),
        { showFileImporter.value = true },
        textColor = Color.Red,
        disabled = !stopped
      )
      divider()
      val chatArchiveTimeVal = chatArchiveTime.value
      val chatLastStartVal = chatLastStart.value
      if (chatArchiveName.value != null && chatArchiveTimeVal != null && chatLastStartVal != null) {
        val title = if (chatArchiveTimeVal < chatLastStartVal) stringResource(R.string.old_database_archive) else stringResource(R.string.new_database_archive)
        SettingsActionItem(
          Icons.Outlined.Inventory2,
          title,
          showSettingsModal { ChatArchiveView() },
          disabled = !stopped
        )
        divider()
      }
      SettingsActionItem(
        Icons.Outlined.Delete,
        stringResource(R.string.delete_database),
        ::deleteChatAlert,
        textColor = Color.Red,
        disabled = !stopped
      )
    }
  }
}

private fun startChat(m: ChatModel, runChat: MutableState<Boolean>) {
  if (m.chatDbChanged.value) {
    // TODO
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

private fun stopChatAlert(m: ChatModel, runChat: MutableState<Boolean>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.stop_chat_question),
    text = generalGetString(R.string.stop_chat_to_export_import_or_delete_chat_database),
    confirmText = generalGetString(R.string.stop_chat_confirmation),
    onConfirm = { stopChat(m, runChat) },
    onDismiss = { runChat.value = true }
  )
}

private fun exportArchive(
  context: Context,
  m: ChatModel,
  progressIndicator: MutableState<Boolean>,
  chatArchiveFile: MutableState<String?>
) {
  progressIndicator.value = true
  withApi {
    try {
      chatArchiveFile.value = exportChatArchive(m, context, chatArchiveFile)
      progressIndicator.value = false
    } catch (e: Error) {
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_exporting_chat_database), e.toString())
      progressIndicator.value = false
    }
  }
}

private suspend fun exportChatArchive(m: ChatModel, context: Context, chatArchiveFile: MutableState<String?>): String {
  val archiveTime = Clock.System.now()
//  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ");
//  val ts = formatter.format(archiveTime.toJavaInstant())
//  val ts = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
  val ts = archiveTime.toString()
  val archiveName = "simplex-chat.$ts.zip"
  val archivePath = "${getFilesDirectory(context)}/$archiveName"
  val config = ArchiveConfig(archivePath, parentTempDirectory = context.cacheDir.toString())
  m.controller.apiExportArchive(config)
  deleteOldArchive(m, context)
  m.controller.appPrefs.chatArchiveName.set(archiveName)
  m.controller.appPrefs.chatArchiveTime.set(archiveTime)
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

private fun deleteChatAlert() {
  // TODO
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
      progressIndicator = remember { mutableStateOf(false) },
      runChat = remember { mutableStateOf(true) },
      showFileImporter = remember { mutableStateOf(false) },
      chatRunning = remember { mutableStateOf(true) },
      chatArchiveName = remember { mutableStateOf("dummy_archive") },
      chatArchiveTime = remember { mutableStateOf(Clock.System.now()) },
      chatLastStart = remember { mutableStateOf(Clock.System.now()) },
      showSettingsModal = { {} },
      startChat = {},
      stopChatAlert = {},
      exportArchive = {}
    )
  }
}
