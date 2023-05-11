package chat.simplex.app.views.database

import SectionBottomSpacer
import SectionDividerSpaced
import SectionTextFooter
import SectionItemView
import SectionView
import android.content.Context
import android.content.res.Configuration
import android.net.Uri
import android.util.Log
import android.widget.Toast
import androidx.activity.compose.ManagedActivityResultLauncher
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.res.painterResource
import androidx.compose.ui.res.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.tooling.preview.Preview
import androidx.compose.ui.unit.dp
import androidx.fragment.app.FragmentActivity
import chat.simplex.app.*
import chat.simplex.app.R
import chat.simplex.app.model.*
import chat.simplex.app.ui.theme.*
import chat.simplex.app.views.helpers.*
import chat.simplex.app.views.usersettings.*
import kotlinx.coroutines.*
import kotlinx.datetime.*
import org.apache.commons.io.IOUtils
import java.io.*
import java.text.SimpleDateFormat
import java.util.*
import kotlin.collections.ArrayList

@Composable
fun DatabaseView(
  m: ChatModel,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)
) {
  val context = LocalContext.current
  val progressIndicator = remember { mutableStateOf(false) }
  val runChat = remember { m.chatRunning }
  val prefs = m.controller.appPrefs
  val useKeychain = remember { mutableStateOf(prefs.storeDBPassphrase.get()) }
  val chatArchiveName = remember { mutableStateOf(prefs.chatArchiveName.get()) }
  val chatArchiveTime = remember { mutableStateOf(prefs.chatArchiveTime.get()) }
  val chatLastStart = remember { mutableStateOf(prefs.chatLastStart.get()) }
  val chatArchiveFile = remember { mutableStateOf<String?>(null) }
  val saveArchiveLauncher = rememberSaveArchiveLauncher(cxt = context, chatArchiveFile)
  val appFilesCountAndSize = remember { mutableStateOf(directoryFileCountAndSize(getAppFilesDirectory(context))) }
  val importArchiveLauncher = rememberGetContentLauncher { uri: Uri? ->
    if (uri != null) {
      importArchiveAlert(m, context, uri, appFilesCountAndSize, progressIndicator)
    }
  }
  LaunchedEffect(m.chatRunning) {
    runChat.value = m.chatRunning.value ?: true
  }
  val chatItemTTL = remember { mutableStateOf(m.chatItemTTL.value) }
  Box(
    Modifier.fillMaxSize(),
  ) {
    DatabaseLayout(
      progressIndicator.value,
      runChat.value != false,
      m.chatDbChanged.value,
      useKeychain.value,
      m.chatDbEncrypted.value,
      m.controller.appPrefs.initialRandomDBPassphrase,
      importArchiveLauncher,
      chatArchiveName,
      chatArchiveTime,
      chatLastStart,
      m.controller.appPrefs.privacyFullBackup,
      appFilesCountAndSize,
      chatItemTTL,
      m.currentUser.value,
      m.users,
      startChat = { startChat(m, runChat, chatLastStart, m.chatDbChanged) },
      stopChatAlert = { stopChatAlert(m, runChat, context) },
      exportArchive = { exportArchive(context, m, progressIndicator, chatArchiveName, chatArchiveTime, chatArchiveFile, saveArchiveLauncher) },
      deleteChatAlert = { deleteChatAlert(m, progressIndicator) },
      deleteAppFilesAndMedia = { deleteFilesAndMediaAlert(context, appFilesCountAndSize) },
      onChatItemTTLSelected = {
        val oldValue = chatItemTTL.value
        chatItemTTL.value = it
        if (it < oldValue) {
          setChatItemTTLAlert(m, chatItemTTL, progressIndicator, appFilesCountAndSize, context)
        } else if (it != oldValue) {
          setCiTTL(m, chatItemTTL, progressIndicator, appFilesCountAndSize, context)
        }
      },
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
          color = MaterialTheme.colors.secondary,
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
  useKeyChain: Boolean,
  chatDbEncrypted: Boolean?,
  initialRandomDBPassphrase: SharedPreference<Boolean>,
  importArchiveLauncher: ManagedActivityResultLauncher<String, Uri?>,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatLastStart: MutableState<Instant?>,
  privacyFullBackup: SharedPreference<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  chatItemTTL: MutableState<ChatItemTTL>,
  currentUser: User?,
  users: List<UserInfo>,
  startChat: () -> Unit,
  stopChatAlert: () -> Unit,
  exportArchive: () -> Unit,
  deleteChatAlert: () -> Unit,
  deleteAppFilesAndMedia: () -> Unit,
  onChatItemTTLSelected: (ChatItemTTL) -> Unit,
  showSettingsModal: (@Composable (ChatModel) -> Unit) -> (() -> Unit)
) {
  val stopped = !runChat
  val operationsDisabled = !stopped || progressIndicator

  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(R.string.your_chat_database))

    SectionView(stringResource(R.string.messages_section_title).uppercase()) {
      TtlOptions(chatItemTTL, enabled = rememberUpdatedState(!stopped && !progressIndicator), onChatItemTTLSelected)
    }
    SectionTextFooter(
      remember(currentUser?.displayName) {
        buildAnnotatedString {
          append(generalGetString(R.string.messages_section_description) + " ")
          withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
            append(currentUser?.displayName ?: "")
          }
          append(".")
        }
      }
    )
    SectionDividerSpaced(maxTopPadding = true)

    SectionView(stringResource(R.string.run_chat_section)) {
      RunChatSetting(runChat, stopped, startChat, stopChatAlert)
    }
    SectionDividerSpaced()

    SectionView(stringResource(R.string.chat_database_section)) {
      val unencrypted = chatDbEncrypted == false
      SettingsActionItem(
        if (unencrypted) painterResource(R.drawable.ic_lock_open) else if (useKeyChain) painterResource(R.drawable.ic_vpn_key_filled)
        else painterResource(R
          .drawable.ic_lock),
        stringResource(R.string.database_passphrase),
        click = showSettingsModal() { DatabaseEncryptionView(it) },
        iconColor = if (unencrypted) WarningOrange else MaterialTheme.colors.secondary,
        disabled = operationsDisabled
      )
      AppDataBackupPreference(privacyFullBackup, initialRandomDBPassphrase)
      SectionDividerSpaced(maxBottomPadding = false)
      SettingsActionItem(
        painterResource(R.drawable.ic_ios_share),
        stringResource(R.string.export_database),
        click = {
          if (initialRandomDBPassphrase.get()) {
            exportProhibitedAlert()
          } else {
            exportArchive()
          }
        },
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
        disabled = operationsDisabled
      )
      SettingsActionItem(
        painterResource(R.drawable.ic_download),
        stringResource(R.string.import_database),
        { importArchiveLauncher.launch("application/zip") },
        textColor = Color.Red,
        iconColor = Color.Red,
        disabled = operationsDisabled
      )
      val chatArchiveNameVal = chatArchiveName.value
      val chatArchiveTimeVal = chatArchiveTime.value
      val chatLastStartVal = chatLastStart.value
      if (chatArchiveNameVal != null && chatArchiveTimeVal != null && chatLastStartVal != null) {
        val title = chatArchiveTitle(chatArchiveTimeVal, chatLastStartVal)
        SettingsActionItem(
          painterResource(R.drawable.ic_inventory_2),
          title,
          click = showSettingsModal { ChatArchiveView(it, title, chatArchiveNameVal, chatArchiveTimeVal) },
          disabled = operationsDisabled
        )
      }
      SettingsActionItem(
        painterResource(R.drawable.ic_delete_forever),
        stringResource(R.string.delete_database),
        deleteChatAlert,
        textColor = Color.Red,
        iconColor = Color.Red,
        disabled = operationsDisabled
      )
    }
    SectionTextFooter(
      if (stopped) {
        stringResource(R.string.you_must_use_the_most_recent_version_of_database)
      } else {
        stringResource(R.string.stop_chat_to_enable_database_actions)
      }
    )
    SectionDividerSpaced(maxTopPadding = true)

    SectionView(stringResource(R.string.files_and_media_section).uppercase()) {
      val deleteFilesDisabled = operationsDisabled || appFilesCountAndSize.value.first == 0
      SectionItemView(
        deleteAppFilesAndMedia,
        disabled = deleteFilesDisabled
      ) {
        Text(
          stringResource(if (users.size > 1) R.string.delete_files_and_media_for_all_users else R.string.delete_files_and_media_all),
          color = if (deleteFilesDisabled) MaterialTheme.colors.secondary else Color.Red
        )
      }
    }
    val (count, size) = appFilesCountAndSize.value
    SectionTextFooter(
      if (count == 0) {
        stringResource(R.string.no_received_app_files)
      } else {
        String.format(stringResource(R.string.total_files_count_and_size), count, formatBytes(size))
      }
    )
    SectionBottomSpacer()
  }
}

@Composable
private fun AppDataBackupPreference(privacyFullBackup: SharedPreference<Boolean>, initialRandomDBPassphrase: SharedPreference<Boolean>) {
  SettingsPreferenceItem(
    painterResource(R.drawable.ic_backup),
    iconColor = MaterialTheme.colors.secondary,
    pref = privacyFullBackup,
    text = stringResource(R.string.full_backup)
  ) {
    if (initialRandomDBPassphrase.get()) {
      exportProhibitedAlert()
      privacyFullBackup.set(false)
    } else {
      privacyFullBackup.set(it)
    }
  }
}

private fun setChatItemTTLAlert(
  m: ChatModel, selectedChatItemTTL: MutableState<ChatItemTTL>,
  progressIndicator: MutableState<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  context: Context
) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.enable_automatic_deletion_question),
    text = generalGetString(R.string.enable_automatic_deletion_message),
    confirmText = generalGetString(R.string.delete_messages),
    onConfirm = { setCiTTL(m, selectedChatItemTTL, progressIndicator, appFilesCountAndSize, context) },
    onDismiss = { selectedChatItemTTL.value = m.chatItemTTL.value },
    destructive = true,
  )
}

@Composable
private fun TtlOptions(current: State<ChatItemTTL>, enabled: State<Boolean>, onSelected: (ChatItemTTL) -> Unit) {
  val values = remember {
    val all: ArrayList<ChatItemTTL> = arrayListOf(ChatItemTTL.None, ChatItemTTL.Month, ChatItemTTL.Week, ChatItemTTL.Day)
    if (current.value is ChatItemTTL.Seconds) {
      all.add(current.value)
    }
    all.map {
      when (it) {
        is ChatItemTTL.None -> it to generalGetString(R.string.chat_item_ttl_none)
        is ChatItemTTL.Day -> it to generalGetString(R.string.chat_item_ttl_day)
        is ChatItemTTL.Week -> it to generalGetString(R.string.chat_item_ttl_week)
        is ChatItemTTL.Month -> it to generalGetString(R.string.chat_item_ttl_month)
        is ChatItemTTL.Seconds -> it to String.format(generalGetString(R.string.chat_item_ttl_seconds), it.secs)
      }
    }
  }
  ExposedDropDownSettingRow(
    generalGetString(R.string.delete_messages_after),
    values,
    current,
    icon = null,
    enabled = enabled,
    onSelected = onSelected
  )
}

@Composable
fun RunChatSetting(
  runChat: Boolean,
  stopped: Boolean,
  startChat: () -> Unit,
  stopChatAlert: () -> Unit
) {
  val chatRunningText = if (stopped) stringResource(R.string.chat_is_stopped) else stringResource(R.string.chat_is_running)
  SettingsActionItemWithContent(
    icon = if (stopped) painterResource(R.drawable.ic_report_filled) else painterResource(R.drawable.ic_play_arrow_filled),
    text = chatRunningText,
    iconColor = if (stopped) Color.Red else MaterialTheme.colors.primary,
  ) {
    DefaultSwitch(
      checked = runChat,
      onCheckedChange = { runChatSwitch ->
        if (runChatSwitch) {
          startChat()
        } else {
          stopChatAlert()
        }
      },
    )
  }
}

@Composable
fun chatArchiveTitle(chatArchiveTime: Instant, chatLastStart: Instant): String {
  return stringResource(if (chatArchiveTime < chatLastStart) R.string.old_database_archive else R.string.new_database_archive)
}

private fun startChat(m: ChatModel, runChat: MutableState<Boolean?>, chatLastStart: MutableState<Instant?>, chatDbChanged: MutableState<Boolean>) {
  withApi {
    try {
      if (chatDbChanged.value) {
        SimplexApp.context.initChatController()
        chatDbChanged.value = false
      }
      if (m.chatDbStatus.value !is DBMigrationResult.OK) {
        /** Hide current view and show [DatabaseErrorView] */
        ModalManager.shared.closeModals()
        return@withApi
      }
      if (m.currentUser.value == null) {
        ModalManager.shared.closeModals()
        return@withApi
      } else {
        m.controller.apiStartChat()
        runChat.value = true
        m.chatRunning.value = true
      }
      val ts = Clock.System.now()
      m.controller.appPrefs.chatLastStart.set(ts)
      chatLastStart.value = ts
      when (m.controller.appPrefs.notificationsMode.get()) {
        NotificationsMode.SERVICE.name -> CoroutineScope(Dispatchers.Default).launch { SimplexService.start(SimplexApp.context) }
        NotificationsMode.PERIODIC.name -> SimplexApp.context.schedulePeriodicWakeUp()
      }
    } catch (e: Error) {
      runChat.value = false
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_starting_chat), e.toString())
    }
  }
}

private fun stopChatAlert(m: ChatModel, runChat: MutableState<Boolean?>, context: Context) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.stop_chat_question),
    text = generalGetString(R.string.stop_chat_to_export_import_or_delete_chat_database),
    confirmText = generalGetString(R.string.stop_chat_confirmation),
    onConfirm = { authStopChat(m, runChat, context) },
    onDismiss = { runChat.value = true }
  )
}

private fun exportProhibitedAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(R.string.set_password_to_export),
    text = generalGetString(R.string.set_password_to_export_desc),
  )
}

private fun authStopChat(m: ChatModel, runChat: MutableState<Boolean?>, context: Context) {
  if (m.controller.appPrefs.performLA.get()) {
    authenticate(
      generalGetString(R.string.auth_stop_chat),
      generalGetString(R.string.auth_log_in_using_credential),
      activity = context as FragmentActivity,
      completed = { laResult ->
        when (laResult) {
          LAResult.Success, is LAResult.Unavailable -> {
            stopChat(m, runChat, context)
          }
          is LAResult.Error -> {
            runChat.value = true
          }
          is LAResult.Failed -> {
            runChat.value = true
          }
        }
      }
    )
  } else {
    stopChat(m, runChat, context)
  }
}

private fun stopChat(m: ChatModel, runChat: MutableState<Boolean?>, context: Context) {
  withApi {
    try {
      runChat.value = false
      stopChatAsync(m)
      SimplexService.safeStopService(SimplexApp.context)
      MessagesFetcherWorker.cancelAll()
    } catch (e: Error) {
      runChat.value = true
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_stopping_chat), e.toString())
    }
  }
}

suspend fun stopChatAsync(m: ChatModel) {
  m.controller.apiStopChat()
  m.chatRunning.value = false
}

suspend fun deleteChatAsync(m: ChatModel) {
  m.controller.apiDeleteStorage()
  DatabaseUtils.ksDatabasePassword.remove()
  m.controller.appPrefs.storeDBPassphrase.set(true)
}

private fun exportArchive(
  context: Context,
  m: ChatModel,
  progressIndicator: MutableState<Boolean>,
  chatArchiveName: MutableState<String?>,
  chatArchiveTime: MutableState<Instant?>,
  chatArchiveFile: MutableState<String?>,
  saveArchiveLauncher: ManagedActivityResultLauncher<String, Uri?>
) {
  progressIndicator.value = true
  withApi {
    try {
      val archiveFile = exportChatArchive(m, context, chatArchiveName, chatArchiveTime, chatArchiveFile)
      chatArchiveFile.value = archiveFile
      saveArchiveLauncher.launch(archiveFile.substringAfterLast("/"))
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
              File(filePath).inputStream().use { it.copyTo(outputStream) }
              outputStream.close()
              Toast.makeText(cxt, generalGetString(R.string.file_saved), Toast.LENGTH_SHORT).show()
            }
          } else {
            Toast.makeText(cxt, generalGetString(R.string.file_not_found), Toast.LENGTH_SHORT).show()
          }
        }
      } catch (e: Error) {
        Toast.makeText(cxt, generalGetString(R.string.error_saving_file), Toast.LENGTH_SHORT).show()
        Log.e(TAG, "rememberSaveArchiveLauncher error saving archive $e")
      } finally {
        chatArchiveFile.value = null
      }
    }
  )

private fun importArchiveAlert(
  m: ChatModel,
  context: Context,
  importedArchiveUri: Uri,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  progressIndicator: MutableState<Boolean>
) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.import_database_question),
    text = generalGetString(R.string.your_current_chat_database_will_be_deleted_and_replaced_with_the_imported_one),
    confirmText = generalGetString(R.string.import_database_confirmation),
    onConfirm = { importArchive(m, context, importedArchiveUri, appFilesCountAndSize, progressIndicator) },
    destructive = true,
  )
}

private fun importArchive(
  m: ChatModel,
  context: Context,
  importedArchiveUri: Uri,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  progressIndicator: MutableState<Boolean>
) {
  progressIndicator.value = true
  val archivePath = saveArchiveFromUri(context, importedArchiveUri)
  if (archivePath != null) {
    withApi {
      try {
        m.controller.apiDeleteStorage()
        try {
          val config = ArchiveConfig(archivePath, parentTempDirectory = context.cacheDir.toString())
          m.controller.apiImportArchive(config)
          DatabaseUtils.ksDatabasePassword.remove()
          appFilesCountAndSize.value = directoryFileCountAndSize(getAppFilesDirectory(context))
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
        File(archivePath).delete()
      }
    }
  }
}

private fun saveArchiveFromUri(context: Context, importedArchiveUri: Uri): String? {
  return try {
    val inputStream = context.contentResolver.openInputStream(importedArchiveUri)
    val archiveName = getFileName(context, importedArchiveUri)
    if (inputStream != null && archiveName != null) {
      val archivePath = "${context.cacheDir}/$archiveName"
      val destFile = File(archivePath)
      IOUtils.copy(inputStream, FileOutputStream(destFile))
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
    onConfirm = { deleteChat(m, progressIndicator) },
    destructive = true,
  )
}

private fun deleteChat(m: ChatModel, progressIndicator: MutableState<Boolean>) {
  progressIndicator.value = true
  withApi {
    try {
      deleteChatAsync(m)
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

private fun setCiTTL(
  m: ChatModel,
  chatItemTTL: MutableState<ChatItemTTL>,
  progressIndicator: MutableState<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  context: Context
) {
  Log.d(TAG, "DatabaseView setChatItemTTL ${chatItemTTL.value.seconds ?: -1}")
  progressIndicator.value = true
  withApi {
    try {
      m.controller.setChatItemTTL(chatItemTTL.value)
      // Update model on success
      m.chatItemTTL.value = chatItemTTL.value
      afterSetCiTTL(m, progressIndicator, appFilesCountAndSize, context)
    } catch (e: Exception) {
      // Rollback to model's value
      chatItemTTL.value = m.chatItemTTL.value
      afterSetCiTTL(m, progressIndicator, appFilesCountAndSize, context)
      AlertManager.shared.showAlertMsg(generalGetString(R.string.error_changing_message_deletion), e.stackTraceToString())
    }
  }
}

private fun afterSetCiTTL(
  m: ChatModel,
  progressIndicator: MutableState<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  context: Context
) {
  progressIndicator.value = false
  appFilesCountAndSize.value = directoryFileCountAndSize(getAppFilesDirectory(context))
  withApi {
    try {
      val chats = m.controller.apiGetChats()
      m.updateChats(chats)
    } catch (e: Exception) {
      Log.e(TAG, "apiGetChats error: ${e.message}")
    }
  }
}

private fun deleteFilesAndMediaAlert(context: Context, appFilesCountAndSize: MutableState<Pair<Int, Long>>) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(R.string.delete_files_and_media_question),
    text = generalGetString(R.string.delete_files_and_media_desc),
    confirmText = generalGetString(R.string.delete_verb),
    onConfirm = { deleteFiles(appFilesCountAndSize, context) },
    destructive = true
  )
}

private fun deleteFiles(appFilesCountAndSize: MutableState<Pair<Int, Long>>, context: Context) {
  deleteAppFiles(context)
  appFilesCountAndSize.value = directoryFileCountAndSize(getAppFilesDirectory(context))
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
      useKeyChain = false,
      chatDbEncrypted = false,
      initialRandomDBPassphrase = SharedPreference({ true }, {}),
      importArchiveLauncher = rememberGetContentLauncher {},
      chatArchiveName = remember { mutableStateOf("dummy_archive") },
      chatArchiveTime = remember { mutableStateOf(Clock.System.now()) },
      chatLastStart = remember { mutableStateOf(Clock.System.now()) },
      privacyFullBackup = SharedPreference({ true }, {}),
      appFilesCountAndSize = remember { mutableStateOf(0 to 0L) },
      chatItemTTL = remember { mutableStateOf(ChatItemTTL.None) },
      currentUser = User.sampleData,
      users = listOf(UserInfo.sampleData),
      startChat = {},
      stopChatAlert = {},
      exportArchive = {},
      deleteChatAlert = {},
      deleteAppFilesAndMedia = {},
      showSettingsModal = { {} },
      onChatItemTTLSelected = {},
    )
  }
}
