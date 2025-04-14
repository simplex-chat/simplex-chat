package chat.simplex.common.views.database

import SectionBottomSpacer
import SectionDividerSpaced
import SectionTextFooter
import SectionItemView
import SectionView
import androidx.compose.desktop.ui.tooling.preview.Preview
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.graphics.painter.Painter
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import androidx.compose.ui.text.*
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.platform.*
import chat.simplex.res.MR
import kotlinx.datetime.*
import java.io.*
import java.net.URI
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.text.SimpleDateFormat
import java.util.*
import kotlin.collections.ArrayList
import kotlinx.coroutines.*

@Composable
fun DatabaseView() {
  val m = chatModel
  val progressIndicator = remember { mutableStateOf(false) }
  val prefs = m.controller.appPrefs
  val useKeychain = remember { mutableStateOf(prefs.storeDBPassphrase.get()) }
  val chatLastStart = remember { mutableStateOf(prefs.chatLastStart.get()) }
  val chatArchiveFile = remember { mutableStateOf<String?>(null) }
  val stopped = remember { m.chatRunning }.value == false
  val saveArchiveLauncher = rememberFileChooserLauncher(false) { to: URI? ->
    val archive = chatArchiveFile.value
    if (archive != null && to != null) {
      copyFileToFile(File(archive), to) {}
    }
    // delete no matter the database was exported or canceled the export process
    if (archive != null) {
      File(archive).delete()
      chatArchiveFile.value = null
    }
  }
  val appFilesCountAndSize = remember { mutableStateOf(directoryFileCountAndSize(appFilesDir.absolutePath)) }
  val importArchiveLauncher = rememberFileChooserLauncher(true) { to: URI? ->
    if (to != null) {
      importArchiveAlert {
        stopChatRunBlockStartChat(stopped, chatLastStart, progressIndicator) {
          importArchive(to, appFilesCountAndSize, progressIndicator, false)
        }
      }
    }
  }
  val chatItemTTL = remember { mutableStateOf(m.chatItemTTL.value) }
  Box(
    Modifier.fillMaxSize(),
  ) {
    val user = m.currentUser.value
    val rhId = user?.remoteHostId
    DatabaseLayout(
      progressIndicator.value,
      stopped,
      useKeychain.value,
      m.chatDbEncrypted.value,
      m.controller.appPrefs.storeDBPassphrase.state.value,
      m.controller.appPrefs.initialRandomDBPassphrase,
      importArchiveLauncher,
      appFilesCountAndSize,
      chatItemTTL,
      user,
      m.users,
      startChat = { startChat(m, chatLastStart, m.chatDbChanged, progressIndicator) },
      stopChatAlert = { stopChatAlert(m, progressIndicator) },
      exportArchive = {
        stopChatRunBlockStartChat(stopped, chatLastStart, progressIndicator) {
          exportArchive(m, progressIndicator, chatArchiveFile, saveArchiveLauncher)
        }
      },
      deleteChatAlert = {
        deleteChatAlert {
          stopChatRunBlockStartChat(stopped, chatLastStart, progressIndicator) {
            deleteChat(m, progressIndicator)
            true
          }
        }
      },
      deleteAppFilesAndMedia = {
        deleteFilesAndMediaAlert {
          stopChatRunBlockStartChat(stopped, chatLastStart, progressIndicator) {
            deleteFiles(appFilesCountAndSize)
            true
          }
        }
      },
      onChatItemTTLSelected = {
        if (it == null) {
          return@DatabaseLayout
        }
        val oldValue = chatItemTTL.value
        chatItemTTL.value = it
        if (it < oldValue) {
          setChatItemTTLAlert(m, rhId, chatItemTTL, progressIndicator, appFilesCountAndSize)
        } else if (it != oldValue) {
          setCiTTL(m, rhId, chatItemTTL, progressIndicator, appFilesCountAndSize)
        }
      },
      disconnectAllHosts = {
        val connected = chatModel.remoteHosts.filter { it.sessionState is RemoteHostSessionState.Connected }
        connected.forEachIndexed { index, h ->
          controller.stopRemoteHostAndReloadHosts(h, index == connected.lastIndex && chatModel.connectedToRemote())
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
fun DatabaseLayout(
  progressIndicator: Boolean,
  stopped: Boolean,
  useKeyChain: Boolean,
  chatDbEncrypted: Boolean?,
  passphraseSaved: Boolean,
  initialRandomDBPassphrase: SharedPreference<Boolean>,
  importArchiveLauncher: FileChooserLauncher,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  chatItemTTL: MutableState<ChatItemTTL>,
  currentUser: User?,
  users: List<UserInfo>,
  startChat: () -> Unit,
  stopChatAlert: () -> Unit,
  exportArchive: () -> Unit,
  deleteChatAlert: () -> Unit,
  deleteAppFilesAndMedia: () -> Unit,
  onChatItemTTLSelected: (ChatItemTTL?) -> Unit,
  disconnectAllHosts: () -> Unit,
) {
  val operationsDisabled = progressIndicator && !chatModel.desktopNoUserNoRemote

  ColumnWithScrollBar {
    AppBarTitle(stringResource(MR.strings.your_chat_database))

    if (!chatModel.desktopNoUserNoRemote) {
      SectionView(stringResource(MR.strings.messages_section_title).uppercase()) {
        TtlOptions(chatItemTTL, enabled = rememberUpdatedState(!stopped && !progressIndicator), onChatItemTTLSelected)
      }
      SectionTextFooter(
        remember(currentUser?.displayName) {
          buildAnnotatedString {
            append(generalGetString(MR.strings.messages_section_description) + " ")
            withStyle(SpanStyle(fontWeight = FontWeight.Bold)) {
              append(currentUser?.displayName ?: "")
            }
            append(".")
          }
        }
      )
      SectionDividerSpaced(maxTopPadding = true)
    }
    val toggleEnabled = remember { chatModel.remoteHosts }.none { it.sessionState is RemoteHostSessionState.Connected }
    if (chatModel.localUserCreated.value == true) {
      // still show the toggle in case database was stopped when the user opened this screen because it can be in the following situations:
      // - database was stopped after migration and the app relaunched
      // - something wrong happened with database operations and the database couldn't be launched when it should
      SectionView(stringResource(MR.strings.run_chat_section)) {
        if (!toggleEnabled) {
          SectionItemView(disconnectAllHosts) {
            Text(generalGetString(MR.strings.disconnect_remote_hosts), Modifier.fillMaxWidth(), color = WarningOrange)
          }
        }
        RunChatSetting(stopped, toggleEnabled && !progressIndicator, startChat, stopChatAlert)
      }
      if (stopped) SectionTextFooter(stringResource(MR.strings.you_must_use_the_most_recent_version_of_database))
      SectionDividerSpaced(maxTopPadding = true)
    }

    SectionView(stringResource(MR.strings.chat_database_section)) {
      if (chatModel.localUserCreated.value != true && !toggleEnabled) {
        SectionItemView(disconnectAllHosts) {
          Text(generalGetString(MR.strings.disconnect_remote_hosts), Modifier.fillMaxWidth(), color = WarningOrange)
        }
      }
      val unencrypted = chatDbEncrypted == false
      SettingsActionItem(
        if (unencrypted) painterResource(MR.images.ic_lock_open_right) else if (useKeyChain) painterResource(MR.images.ic_vpn_key_filled)
        else painterResource(MR.images.ic_lock),
        stringResource(MR.strings.database_passphrase),
        click = { ModalManager.start.showModal { DatabaseEncryptionView(chatModel, false) } },
        iconColor = if (unencrypted || (appPlatform.isDesktop && passphraseSaved)) WarningOrange else MaterialTheme.colors.secondary,
        disabled = operationsDisabled
      )
      if (appPlatform.isDesktop) {
        SettingsActionItem(
          painterResource(MR.images.ic_folder_open),
          stringResource(MR.strings.open_database_folder),
          ::desktopOpenDatabaseDir,
          disabled = operationsDisabled
        )
      }
      SettingsActionItem(
        painterResource(MR.images.ic_ios_share),
        stringResource(MR.strings.export_database),
        click = {
          if (initialRandomDBPassphrase.get()) {
            exportProhibitedAlert()
            ModalManager.start.showModal {
              DatabaseEncryptionView(chatModel, false)
            }
          } else {
            exportArchive()
          }
        },
        textColor = MaterialTheme.colors.primary,
        iconColor = MaterialTheme.colors.primary,
        disabled = operationsDisabled
      )
      SettingsActionItem(
        painterResource(MR.images.ic_download),
        stringResource(MR.strings.import_database),
        { withLongRunningApi { importArchiveLauncher.launch("application/zip") } },
        textColor = Color.Red,
        iconColor = Color.Red,
        disabled = operationsDisabled
      )
      SettingsActionItem(
        painterResource(MR.images.ic_delete_forever),
        stringResource(MR.strings.delete_database),
        deleteChatAlert,
        textColor = Color.Red,
        iconColor = Color.Red,
        disabled = operationsDisabled
      )
    }
    SectionDividerSpaced()

    SectionView(stringResource(MR.strings.files_and_media_section).uppercase()) {
      val deleteFilesDisabled = operationsDisabled || appFilesCountAndSize.value.first == 0
      SectionItemView(
        deleteAppFilesAndMedia,
        disabled = deleteFilesDisabled
      ) {
        Text(
          stringResource(if (users.size > 1) MR.strings.delete_files_and_media_for_all_users else MR.strings.delete_files_and_media_all),
          color = if (deleteFilesDisabled) MaterialTheme.colors.secondary else Color.Red
        )
      }
    }
    val (count, size) = appFilesCountAndSize.value
    SectionTextFooter(
      if (count == 0) {
        stringResource(MR.strings.no_received_app_files)
      } else {
        String.format(stringResource(MR.strings.total_files_count_and_size), count, formatBytes(size))
      }
    )
    SectionBottomSpacer()
  }
}

private fun setChatItemTTLAlert(
  m: ChatModel, rhId: Long?, selectedChatItemTTL: MutableState<ChatItemTTL>,
  progressIndicator: MutableState<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.enable_automatic_deletion_question),
    text = generalGetString(MR.strings.enable_automatic_deletion_message),
    confirmText = generalGetString(MR.strings.delete_messages),
    onConfirm = { setCiTTL(m, rhId, selectedChatItemTTL, progressIndicator, appFilesCountAndSize) },
    onDismiss = { selectedChatItemTTL.value = m.chatItemTTL.value },
    onDismissRequest = { selectedChatItemTTL.value = m.chatItemTTL.value },
    destructive = true,
  )
}

@Composable
fun TtlOptions(
  current: State<ChatItemTTL?>,
  enabled: State<Boolean>,
  onSelected: (ChatItemTTL?) -> Unit,
  default: State<ChatItemTTL>? = null
) {
  val values = remember {
    val all: ArrayList<ChatItemTTL> = arrayListOf(ChatItemTTL.None, ChatItemTTL.Year, ChatItemTTL.Month, ChatItemTTL.Week, ChatItemTTL.Day)
    val currentValue = current.value
    if (currentValue is ChatItemTTL.Seconds) {
      all.add(currentValue)
    }
    val options: MutableList<Pair<ChatItemTTL?, String>> = all.map { it to it.text }.toMutableList()

    if (default != null) {
      options.add(null to String.format(generalGetString(MR.strings.chat_item_ttl_default), default.value.text))
    }

    options
  }
  ExposedDropDownSettingRow(
    generalGetString(MR.strings.delete_messages_after),
    values,
    current,
    icon = null,
    enabled = enabled,
    onSelected = onSelected
  )
}

@Composable
fun RunChatSetting(
  stopped: Boolean,
  enabled: Boolean,
  startChat: () -> Unit,
  stopChatAlert: () -> Unit
) {
  val chatRunningText = if (stopped) stringResource(MR.strings.chat_is_stopped) else stringResource(MR.strings.chat_is_running)
  SettingsActionItemWithContent(
    icon = if (stopped) painterResource(MR.images.ic_report_filled) else painterResource(MR.images.ic_play_arrow_filled),
    text = chatRunningText,
    iconColor = if (stopped) Color.Red else MaterialTheme.colors.primary,
  ) {
    DefaultSwitch(
      checked = !stopped,
      onCheckedChange = { runChatSwitch ->
        if (runChatSwitch) {
          startChat()
        } else {
          stopChatAlert()
        }
      },
      enabled = enabled,
    )
  }
}

fun startChat(
  m: ChatModel,
  chatLastStart: MutableState<Instant?>,
  chatDbChanged: MutableState<Boolean>,
  progressIndicator: MutableState<Boolean>? = null
) {
  withLongRunningApi {
    try {
      progressIndicator?.value = true
      if (chatDbChanged.value) {
        initChatController()
        chatDbChanged.value = false
      }
      if (m.chatDbStatus.value !is DBMigrationResult.OK) {
        /** Hide current view and show [DatabaseErrorView] */
        ModalManager.closeAllModalsEverywhere()
        return@withLongRunningApi
      }
      val user = m.currentUser.value
      if (user == null) {
        ModalManager.closeAllModalsEverywhere()
        return@withLongRunningApi
      } else {
        m.controller.startChat(user)
      }
      val ts = Clock.System.now()
      m.controller.appPrefs.chatLastStart.set(ts)
      chatLastStart.value = ts
      platform.androidChatStartedAfterBeingOff()
    } catch (e: Throwable) {
      m.chatRunning.value = false
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_starting_chat), e.toString())
    } finally {
      progressIndicator?.value = false
    }
  }
}

private fun stopChatAlert(m: ChatModel, progressIndicator: MutableState<Boolean>? = null) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.stop_chat_question),
    text = generalGetString(MR.strings.stop_chat_to_export_import_or_delete_chat_database),
    confirmText = generalGetString(MR.strings.stop_chat_confirmation),
    onConfirm = { authStopChat(m, progressIndicator = progressIndicator) },
    onDismiss = { m.chatRunning.value = true }
  )
}

expect fun restartChatOrApp()

private fun exportProhibitedAlert() {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.set_password_to_export),
    text = generalGetString(MR.strings.set_password_to_export_desc),
  )
}

fun authStopChat(m: ChatModel, progressIndicator: MutableState<Boolean>? = null, onStop: (() -> Unit)? = null) {
  if (m.controller.appPrefs.performLA.get()) {
    authenticate(
      generalGetString(MR.strings.auth_stop_chat),
      generalGetString(MR.strings.auth_log_in_using_credential),
      oneTime = true,
      completed = { laResult ->
        when (laResult) {
          LAResult.Success, is LAResult.Unavailable -> {
            stopChat(m, progressIndicator, onStop)
          }
          is LAResult.Error -> {
            m.chatRunning.value = true
            laFailedAlert()
          }
          is LAResult.Failed -> {
            m.chatRunning.value = true
          }
        }
      }
    )
  } else {
    stopChat(m, progressIndicator, onStop)
  }
}

private fun stopChat(m: ChatModel, progressIndicator: MutableState<Boolean>? = null, onStop: (() -> Unit)? = null) {
  withBGApi {
    try {
      progressIndicator?.value = true
      stopChatAsync(m)
      platform.androidChatStopped()
      // close chat view for desktop
      chatModel.chatId.value = null
      if (appPlatform.isDesktop) {
        ModalManager.end.closeModals()
      }
      onStop?.invoke()
    } catch (e: Error) {
      m.chatRunning.value = true
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_stopping_chat), e.toString())
    } finally {
      progressIndicator?.value = false
    }
  }
}

suspend fun stopChatAsync(m: ChatModel) {
  m.controller.apiStopChat()
  m.chatRunning.value = false
  controller.appPrefs.chatStopped.set(true)
}

fun stopChatRunBlockStartChat(
  stopped: Boolean,
  chatLastStart: MutableState<Instant?>,
  progressIndicator: MutableState<Boolean>,
  block: suspend () -> Boolean
) {
  // if the chat was running, the sequence is: stop chat, run block, start chat.
  // Otherwise, just run block and do nothing - the toggle will be visible anyway and the user can start the chat or not
  if (stopped) {
    withLongRunningApi {
      try {
        block()
      } catch (e: Throwable) {
        Log.e(TAG, e.stackTraceToString())
      }
    }
  } else {
    authStopChat(chatModel, progressIndicator) {
      withLongRunningApi {
        // if it throws, let's start chat again anyway
        val canStart = try {
          block()
        } catch (e: Throwable) {
          Log.e(TAG, e.stackTraceToString())
          true
        }
        if (canStart) {
          startChat(chatModel, chatLastStart, chatModel.chatDbChanged, progressIndicator)
        }
      }
    }
  }
}

suspend fun deleteChatAsync(m: ChatModel) {
  m.controller.apiDeleteStorage()
  DatabaseUtils.ksDatabasePassword.remove()
  m.controller.appPrefs.storeDBPassphrase.set(true)
  deleteChatDatabaseFilesAndState()
}

fun deleteChatDatabaseFilesAndState() {
  val chat = File(dataDir, chatDatabaseFileName)
  val chatBak = File(dataDir, "$chatDatabaseFileName.bak")
  val agent = File(dataDir, agentDatabaseFileName)
  val agentBak = File(dataDir, "$agentDatabaseFileName.bak")
  chat.delete()
  chatBak.delete()
  agent.delete()
  agentBak.delete()
  filesDir.deleteRecursively()
  filesDir.mkdir()
  remoteHostsDir.deleteRecursively()
  tmpDir.deleteRecursively()
  getMigrationTempFilesDirectory().deleteRecursively()
  tmpDir.mkdir()
  wallpapersDir.deleteRecursively()
  wallpapersDir.mkdirs()
  DatabaseUtils.ksDatabasePassword.remove()
  appPrefs.newDatabaseInitialized.set(false)
  chatModel.desktopOnboardingRandomPassword.value = false
  controller.appPrefs.storeDBPassphrase.set(true)
  controller.ctrl = null

  // Clear sensitive data on screen just in case ModalManager will fail to prevent hiding its modals while database encrypts itself
  chatModel.chatId.value = null
  withLongRunningApi {
    withContext(Dispatchers.Main) {
      chatModel.chatsContext.chatItems.clearAndNotify()
      chatModel.chatsContext.chats.clear()
      chatModel.chatsContext.popChatCollector.clear()
    }
    withContext(Dispatchers.Main) {
      chatModel.secondaryChatsContext.value?.chatItems?.clearAndNotify()
      chatModel.secondaryChatsContext.value?.chats?.clear()
      chatModel.secondaryChatsContext.value?.popChatCollector?.clear()
    }
  }
  chatModel.users.clear()
  ntfManager.cancelAllNotifications()
}

private suspend fun exportArchive(
  m: ChatModel,
  progressIndicator: MutableState<Boolean>,
  chatArchiveFile: MutableState<String?>,
  saveArchiveLauncher: FileChooserLauncher
): Boolean {
  progressIndicator.value = true
  try {
    val (archiveFile, archiveErrors) = exportChatArchive(m, null, chatArchiveFile)
    chatArchiveFile.value = archiveFile
    if (archiveErrors.isEmpty()) {
      saveArchiveLauncher.launch(archiveFile.substringAfterLast(File.separator))
    } else {
      showArchiveExportedWithErrorsAlert(generalGetString(MR.strings.chat_database_exported_save), archiveErrors) {
        withLongRunningApi {
          saveArchiveLauncher.launch(archiveFile.substringAfterLast(File.separator))
        }
      }
    }
    progressIndicator.value = false
  } catch (e: Throwable) {
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_exporting_chat_database), e.toString())
    progressIndicator.value = false
  }
  return false
}

suspend fun exportChatArchive(
  m: ChatModel,
  storagePath: File?,
  chatArchiveFile: MutableState<String?>
): Pair<String, List<ArchiveError>> {
  val archiveTime = Clock.System.now()
  val ts = SimpleDateFormat("yyyy-MM-dd'T'HHmmss", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
  val archiveName = "simplex-chat.$ts.zip"
  val archivePath = "${(storagePath ?: databaseExportDir).absolutePath}${File.separator}$archiveName"
  val config = ArchiveConfig(archivePath, parentTempDirectory = databaseExportDir.toString())
  // Settings should be saved before changing a passphrase, otherwise the database needs to be migrated first
  if (!m.chatDbChanged.value) {
    controller.apiSaveAppSettings(AppSettings.current.prepareForExport())
  }
  wallpapersDir.mkdirs()
  val archiveErrors = m.controller.apiExportArchive(config)
  if (storagePath == null) {
    deleteOldChatArchive()
    m.controller.appPrefs.chatArchiveName.set(archiveName)
    m.controller.appPrefs.chatArchiveTime.set(archiveTime)
  }
  chatArchiveFile.value = archivePath
  return archivePath to archiveErrors
}

// Deprecated. Remove in the end of 2025. All unused archives should be deleted for the most users til then.
/** Remove [AppPreferences.chatArchiveName] and [AppPreferences.chatArchiveTime] as well */
fun deleteOldChatArchive() {
  val chatArchiveName = chatModel.controller.appPrefs.chatArchiveName.get()
  if (chatArchiveName != null) {
    val file1 = File("${filesDir.absolutePath}${File.separator}$chatArchiveName")
    val file2 = File("${databaseExportDir.absolutePath}${File.separator}$chatArchiveName")
    val fileDeleted = file1.delete() || file2.delete()
    if (fileDeleted || (!file1.exists() && !file2.exists())) {
      chatModel.controller.appPrefs.chatArchiveName.set(null)
      chatModel.controller.appPrefs.chatArchiveTime.set(null)
    } else {
      Log.e(TAG, "deleteOldArchive file.delete() error")
    }
  }
}

private fun importArchiveAlert(onConfirm: () -> Unit, ) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.import_database_question),
    text = generalGetString(MR.strings.your_current_chat_database_will_be_deleted_and_replaced_with_the_imported_one),
    confirmText = generalGetString(MR.strings.import_database_confirmation),
    onConfirm = onConfirm,
    destructive = true,
  )
}

fun showArchiveImportedWithErrorsAlert(archiveErrors: List<ArchiveError>) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.chat_database_imported),
    text = generalGetString(MR.strings.restart_the_app_to_use_imported_chat_database) + "\n\n" + generalGetString(MR.strings.non_fatal_errors_occured_during_import) + archiveErrorsText(archiveErrors))
}

fun showArchiveExportedWithErrorsAlert(description: String, archiveErrors: List<ArchiveError>, onConfirm: () -> Unit) {
  AlertManager.shared.showAlertMsg(
    title = generalGetString(MR.strings.chat_database_exported_title),
    text = description + "\n\n" + generalGetString(MR.strings.chat_database_exported_not_all_files) + archiveErrorsText(archiveErrors),
    confirmText = generalGetString(MR.strings.chat_database_exported_continue),
    onConfirm = onConfirm
  )
}

private fun archiveErrorsText(errs: List<ArchiveError>): String = "\n" + errs.map {
  when (it) {
    is ArchiveError.ArchiveErrorImport -> it.importError
    is ArchiveError.ArchiveErrorFile -> "${it.file}: ${it.fileError}"
  }
}.joinToString(separator = "\n")

suspend fun importArchive(
  importedArchiveURI: URI,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
  progressIndicator: MutableState<Boolean>,
  migration: Boolean
): Boolean {
  val m = chatModel
  progressIndicator.value = true
  val archivePath = saveArchiveFromURI(importedArchiveURI)
  if (archivePath != null) {
    try {
      m.controller.apiDeleteStorage()
      wallpapersDir.mkdirs()
      try {
        val config = ArchiveConfig(archivePath, parentTempDirectory = databaseExportDir.toString())
        val archiveErrors = m.controller.apiImportArchive(config)
        appPrefs.shouldImportAppSettings.set(true)
        DatabaseUtils.ksDatabasePassword.remove()
        appFilesCountAndSize.value = directoryFileCountAndSize(appFilesDir.absolutePath)
        if (archiveErrors.isEmpty()) {
          operationEnded(m, progressIndicator) {
            AlertManager.shared.showAlertMsg(generalGetString(MR.strings.chat_database_imported), text = generalGetString(MR.strings.restart_the_app_to_use_imported_chat_database))
          }
          if (chatModel.localUserCreated.value == false) {
            chatModel.chatRunning.value = false
          }
          return true
        } else {
          operationEnded(m, progressIndicator) {
            showArchiveImportedWithErrorsAlert(archiveErrors)
          }
          return migration
        }
      } catch (e: Error) {
        operationEnded(m, progressIndicator) {
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_importing_database), e.toString())
        }
      }
    } catch (e: Error) {
      operationEnded(m, progressIndicator) {
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_deleting_database), e.toString())
      }
    } finally {
      File(archivePath).delete()
    }
  } else {
    progressIndicator.value = false
  }
  return false
}

private fun saveArchiveFromURI(importedArchiveURI: URI): String? {
  return try {
    val inputStream = importedArchiveURI.inputStream()
    val archiveName = getFileName(importedArchiveURI)
    if (inputStream != null && archiveName != null) {
      val archivePath = "$databaseExportDir${File.separator}$archiveName"
      val destFile = File(archivePath)
      Files.copy(inputStream, destFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
      archivePath
    } else {
      Log.e(TAG, "saveArchiveFromURI null inputStream")
      null
    }
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_saving_database), e.stackTraceToString())
    Log.e(TAG, "saveArchiveFromURI error: ${e.stackTraceToString()}")
    null
  }
}

private fun deleteChatAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_chat_profile_question),
    text = generalGetString(MR.strings.delete_chat_profile_action_cannot_be_undone_warning),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = onConfirm,
    destructive = true,
  )
}

private suspend fun deleteChat(m: ChatModel, progressIndicator: MutableState<Boolean>) {
  if (!DatabaseUtils.hasAtLeastOneDatabase(dataDir.absolutePath)) {
    return
  }
  progressIndicator.value = true
  try {
    deleteChatAsync(m)
    operationEnded(m, progressIndicator) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.chat_database_deleted), generalGetString(MR.strings.restart_the_app_to_create_a_new_chat_profile))
    }
  } catch (e: Throwable) {
    operationEnded(m, progressIndicator) {
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_deleting_database), e.toString())
    }
  }
}

private fun setCiTTL(
  m: ChatModel,
  rhId: Long?,
  chatItemTTL: MutableState<ChatItemTTL>,
  progressIndicator: MutableState<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
) {
  Log.d(TAG, "DatabaseView setChatItemTTL ${chatItemTTL.value.seconds ?: -1}")
  progressIndicator.value = true
  withBGApi {
    try {
      m.controller.setChatItemTTL(rhId, chatItemTTL.value)
      // Update model on success
      m.chatItemTTL.value = chatItemTTL.value
      afterSetCiTTL(m, progressIndicator, appFilesCountAndSize)
    } catch (e: Exception) {
      // Rollback to model's value
      chatItemTTL.value = m.chatItemTTL.value
      afterSetCiTTL(m, progressIndicator, appFilesCountAndSize)
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_changing_message_deletion), e.stackTraceToString())
    }
  }
}

private fun afterSetCiTTL(
  m: ChatModel,
  progressIndicator: MutableState<Boolean>,
  appFilesCountAndSize: MutableState<Pair<Int, Long>>,
) {
  progressIndicator.value = false
  appFilesCountAndSize.value = directoryFileCountAndSize(appFilesDir.absolutePath)
  withApi {
    try {
      withContext(Dispatchers.Main) {
        // this is using current remote host on purpose - if it changes during update, it will load correct chats
        val chats = m.controller.apiGetChats(m.remoteHostId())
        chatModel.chatsContext.updateChats(chats)
      }
    } catch (e: Exception) {
      Log.e(TAG, "apiGetChats error: ${e.message}")
    }
  }
}

private fun deleteFilesAndMediaAlert(onConfirm: () -> Unit) {
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.delete_files_and_media_question),
    text = generalGetString(MR.strings.delete_files_and_media_desc),
    confirmText = generalGetString(MR.strings.delete_verb),
    onConfirm = onConfirm,
    destructive = true
  )
}

private fun deleteFiles(appFilesCountAndSize: MutableState<Pair<Int, Long>>) {
  deleteAppFiles()
  appFilesCountAndSize.value = directoryFileCountAndSize(appFilesDir.absolutePath)
}

private fun operationEnded(m: ChatModel, progressIndicator: MutableState<Boolean>, alert: () -> Unit) {
  m.chatDbChanged.value = true
  progressIndicator.value = false
  alert.invoke()
}

@Preview/*(
  uiMode = Configuration.UI_MODE_NIGHT_YES,
  showBackground = true,
  name = "Dark Mode"
)*/
@Composable
fun PreviewDatabaseLayout() {
  SimpleXTheme {
    DatabaseLayout(
      progressIndicator = false,
      stopped = false,
      useKeyChain = false,
      chatDbEncrypted = false,
      passphraseSaved = false,
      initialRandomDBPassphrase = SharedPreference({ true }, {}),
      importArchiveLauncher = rememberFileChooserLauncher(true) {},
      appFilesCountAndSize = remember { mutableStateOf(0 to 0L) },
      chatItemTTL = remember { mutableStateOf(ChatItemTTL.None) },
      currentUser = User.sampleData,
      users = listOf(UserInfo.sampleData),
      startChat = {},
      stopChatAlert = {},
      exportArchive = {},
      deleteChatAlert = {},
      deleteAppFilesAndMedia = {},
      onChatItemTTLSelected = {},
      disconnectAllHosts = {},
    )
  }
}
