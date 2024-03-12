package chat.simplex.common.views.migration

import SectionBottomSpacer
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.rotate
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.getNetCfg
import chat.simplex.common.model.ChatController.startChat
import chat.simplex.common.model.ChatController.startChatWithTemporaryDatabase
import chat.simplex.common.model.ChatCtrl
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.database.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.newchat.LinkTextView
import chat.simplex.common.views.newchat.SimpleXLinkQRCode
import chat.simplex.common.views.usersettings.*
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.datetime.*
import kotlinx.serialization.*
import java.io.File
import java.net.URLEncoder
import kotlin.math.max

@Serializable
data class MigrationFileLinkData(
  val networkConfig: NetworkConfig?,
) {
  @Serializable
  data class NetworkConfig(
    val socksProxy: String?,
    val hostMode: HostMode?,
    val requiredHostMode: Boolean?
  ) {
    fun hasOnionConfigured(): Boolean = socksProxy != null || hostMode == HostMode.Onion

    fun transformToPlatformSupported(): NetworkConfig {
      return if (hostMode != null && requiredHostMode != null) {
        NetworkConfig(
          socksProxy = if (hostMode == HostMode.Onion) socksProxy ?: NetCfg.proxyDefaults.socksProxy else socksProxy,
          hostMode = if (hostMode == HostMode.Onion) HostMode.OnionViaSocks else hostMode,
          requiredHostMode = requiredHostMode
        )
      } else this
    }
  }

  fun addToLink(link: String) = link + "&data=" + URLEncoder.encode(jsonShort.encodeToString(this), "UTF-8")

  companion object {
    suspend fun readFromLink(link: String): MigrationFileLinkData? =
      try {
        // val data = link.substringAfter("&data=").substringBefore("&")
        // json.decodeFromString(URLDecoder.decode(data, "UTF-8"))
        controller.standaloneFileInfo(link)
      } catch (e: Exception) {
        null
      }
  }
}



@Serializable
private sealed class MigrationFromState {
  @Serializable object ChatStopInProgress: MigrationFromState()
  @Serializable data class ChatStopFailed(val reason: String): MigrationFromState()
  @Serializable object PassphraseNotSet: MigrationFromState()
  @Serializable object PassphraseConfirmation: MigrationFromState()
  @Serializable object UploadConfirmation: MigrationFromState()
  @Serializable object Archiving: MigrationFromState()
  @Serializable data class DatabaseInit(val totalBytes: Long, val archivePath: String): MigrationFromState()
  @Serializable data class UploadProgress(val uploadedBytes: Long, val totalBytes: Long, val fileId: Long, val archivePath: String, val ctrl: ChatCtrl, val user: User): MigrationFromState()
  @Serializable data class UploadFailed(val totalBytes: Long, val archivePath: String): MigrationFromState()
  @Serializable object LinkCreation: MigrationFromState()
  @Serializable data class LinkShown(val fileId: Long, val link: String, val  ctrl: ChatCtrl): MigrationFromState()
  @Serializable data class Finished(val chatDeletion: Boolean): MigrationFromState()
}

private var MutableState<MigrationFromState>.state: MigrationFromState
  get() = value
  set(v) { value = v }

@Composable
fun MigrateFromDeviceView(close: () -> Unit) {
  val migrationState = rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf<MigrationFromState>(MigrationFromState.ChatStopInProgress) }
  // Prevent from hiding the view until migration is finished or app deleted
  val backDisabled = remember {
    derivedStateOf {
      when (migrationState.value) {
        is MigrationFromState.ChatStopInProgress,
        is MigrationFromState.DatabaseInit,
        is MigrationFromState.Archiving,
        is MigrationFromState.LinkShown,
        is MigrationFromState.Finished -> true

        is MigrationFromState.ChatStopFailed,
        is MigrationFromState.PassphraseNotSet,
        is MigrationFromState.PassphraseConfirmation,
        is MigrationFromState.UploadConfirmation,
        is MigrationFromState.UploadProgress,
        is MigrationFromState.UploadFailed,
        is MigrationFromState.LinkCreation -> false
      }
    }
  }
  val chatReceiver = remember { mutableStateOf(null as MigrationFromChatReceiver?) }
  ModalView(
    enableClose = !backDisabled.value,
    close = {
      withBGApi {
        migrationState.cleanUpOnBack(chatReceiver.value)
      }
      close()
    },
  ) {
    MigrateToAnotherDeviceLayout(
      migrationState = migrationState,
      chatReceiver = chatReceiver
    )
  }
}

@Composable
private fun MigrateToAnotherDeviceLayout(
  migrationState: MutableState<MigrationFromState>,
  chatReceiver: MutableState<MigrationFromChatReceiver?>
) {
  val tempDatabaseFile = rememberSaveable { mutableStateOf(fileForTemporaryDatabase()) }

  Column(
    Modifier.fillMaxSize().verticalScroll(rememberScrollState()).height(IntrinsicSize.Max),
  ) {
    AppBarTitle(stringResource(MR.strings.migrate_from_device_title))
    SectionByState(migrationState, tempDatabaseFile.value, chatReceiver)
    SectionBottomSpacer()
  }
  platform.androidLockPortraitOrientation()
}

@Composable
private fun SectionByState(
  migrationState: MutableState<MigrationFromState>,
  tempDatabaseFile: File,
  chatReceiver: MutableState<MigrationFromChatReceiver?>
) {
  when (val s = migrationState.value) {
    is MigrationFromState.ChatStopInProgress -> migrationState.ChatStopInProgressView()
    is MigrationFromState.ChatStopFailed -> migrationState.ChatStopFailedView(s.reason)
    is MigrationFromState.PassphraseNotSet -> migrationState.PassphraseNotSetView()
    is MigrationFromState.PassphraseConfirmation -> migrationState.PassphraseConfirmationView()
    is MigrationFromState.UploadConfirmation -> migrationState.UploadConfirmationView()
    is MigrationFromState.Archiving -> migrationState.ArchivingView()
    is MigrationFromState.DatabaseInit -> migrationState.DatabaseInitView(tempDatabaseFile, s.totalBytes, s.archivePath)
    is MigrationFromState.UploadProgress -> migrationState.UploadProgressView(s.uploadedBytes, s.totalBytes, s.ctrl, s.user, tempDatabaseFile, chatReceiver, s.archivePath)
    is MigrationFromState.UploadFailed -> migrationState.UploadFailedView(s.totalBytes, s.archivePath, chatReceiver.value)
    is MigrationFromState.LinkCreation -> LinkCreationView()
    is MigrationFromState.LinkShown -> migrationState.LinkShownView(s.fileId, s.link, s.ctrl)
    is MigrationFromState.Finished -> migrationState.FinishedView(s.chatDeletion)
  }
}

@Composable
private fun MutableState<MigrationFromState>.ChatStopInProgressView() {
  Box {
    SectionView(stringResource(MR.strings.migrate_from_device_stopping_chat).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    stopChat()
  }
}

@Composable
private fun MutableState<MigrationFromState>.ChatStopFailedView(reason: String) {
  SectionView(stringResource(MR.strings.error_stopping_chat).uppercase()) {
    Text(reason)
    SectionSpacer()
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_report_filled),
      text = stringResource(MR.strings.auth_stop_chat),
      textColor = MaterialTheme.colors.error,
      click = ::stopChat
    ){}
    SectionTextFooter(stringResource(MR.strings.migrate_from_device_chat_should_be_stopped))
  }
}

@Composable
private fun MutableState<MigrationFromState>.PassphraseNotSetView() {
  DatabaseEncryptionView(chatModel, true)
  KeyChangeEffect(appPreferences.initialRandomDBPassphrase.state.value) {
    if (!appPreferences.initialRandomDBPassphrase.get()) {
      state = MigrationFromState.UploadConfirmation
    }
  }
}

@Composable
private fun MutableState<MigrationFromState>.PassphraseConfirmationView() {
  val useKeychain = remember { appPreferences.storeDBPassphrase.get() }
  val currentKey = rememberSaveable { mutableStateOf("") }
  val verifyingPassphrase = rememberSaveable { mutableStateOf(false) }
  Box {
    val view = LocalMultiplatformView()
    Column {
      ChatStoppedView()
      SectionSpacer()

      SectionView(stringResource(MR.strings.migrate_from_device_verify_database_passphrase).uppercase()) {
        PassphraseField(currentKey, placeholder = stringResource(MR.strings.current_passphrase), Modifier.padding(horizontal = DEFAULT_PADDING), isValid = ::validKey, requestFocus = true)

        SettingsActionItemWithContent(
          icon = painterResource(if (useKeychain) MR.images.ic_vpn_key_filled else MR.images.ic_lock),
          text = stringResource(MR.strings.migrate_from_device_verify_passphrase),
          textColor = MaterialTheme.colors.primary,
          disabled = verifyingPassphrase.value || currentKey.value.isEmpty(),
          click = {
            verifyingPassphrase.value = true
            hideKeyboard(view)
            withBGApi {
              verifyDatabasePassphrase(currentKey.value)
              verifyingPassphrase.value = false
            }
          }
        ) {}
        SectionTextFooter(stringResource(MR.strings.migrate_from_device_confirm_you_remember_passphrase))
      }
    }
    if (verifyingPassphrase.value) {
      ProgressView()
    }
  }
}

@Composable
private fun MutableState<MigrationFromState>.UploadConfirmationView() {
  SectionView(stringResource(MR.strings.migrate_from_device_confirm_upload).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_ios_share),
      text = stringResource(MR.strings.migrate_from_device_archive_and_upload),
      textColor = MaterialTheme.colors.primary,
      click = { state = MigrationFromState.Archiving }
    ){}
    SectionTextFooter(stringResource(MR.strings.migrate_from_device_all_data_will_be_uploaded))
  }
}

@Composable
private fun MutableState<MigrationFromState>.ArchivingView() {
  Box {
    SectionView(stringResource(MR.strings.migrate_from_device_archiving_database).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    exportArchive()
  }
}

@Composable
private fun MutableState<MigrationFromState>.DatabaseInitView(tempDatabaseFile: File, totalBytes: Long, archivePath: String) {
  Box {
    SectionView(stringResource(MR.strings.migrate_from_device_database_init).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    prepareDatabase(tempDatabaseFile, totalBytes, archivePath)
  }
}

@Composable
private fun MutableState<MigrationFromState>.UploadProgressView(
  uploadedBytes: Long,
  totalBytes: Long,
  ctrl: ChatCtrl,
  user: User,
  tempDatabaseFile: File,
  chatReceiver: MutableState<MigrationFromChatReceiver?>,
  archivePath: String,
) {
  Box {
    SectionView(stringResource(MR.strings.migrate_from_device_uploading_archive).uppercase()) {
      val ratio = uploadedBytes.toFloat() / max(totalBytes, 1)
      LargeProgressView(ratio, "${(ratio * 100).toInt()}%", stringResource(MR.strings.migrate_from_device_bytes_uploaded).format(formatBytes(uploadedBytes)))
    }
  }
  LaunchedEffect(Unit) {
    startUploading(totalBytes, ctrl, user, tempDatabaseFile, chatReceiver, archivePath)
  }
}

@Composable
private fun MutableState<MigrationFromState>.UploadFailedView(totalBytes: Long, archivePath: String, chatReceiver: MigrationFromChatReceiver?) {
  SectionView(stringResource(MR.strings.migrate_from_device_upload_failed).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_ios_share),
      text = stringResource(MR.strings.migrate_from_device_repeat_upload),
      textColor = MaterialTheme.colors.primary,
      click = {
        state = MigrationFromState.DatabaseInit(totalBytes, archivePath)
      }
    ) {}
    SectionTextFooter(stringResource(MR.strings.migrate_from_device_try_again))
  }
  LaunchedEffect(Unit) {
    chatReceiver?.stopAndCleanUp()
  }
}

@Composable
private fun LinkCreationView() {
  Box {
    SectionView(stringResource(MR.strings.migrate_from_device_creating_archive_link).uppercase()) {}
    ProgressView()
  }
}

@Composable
private fun MutableState<MigrationFromState>.LinkShownView(fileId: Long, link: String, ctrl: ChatCtrl) {
  SectionView {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_close),
      text = stringResource(MR.strings.migrate_from_device_cancel_migration),
      textColor = MaterialTheme.colors.error,
      click = {
        cancelMigration(fileId, ctrl)
      }
    ) {}
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_check),
      text = stringResource(MR.strings.migrate_from_device_finalize_migration),
      textColor = MaterialTheme.colors.primary,
      click = {
        finishMigration(fileId, ctrl)
      }
    ) {}
    SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_archive_will_be_deleted))
    SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_choose_migrate_from_another_device))
  }
  SectionSpacer()
  SectionView(stringResource(MR.strings.show_QR_code).uppercase()) {
    SimpleXLinkQRCode(link, onShare = {})
  }
  SectionSpacer()
  SectionView(stringResource(MR.strings.migrate_from_device_or_share_this_file_link).uppercase()) {
    LinkTextView(link, true)
  }
}

@Composable
private fun MutableState<MigrationFromState>.FinishedView(chatDeletion: Boolean) {
  Box {
    SectionView(stringResource(MR.strings.migrate_from_device_migration_complete).uppercase()) {
      SettingsActionItemWithContent(
        icon = painterResource(MR.images.ic_delete_forever),
        text = stringResource(MR.strings.migrate_from_device_delete_database_from_device),
        textColor = MaterialTheme.colors.primary,
        click = {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.delete_chat_profile_question),
            text = generalGetString(MR.strings.delete_chat_profile_action_cannot_be_undone_warning),
            confirmText = generalGetString(MR.strings.delete_verb),
            onConfirm = {
              deleteChatAndDismiss()
            }
          )
        }
      ) {}

      SettingsActionItemWithContent(
        icon = painterResource(MR.images.ic_play_arrow_filled),
        text = stringResource(MR.strings.migrate_from_device_start_chat),
        textColor = MaterialTheme.colors.error,
        click = {
          AlertManager.shared.showAlertDialog(
            title = generalGetString(MR.strings.start_chat_question),
            text = generalGetString(MR.strings.migrate_from_device_starting_chat_on_multiple_devices_unsupported),
            confirmText = generalGetString(MR.strings.migrate_from_device_start_chat),
            onConfirm = {
              withLongRunningApi { startChatAndDismiss() }
            }
          )
        }
      ) {}
      SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_you_must_not_start_database_on_two_device))
      SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_using_on_two_device_breaks_encryption))
    }
    if (chatDeletion) {
      ProgressView()
    }
  }
}

@Composable
private fun ProgressView() {
  DefaultProgressView(null)
}

@Composable
fun LargeProgressView(value: Float, title: String, description: String) {
  Box(Modifier.padding(DEFAULT_PADDING).fillMaxSize(), contentAlignment = Alignment.Center) {
    CircularProgressIndicator(
      progress = value,
      (if (appPlatform.isDesktop) Modifier.size(DEFAULT_START_MODAL_WIDTH) else Modifier.size(windowWidth() - DEFAULT_PADDING * 2))
        .rotate(-90f),
      color = MaterialTheme.colors.primary,
      strokeWidth = 25.dp
    )
    Column(horizontalAlignment = Alignment.CenterHorizontally) {
      Text(description, color = Color.Transparent)
      Text(title, style = MaterialTheme.typography.h1.copy(fontSize = 50.sp, fontWeight = FontWeight.Bold), color = MaterialTheme.colors.primary)
      Text(description, style = MaterialTheme.typography.subtitle1)
    }
  }
}

private fun MutableState<MigrationFromState>.stopChat() {
  withBGApi {
    try {
      stopChatAsync(chatModel)
      try {
        controller.apiSaveAppSettings(AppSettings.current.prepareForExport())
        state = if (appPreferences.initialRandomDBPassphrase.get()) MigrationFromState.PassphraseNotSet else MigrationFromState.PassphraseConfirmation
      } catch (e: Exception) {
        AlertManager.shared.showAlertMsg(
          title = generalGetString(MR.strings.migrate_from_device_error_saving_settings),
          text = e.stackTraceToString()
        )
        state = MigrationFromState.ChatStopFailed(reason = generalGetString(MR.strings.migrate_from_device_error_saving_settings))
      }
    } catch (e: Exception) {
      state = MigrationFromState.ChatStopFailed(reason = e.stackTraceToString().take(10))
    }
  }
}

private suspend fun MutableState<MigrationFromState>.verifyDatabasePassphrase(dbKey: String) {
  val error = controller.testStorageEncryption(dbKey)
  if (error == null) {
    state = MigrationFromState.UploadConfirmation
  } else if (((error.chatError as? ChatError.ChatErrorDatabase)?.databaseError as? DatabaseError.ErrorOpen)?.sqliteError is SQLiteError.ErrorNotADatabase) {
    showErrorOnMigrationIfNeeded(DBMigrationResult.ErrorNotADatabase(""))
  } else {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.error),
      text = generalGetString(MR.strings.migrate_from_device_error_verifying_passphrase) + " " + error.details
    )
  }
}

private fun MutableState<MigrationFromState>.exportArchive() {
  withLongRunningApi {
    try {
      getMigrationTempFilesDirectory().mkdir()
      val archivePath = exportChatArchive(chatModel, getMigrationTempFilesDirectory(), mutableStateOf(""), mutableStateOf(Instant.DISTANT_PAST), mutableStateOf(""))
      val totalBytes = File(archivePath).length()
      if (totalBytes > 0L) {
        state = MigrationFromState.DatabaseInit(totalBytes, archivePath)
      } else {
        AlertManager.shared.showAlertMsg(generalGetString(MR.strings.migrate_from_device_exported_file_doesnt_exist))
        state = MigrationFromState.UploadConfirmation
      }
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.migrate_from_device_error_exporting_archive),
        text = e.stackTraceToString()
      )
      state = MigrationFromState.UploadConfirmation
    }
  }
}

suspend fun initTemporaryDatabase(tempDatabaseFile: File, netCfg: NetCfg): Pair<ChatCtrl, User>? {
  val (status, ctrl) = chatInitTemporaryDatabase(tempDatabaseFile.absolutePath)
  showErrorOnMigrationIfNeeded(status)
  try {
    if (ctrl != null) {
      val user = startChatWithTemporaryDatabase(ctrl, netCfg)
      return if (user != null) ctrl to user else null
    }
  } catch (e: Throwable) {
    Log.e(TAG, "Error while starting chat in temporary database: ${e.stackTraceToString()}")
  }
  return null
}

private fun MutableState<MigrationFromState>.prepareDatabase(
  tempDatabaseFile: File,
  totalBytes: Long,
  archivePath: String,
) {
  withLongRunningApi {
    val ctrlAndUser = initTemporaryDatabase(tempDatabaseFile, getNetCfg())
    if (ctrlAndUser == null) {
      state = MigrationFromState.UploadFailed(totalBytes, archivePath)
      return@withLongRunningApi
    }

    val (ctrl, user) = ctrlAndUser
    state = MigrationFromState.UploadProgress(0L, totalBytes, 0L, archivePath, ctrl, user)
  }
}

private fun MutableState<MigrationFromState>.startUploading(
  totalBytes: Long,
  ctrl: ChatCtrl,
  user: User,
  tempDatabaseFile: File,
  chatReceiver: MutableState<MigrationFromChatReceiver?>,
  archivePath: String,
) {
  withBGApi {
    chatReceiver.value = MigrationFromChatReceiver(ctrl, tempDatabaseFile) { msg ->
      when (msg) {
        is CR.SndFileProgressXFTP -> {
          val s = state
          if (s is MigrationFromState.UploadProgress && s.uploadedBytes != s.totalBytes) {
            state = MigrationFromState.UploadProgress(msg.sentSize, msg.totalSize, msg.fileTransferMeta.fileId, archivePath, ctrl, user)
          }
        }
        is CR.SndFileRedirectStartXFTP -> {
          delay(500)
          state = MigrationFromState.LinkCreation
        }
        is CR.SndStandaloneFileComplete -> {
          delay(500)
          val cfg = getNetCfg()
          val data = MigrationFileLinkData(
            networkConfig = MigrationFileLinkData.NetworkConfig(
              socksProxy = cfg.socksProxy,
              hostMode = cfg.hostMode,
              requiredHostMode = cfg.requiredHostMode
            )
          )
          state = MigrationFromState.LinkShown(msg.fileTransferMeta.fileId, data.addToLink(msg.rcvURIs[0]), ctrl)
        }
        is CR.SndFileError -> {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.migrate_from_device_upload_failed),
            generalGetString(MR.strings.migrate_from_device_check_connection_and_try_again)
          )
          state = MigrationFromState.UploadFailed(totalBytes, archivePath)
        }
        else -> {
          Log.d(TAG, "unsupported event: ${msg.responseType}")
        }
      }
    }

    chatReceiver.value?.start()

    val (res, error) = controller.uploadStandaloneFile(user, CryptoFile.plain(File(archivePath).name), ctrl)
    if (res == null) {
      state = MigrationFromState.UploadFailed(totalBytes, archivePath)
      return@withBGApi AlertManager.shared.showAlertMsg(
        generalGetString(MR.strings.migrate_from_device_error_uploading_archive),
        error
      )
    }
    state = MigrationFromState.UploadProgress(0, res.fileSize, res.fileId, archivePath, ctrl, user)
  }
}

private suspend fun cancelUploadedArchive(fileId: Long, ctrl: ChatCtrl) {
  controller.apiCancelFile(null, fileId, ctrl)
}

private fun cancelMigration(fileId: Long, ctrl: ChatCtrl) {
  withBGApi {
    cancelUploadedArchive(fileId, ctrl)
    startChatAndDismiss()
  }
}

private fun MutableState<MigrationFromState>.finishMigration(fileId: Long, ctrl: ChatCtrl) {
  withBGApi {
    cancelUploadedArchive(fileId, ctrl)
    state = MigrationFromState.Finished(false)
  }
}

private fun MutableState<MigrationFromState>.deleteChatAndDismiss() {
  withBGApi {
    try {
      deleteChatAsync(chatModel)
      chatModel.chatDbChanged.value = true
      state = MigrationFromState.Finished(true)
      try {
        initChatController(startChat = { CompletableDeferred(false) })
        chatModel.chatDbChanged.value = false
        ModalManager.fullscreen.closeModals()
      } catch (e: Exception) {
        throw Exception(generalGetString(MR.strings.error_starting_chat) + "\n" + e.stackTraceToString())
      }
    } catch (e: Exception) {
      AlertManager.shared.showAlertMsg(
        title = generalGetString(MR.strings.migrate_from_device_error_deleting_database),
        text = e.stackTraceToString()
      )
    }
  }
}

private suspend fun startChatAndDismiss(dismiss: Boolean = true) {
  try {
    val user = chatModel.currentUser.value
    if (chatModel.chatDbChanged.value) {
      initChatController()
      chatModel.chatDbChanged.value = false
    } else if (user != null) {
      startChat(user)
    }
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.error_starting_chat),
      text = e.stackTraceToString()
    )
  }
  // Hide settings anyway if chatDbStatus is not ok, probably passphrase needs to be entered
  if (dismiss || chatModel.chatDbStatus.value != DBMigrationResult.OK) {
    ModalManager.fullscreen.closeModals()
  }
}

private suspend fun MutableState<MigrationFromState>.cleanUpOnBack(chatReceiver: MigrationFromChatReceiver?) {
  val s = state
  if (s !is MigrationFromState.LinkShown && s !is MigrationFromState.Finished) {
    chatModel.switchingUsersAndHosts.value = true
    startChatAndDismiss(false)
    chatModel.switchingUsersAndHosts.value = false
  }
  if (s is MigrationFromState.UploadProgress) {
    cancelUploadedArchive(s.fileId, s.ctrl)
  }
  chatReceiver?.stopAndCleanUp()
  getMigrationTempFilesDirectory().deleteRecursively()
}

private fun fileForTemporaryDatabase(): File =
  File(getMigrationTempFilesDirectory(), generateNewFileName("migration", "db", getMigrationTempFilesDirectory()))

private class MigrationFromChatReceiver(
  val ctrl: ChatCtrl,
  val databaseUrl: File,
  var receiveMessages: Boolean = true,
  val processReceivedMsg: suspend (CR) -> Unit
) {
  fun start() {
    Log.d(TAG, "MigrationChatReceiver startReceiver")
    CoroutineScope(Dispatchers.IO).launch {
      while (receiveMessages) {
        try {
          val msg = ChatController.recvMsg(ctrl)
          if (msg != null && receiveMessages) {
            val r = msg.resp
            val rhId = msg.remoteHostId
            Log.d(TAG, "processReceivedMsg: ${r.responseType}")
            chatModel.addTerminalItem(TerminalItem.resp(rhId, r))
            val finishedWithoutTimeout = withTimeoutOrNull(60_000L) {
              processReceivedMsg(r)
            }
            if (finishedWithoutTimeout == null) {
              Log.e(TAG, "Timeout reached while processing received message: " + msg.resp.responseType)
              if (appPreferences.developerTools.get() && appPreferences.showSlowApiCalls.get()) {
                AlertManager.shared.showAlertMsg(
                  title = generalGetString(MR.strings.possible_slow_function_title),
                  text = generalGetString(MR.strings.possible_slow_function_desc).format(60, msg.resp.responseType + "\n" + Exception().stackTraceToString()),
                  shareText = true
                )
              }
            }
          }
        } catch (e: Exception) {
          Log.e(TAG, "MigrationChatReceiver recvMsg/processReceivedMsg exception: " + e.stackTraceToString())
        } catch (e: Exception) {
          Log.e(TAG, "MigrationChatReceiver recvMsg/processReceivedMsg throwable: " + e.stackTraceToString())
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
        }
      }
    }
  }

  fun stopAndCleanUp() {
    Log.d(TAG, "MigrationChatReceiver.stop")
    receiveMessages = false
    chatCloseStore(ctrl)
    File(databaseUrl.absolutePath + "_chat.db").delete()
    File(databaseUrl.absolutePath + "_agent.db").delete()
  }
}
