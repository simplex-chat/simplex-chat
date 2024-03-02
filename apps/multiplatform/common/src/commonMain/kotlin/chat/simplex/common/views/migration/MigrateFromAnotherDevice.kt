package chat.simplex.common.views.migration

import SectionItemView
import SectionTextFooter
import SectionView
import androidx.compose.foundation.*
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.model.AppPreferences.Companion.SHARED_PREFS_MIGRATION_STAGE
import chat.simplex.common.model.ChatController.startChat
import chat.simplex.common.model.ChatController.startChatWithTemporaryDatabase
import chat.simplex.common.model.ChatCtrl
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.database.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.DatabaseUtils.ksDatabasePassword
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.usersettings.SettingsActionItemWithContent
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.datetime.Clock
import kotlinx.datetime.toJavaInstant
import kotlinx.serialization.*
import java.io.File
import java.text.SimpleDateFormat
import java.util.*
import kotlin.math.max

@Serializable
sealed class MigrationFromAnotherDeviceState {
  data class DownloadProgress(val link: String, val archiveName: String): MigrationFromAnotherDeviceState()
  data class ArchiveImport(val archiveName: String): MigrationFromAnotherDeviceState()
  object Passphrase: MigrationFromAnotherDeviceState()

  companion object  {
    // Here we check whether it's needed to show migration process after app restart or not
    // It's important to NOT show the process when archive was corrupted/not fully downloaded
    fun transform(): MigrationFromAnotherDeviceState? {
      val stage = settings.getStringOrNull(SHARED_PREFS_MIGRATION_STAGE)
      val state: MigrationFromAnotherDeviceState? = if (stage != null) json.decodeFromString(stage) else null
      if (state is DownloadProgress) {
        // iOS changes absolute directory every launch, check this way
        val archivePath = File(getMigrationTempFilesDirectory(), state.archiveName)
        archivePath.delete()
        settings.remove(SHARED_PREFS_MIGRATION_STAGE)
        // No migration happens at the moment actually since archive were not downloaded fully
        Log.e(TAG, "MigrateFromDevice: archive wasn't fully downloaded, removed broken file")
        return null
      }
      return state
    }

    fun save(state: MigrationFromAnotherDeviceState?) {
      if (state != null) {
        appPreferences.migrationStage.set(json.encodeToString(state))
      } else {
        appPreferences.migrationStage.set(null)
      }
      chatModel.migrationState.value = state
    }
  }
}

@Serializable
private sealed class MigrationState {
  object PasteOrScanLink: MigrationState()
  data class LinkDownloading(val link: String): MigrationState()
  data class DownloadProgress(val downloadedBytes: Long, val totalBytes: Long, val fileId: Long, val link: String, val archivePath: String, val ctrl: ChatCtrl?): MigrationState()
  data class DownloadFailed(val totalBytes: Long, val link: String, val archivePath: String): MigrationState()
  data class ArchiveImport(val archivePath: String): MigrationState()
  data class ArchiveImportFailed(val archivePath: String): MigrationState()
  data class Passphrase(val passphrase: String): MigrationState()
  data class Migration(val passphrase: String): MigrationState()
  data class Onion(val appSettings: AppSettings): MigrationState()
}

private var MutableState<MigrationState>.state: MigrationState
  get() = value
  set(v) { value = v }

@Composable
fun MigrateFromAnotherDeviceView(state: MigrationFromAnotherDeviceState? = null, close: () -> Unit) {
  // Prevent from hiding the view until migration is finished or app deleted
  val backDisabled = remember { mutableStateOf(state != null && state !is MigrationFromAnotherDeviceState.ArchiveImport) }
  val migrationState = rememberSaveable(stateSaver = serializableSaver()) {
    mutableStateOf(
      when (state) {
        null -> MigrationState.PasteOrScanLink
        is MigrationFromAnotherDeviceState.DownloadProgress -> {
          val archivePath = File(getMigrationTempFilesDirectory(), state.archiveName)
          archivePath.delete()
          // SHOULDN'T BE HERE because the app checks this before opening migration screen and will not open it in this case.
          // See analyzeMigrationState()
          MigrationState.DownloadFailed(totalBytes = 0, link = state.link, archivePath = archivePath.absolutePath)
        }
        is MigrationFromAnotherDeviceState.ArchiveImport -> {
          val archivePath = File(getMigrationTempFilesDirectory(), state.archiveName)
          MigrationState.ArchiveImportFailed(archivePath.absolutePath)
        }
        is MigrationFromAnotherDeviceState.Passphrase -> {
          MigrationState.Passphrase("")
        }
      }
    )
  }
  ModalView(
    enableClose = !backDisabled.value,
    close = {
      close()
    },
  ) {
    MigrateFromAnotherDeviceLayout(
      migrationState = migrationState,
      backDisabled = backDisabled,
      close = close,
    )
  }
}

@Composable
private fun MigrateFromAnotherDeviceLayout(migrationState: MutableState<MigrationState>, backDisabled: MutableState<Boolean>, close: () -> Unit) {
  val tempDatabaseFile = rememberSaveable { mutableStateOf(fileForTemporaryDatabase()) }

  Column(
    Modifier.fillMaxWidth().verticalScroll(rememberScrollState()),
  ) {
    AppBarTitle(stringResource(MR.strings.migrate_here))
    SectionByState(migrationState, backDisabled, tempDatabaseFile.value, close)
  }
}

@Composable
private fun SectionByState(migrationState: MutableState<MigrationState>, backDisabled: MutableState<Boolean>, tempDatabaseFile: File, close: () -> Unit) {
  val chatReceiver = remember { mutableStateOf(null as MigrationChatReceiver?) }
  when (val s = migrationState.value) {
    is MigrationState.PasteOrScanLink -> migrationState.PasteOrScanLinkView()
    is MigrationState.LinkDownloading -> migrationState.LinkDownloadingView(s.link, tempDatabaseFile, chatReceiver)
    is MigrationState.DownloadProgress -> DownloadProgressView(s.downloadedBytes, totalBytes = s.totalBytes)
    is MigrationState.DownloadFailed -> migrationState.DownloadFailedView(totalBytes = s.totalBytes, chatReceiver.value, s.link, s.archivePath)
    is MigrationState.ArchiveImport -> migrationState.ArchiveImportView(s.archivePath)
    is MigrationState.ArchiveImportFailed -> migrationState.ArchiveImportFailedView(s.archivePath)
    is MigrationState.Passphrase -> migrationState.PassphraseEnteringView(currentKey = s.passphrase)
    is MigrationState.Migration -> migrationState.MigrationView(s.passphrase, close)
    is MigrationState.Onion -> migrationState.OnionView(appSettings = s.appSettings)
  }
  KeyChangeEffect(migrationState.value) {
    backDisabled.value = migrationState.value is MigrationState.ArchiveImport || chatModel.migrationState.value != null
  }
  DisposableEffectOnGone {
    val state = migrationState.value
    if (state is MigrationState.ArchiveImportFailed) {
      // Original database is not exist, nothing is set up correctly for showing to a user yet. Return to clean state
      deleteChatDatabaseFilesAndState()
      initChatControllerAndRunMigrations()
    } else if (state is MigrationState.DownloadProgress && state.ctrl != null) {
      withBGApi {
        stopArchiveDownloading(state.fileId, state.ctrl)
      }
    }
    chatReceiver.value?.stopAndCleanUp()
    if (!backDisabled.value) {
      getMigrationTempFilesDirectory().deleteRecursively()
      MigrationFromAnotherDeviceState.save(null)
    }
  }
}

@Composable
private fun MutableState<MigrationState>.PasteOrScanLinkView() {
  if (appPlatform.isAndroid) {
    SectionView(stringResource(MR.strings.scan_QR_code).uppercase()) {
      QRCodeScanner(showQRCodeScanner = remember { mutableStateOf(true) }) { text ->
        checkUserLink(text)
      }
    }
  }

  if (appPreferences.developerTools.get()) {
    SectionView(stringResource(if (appPlatform.isAndroid) MR.strings.or_paste_archive_link else MR.strings.paste_archive_link).uppercase()) {
      PasteLinkView()
    }
  }
}

@Composable
private fun MutableState<MigrationState>.PasteLinkView() {
  val clipboard = LocalClipboardManager.current
  SectionItemView({
    val str = clipboard.getText()?.text ?: return@SectionItemView
    checkUserLink(str)
  }) {
    Text(stringResource(MR.strings.tap_to_paste_link))
  }
}

@Composable
private fun MutableState<MigrationState>.LinkDownloadingView(link: String, tempDatabaseFile: File, chatReceiver: MutableState<MigrationChatReceiver?>) {
  Box {
    SectionView(stringResource(MR.strings.migration_from_device_downloading_details).uppercase()) {}
    DefaultProgressView("")
  }
  LaunchedEffect(Unit) {
    downloadLinkDetails(link, tempDatabaseFile, chatReceiver)
  }
}

@Composable
private fun DownloadProgressView(downloadedBytes: Long, totalBytes: Long) {
  Box {
    SectionView(stringResource(MR.strings.migration_from_device_downloading_archive).uppercase()) {
      val ratio = downloadedBytes.toFloat() / max(totalBytes, 1)
      LargeProgressView(ratio, "${(ratio * 100).toInt()}%", stringResource(MR.strings.migration_from_device_bytes_downloaded).format(formatBytes(downloadedBytes)))
    }
  }
}

@Composable
private fun MutableState<MigrationState>.DownloadFailedView(totalBytes: Long, chatReceiver: MigrationChatReceiver?, link: String, archivePath: String) {
  SectionView(stringResource(MR.strings.migration_from_device_download_failed).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_download),
      text = stringResource(MR.strings.migration_from_device_repeat_download),
      textColor = MaterialTheme.colors.primary,
      click = {
        state = MigrationState.LinkDownloading(archivePath)
      }
    ) {}
    SectionTextFooter(stringResource(MR.strings.migration_from_device_try_again))
  }
  LaunchedEffect(Unit) {
    chatReceiver?.stopAndCleanUp()
    File(archivePath).delete()
    MigrationFromAnotherDeviceState.save(null)
  }
}

@Composable
private fun MutableState<MigrationState>.ArchiveImportView(archivePath: String) {
  Box {
    SectionView(stringResource(MR.strings.migration_from_device_importing_archive).uppercase()) {}
    DefaultProgressView("")
  }
  LaunchedEffect(Unit) {
    importArchive(archivePath)
  }
}

@Composable
private fun MutableState<MigrationState>.ArchiveImportFailedView(archivePath: String) {
  SectionView(stringResource(MR.strings.migration_from_device_import_failed).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_download),
      text = stringResource(MR.strings.migration_from_device_repeat_download),
      textColor = MaterialTheme.colors.primary,
      click = {
        state = MigrationState.ArchiveImport(archivePath)
      }
    ) {}
    SectionTextFooter(stringResource(MR.strings.migration_from_device_try_again))
  }
}

@Composable
private fun MutableState<MigrationState>.PassphraseEnteringView(currentKey: String) {
  val currentKey = rememberSaveable { mutableStateOf(currentKey) }
  val verifyingPassphrase = rememberSaveable { mutableStateOf(false) }
  Box {
    val view = LocalMultiplatformView()
    SectionView(stringResource(MR.strings.migration_from_device_enter_passphrase).uppercase()) {
      PassphraseField(currentKey, placeholder = stringResource(MR.strings.current_passphrase), isValid = ::validKey)

      SettingsActionItemWithContent(
        icon = painterResource(MR.images.ic_vpn_key_filled),
        text = stringResource(MR.strings.open_chat),
        textColor = MaterialTheme.colors.primary,
        click = {
          verifyingPassphrase.value = true
          hideKeyboard(view)
          withBGApi {
            val (status, ctrl) = chatInitTemporaryDatabase(dataDir, key = currentKey.value)
            val success = status == DBMigrationResult.OK || status == DBMigrationResult.InvalidConfirmation
            if (success) {
              ChatController.ctrl = ctrl
              state = MigrationState.Migration(currentKey.value)
            } else {
              showErrorOnMigrationIfNeeded(status)
            }
            verifyingPassphrase.value = false
          }
        }
      ) {}
      SectionTextFooter(stringResource(MR.strings.migration_from_device_passphrase_will_be_stored))
    }
    if (verifyingPassphrase.value) {
      DefaultProgressView("")
    }
  }
}

@Composable
private fun MutableState<MigrationState>.MigrationView(passphrase: String, close: () -> Unit) {
  Box {
    SectionView(stringResource(MR.strings.migration_from_device_migrating).uppercase()) {}
    DefaultProgressView("")
  }
  LaunchedEffect(Unit) {
    startChat(passphrase, close)
  }
}

@Composable
private fun MutableState<MigrationState>.OnionView(appSettings: AppSettings) {
  /*val appSettings = rememberSaveable(stateSaver = serializableSaver()) { mutableStateOf(appSettings) }
  val onionHosts = remember { mutableStateOf(OnionHosts.NEVER) }
    List {
      Section {
        Button(action: {
          var updated = appSettings.networkConfig!
          let (hostMode, requiredHostMode) = onionHosts.hostMode
          updated.hostMode = hostMode
          updated.requiredHostMode = requiredHostMode
          appSettings.networkConfig = updated
          finishMigration(appSettings)
        }) {
        settingsRow("checkmark") {
          Text("Apply").foregroundColor(.accentColor)
        }
      }
      } header: {
        Text("Review .onion settings")
      } footer: {
        Text("Since you migrated the database between platforms, make sure settings for .onion hosts are correct")
          .font(.callout)
      }

      Section {
        Picker("Use .onion hosts", selection: $onionHosts) {
        ForEach(OnionHosts.values, id: \.self) { Text($0.text) }
      }
        .frame(height: 36)
      } footer: {
        let text: LocalizedStringKey = switch onionHosts {
          case .no:
          "Selected No"
          case .prefer:
          "Selected Prefer"
          case .require:
          "Selected Require"
        }
        Text(text).font(.callout)
      }
    }*/
}

@Composable
private fun LargeProgressView(value: Float, title: String, description: String) {
  Box(Modifier.fillMaxSize(), contentAlignment = Alignment.Center) {
      CircularProgressIndicator(
        progress = value,
        Modifier
          .padding(bottom = DEFAULT_PADDING)
          .fillMaxWidth()
          .align(Alignment.TopCenter),
        color = MaterialTheme.colors.primary,
        strokeWidth = 10.dp
      )
    Column {
      Text(description, color = Color.Transparent)
      Text(title, style = MaterialTheme.typography.h1, color = MaterialTheme.colors.primary)
      Text(description, style = MaterialTheme.typography.subtitle1)
    }
  }
}

private fun MutableState<MigrationState>.checkUserLink(link: String) {
  if (strHasSimplexFileLink(link.trim())) {
    state = MigrationState.LinkDownloading(link.trim())
  } else {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_file_link),
      text = generalGetString(MR.strings.the_text_you_pasted_is_not_a_link)
    )
  }
}


private fun MutableState<MigrationState>.downloadLinkDetails(link: String, tempDatabaseFile: File, chatReceiver: MutableState<MigrationChatReceiver?>) {
  val archiveTime = Clock.System.now()
  val ts = SimpleDateFormat("yyyy-MM-dd'T'HHmmss", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
  val archiveName = "simplex-chat.$ts.zip"
  val archivePath = File(getMigrationTempFilesDirectory(), archiveName)

  startDownloading(0, tempDatabaseFile, chatReceiver, link, archivePath.absolutePath)
}

private suspend fun initTemporaryDatabase(tempDatabaseFile: File): Pair<ChatCtrl, User>? {
  val (status, ctrl) = chatInitTemporaryDatabase(tempDatabaseFile)
  showErrorOnMigrationIfNeeded(status)
  try {
    if (ctrl != null) {
      val user = startChatWithTemporaryDatabase(ctrl)
      return if (user != null) ctrl to user else null
    }
  } catch (e: Throwable) {
    Log.e(TAG, "Error while starting chat in temporary database: ${e.stackTraceToString()}")
  }
  return null
}

private fun MutableState<MigrationState>.startDownloading(totalBytes: Long, tempDatabaseFile: File, chatReceiver: MutableState<MigrationChatReceiver?>, link: String, archivePath: String) {
  withBGApi {
    val ctrlAndUser = initTemporaryDatabase(tempDatabaseFile)
    if (ctrlAndUser == null) {
      state = MigrationState.DownloadFailed(totalBytes, link, archivePath)
      return@withBGApi
    }

    val (ctrl, user) = ctrlAndUser
    chatReceiver.value = MigrationChatReceiver(ctrl, tempDatabaseFile) { msg ->
        when (msg) {
          is CR.RcvFileProgressXFTP -> {
            state = MigrationState.DownloadProgress(msg.receivedSize, msg.totalSize, msg.rcvFileTransfer.fileId, link, archivePath, ctrl)
            MigrationFromAnotherDeviceState.save(MigrationFromAnotherDeviceState.DownloadProgress(link, File(archivePath).path))
          }
          is CR.RcvStandaloneFileComplete -> {
            delay(500)
            state = MigrationState.ArchiveImport(archivePath)
            MigrationFromAnotherDeviceState.save(MigrationFromAnotherDeviceState.ArchiveImport(File(archivePath).path))
          }
          is CR.RcvFileError -> {
            AlertManager.shared.showAlertMsg(
              generalGetString(MR.strings.migration_from_device_download_failed),
              generalGetString(MR.strings.migration_from_device_file_delete_or_link_invalid)
            )
            state = MigrationState.DownloadFailed(totalBytes, link, archivePath)
          }
          else -> Log.d(TAG, "unsupported event: ${msg.responseType}")
        }
    }
    chatReceiver.value?.start()

    val (res, error) = controller.downloadStandaloneFile(user, link, CryptoFile.plain(File(archivePath).path), ctrl)
    if (res == null) {
      state = MigrationState.DownloadFailed(totalBytes, link, archivePath)
      AlertManager.shared.showAlertMsg(
        generalGetString(MR.strings.migration_from_device_error_downloading_archive),
        error
      )
    }
  }
}

private fun MutableState<MigrationState>.importArchive(archivePath: String) {
  withBGApi {
    try {
      if (ChatController.ctrl == null || ChatController.ctrl == -1L) {
        chatInitControllerRemovingDatabases()
      }
      controller.apiDeleteStorage()
      try {
        val config = ArchiveConfig(archivePath)
        val archiveErrors = controller.apiImportArchive(config)
        if (archiveErrors.isNotEmpty()) {
          AlertManager.shared.showAlertMsg(
            generalGetString(MR.strings.chat_database_imported),
            generalGetString(MR.strings.non_fatal_errors_occured_during_import)
          )
        }
        state = MigrationState.Passphrase("")
        MigrationFromAnotherDeviceState.save(MigrationFromAnotherDeviceState.Passphrase)
      } catch (e: Exception) {
        state = MigrationState.ArchiveImportFailed(archivePath)
        AlertManager.shared.showAlertMsg (generalGetString(MR.strings.error_importing_database), e.stackTraceToString())
      }
    } catch (e: Exception) {
      state = MigrationState.ArchiveImportFailed(archivePath)
      AlertManager.shared.showAlertMsg (generalGetString(MR.strings.error_deleting_database), e.stackTraceToString())
    }
  }
}


private suspend fun stopArchiveDownloading(fileId: Long, ctrl: ChatCtrl) {
  controller.apiCancelFile(null, fileId, ctrl)
}

private fun cancelMigration(fileId: Long, ctrl: ChatCtrl, close: () -> Unit) {
  withBGApi {
    stopArchiveDownloading(fileId, ctrl)
    close()
  }
}

private fun MutableState<MigrationState>.startChat(passphrase: String, close: () -> Unit) {
  ksDatabasePassword.set(passphrase)
  appPreferences.storeDBPassphrase.set(true)
  appPreferences.initialRandomDBPassphrase.set(false)
  withBGApi {
    try {
      initChatController(useKey = passphrase) { CompletableDeferred(false) }
      var appSettings = controller.apiGetAppSettings(AppSettings.current)
      // LALAL
      if (true/*appSettings.networkConfig?.socksProxy != nil*/) {
        appSettings = appSettings.copy(
          networkConfig = appSettings.networkConfig?.copy(
            socksProxy = "127.0.0.1:1234",
            hostMode = HostMode.Public,
            requiredHostMode = true,
          )
        )
        state = MigrationState.Onion(appSettings)
      } else {
        finishMigration(appSettings, close)
      }
    } catch (e: Exception) {
      hideView(close)
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_starting_chat), e.stackTraceToString())
    }
  }
}

private suspend fun finishMigration(appSettings: AppSettings, close: () -> Unit) {
  try {
    getMigrationTempFilesDirectory().deleteRecursively()
    MigrationFromAnotherDeviceState.save(null)
    val user = chatModel.currentUser.value
    if (user != null) {
      startChat(user)
    }
    appSettings.importIntoApp()
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.migration_from_device_chat_migrated), generalGetString(MR.strings.migration_from_device_finalize_migration))
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_starting_chat), e.stackTraceToString())
  }
  hideView(close)
}

private fun hideView(close: () -> Unit) {
  appPreferences.onboardingStage.set(OnboardingStage.OnboardingComplete)
  close()
}

private fun strHasSimplexFileLink(text: String): Boolean =
  text.startsWith("simplex:/file") || text.startsWith("https://simplex.chat/file")

private fun fileForTemporaryDatabase(): File =
  File(getMigrationTempFilesDirectory(), generateNewFileName("migration", "db", getMigrationTempFilesDirectory()))

private class MigrationChatReceiver(
  val ctrl: ChatCtrl,
  val databaseUrl: File,
  val processReceivedMsg: suspend (CR) -> Unit,
) {
  fun start() {
    Log.d(TAG, "MigrationChatReceiver startReceiver")
    CoroutineScope(Dispatchers.IO).launch {
      while (true) {
        try {
          val msg = ChatController.recvMsg(ctrl)
          if (msg != null) {
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
          Log.e(TAG, "MigrationChatReceiver recvMsg/processReceivedMsg exception: " + e.stackTraceToString());
        } catch (e: Exception) {
          Log.e(TAG, "MigrationChatReceiver recvMsg/processReceivedMsg throwable: " + e.stackTraceToString())
          AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error), e.stackTraceToString())
        }
      }
    }
  }

  fun stopAndCleanUp() {
    Log.d(TAG, "MigrationChatReceiver.stop")
    chatCloseStore(ctrl)
    File(databaseUrl, "_chat.db").delete()
    File(databaseUrl, "_agent.db").delete()
  }
}
