package chat.simplex.common.views.migration

import SectionBottomSpacer
import SectionItemView
import SectionSpacer
import SectionTextFooter
import SectionView
import androidx.compose.foundation.layout.*
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalClipboardManager
import chat.simplex.common.model.*
import chat.simplex.common.model.AppPreferences.Companion.SHARED_PREFS_MIGRATION_TO_STAGE
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatController.getNetCfg
import chat.simplex.common.model.ChatController.startChat
import chat.simplex.common.model.ChatCtrl
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.platform.*
import chat.simplex.common.ui.theme.*
import chat.simplex.common.views.database.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.DatabaseUtils.ksDatabasePassword
import chat.simplex.common.views.newchat.QRCodeScanner
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.usersettings.*
import chat.simplex.common.views.usersettings.networkAndServers.OnionRelatedLayout
import chat.simplex.res.MR
import dev.icerock.moko.resources.compose.painterResource
import dev.icerock.moko.resources.compose.stringResource
import kotlinx.coroutines.*
import kotlinx.datetime.Clock
import kotlinx.datetime.toJavaInstant
import kotlinx.serialization.*
import java.io.File
import java.net.URI
import java.text.SimpleDateFormat
import java.util.*
import kotlin.math.max

@Serializable
sealed class MigrationToDeviceState {
  @Serializable @SerialName("onion") data class Onion(val link: String, val socksProxy: String?, val networkProxy: NetworkProxy?, val hostMode: HostMode, val requiredHostMode: Boolean): MigrationToDeviceState()
  @Serializable @SerialName("downloadProgress") data class DownloadProgress(val link: String, val archiveName: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToDeviceState()
  @Serializable @SerialName("archiveImport") data class ArchiveImport(val archiveName: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToDeviceState()
  @Serializable @SerialName("passphrase") data class Passphrase(val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToDeviceState()

  companion object  {
    // Here we check whether it's needed to show migration process after app restart or not
    // It's important to NOT show the process when archive was corrupted/not fully downloaded
    fun makeMigrationState(): MigrationToState? {
      val stage = settings.getStringOrNull(SHARED_PREFS_MIGRATION_TO_STAGE)
      val state: MigrationToDeviceState? = if (stage != null) json.decodeFromString(stage) else null
      val initial: MigrationToState? = when(state) {
        null -> null
        is DownloadProgress -> {
          // No migration happens at the moment actually since archive were not downloaded fully
          Log.e(TAG, "MigrateToDevice: archive wasn't fully downloaded, removed broken file")
          null
        }
        is Onion -> null
        is ArchiveImport -> {
          if (!File(getMigrationTempFilesDirectory(), state.archiveName).exists()) {
            Log.e(TAG, "MigrateToDevice: archive was removed unintentionally or state is broken, dropping migration")
            null
          } else {
            val archivePath = File(getMigrationTempFilesDirectory(), state.archiveName)
            MigrationToState.ArchiveImportFailed(archivePath.absolutePath, state.netCfg, state.networkProxy)
          }
        }
        is Passphrase -> MigrationToState.Passphrase("", state.netCfg, state.networkProxy)
      }
      if (initial == null) {
        settings.remove(SHARED_PREFS_MIGRATION_TO_STAGE)
        getMigrationTempFilesDirectory().deleteRecursively()
      }
      return initial
    }

    fun save(state: MigrationToDeviceState?) {
      if (state != null) {
        appPreferences.migrationToStage.set(json.encodeToString(state))
      } else {
        appPreferences.migrationToStage.set(null)
      }
    }
  }
}

@Serializable
sealed class MigrationToState {
  @Serializable object PasteOrScanLink: MigrationToState()
  @Serializable data class Onion(
    val link: String,
    // Legacy, remove in 2025
    @SerialName("socksProxy")
    val legacySocksProxy: String?,
    val networkProxy: NetworkProxy?,
    val hostMode: HostMode,
    val requiredHostMode: Boolean
  ): MigrationToState()
  @Serializable data class DatabaseInit(val link: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class LinkDownloading(val link: String, val ctrl: ChatCtrl, val user: User, val archivePath: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class DownloadProgress(val downloadedBytes: Long, val totalBytes: Long, val fileId: Long, val link: String, val archivePath: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?, val ctrl: ChatCtrl?): MigrationToState()
  @Serializable data class DownloadFailed(val totalBytes: Long, val link: String, val archivePath: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class ArchiveImport(val archivePath: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class ArchiveImportFailed(val archivePath: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class Passphrase(val passphrase: String, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class MigrationConfirmation(val status: DBMigrationResult, val passphrase: String, val useKeychain: Boolean, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
  @Serializable data class Migration(val passphrase: String, val confirmation: chat.simplex.common.views.helpers.MigrationConfirmation, val useKeychain: Boolean, val netCfg: NetCfg, val networkProxy: NetworkProxy?): MigrationToState()
}

private var MutableState<MigrationToState?>.state: MigrationToState?
  get() = value
  set(v) { value = v }

@Composable
fun ModalData.MigrateToDeviceView(close: () -> Unit) {
  val migrationState = remember { chatModel.migrationState }
  // Prevent from hiding the view until migration is finished or app deleted
  val backDisabled = remember {
    derivedStateOf {
      when (chatModel.migrationState.value) {
        null,
        is MigrationToState.PasteOrScanLink,
        is MigrationToState.Onion,
        is MigrationToState.LinkDownloading,
        is MigrationToState.DownloadProgress,
        is MigrationToState.DownloadFailed,
        is MigrationToState.ArchiveImportFailed -> false

        is MigrationToState.ArchiveImport,
        is MigrationToState.DatabaseInit,
        is MigrationToState.Migration,
        is MigrationToState.MigrationConfirmation,
        is MigrationToState.Passphrase -> true
      }
    }
  }
  val chatReceiver = remember { mutableStateOf(null as MigrationToChatReceiver?) }
  ModalView(
    enableClose = !backDisabled.value,
    close = {
      withBGApi {
        migrationState.cleanUpOnBack(chatReceiver.value)
        close()
      }
    },
  ) {
    MigrateToDeviceLayout(
      migrationState = migrationState,
      chatReceiver = chatReceiver,
      close = close,
    )
  }
}

@Composable
private fun ModalData.MigrateToDeviceLayout(
  migrationState: MutableState<MigrationToState?>,
  chatReceiver: MutableState<MigrationToChatReceiver?>,
  close: () -> Unit,
) {
  val tempDatabaseFile = rememberSaveable { mutableStateOf(fileForTemporaryDatabase()) }
  ColumnWithScrollBar(maxIntrinsicSize = true) {
    AppBarTitle(stringResource(MR.strings.migrate_to_device_title))
    SectionByState(migrationState, tempDatabaseFile.value, chatReceiver, close)
    SectionBottomSpacer()
  }
  platform.androidLockPortraitOrientation()
}

@Composable
private fun ModalData.SectionByState(
  migrationState: MutableState<MigrationToState?>,
  tempDatabaseFile: File,
  chatReceiver: MutableState<MigrationToChatReceiver?>,
  close: () -> Unit
) {
  when (val s = migrationState.value) {
    null -> {}
    is MigrationToState.PasteOrScanLink -> migrationState.PasteOrScanLinkView(close)
    is MigrationToState.Onion -> OnionView(s.link, s.legacySocksProxy, s.networkProxy, s.hostMode, s.requiredHostMode, migrationState)
    is MigrationToState.DatabaseInit -> migrationState.DatabaseInitView(s.link, tempDatabaseFile, s.netCfg, s.networkProxy)
    is MigrationToState.LinkDownloading -> migrationState.LinkDownloadingView(s.link, s.ctrl, s.user, s.archivePath, tempDatabaseFile, chatReceiver, s.netCfg, s.networkProxy)
    is MigrationToState.DownloadProgress -> DownloadProgressView(s.downloadedBytes, totalBytes = s.totalBytes)
    is MigrationToState.DownloadFailed -> migrationState.DownloadFailedView(s.link, chatReceiver.value, s.archivePath, s.netCfg, s.networkProxy)
    is MigrationToState.ArchiveImport -> migrationState.ArchiveImportView(s.archivePath, s.netCfg, s.networkProxy)
    is MigrationToState.ArchiveImportFailed -> migrationState.ArchiveImportFailedView(s.archivePath, s.netCfg, s.networkProxy)
    is MigrationToState.Passphrase -> migrationState.PassphraseEnteringView(currentKey = s.passphrase, s.netCfg, s.networkProxy)
    is MigrationToState.MigrationConfirmation -> migrationState.MigrationConfirmationView(s.status, s.passphrase, s.useKeychain, s.netCfg, s.networkProxy)
    is MigrationToState.Migration -> MigrationView(s.passphrase, s.confirmation, s.useKeychain, s.netCfg, s.networkProxy, close)
  }
}

@Composable
private fun MutableState<MigrationToState?>.PasteOrScanLinkView(close: () -> Unit) {
  Box {
    val progressIndicator = remember { mutableStateOf(false) }
    Column {
      if (appPlatform.isAndroid) {
        SectionView(stringResource(MR.strings.scan_QR_code).replace('\n', ' ').uppercase()) {
          QRCodeScanner(showQRCodeScanner = remember { mutableStateOf(true) }) { text ->
            checkUserLink(text)
          }
        }
        SectionSpacer()
      }

      SectionView(stringResource(if (appPlatform.isAndroid) MR.strings.or_paste_archive_link else MR.strings.paste_archive_link).uppercase()) {
        PasteLinkView()
      }
      SectionSpacer()

      SectionView(stringResource(MR.strings.chat_archive).uppercase()) {
        ArchiveImportView(progressIndicator, close)
      }
    }
    if (progressIndicator.value)
    ProgressView()
  }
}

@Composable
private fun MutableState<MigrationToState?>.PasteLinkView() {
  val clipboard = LocalClipboardManager.current
  SectionItemView({
    val str = clipboard.getText()?.text ?: return@SectionItemView
    withBGApi { checkUserLink(str) }
  }) {
    Text(stringResource(MR.strings.tap_to_paste_link))
  }
}

@Composable
private fun ArchiveImportView(progressIndicator: MutableState<Boolean>, close: () -> Unit) {
  val importArchiveLauncher = rememberFileChooserLauncher(true) { to: URI? ->
    if (to != null) {
      withLongRunningApi {
        val success = importArchive(to, mutableStateOf(0 to 0), progressIndicator, true)
        if (success) {
          startChat(
            chatModel,
            mutableStateOf(Clock.System.now()),
            chatModel.chatDbChanged,
            progressIndicator
          )
          hideView(close)
        }
      }
    }
  }
  SectionItemView({
    withLongRunningApi { importArchiveLauncher.launch("application/zip") }
  }) {
    Text(stringResource(MR.strings.import_database))
  }
}

@Composable
private fun ModalData.OnionView(link: String, legacyLinkSocksProxy: String?, linkNetworkProxy: NetworkProxy?, hostMode: HostMode, requiredHostMode: Boolean, state: MutableState<MigrationToState?>) {
  val onionHosts = remember { stateGetOrPut("onionHosts") {
    getNetCfg().copy(socksProxy = linkNetworkProxy?.toProxyString() ?: legacyLinkSocksProxy, hostMode = hostMode, requiredHostMode = requiredHostMode).onionHosts
  } }
  val networkUseSocksProxy = remember { stateGetOrPut("networkUseSocksProxy") { linkNetworkProxy != null || legacyLinkSocksProxy != null } }
  val sessionMode = remember { stateGetOrPut("sessionMode") { TransportSessionMode.User} }
  val networkProxy = remember { stateGetOrPut("networkProxy") {
    linkNetworkProxy
      ?: if (legacyLinkSocksProxy != null) {
        NetworkProxy(host = legacyLinkSocksProxy.substringBefore(":").ifBlank { "localhost" }, port = legacyLinkSocksProxy.substringAfter(":").toIntOrNull() ?: 9050)
      } else {
        appPrefs.networkProxy.get()
      }
    }
  }

  val netCfg = rememberSaveable(stateSaver = serializableSaver()) {
    mutableStateOf(getNetCfg().withOnionHosts(onionHosts.value).copy(socksProxy = linkNetworkProxy?.toProxyString() ?: legacyLinkSocksProxy, sessionMode = sessionMode.value))
  }

  SectionView(stringResource(MR.strings.migrate_to_device_confirm_network_settings).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_check),
      text = stringResource(MR.strings.migrate_to_device_apply_onion),
      textColor = MaterialTheme.colors.primary,
      click = {
        val updated = netCfg.value
          .withOnionHosts(onionHosts.value)
          .withProxy(if (networkUseSocksProxy.value) networkProxy.value else null, null)
          .copy(
            sessionMode = sessionMode.value
          )
        withBGApi {
          state.value = MigrationToState.DatabaseInit(link, updated, if (networkUseSocksProxy.value) networkProxy.value else null)
        }
      }
    ){}
    SectionTextFooter(stringResource(MR.strings.migrate_to_device_confirm_network_settings_footer))
  }

  SectionSpacer()

  val networkProxyPref = SharedPreference(get = { networkProxy.value }, set = {
    networkProxy.value = it
  })
  SectionView(stringResource(MR.strings.network_settings_title).uppercase()) {
    OnionRelatedLayout(
      appPreferences.developerTools.get(),
      networkUseSocksProxy,
      onionHosts,
      sessionMode,
      networkProxyPref,
      toggleSocksProxy = { enable ->
        networkUseSocksProxy.value = enable
      },
      updateSessionMode = {
        sessionMode.value = it
      }
    )
  }
}

@Composable
private fun MutableState<MigrationToState?>.DatabaseInitView(link: String, tempDatabaseFile: File, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  Box {
    SectionView(stringResource(MR.strings.migrate_to_device_database_init).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    prepareDatabase(link, tempDatabaseFile, netCfg, networkProxy)
  }
}

@Composable
private fun MutableState<MigrationToState?>.LinkDownloadingView(
  link: String,
  ctrl: ChatCtrl,
  user: User,
  archivePath: String,
  tempDatabaseFile: File,
  chatReceiver: MutableState<MigrationToChatReceiver?>,
  netCfg: NetCfg,
  networkProxy: NetworkProxy?
) {
  Box {
    SectionView(stringResource(MR.strings.migrate_to_device_downloading_details).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    startDownloading(0, ctrl, user, tempDatabaseFile, chatReceiver, link, archivePath, netCfg, networkProxy)
  }
}

@Composable
private fun DownloadProgressView(downloadedBytes: Long, totalBytes: Long) {
  Box {
    SectionView(stringResource(MR.strings.migrate_to_device_downloading_archive).uppercase()) {
      val ratio = downloadedBytes.toFloat() / max(totalBytes, 1)
      LargeProgressView(ratio, "${(ratio * 100).toInt()}%", stringResource(MR.strings.migrate_to_device_bytes_downloaded).format(formatBytes(downloadedBytes)))
    }
  }
}

@Composable
private fun MutableState<MigrationToState?>.DownloadFailedView(link: String, chatReceiver: MigrationToChatReceiver?, archivePath: String, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  SectionView(stringResource(MR.strings.migrate_to_device_download_failed).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_download),
      text = stringResource(MR.strings.migrate_to_device_repeat_download),
      textColor = MaterialTheme.colors.primary,
      click = {
        state = MigrationToState.DatabaseInit(link, netCfg, networkProxy)
      }
    ) {}
    SectionTextFooter(stringResource(MR.strings.migrate_to_device_try_again))
  }
  LaunchedEffect(Unit) {
    chatReceiver?.stopAndCleanUp()
    File(archivePath).delete()
    MigrationToDeviceState.save(null)
  }
}

@Composable
private fun MutableState<MigrationToState?>.ArchiveImportView(archivePath: String, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  Box {
    SectionView(stringResource(MR.strings.migrate_to_device_importing_archive).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    importArchive(archivePath, netCfg, networkProxy)
  }
}

@Composable
private fun MutableState<MigrationToState?>.ArchiveImportFailedView(archivePath: String, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  SectionView(stringResource(MR.strings.migrate_to_device_import_failed).uppercase()) {
    SettingsActionItemWithContent(
      icon = painterResource(MR.images.ic_download),
      text = stringResource(MR.strings.migrate_to_device_repeat_import),
      textColor = MaterialTheme.colors.primary,
      click = {
        state = MigrationToState.ArchiveImport(archivePath, netCfg, networkProxy)
      }
    ) {}
    SectionTextFooter(stringResource(MR.strings.migrate_to_device_try_again))
  }
}

@Composable
private fun MutableState<MigrationToState?>.PassphraseEnteringView(currentKey: String, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  val currentKey = rememberSaveable { mutableStateOf(currentKey) }
  val verifyingPassphrase = rememberSaveable { mutableStateOf(false) }
  val useKeychain = rememberSaveable { mutableStateOf(appPreferences.storeDBPassphrase.get()) }

  Box {
    val view = LocalMultiplatformView()
    SectionView(stringResource(MR.strings.migrate_to_device_enter_passphrase).uppercase()) {
      SavePassphraseSetting(
        useKeychain.value,
        false,
        false,
        enabled = !verifyingPassphrase.value,
        smallPadding = false
      ) { checked -> useKeychain.value = checked }

      PassphraseField(currentKey, placeholder = stringResource(MR.strings.current_passphrase), Modifier.padding(horizontal = DEFAULT_PADDING), isValid = ::validKey, requestFocus = true)

      SettingsActionItemWithContent(
        icon = painterResource(MR.images.ic_vpn_key_filled),
        text = stringResource(MR.strings.open_chat),
        textColor = MaterialTheme.colors.primary,
        disabled = verifyingPassphrase.value || currentKey.value.isEmpty(),
        click = {
          verifyingPassphrase.value = true
          hideKeyboard(view)
          withBGApi {
            val (status, _) = chatInitTemporaryDatabase(dbAbsolutePrefixPath, key = currentKey.value, confirmation = MigrationConfirmation.YesUp)
            val success = status == DBMigrationResult.OK || status == DBMigrationResult.InvalidConfirmation
            if (success) {
              state = MigrationToState.Migration(currentKey.value, MigrationConfirmation.YesUp, useKeychain.value, netCfg, networkProxy)
            } else if (status is DBMigrationResult.ErrorMigration) {
              state = MigrationToState.MigrationConfirmation(status, currentKey.value, useKeychain.value, netCfg, networkProxy)
            } else {
              showErrorOnMigrationIfNeeded(status)
            }
            verifyingPassphrase.value = false
          }
        }
      ) {}
      DatabaseEncryptionFooter(useKeychain, chatDbEncrypted = true, remember { mutableStateOf(false) }, remember { mutableStateOf(false) }, true)
    }
    if (verifyingPassphrase.value) {
      ProgressView()
    }
  }
}

@Composable
private fun MutableState<MigrationToState?>.MigrationConfirmationView(status: DBMigrationResult, passphrase: String, useKeychain: Boolean, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  data class Tuple4<A,B,C,D>(val a: A, val b: B, val c: C, val d: D)
  val (header: String, button: String?, footer: String, confirmation: MigrationConfirmation?) = when (status) {
    is DBMigrationResult.ErrorMigration -> when (val err = status.migrationError) {
      is MigrationError.Upgrade ->
        Tuple4(
          generalGetString(MR.strings.database_upgrade),
          generalGetString(MR.strings.upgrade_and_open_chat),
          "",
          MigrationConfirmation.YesUp
        )
      is MigrationError.Downgrade ->
        Tuple4(
          generalGetString(MR.strings.database_downgrade),
          generalGetString(MR.strings.downgrade_and_open_chat),
          generalGetString(MR.strings.database_downgrade_warning),
          MigrationConfirmation.YesUpDown
        )
      is MigrationError.Error ->
        Tuple4(
          generalGetString(MR.strings.incompatible_database_version),
          null,
          mtrErrorDescription(err.mtrError),
          null
        )
    }
    else -> Tuple4(generalGetString(MR.strings.error), null, generalGetString(MR.strings.unknown_error), null)
  }
  SectionView(header.uppercase()) {
    if (button != null && confirmation != null) {
      SettingsActionItemWithContent(
        icon = painterResource(MR.images.ic_download),
        text = button,
        textColor = MaterialTheme.colors.primary,
        click = {
          state = MigrationToState.Migration(passphrase, confirmation, useKeychain, netCfg, networkProxy)
        }
      ) {}
    }
    SectionTextFooter(footer)
  }
}

@Composable
private fun MigrationView(passphrase: String, confirmation: MigrationConfirmation, useKeychain: Boolean, netCfg: NetCfg, networkProxy: NetworkProxy?, close: () -> Unit) {
  Box {
    SectionView(stringResource(MR.strings.migrate_to_device_migrating).uppercase()) {}
    ProgressView()
  }
  LaunchedEffect(Unit) {
    startChat(passphrase, confirmation, useKeychain, netCfg, networkProxy, close)
  }
}

@Composable
private fun ProgressView() {
  DefaultProgressView(null)
}

private suspend fun MutableState<MigrationToState?>.checkUserLink(link: String): Boolean {
  return if (strHasSimplexFileLink(link.trim())) {
    val data = MigrationFileLinkData.readFromLink(link)
    val hasProxyConfigured = data?.networkConfig?.hasProxyConfigured() ?: false
    val networkConfig = data?.networkConfig?.transformToPlatformSupported()
    // If any of iOS or Android had onion enabled, show onion screen
    if (hasProxyConfigured && networkConfig?.hostMode != null && networkConfig.requiredHostMode != null) {
      state = MigrationToState.Onion(link.trim(), networkConfig.legacySocksProxy, networkConfig.networkProxy, networkConfig.hostMode, networkConfig.requiredHostMode)
      MigrationToDeviceState.save(MigrationToDeviceState.Onion(link.trim(), networkConfig.legacySocksProxy, networkConfig.networkProxy, networkConfig.hostMode, networkConfig.requiredHostMode))
    } else {
      val current = getNetCfg()
      state = MigrationToState.DatabaseInit(link.trim(), current.copy(
        socksProxy = null,
        hostMode = networkConfig?.hostMode ?: current.hostMode,
        requiredHostMode = networkConfig?.requiredHostMode ?: current.requiredHostMode
      ),
        networkProxy = null
      )
    }
    true
  } else {
    AlertManager.shared.showAlertMsg(
      title = generalGetString(MR.strings.invalid_file_link),
      text = generalGetString(MR.strings.the_text_you_pasted_is_not_a_link)
    )
    false
  }
}

private fun MutableState<MigrationToState?>.prepareDatabase(
  link: String,
  tempDatabaseFile: File,
  netCfg: NetCfg,
  networkProxy: NetworkProxy?
) {
  withLongRunningApi {
    val ctrlAndUser = initTemporaryDatabase(tempDatabaseFile, netCfg)
    if (ctrlAndUser == null) {
      // Probably, something wrong with network config or database initialization, let's start from scratch
      state = MigrationToState.PasteOrScanLink
      MigrationToDeviceState.save(null)
      return@withLongRunningApi
    }

    val (ctrl, user) = ctrlAndUser
    state = MigrationToState.LinkDownloading(link, ctrl, user, archivePath(), netCfg, networkProxy)
  }
}

private fun MutableState<MigrationToState?>.startDownloading(
  totalBytes: Long,
  ctrl: ChatCtrl,
  user: User,
  tempDatabaseFile: File,
  chatReceiver: MutableState<MigrationToChatReceiver?>,
  link: String,
  archivePath: String,
  netCfg: NetCfg,
  networkProxy: NetworkProxy?
) {
  withBGApi {
    chatReceiver.value = MigrationToChatReceiver(ctrl, tempDatabaseFile) { msg ->
        when (msg) {
          is CR.RcvFileProgressXFTP -> {
            state = MigrationToState.DownloadProgress(msg.receivedSize, msg.totalSize, msg.rcvFileTransfer.fileId, link, archivePath, netCfg, networkProxy, ctrl)
            MigrationToDeviceState.save(MigrationToDeviceState.DownloadProgress(link, File(archivePath).name, netCfg, networkProxy))
          }
          is CR.RcvStandaloneFileComplete -> {
            delay(500)
            // User closed the whole screen before new state was saved
            if (state == null) {
              MigrationToDeviceState.save(null)
            } else {
              state = MigrationToState.ArchiveImport(archivePath, netCfg, networkProxy)
              MigrationToDeviceState.save(MigrationToDeviceState.ArchiveImport(File(archivePath).name, netCfg, networkProxy))
            }
          }
          is CR.RcvFileError -> {
            AlertManager.shared.showAlertMsg(
              generalGetString(MR.strings.migrate_to_device_download_failed),
              generalGetString(MR.strings.migrate_to_device_file_delete_or_link_invalid)
            )
            state = MigrationToState.DownloadFailed(totalBytes, link, archivePath, netCfg, networkProxy)
          }
          is CR.ChatRespError -> {
            if (msg.chatError is ChatError.ChatErrorChat && msg.chatError.errorType is ChatErrorType.NoRcvFileUser) {
              AlertManager.shared.showAlertMsg(
                generalGetString(MR.strings.migrate_to_device_download_failed),
                generalGetString(MR.strings.migrate_to_device_file_delete_or_link_invalid)
              )
              state = MigrationToState.DownloadFailed(totalBytes, link, archivePath, netCfg, networkProxy)
            } else {
              Log.d(TAG, "unsupported error: ${msg.responseType}, ${json.encodeToString(msg.chatError)}")
            }
          }
          else -> Log.d(TAG, "unsupported event: ${msg.responseType}")
        }
    }
    chatReceiver.value?.start()

    val (res, error) = controller.downloadStandaloneFile(user, link, CryptoFile.plain(File(archivePath).path), ctrl)
    if (res == null) {
      state = MigrationToState.DownloadFailed(totalBytes, link, archivePath, netCfg, networkProxy)
      AlertManager.shared.showAlertMsg(
        generalGetString(MR.strings.migrate_to_device_error_downloading_archive),
        error
      )
    }
  }
}

private fun MutableState<MigrationToState?>.importArchive(archivePath: String, netCfg: NetCfg, networkProxy: NetworkProxy?) {
  withLongRunningApi {
    try {
      if (ChatController.ctrl == null || ChatController.ctrl == -1L) {
        chatInitControllerRemovingDatabases()
      }
      controller.apiDeleteStorage()
      wallpapersDir.mkdirs()
      try {
        val config = ArchiveConfig(archivePath, parentTempDirectory = databaseExportDir.toString())
        val archiveErrors = controller.apiImportArchive(config)
        if (archiveErrors.isNotEmpty()) {
          showArchiveImportedWithErrorsAlert(archiveErrors)
        }
        state = MigrationToState.Passphrase("", netCfg, networkProxy)
        MigrationToDeviceState.save(MigrationToDeviceState.Passphrase(netCfg, networkProxy))
      } catch (e: Exception) {
        state = MigrationToState.ArchiveImportFailed(archivePath, netCfg, networkProxy)
        AlertManager.shared.showAlertMsg (generalGetString(MR.strings.error_importing_database), e.stackTraceToString())
      }
    } catch (e: Exception) {
      state = MigrationToState.ArchiveImportFailed(archivePath, netCfg, networkProxy)
      AlertManager.shared.showAlertMsg (generalGetString(MR.strings.error_deleting_database), e.stackTraceToString())
    }
  }
}

private suspend fun stopArchiveDownloading(fileId: Long, ctrl: ChatCtrl) {
  controller.apiCancelFile(null, fileId, ctrl)
}

private fun startChat(passphrase: String, confirmation: MigrationConfirmation, useKeychain: Boolean, netCfg: NetCfg, networkProxy: NetworkProxy?, close: () -> Unit) {
  if (useKeychain) {
    ksDatabasePassword.set(passphrase)
  } else {
    ksDatabasePassword.remove()
  }
  appPreferences.storeDBPassphrase.set(useKeychain)
  appPreferences.initialRandomDBPassphrase.set(false)
  withBGApi {
    try {
      initChatController(useKey = passphrase, confirmMigrations = confirmation) { CompletableDeferred(false) }
      val appSettings = controller.apiGetAppSettings(AppSettings.current.prepareForExport()).copy(
        networkConfig = netCfg,
        networkProxy = networkProxy
      )
      finishMigration(appSettings, close)
    } catch (e: Exception) {
      hideView(close)
      AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_starting_chat), e.stackTraceToString())
    }
  }
}

private suspend fun finishMigration(appSettings: AppSettings, close: () -> Unit) {
  try {
    getMigrationTempFilesDirectory().deleteRecursively()
    appSettings.importIntoApp()
    val user = chatModel.currentUser.value
    if (user != null) {
      startChat(user)
    }
    platform.androidChatStartedAfterBeingOff()
    hideView(close)
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.migrate_to_device_chat_migrated), generalGetString(MR.strings.migrate_to_device_finalize_migration))
  } catch (e: Exception) {
    AlertManager.shared.showAlertMsg(generalGetString(MR.strings.error_starting_chat), e.stackTraceToString())
  }
  MigrationToDeviceState.save(null)
}

private fun hideView(close: () -> Unit) {
  appPreferences.onboardingStage.set(OnboardingStage.OnboardingComplete)
  chatModel.migrationState.value = null
  close()
}

private suspend fun MutableState<MigrationToState?>.cleanUpOnBack(chatReceiver: MigrationToChatReceiver?) {
  val state = state
  if (state is MigrationToState.ArchiveImportFailed) {
    // Original database is not exist, nothing is set up correctly for showing to a user yet. Return to clean state
    deleteChatDatabaseFilesAndState()
    initChatControllerOnStart()
  } else if (state is MigrationToState.DownloadProgress && state.ctrl != null) {
    stopArchiveDownloading(state.fileId, state.ctrl)
  }
  chatReceiver?.stopAndCleanUp()
  getMigrationTempFilesDirectory().deleteRecursively()
  MigrationToDeviceState.save(null)
  chatModel.migrationState.value = null
}

private fun strHasSimplexFileLink(text: String): Boolean =
  text.startsWith("simplex:/file") || text.startsWith("https://simplex.chat/file")

private fun fileForTemporaryDatabase(): File =
  File(getMigrationTempFilesDirectory(), generateNewFileName("migration", "db", getMigrationTempFilesDirectory()))

private fun archivePath(): String {
  val archiveTime = Clock.System.now()
  val ts = SimpleDateFormat("yyyy-MM-dd'T'HHmmss", Locale.US).format(Date.from(archiveTime.toJavaInstant()))
  val archiveName = "simplex-chat.$ts.zip"
  val archivePath = File(getMigrationTempFilesDirectory(), archiveName)
  return archivePath.absolutePath
}

private class MigrationToChatReceiver(
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
