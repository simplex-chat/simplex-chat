package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.model.ChatModel.controller
import chat.simplex.common.model.ChatModel.currentUser
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.helpers.DatabaseUtils.ksDatabasePassword
import chat.simplex.common.views.helpers.DatabaseUtils.randomDatabasePassword
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.res.MR
import kotlinx.coroutines.*
import java.io.File
import java.nio.ByteBuffer

// ghc's rts
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
external fun chatCloseStore(ctrl: ChatCtrl): String
external fun chatSendCmdRetry(ctrl: ChatCtrl, msg: String, retryNum: Int): String
external fun chatSendRemoteCmdRetry(ctrl: ChatCtrl, rhId: Int, msg: String, retryNum: Int): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String
external fun chatParseServer(str: String): String
external fun chatParseUri(str: String, safe: Int): String
external fun chatPasswordHash(pwd: String, salt: String): String
external fun chatValidName(name: String): String
external fun chatJsonLength(str: String): Int
external fun chatWriteFile(ctrl: ChatCtrl, path: String, buffer: ByteBuffer): String
external fun chatReadFile(path: String, key: String, nonce: String): Array<Any>
external fun chatEncryptFile(ctrl: ChatCtrl, fromPath: String, toPath: String): String
external fun chatDecryptFile(fromPath: String, key: String, nonce: String, toPath: String): String

val chatModel: ChatModel
  get() = chatController.chatModel

val appPreferences: AppPreferences
  get() = chatController.appPrefs

val chatController: ChatController = ChatController

fun initChatControllerOnStart() {
  withLongRunningApi {
    if (appPreferences.chatStopped.get() && appPreferences.storeDBPassphrase.get() && ksDatabasePassword.get() != null) {
      initChatController(startChat = ::showStartChatAfterRestartAlert)
    } else {
      initChatController()
    }
  }
}

suspend fun applyMdmServers() {
  val mdmServers = platform.androidMdmGetServers() ?: return
  val (smpAddresses, xftpAddresses) = mdmServers
  if (smpAddresses.isEmpty() && xftpAddresses.isEmpty()) return

  Log.d(TAG, "Applying MDM servers: ${smpAddresses.size} SMP, ${xftpAddresses.size} XFTP")

  // Get current user servers from the backend
  val currentServers = chatController.getUserServers(null) ?: run {
    Log.e(TAG, "applyMdmServers: failed to get current user servers")
    return
  }

  // Build MDM UserServer objects
  val mdmSmpServers = smpAddresses.map { address ->
    UserServer(
      remoteHostId = null,
      serverId = null,
      server = address,
      preset = false,
      tested = null,
      enabled = true,
      deleted = false
    )
  }
  val mdmXftpServers = xftpAddresses.map { address ->
    UserServer(
      remoteHostId = null,
      serverId = null,
      server = address,
      preset = false,
      tested = null,
      enabled = true,
      deleted = false
    )
  }

  // Create updated server list:
  // - For operator=null entry: replace servers with MDM servers
  // - For operator entries: if lock=true, disable them; if lock=false, keep them
  val locked = platform.androidMdmIsConfigLocked()

  val updatedServers = currentServers.map { ops ->
    if (ops.operator == null) {
      // Replace the null-operator entry with MDM servers
      ops.copy(
        smpServers = mdmSmpServers,
        xftpServers = mdmXftpServers
      )
    } else if (locked) {
      // When locked, disable operator servers (MDM takes full control)
      ops.copy(
        smpServers = ops.smpServers.map { it.copy(enabled = false) },
        xftpServers = ops.xftpServers.map { it.copy(enabled = false) }
      )
    } else {
      // When not locked, keep operator servers as-is (user can use both)
      ops
    }
  }

  val success = chatController.setUserServersDirect(null, updatedServers)
  if (success) {
    Log.d(TAG, "MDM servers applied successfully")
  } else {
    Log.e(TAG, "Failed to apply MDM servers")
  }
}

suspend fun initChatController(useKey: String? = null, confirmMigrations: MigrationConfirmation? = null, startChat: () -> CompletableDeferred<Boolean> = { CompletableDeferred(true) }) {
  Log.d(TAG, "initChatController")
  try {
    if (chatModel.ctrlInitInProgress.value) return
    chatModel.ctrlInitInProgress.value = true
    if (!appPrefs.storeDBPassphrase.get() && !appPrefs.initialRandomDBPassphrase.get()) {
      ksDatabasePassword.remove()
    }
    val dbKey = useKey ?: DatabaseUtils.useDatabaseKey()
    val confirm = confirmMigrations ?: if (appPreferences.developerTools.get() && appPreferences.confirmDBUpgrades.get()) MigrationConfirmation.Error else MigrationConfirmation.YesUp
    var migrated: Array<Any> = if (databaseBackend == "postgres") {
      chatMigrateInit("simplex_v1", "postgresql://simplex@/simplex_v1", MigrationConfirmation.Error.value)
    } else {
      chatMigrateInit(dbAbsolutePrefixPath, dbKey, MigrationConfirmation.Error.value)
    }
    var res: DBMigrationResult = runCatching {
      json.decodeFromString<DBMigrationResult>(migrated[0] as String)
    }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }
    val rerunMigration = res is DBMigrationResult.ErrorMigration && when (res.migrationError) {
      // we don't allow to run down migrations without confirmation in UI, so currently it won't be YesUpDown
      is MigrationError.Upgrade -> confirm == MigrationConfirmation.YesUp || confirm == MigrationConfirmation.YesUpDown
      is MigrationError.Downgrade ->  confirm == MigrationConfirmation.YesUpDown
      is MigrationError.Error -> false
    }
    if (rerunMigration) {
      chatModel.dbMigrationInProgress.value = true
      migrated = if (databaseBackend == "postgres") {
        chatMigrateInit("simplex_v1", "postgresql://simplex@/simplex_v1", confirm.value)
      } else {
        chatMigrateInit(dbAbsolutePrefixPath, dbKey, confirm.value)
      }
      res = runCatching {
        json.decodeFromString<DBMigrationResult>(migrated[0] as String)
      }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }
    }
    val ctrl = if (res is DBMigrationResult.OK) {
      migrated[1] as Long
    } else null
    chatController.setChatCtrl(ctrl)
    chatModel.chatDbEncrypted.value = dbKey != ""
    chatModel.chatDbStatus.value = res
    if (res != DBMigrationResult.OK) {
      Log.d(TAG, "Unable to migrate successfully: $res")
      if (!appPrefs.newDatabaseInitialized.get() && DatabaseUtils.hasOnlyOneDatabase(dataDir.absolutePath)) {
        if (chatModel.incompleteInitializedDbRemoved.value) {
          Log.d(TAG, "Incomplete initialized databases were removed but after repeated migration only one database exists again, not trying to remove again")
        } else {
          val dbPath = dbAbsolutePrefixPath
          File(dbPath + "_chat.db").delete()
          File(dbPath + "_agent.db").delete()
          chatModel.incompleteInitializedDbRemoved.value = true
          Log.d(TAG, "Incomplete initialized databases were removed for the first time, repeating migration")
          chatModel.ctrlInitInProgress.value = false
          initChatController(useKey, confirmMigrations, startChat)
        }
      }
      return
    }
    appPrefs.newDatabaseInitialized.set(true)
    chatModel.incompleteInitializedDbRemoved.value = false
    platform.androidRestartNetworkObserver()
    controller.apiSetAppFilePaths(
      appFilesDir.absolutePath,
      coreTmpDir.absolutePath,
      wallpapersDir.parentFile.absolutePath,
      remoteHostsDir.absolutePath,
      ctrl
    )
    controller.apiSetEncryptLocalFiles(controller.appPrefs.privacyEncryptLocalFiles.get())
    // If we migrated successfully means previous re-encryption process on database level finished successfully too
    if (appPreferences.encryptionStartedAt.get() != null) appPreferences.encryptionStartedAt.set(null)
    val user = chatController.apiGetActiveUser(null)
    chatModel.currentUser.value = user
    chatModel.conditions.value = chatController.getServerOperators(null) ?: ServerOperatorConditionsDetail.empty
    if (appPrefs.shouldImportAppSettings.get()) {
      try {
        val appSettings = controller.apiGetAppSettings(AppSettings.current.prepareForExport())
        appSettings.importIntoApp()
        appPrefs.shouldImportAppSettings.set(false)
      } catch (e: Exception) {
        Log.e(TAG, "Error while importing app settings: " + e.stackTraceToString())
      }
    }
    if (user == null) {
      chatModel.controller.appPrefs.privacyDeliveryReceiptsSet.set(true)
      chatModel.currentUser.value = null
      chatModel.users.clear()
      if (appPlatform.isDesktop) {
        /**
         * Setting it here to null because otherwise the screen will flash in [MainScreen] after the first start
         * because of default value of [OnboardingStage.OnboardingComplete]
         * */
        chatModel.localUserCreated.value = null
        if (chatController.listRemoteHosts()?.isEmpty() == true) {
          chatController.appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
        }
        chatController.startChatWithoutUser()
      } else {
        chatController.appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
      }
    } else if (startChat().await()) {
      val savedOnboardingStage = appPreferences.onboardingStage.get()
      val newStage = if (listOf(OnboardingStage.Step1_SimpleXInfo, OnboardingStage.Step2_CreateProfile).contains(savedOnboardingStage) && chatModel.users.size == 1) {
        if (appPlatform.isAndroid) {
          OnboardingStage.Step4_SetNotificationsMode
        } else {
          OnboardingStage.OnboardingComplete
        }
      } else {
        savedOnboardingStage
      }
      if (appPreferences.onboardingStage.get() != newStage) {
        appPreferences.onboardingStage.set(newStage)
      }
      chatController.startChat(user)
      applyMdmServers()
      platform.androidChatInitializedAndStarted()
    } else {
      chatController.getUserChatData(null)
      chatModel.localUserCreated.value = currentUser.value != null
      chatModel.chatRunning.value = false
    }
  } finally {
    chatModel.ctrlInitInProgress.value = false
    chatModel.dbMigrationInProgress.value = false
  }
}

fun chatInitTemporaryDatabase(dbPath: String, key: String? = null, confirmation: MigrationConfirmation = MigrationConfirmation.Error): Pair<DBMigrationResult, ChatCtrl?> {
  val dbKey = key ?: randomDatabasePassword()
  Log.d(TAG, "chatInitTemporaryDatabase path: $dbPath")
  val migrated = chatMigrateInit(dbPath, dbKey, confirmation.value)
  val res = runCatching {
    json.decodeFromString<DBMigrationResult>(migrated[0] as String)
  }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }

  return res to migrated[1] as ChatCtrl
}

fun chatInitControllerRemovingDatabases() {
  val dbPath = dbAbsolutePrefixPath
  // Remove previous databases, otherwise, can be .errorNotADatabase with null controller
  File(dbPath + "_chat.db").delete()
  File(dbPath + "_agent.db").delete()

  val dbKey = randomDatabasePassword()
  Log.d(TAG, "chatInitControllerRemovingDatabases path: $dbPath")
  val migrated = chatMigrateInit(dbPath, dbKey, MigrationConfirmation.Error.value)
  val res = runCatching {
    json.decodeFromString<DBMigrationResult>(migrated[0] as String)
  }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }

  val ctrl = migrated[1] as Long
  chatController.setChatCtrl(ctrl)
  // We need only controller, not databases
  File(dbPath + "_chat.db").delete()
  File(dbPath + "_agent.db").delete()
}

fun showStartChatAfterRestartAlert(): CompletableDeferred<Boolean> {
  val deferred = CompletableDeferred<Boolean>()
  AlertManager.shared.showAlertDialog(
    title = generalGetString(MR.strings.start_chat_question),
    text = generalGetString(MR.strings.chat_is_stopped_you_should_transfer_database),
    onConfirm = { deferred.complete(true) },
    onDismiss = { deferred.complete(false) },
    onDismissRequest = { deferred.complete(false) }
  )
  return deferred
}
