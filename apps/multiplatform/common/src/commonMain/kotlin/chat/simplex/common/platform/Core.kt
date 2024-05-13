package chat.simplex.common.platform

import chat.simplex.common.model.*
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
external fun chatSendCmd(ctrl: ChatCtrl, msg: String): String
external fun chatSendRemoteCmd(ctrl: ChatCtrl, rhId: Int, msg: String): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String
external fun chatParseServer(str: String): String
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

fun initChatControllerAndRunMigrations() {
  withLongRunningApi {
    if (appPreferences.chatStopped.get() && appPreferences.storeDBPassphrase.get() && ksDatabasePassword.get() != null) {
      initChatController(startChat = ::showStartChatAfterRestartAlert)
    } else {
      initChatController()
    }
    runMigrations()
  }
}

suspend fun initChatController(useKey: String? = null, confirmMigrations: MigrationConfirmation? = null, startChat: () -> CompletableDeferred<Boolean> = { CompletableDeferred(true) }) {
  try {
    if (chatModel.ctrlInitInProgress.value) return
    chatModel.ctrlInitInProgress.value = true
    val dbKey = useKey ?: DatabaseUtils.useDatabaseKey()
    val confirm = confirmMigrations ?: if (appPreferences.developerTools.get() && appPreferences.confirmDBUpgrades.get()) MigrationConfirmation.Error else MigrationConfirmation.YesUp
    var migrated: Array<Any> = chatMigrateInit(dbAbsolutePrefixPath, dbKey, MigrationConfirmation.Error.value)
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
      migrated = chatMigrateInit(dbAbsolutePrefixPath, dbKey, confirm.value)
      res = runCatching {
        json.decodeFromString<DBMigrationResult>(migrated[0] as String)
      }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }
    }
    val ctrl = if (res is DBMigrationResult.OK) {
      migrated[1] as Long
    } else null
    chatController.ctrl = ctrl
    chatModel.chatDbEncrypted.value = dbKey != ""
    chatModel.chatDbStatus.value = res
    if (res != DBMigrationResult.OK) {
      Log.d(TAG, "Unable to migrate successfully: $res")
      return
    }
    platform.androidRestartNetworkObserver()
    controller.apiSetTempFolder(coreTmpDir.absolutePath)
    controller.apiSetFilesFolder(appFilesDir.absolutePath)
    if (appPlatform.isDesktop) {
      controller.apiSetRemoteHostsFolder(remoteHostsDir.absolutePath)
    }
    controller.apiSetEncryptLocalFiles(controller.appPrefs.privacyEncryptLocalFiles.get())
    // If we migrated successfully means previous re-encryption process on database level finished successfully too
    if (appPreferences.encryptionStartedAt.get() != null) appPreferences.encryptionStartedAt.set(null)
    val user = chatController.apiGetActiveUser(null)
    chatModel.currentUser.value = user
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
        OnboardingStage.Step3_CreateSimpleXAddress
      } else {
        savedOnboardingStage
      }
      if (appPreferences.onboardingStage.get() != newStage) {
        appPreferences.onboardingStage.set(newStage)
      }
      chatController.startChat(user)
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
  chatController.ctrl = ctrl
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
