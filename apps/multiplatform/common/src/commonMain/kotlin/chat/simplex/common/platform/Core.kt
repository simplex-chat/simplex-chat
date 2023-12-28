package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.serialization.decodeFromString
import java.nio.ByteBuffer

// ghc's rts
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
external fun chatSendCmd(ctrl: ChatCtrl, msg: String): String
external fun chatSendRemoteCmd(ctrl: ChatCtrl, rhId: Int, msg: String): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String
external fun chatParseServer(str: String): String
external fun chatPasswordHash(pwd: String, salt: String): String
external fun chatValidName(name: String): String
external fun chatWriteFile(ctrl: ChatCtrl, path: String, buffer: ByteBuffer): String
external fun chatReadFile(path: String, key: String, nonce: String): Array<Any>
external fun chatEncryptFile(ctrl: ChatCtrl, fromPath: String, toPath: String): String
external fun chatDecryptFile(fromPath: String, key: String, nonce: String, toPath: String): String

val chatModel: ChatModel
  get() = chatController.chatModel

val appPreferences: AppPreferences
  get() = chatController.appPrefs

val chatController: ChatController = ChatController

suspend fun initChatController(useKey: String? = null, confirmMigrations: MigrationConfirmation? = null, startChat: Boolean = true) {
  val dbKey = useKey ?: DatabaseUtils.useDatabaseKey()
  val confirm = confirmMigrations ?: if (appPreferences.confirmDBUpgrades.get()) MigrationConfirmation.Error else MigrationConfirmation.YesUp
  val migrated: Array<Any> = chatMigrateInit(dbAbsolutePrefixPath, dbKey, confirm.value)
  val res: DBMigrationResult = kotlin.runCatching {
    json.decodeFromString<DBMigrationResult>(migrated[0] as String)
  }.getOrElse { DBMigrationResult.Unknown(migrated[0] as String) }
  val ctrl = if (res is DBMigrationResult.OK) {
    migrated[1] as Long
  } else null
  chatController.ctrl = ctrl
  chatModel.chatDbEncrypted.value = dbKey != ""
  chatModel.chatDbStatus.value = res
  if (res != DBMigrationResult.OK) {
    Log.d(TAG, "Unable to migrate successfully: $res")
  } else if (startChat) {
    // If we migrated successfully means previous re-encryption process on database level finished successfully too
    if (appPreferences.encryptionStartedAt.get() != null) appPreferences.encryptionStartedAt.set(null)
    val user = chatController.apiGetActiveUser(null)
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
    } else {
      val savedOnboardingStage = appPreferences.onboardingStage.get()
      appPreferences.onboardingStage.set(if (listOf(OnboardingStage.Step1_SimpleXInfo, OnboardingStage.Step2_CreateProfile).contains(savedOnboardingStage) && chatModel.users.size == 1) {
        OnboardingStage.Step3_CreateSimpleXAddress
      } else {
        savedOnboardingStage
      })
      if (appPreferences.onboardingStage.get() == OnboardingStage.OnboardingComplete && !chatModel.controller.appPrefs.privacyDeliveryReceiptsSet.get()) {
        chatModel.setDeliveryReceipts.value = true
      }
      chatController.startChat(user)
      platform.androidChatInitializedAndStarted()
    }
  }
}
