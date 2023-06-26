package chat.simplex.common.platform

import chat.simplex.common.model.*
import chat.simplex.common.model.ChatCtrl
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import kotlinx.serialization.decodeFromString

// ghc's rts
external fun initHS()
// android-support
external fun pipeStdOutToSocket(socketName: String) : Int

// SimpleX API
typealias ChatCtrl = Long
external fun chatMigrateInit(dbPath: String, dbKey: String, confirm: String): Array<Any>
external fun chatSendCmd(ctrl: ChatCtrl, msg: String): String
external fun chatRecvMsg(ctrl: ChatCtrl): String
external fun chatRecvMsgWait(ctrl: ChatCtrl, timeout: Int): String
external fun chatParseMarkdown(str: String): String
external fun chatParseServer(str: String): String
external fun chatPasswordHash(pwd: String, salt: String): String

val chatModel: ChatModel
  get() = chatController.chatModel

private val appPreferences: AppPreferences by lazy { ChatController.appPrefs }

val chatController: ChatController = ChatController

suspend fun initChatController(useKey: String? = null, confirmMigrations: MigrationConfirmation? = null, startChat: Boolean = true) {
  val dbKey = useKey ?: DatabaseUtils.useDatabaseKey()
  val dbAbsolutePathPrefix = getFilesDirectory()
  val confirm = confirmMigrations ?: if (appPreferences.confirmDBUpgrades.get()) MigrationConfirmation.Error else MigrationConfirmation.YesUp
  val migrated: Array<Any> = chatMigrateInit(dbAbsolutePathPrefix, dbKey, confirm.value)
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
    val user = chatController.apiGetActiveUser()
    if (user == null) {
      chatModel.controller.appPrefs.onboardingStage.set(OnboardingStage.Step1_SimpleXInfo)
      chatModel.onboardingStage.value = OnboardingStage.Step1_SimpleXInfo
      chatModel.currentUser.value = null
      chatModel.users.clear()
    } else {
      val savedOnboardingStage = appPreferences.onboardingStage.get()
      chatModel.onboardingStage.value = if (listOf(OnboardingStage.Step1_SimpleXInfo, OnboardingStage.Step2_CreateProfile).contains(savedOnboardingStage) && chatModel.users.size == 1) {
        OnboardingStage.Step3_CreateSimpleXAddress
      } else {
        savedOnboardingStage
      }
      chatController.startChat(user)
      chatInitializedAndStarted()
    }
  }
}