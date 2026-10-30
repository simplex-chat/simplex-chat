package chat.simplex.app.ui.harness

import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.views.helpers.DBMigrationResult
import chat.simplex.common.model.ChatError
import chat.simplex.common.model.Profile
import chat.simplex.common.platform.chatCloseStore
import chat.simplex.common.platform.chatController
import chat.simplex.common.platform.chatModel
import chat.simplex.common.platform.initChatController
import chat.simplex.common.platform.initChatControllerOnStart
import chat.simplex.common.views.database.deleteChatDatabaseFilesAndState
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.onboarding.setLastVersionDefault
import chat.simplex.common.platform.agentDatabaseFileName
import chat.simplex.common.platform.androidAppContext
import chat.simplex.common.platform.chatDatabaseFileName
import chat.simplex.common.platform.dataDir
import chat.simplex.common.views.helpers.DatabaseUtils
import java.io.File
import kotlinx.coroutines.runBlocking

object UiTestApp {
  private var booted = false

  fun bootOnce() {
    if (booted) return
    booted = true
    java.util.TimeZone.setDefault(java.util.TimeZone.getTimeZone("UTC"))
    waitUntil(STARTUP_MS, "the app's own startup init") { chatModel.chatDbStatus.value != null && !chatModel.ctrlInitInProgress.value }
    check(chatModel.chatDbStatus.value == DBMigrationResult.OK) { "startup database: ${chatModel.chatDbStatus.value}" }
    Timing.phase("boot.saveTemplate") {
      if (runBlocking { chatController.listUsers(null) }.isNotEmpty()) {
        stopCore()
        deleteChatDatabaseFilesAndState()
        runBlocking { initChatController() }
        check(chatModel.chatDbStatus.value == DBMigrationResult.OK) { "fresh database: ${chatModel.chatDbStatus.value}" }
      }
      val key = DatabaseUtils.ksDatabasePassword.get() ?: ""
      val decryptError = if (key.isEmpty()) null else runBlocking {
        chatController.apiStopChat()
        chatController.apiStorageEncryption(currentKey = key, newKey = "")
      }
      if (decryptError != null) android.util.Log.e("UITEST", "template stays encrypted, removing encryption failed: ${chat.simplex.common.model.json.encodeToString(ChatError.serializer(), decryptError)}")
      stopCore()
      if (decryptError == null) DatabaseUtils.ksDatabasePassword.remove()
      DbTemplate.save()
    }
    presetAndroidPrefs()
    PrefStores.capture()
    ModelSnapshot.capture()
  }

  fun drain(): List<String> {
    val deadline = System.currentTimeMillis() + DRAIN_MS
    while (chat.simplex.common.ui.UiIdle.inFlight().isNotEmpty() && System.currentTimeMillis() < deadline) Thread.sleep(20)
    val stuck = chat.simplex.common.ui.UiIdle.inFlight()
    if (stuck.isNotEmpty()) {
      android.util.Log.w("UITEST", "drain gave up after ${DRAIN_MS / 1000} s; abandoned: ${stuck.joinToString { it.what }}")
      chat.simplex.common.ui.UiIdle.abandon(stuck)
      LeakedWork.record(stuck.map { it.what })
    }
    return stuck.map { it.what }
  }

  fun onMain(action: () -> Unit) = androidx.test.platform.app.InstrumentationRegistry.getInstrumentation().runOnMainSync(action)

  private fun presetAndroidPrefs() {
    appPrefs.appearanceBarsBlurRadius.set(0)
    appPrefs.notificationsMode.set(chat.simplex.common.model.NotificationsMode.OFF)
    appPrefs.backgroundServiceNoticeShown.set(true)
    appPrefs.backgroundServiceBatteryNoticeShown.set(true)
    appPrefs.canAskToEnableNotifications.set(false)
    appPrefs.privacyProtectScreen.set(false)
  }

  fun resetForTest(user: String?) {
    Timing.phase("reset.stop running=${chatModel.chatRunning.value}") { stopCore() }
    Timing.phase("reset.delete") { deleteChatDatabaseFilesAndState() }
    Timing.phase("reset.prefs") { PrefStores.restore() }
    Timing.phase("reset.serversBaseline") { TestServers.restoreBaseline() }
    androidx.test.platform.app.InstrumentationRegistry.getInstrumentation().uiAutomation.executeShellCommand("cmd statusbar collapse").close()
    if (chat.simplex.app.SimplexService.isServiceStarted) Timing.phase("reset.ntfs") {
      onMain { chat.simplex.common.views.usersettings.changeNotificationsMode(chat.simplex.common.model.NotificationsMode.OFF, chatModel) }
      waitUntil(10_000, "the notification service to stop") { !chat.simplex.app.SimplexService.isServiceStarted }
    }
    Timing.phase("reset.ui") { CommonUiState.clear(::onMain) }
    if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.P) Timing.phase("reset.clipboard") {
      onMain { (androidAppContext.getSystemService(android.content.Context.CLIPBOARD_SERVICE) as android.content.ClipboardManager).clearPrimaryClip() }
    }
    if (user != null) TestPrefs.preset()
    DbTemplate.install()
    Timing.phase("reset.init") { runBlocking { initChatController() } }
    check(chatModel.chatDbStatus.value == DBMigrationResult.OK) { "database: ${chatModel.chatDbStatus.value}" }
    if (user == null) return
    val u = Timing.phase("reset.createUser") { runBlocking { chatController.apiCreateActiveUser(null, Profile(user, "", null)) } } ?: error("apiCreateActiveUser($user) failed")
    chatModel.currentUser.value = u
    chatModel.localUserCreated.value = true
    appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
    Timing.phase("reset.startChat") { runBlocking { chatController.startChat(u) } }
    val (smp, xftp) = Bridge.servers()
    Timing.phase("reset.servers") { configureServers(smp, xftp) }
    setLastVersionDefault(chatModel)
    CommonUiState.clearTransient(::onMain)
  }

  fun restartCore() {
    runBlocking {
      val ctrl = chatController.getChatCtrl()
      if (ctrl != null && ctrl != -1L && chatModel.chatRunning.value != false) chatController.apiStopChat()
    }
    chatModel.chatRunning.value = null
    val ctrl = chatController.getChatCtrl()
    if (ctrl != null && ctrl != -1L) chatCloseStore(ctrl).let { check(it.isEmpty()) { "chatCloseStore failed: $it" } }
    chatController.setChatCtrl(null)
    CommonUiState.clear(::onMain)
    initChatControllerOnStart()
  }

  private fun stopCore() {
    val deadline = System.currentTimeMillis() + 10_000
    while (chatModel.ctrlInitInProgress.value && System.currentTimeMillis() < deadline) Thread.sleep(50)
    if (chatModel.ctrlInitInProgress.value) {
      android.util.Log.w("UITEST", "ctrlInitInProgress still set after 10 s (left by a failed test); clearing it to reset")
      chatModel.ctrlInitInProgress.value = false
    }
    runBlocking {
      val ctrl = chatController.getChatCtrl()
      if (ctrl != null && ctrl != -1L && chatModel.chatRunning.value == true) chatController.apiStopChat()
    }
    chatModel.chatRunning.value = null
    val ctrl = chatController.getChatCtrl()
    if (ctrl != null && ctrl != -1L) chatCloseStore(ctrl).let { check(it.isEmpty()) { "chatCloseStore failed: $it" } }
    chatController.setChatCtrl(null)
  }

  private fun waitUntil(timeoutMs: Long, what: String, cond: () -> Boolean) {
    val deadline = System.currentTimeMillis() + timeoutMs
    while (!cond()) {
      if (System.currentTimeMillis() > deadline) throw AssertionError("timed out after $timeoutMs ms waiting for $what")
      Thread.sleep(50)
    }
  }

  private const val STARTUP_MS = 180_000L
  private const val DRAIN_MS = 30_000L
}

object DbTemplate {
  private val dir get() = File(dataDir, "uitest-db-template")
  private val keyFile get() = File(dir, "key")

  fun install() {
    check(keyFile.isFile) { "no database template; bootOnce saves it" }
    dbFiles(dir).forEach { it.copyTo(File(dataDir, it.name), overwrite = true) }
    val key = keyFile.readText()
    if (key.isEmpty()) DatabaseUtils.ksDatabasePassword.remove() else DatabaseUtils.ksDatabasePassword.set(key)
    appPrefs.storeDBPassphrase.set(true)
    appPrefs.initialRandomDBPassphrase.set(true)
  }

  fun save() {
    dir.deleteRecursively()
    dir.mkdirs()
    dbFiles(dataDir).forEach { it.copyTo(File(dir, it.name)) }
    keyFile.writeText(DatabaseUtils.ksDatabasePassword.get() ?: "")
  }

  private fun dbFiles(from: File): List<File> =
    from.listFiles { f -> f.isFile && (f.name.startsWith(chatDatabaseFileName) || f.name.startsWith(agentDatabaseFileName)) }.orEmpty().toList()
}
