package chat.simplex.app.ui.harness

import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.*
import chat.simplex.common.platform.NtfManager
import chat.simplex.common.simplexWindowState
import chat.simplex.common.ui.theme.ThemeManager
import chat.simplex.common.ui.UiIdle
import chat.simplex.common.ui.uiTestHooksEnabled
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.database.deleteChatDatabaseFilesAndState
import chat.simplex.common.views.database.stopChatAsync
import chat.simplex.common.views.helpers.*
import chat.simplex.common.views.onboarding.OnboardingStage
import chat.simplex.common.views.onboarding.setLastVersionDefault
import kotlinx.coroutines.runBlocking
import java.io.File
import java.util.Locale

object UiTestApp {
  private var booted = false

  fun bootJvm() {
    if (booted) return
    booted = true
    check(dataDir.startsWith(UiEnv.jvmDir) && File(desktopPlatform.configPath).startsWith(UiEnv.jvmDir)) {
      "app paths outside the per-JVM directory: dataDir=$dataDir configPath=${desktopPlatform.configPath}"
    }
    System.load(UiEnv.libAppLib)
    initHS()
    Locale.setDefault(Locale.ENGLISH)
    uiTestHooksEnabled = true
    ntfManager = NoNtfManager
    simplexWindowState.windowState = WindowState(size = DpSize(1600.dp, 900.dp))
    tmpDir.mkdirs()
    runMigrations()
    ThemeManager.applyTheme(appPrefs.currentTheme.get()!!)
    appPrefs.appearanceBarsBlurRadius.set(0)
    PrefStores.capture()
    ModelSnapshot.capture()
  }

  private var coreStarted = false

  fun drain(): List<String> {
    val deadline = System.currentTimeMillis() + DRAIN_MS
    while (UiIdle.inFlight().isNotEmpty() && System.currentTimeMillis() < deadline) Thread.sleep(20)
    val stuck = UiIdle.inFlight()
    if (stuck.isNotEmpty()) {
      println("UITEST drain gave up after ${DRAIN_MS / 1000} s; abandoned: ${stuck.joinToString { it.what }}")
      UiIdle.abandon(stuck)
      LeakedWork.record(stuck.map { it.what })
    }
    return stuck.map { it.what }
  }

  fun resetForTest(user: String?) {
    drain()
    if (coreStarted) stopAndDeleteCore()
    drain()
    coreStarted = true
    PrefStores.restore()
    UiState.clearUiState()
    TestServers.restoreBaseline()
    DbTemplate.installIfPresent()
    if (user != null) TestPrefs.preset()
    initCore()
    if (!DbTemplate.exists) {
      stopCore()
      DbTemplate.save()
      initCore()
    }
    if (user == null) return UiState.clearTransientUi()
    val u = runBlocking { chatController.apiCreateActiveUser(null, Profile(user, "", null)) }
      ?: error("apiCreateActiveUser($user) failed; users ${runCatching { runBlocking { chatController.listUsers(null) }.map { it.user.displayName } }.getOrElse { it.message }}, database ${chatModel.chatDbStatus.value}")
    chatModel.currentUser.value = u
    chatModel.localUserCreated.value = true
    appPrefs.onboardingStage.set(OnboardingStage.OnboardingComplete)
    runBlocking { chatController.startChat(u) }
    TestServers.configure()
    setLastVersionDefault(chatModel)
    UiState.clearTransientUi()
  }

  fun restartCore(beforeInit: () -> Unit = {}) {
    drain()
    runBlocking {
      val ctrl = chatController.getChatCtrl()
      if (ctrl != null && ctrl != -1L && chatModel.chatRunning.value != false) chatController.apiStopChat()
    }
    chatModel.chatRunning.value = null
    val ctrl = chatController.getChatCtrl()
    if (ctrl != null && ctrl != -1L) chatCloseStore(ctrl).let { check(it.isEmpty()) { "chatCloseStore failed: $it" } }
    chatController.setChatCtrl(null)
    drain()
    UiState.clearUiState()
    beforeInit()
    initChatControllerOnStart()
  }

  private fun initCore() {
    runBlocking { initChatController() }
    check(chatModel.chatDbStatus.value == DBMigrationResult.OK) { "database: ${chatModel.chatDbStatus.value}" }
  }

  fun stopCore() {
    val deadline = System.currentTimeMillis() + 10_000
    while (chatModel.ctrlInitInProgress.value && System.currentTimeMillis() < deadline) Thread.sleep(50)
    if (chatModel.ctrlInitInProgress.value) {
      println("UITEST ctrlInitInProgress still set after 10 s (left by a failed test); clearing it to reset")
      chatModel.ctrlInitInProgress.value = false
    }
    runBlocking { if (chatModel.chatRunning.value == true) stopChatAsync(chatModel) }
    val ctrl = chatController.getChatCtrl()
    if (ctrl != null && ctrl != -1L) chatCloseStore(ctrl).let { check(it.isEmpty()) { "chatCloseStore failed: $it" } }
    chatController.setChatCtrl(null)
  }

  private fun stopAndDeleteCore() {
    stopCore()
    deleteChatDatabaseFilesAndState()
  }

  private const val DRAIN_MS = 30_000L

  private object NoNtfManager : NtfManager() {
    override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = false
    override fun hasNotificationsForChat(chatId: String): Boolean = false
    override fun cancelNotificationsForChat(chatId: String) {}
    override fun cancelNotificationsForUser(userId: Long) {}
    override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) {}
    override fun androidCreateNtfChannelsMaybeShowAlert() {}
    override fun cancelCallNotification() {}
    override fun cancelAllNotifications() {}
    override fun showMessage(title: String, text: String) {}
  }
}
