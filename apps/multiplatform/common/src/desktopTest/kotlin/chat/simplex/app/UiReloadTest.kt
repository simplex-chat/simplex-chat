package chat.simplex.app

import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.acquireSingleInstance
import chat.simplex.common.handOffSingleInstance
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.ChatCtrl
import chat.simplex.common.platform.chatRecvMsgWait
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.platform.initChatController
import chat.simplex.common.platform.reloadedChatCtrl
import chat.simplex.common.platform.reloadedChatDbEncrypted
import chat.simplex.common.platform.reloadedInstanceLock
import chat.simplex.common.platform.uiReloadRequested
import chat.simplex.common.reloadUi
import chat.simplex.common.reloadUiOrAlert
import chat.simplex.common.simplexWindowState
import chat.simplex.common.singleInstanceLock
import chat.simplex.common.uiReloadRefusal
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.DBMigrationResult
import chat.simplex.common.views.helpers.DatabaseUtils
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.helpers.installedUiJars
import kotlinx.coroutines.CoroutineStart
import kotlinx.coroutines.TimeoutCancellationException
import kotlinx.coroutines.async
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObjectBuilder
import kotlinx.serialization.json.addJsonObject
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonArray
import kotlinx.serialization.json.putJsonObject
import java.nio.channels.FileChannel
import java.nio.file.Files
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import java.util.Locale
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import kotlin.test.*

private const val CTRL: ChatCtrl = 1001
private const val USER_CTRL: ChatCtrl = 1002
private const val CMD_OK = """{"result":{"type":"cmdOk"}}"""
private const val NO_ACTIVE_USER = """{"error":{"type":"error","errorType":{"type":"noActiveUser"}}}"""
private const val NO_REMOTE_HOSTS = """{"result":{"type":"remoteHostList","remoteHosts":[]}}"""
private const val NO_ADDRESS = """{"error":{"type":"errorStore","storeError":{"type":"userContactLinkNotFound"}}}"""

@OptIn(ExperimentalTestApi::class)
internal fun takeEarlierUncaughtExceptions() {
  runCatching { runComposeUiTest {} }.exceptionOrNull()?.let { e ->
    println("uncaught exceptions of earlier tests:\n${e.stackTraceToString()}")
  }
}

@OptIn(ExperimentalTestApi::class)
class UiReloadTest {
  private val testRoot = Files.createTempDirectory("ui-reload").toFile()
  private val commands = CopyOnWriteArrayList<Pair<ChatCtrl, String>>()
  private val sent = LinkedBlockingQueue<String>()
  private val events = LinkedBlockingQueue<String>()
  private val reads = LinkedBlockingQueue<ChatCtrl>()
  private val readCalls = AtomicInteger()
  private val taken = AtomicInteger()
  private val nameMoved = fixtureText("names/event-name-moved")
  private val user = Json.parseToJsonElement(nameMoved).jsonObject.getValue("result").jsonObject.getValue("user")
  private var respond: (String) -> String = this::response
  private val sendCmd = chatSendCmdRetry
  private val recvMsg = chatRecvMsgWait
  private val uiJars = installedUiJars
  private val chatCtrl = ChatController.getChatCtrl()
  private val locale = Locale.getDefault()
  private val chatRunning = ChatModel.chatRunning.value
  private val chatDbEncrypted = ChatModel.chatDbEncrypted.value
  private val chatDbStatus = ChatModel.chatDbStatus.value
  private val currentUser = ChatModel.currentUser.value
  private val users = ChatModel.users.toList()
  private val localUserCreated = ChatModel.localUserCreated.value
  private val conditions = ChatModel.conditions.value
  private val incompleteDbRemoved = ChatModel.incompleteInitializedDbRemoved.value
  private val terminalItems = ChatModel.terminalItems.value
  private val userAddress = ChatModel.userAddress.value
  private val chatItemTTL = ChatModel.chatItemTTL.value
  private val userTags = ChatModel.userTags.value
  private val activeChatTagFilter = ChatModel.activeChatTagFilter.value
  private val setDeliveryReceipts = ChatModel.setDeliveryReceipts.value
  private val selfDestructPassphrase = DatabaseUtils.ksSelfDestructPassword.passphrase.get()
  private val selfDestructInitVector = DatabaseUtils.ksSelfDestructPassword.initVector.get()
  private val newDatabaseInitialized = appPrefs.newDatabaseInitialized.get()
  private val deliveryReceiptsSet = appPrefs.privacyDeliveryReceiptsSet.get()
  private val onboardingStage = appPrefs.onboardingStage.get()
  private val chatLastStart = appPrefs.chatLastStart.get()
  private val chatStopped = appPrefs.chatStopped.get()
  private val deleteDatabaseBackups = appPrefs.shouldDeleteDatabaseBackups.get()

  @BeforeTest
  fun setUp() {
    takeEarlierUncaughtExceptions()
    Locale.setDefault(Locale.US)
    chatSendCmdRetry = { ctrl, cmd, _ ->
      commands += ctrl to cmd
      sent.put(cmd)
      respond(cmd)
    }
    chatRecvMsgWait = { ctrl, _ ->
      reads.put(ctrl)
      val reader = Thread.currentThread()
      val read = readCalls.incrementAndGet()
      println("UiReloadTest: read $read on ${reader.name}#${reader.id}, controller $ctrl")
      events.take().also { println("UiReloadTest: read $read took event ${taken.incrementAndGet()}") }
    }
    installedUiJars = { listOf(testRoot.resolve("common.jar")) }
    DatabaseUtils.ksSelfDestructPassword.remove()
    ChatModel.chatRunning.value = null
  }

  @AfterTest
  fun tearDown() {
    ChatController.setChatCtrl(null)
    ChatController.setChatCtrl(chatCtrl)
    reloadedInstanceLock?.channel()?.close()
    handOffSingleInstance()
    reloadedInstanceLock?.channel()?.close()
    reloadedInstanceLock = null
    reloadedChatCtrl = null
    reloadedChatDbEncrypted = null
    uiReloadRequested = false
    simplexWindowState.reloading.value = false
    chatSendCmdRetry = sendCmd
    chatRecvMsgWait = recvMsg
    installedUiJars = uiJars
    AlertManager.shared.hideAllAlerts()
    ChatModel.activeCall.value = null
    ChatModel.chatRunning.value = chatRunning
    ChatModel.chatDbEncrypted.value = chatDbEncrypted
    ChatModel.chatDbStatus.value = chatDbStatus
    ChatModel.currentUser.value = currentUser
    ChatModel.users.clear()
    ChatModel.users.addAll(users)
    ChatModel.localUserCreated.value = localUserCreated
    ChatModel.conditions.value = conditions
    ChatModel.incompleteInitializedDbRemoved.value = incompleteDbRemoved
    ChatModel.terminalItems.value = terminalItems
    ChatModel.userAddress.value = userAddress
    ChatModel.chatItemTTL.value = chatItemTTL
    ChatModel.userTags.value = userTags
    ChatModel.activeChatTagFilter.value = activeChatTagFilter
    ChatModel.setDeliveryReceipts.value = setDeliveryReceipts
    DatabaseUtils.ksSelfDestructPassword.passphrase.set(selfDestructPassphrase)
    DatabaseUtils.ksSelfDestructPassword.initVector.set(selfDestructInitVector)
    appPrefs.newDatabaseInitialized.set(newDatabaseInitialized)
    appPrefs.privacyDeliveryReceiptsSet.set(deliveryReceiptsSet)
    appPrefs.onboardingStage.set(onboardingStage)
    appPrefs.chatLastStart.set(chatLastStart)
    appPrefs.chatStopped.set(chatStopped)
    appPrefs.shouldDeleteDatabaseBackups.set(deleteDatabaseBackups)
    Locale.setDefault(locale)
    testRoot.deleteRecursively()
  }

  @Test
  fun reloadIsRefusedWhileAnyConditionIsUnmet() {
    runningChat()
    assertNull(uiReloadRefusal())
    ChatModel.activeCall.value = call(Contact.sampleData)
    assertEquals("Reload the UI after the call ends.", uiReloadRefusal()?.let(::generalGetString))
    ChatModel.activeCall.value = null
    ChatController.setChatCtrl(null)
    assertEquals("Reload the UI while the chat is running.", uiReloadRefusal()?.let(::generalGetString))
    ChatController.setChatCtrl(CTRL)
    ChatModel.chatRunning.value = false
    assertEquals("Reload the UI while the chat is running.", uiReloadRefusal()?.let(::generalGetString))
    ChatModel.chatRunning.value = true
    DatabaseUtils.ksSelfDestructPassword.set("passcode")
    assertEquals("The UI is not reloaded while a self-destruct passcode is set. Restart the app instead.", uiReloadRefusal()?.let(::generalGetString))
    DatabaseUtils.ksSelfDestructPassword.remove()
    installedUiJars = { null }
    assertEquals("The app's own files are not found. Restart the app instead.", uiReloadRefusal()?.let(::generalGetString))
  }

  @Test
  fun refusedReloadExplainsWhyAndKeepsTheChat() = runComposeUiTest {
    runningChat()
    ChatModel.activeCall.value = call(Contact.sampleData)
    setContent { AlertManager.shared.showInView() }
    reloadUiOrAlert()
    waitForIdle()
    onNodeWithText("UI not reloaded").assertExists()
    onNodeWithText("Reload the UI after the call ends.").assertExists()
    assertFalse(reloadUi())
    assertEquals(CTRL, ChatController.getChatCtrl())
    assertNull(reloadedChatCtrl)
    assertFalse(uiReloadRequested)
    assertFalse(simplexWindowState.reloading.value)
  }

  @Test
  fun reloadedUiReusesTheControllerAndReceivesItsEvents() {
    ChatController.setChatCtrl(CTRL)
    ChatModel.chatDbEncrypted.value = true
    assertIs<CR.NameMoved>(receivedEvent {
      ChatController.startChatWithoutUser()
      events.put(nameMoved)
    }.result)

    assertTrue(reloadUi())
    assertEquals(CTRL, reloadedChatCtrl)
    assertEquals(true, reloadedChatDbEncrypted)
    assertFalse(ChatController.hasChatCtrl())
    assertTrue(uiReloadRequested)
    assertTrue(simplexWindowState.reloading.value)
    events.put(nameMoved)
    repeat(3) { assertEquals(CTRL, reads.poll(5, TimeUnit.SECONDS)) }

    ChatModel.chatRunning.value = null
    ChatModel.chatDbEncrypted.value = false
    assertIs<CR.NameMoved>(receivedEvent { initChatController() }.result)
    assertEquals(CTRL, ChatController.getChatCtrl())
    assertNull(reloadedChatCtrl)
    assertEquals(true, ChatModel.chatDbEncrypted.value)
    assertEquals(DBMigrationResult.OK, ChatModel.chatDbStatus.value)
    assertEquals(setOf(CTRL), commands.map { it.first }.toSet())
  }

  @Test
  fun reloadedUiOfAUserStartsTheChatThatTheCoreRuns() {
    respond = this::userResponse
    appPrefs.shouldDeleteDatabaseBackups.set(false)
    runningChat(USER_CTRL)
    assertTrue(reloadUi())

    ChatModel.chatRunning.value = null
    ChatModel.localUserCreated.value = null
    assertIs<CR.NameMoved>(receivedEvent {
      initChatController()
      events.put(nameMoved)
    }.result)
    assertEquals(true, ChatModel.chatRunning.value)
    assertEquals(true, ChatModel.localUserCreated.value)
    awaitSent(CC.ApiGetBadgeState(1).cmdString, times = 2)
    assertEquals(setOf(USER_CTRL), commands.map { it.first }.toSet())
  }

  @Test
  fun instanceLockPassesToTheReloadedUi() {
    val lock = assertNotNull(FileChannel.open(testRoot.resolve("simplex.started").toPath(), READ, WRITE, CREATE).tryLock(0L, 1L, false))
    reloadedInstanceLock = lock
    assertTrue(acquireSingleInstance(null))
    runningChat()
    reloadUiOrAlert()
    assertFalse(AlertManager.shared.hasAlertsShown())
    assertSame(lock, reloadedInstanceLock)
    assertFalse(singleInstanceLock)

    assertTrue(acquireSingleInstance(null))
    assertTrue(singleInstanceLock)
    assertNull(reloadedInstanceLock)
    assertTrue(lock.isValid)
    handOffSingleInstance()
    assertSame(lock, reloadedInstanceLock)
  }

  private fun runningChat(ctrl: ChatCtrl = CTRL) {
    ChatController.setChatCtrl(ctrl)
    ChatModel.chatRunning.value = true
  }

  private fun receivedEvent(steps: suspend () -> Unit): API = runBlocking {
    val event = async(start = CoroutineStart.UNDISPATCHED) { withTimeout(5_000) { ChatController.messagesChannel.receive() } }
    steps()
    try {
      event.await()
    } catch (e: TimeoutCancellationException) {
      println("UiReloadTest: no event received; controller ${ChatController.getChatCtrl()}, chatRunning ${ChatModel.chatRunning.value}, messagesChannel ${ChatController.messagesChannel}")
      println("UiReloadTest: last terminal items ${ChatModel.terminalItems.value.takeLast(12).map { it.label }}")
      Thread.getAllStackTraces().filterKeys { it.name == "simplex-core-events" }.forEach { (thread, stack) ->
        println("UiReloadTest: ${thread.name}#${thread.id} ${thread.state}\n  ${stack.take(8).joinToString("\n  ")}")
      }
      throw e
    }
  }

  private fun awaitSent(cmd: String, times: Int) {
    var count = 0
    while (count < times) {
      if (assertNotNull(sent.poll(5, TimeUnit.SECONDS), "$cmd sent $count of $times times") == cmd) count++
    }
  }

  private fun response(cmd: String): String = when (cmd) {
    "/u" -> NO_ACTIVE_USER
    "/list remote hosts" -> NO_REMOTE_HOSTS
    else -> CMD_OK
  }

  private fun userResponse(cmd: String): String = when (cmd) {
    CC.ShowActiveUser().cmdString -> result("activeUser") { put("user", user) }
    CC.ListUsers().cmdString -> result("usersList") {
      putJsonArray("users") {
        addJsonObject {
          put("user", user)
          put("unreadCount", 0)
        }
      }
    }
    CC.CheckChatRunning().cmdString, CC.StartChat(mainApp = true).cmdString -> result("chatRunning") {}
    CC.ApiShowMyAddress(1).cmdString -> NO_ADDRESS
    CC.APIGetChatItemTTL(1).cmdString -> result("chatItemTTL") { put("user", user) }
    CC.ApiGetChats(1).cmdString -> result("apiChats") {
      put("user", user)
      putJsonArray("chats") {}
    }
    CC.ApiGetChatTags(1).cmdString -> result("chatTags") {
      put("user", user)
      putJsonArray("userTags") {}
    }
    else -> CMD_OK
  }

  private fun result(type: String, fields: JsonObjectBuilder.() -> Unit): String =
    buildJsonObject {
      putJsonObject("result") {
        put("type", type)
        fields()
      }
    }.toString()
}
