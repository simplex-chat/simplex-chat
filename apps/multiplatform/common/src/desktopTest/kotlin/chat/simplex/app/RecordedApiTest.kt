package chat.simplex.app

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.platform.NotificationAction
import chat.simplex.common.platform.NtfManager
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatParseUri
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.newchat.planAndConnect
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonObject
import java.io.File
import java.util.Locale
import java.util.TimeZone
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
abstract class RecordedApiTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val parseUri = chatParseUri
  private val locale = Locale.getDefault()
  private val timeZone = TimeZone.getDefault()
  protected val unrecorded = mutableListOf<String>()
  protected val shownLocalChats = mutableListOf<String>()
  private var commands: JsonObject? = null

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
    ChatModel.currentUser.value = User.sampleData
    simplexWindowState.windowState = WindowState(size = DpSize(1200.dp, 800.dp))
    ntfManager = object : NtfManager() {
      override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = false
      override fun hasNotificationsForChat(chatId: String): Boolean = false
      override fun cancelNotificationsForChat(chatId: String) {}
      override fun cancelNotificationsForUser(userId: Long) {}
      override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) {}
      override fun cancelCallNotification() {}
      override fun cancelAllNotifications() {}
      override fun showMessage(title: String, text: String) {}
      override fun androidCreateNtfChannelsMaybeShowAlert() {}
    }
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    AlertManager.privacySensitive.hideAllAlerts()
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatId.value = null
    ChatModel.chatRunning.value = null
    ChatModel.appOpenUrlConnecting.value = false
    ChatModel.currentUser.value = null
    simplexWindowState.toasts.clear()
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    chatParseUri = parseUri
    TimeZone.setDefault(timeZone)
    Locale.setDefault(locale)
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  protected fun recordedAlerts(fixture: String, test: ComposeUiTest.() -> Unit) = recorded(fixture) {
    setContent {
      AlertManager.shared.showInView()
      AlertManager.privacySensitive.showInView()
    }
    test()
  }

  protected fun recorded(fixture: String, test: ComposeUiTest.() -> Unit) = runComposeUiTest {
    val responses = Json.parseToJsonElement(fixtureText(fixture)).jsonObject
    val commands = responses["commands"]?.jsonObject
    val markdown = responses["markdown"]?.jsonObject
    val uris = responses["uris"]?.jsonObject
    this@RecordedApiTest.commands = commands
    chatSendCmdRetry = { _, cmd, _ -> commands?.get(cmd)?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown?.get(str)?.toString() ?: unrecordedRequest("markdown $str") }
    chatParseUri = { str, safe -> uris?.get(str)?.takeIf { safe == 0 }?.toString() ?: unrecordedRequest("uri $str") }
    test()
  }

  protected fun recordedResult(cmd: String): CR {
    val response = assertNotNull(commands?.get(cmd), "no recorded response to $cmd")
    return assertIs<API.Result>(json.decodeFromString<API>(response.toString()), cmd).res
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  protected fun ComposeUiTest.connect(name: String, cleanup: (() -> Unit)? = null) {
    runBlocking { planAndConnect(rhId = null, name, close = null, cleanup = cleanup, showLocalChats = { chats -> shownLocalChats += chats.map { it.id } }) }
    waitForIdle()
  }

  protected fun ComposeUiTest.press(text: String) {
    onNodeWithText(text).performClick()
    waitForIdle()
  }

  protected fun ComposeUiTest.assertTexts(vararg texts: String) {
    runCatching { waitUntil(timeoutMillis = 5_000) { shownTexts() == texts.toList() } }
    assertEquals(texts.toList(), shownTexts())
  }

  protected fun ComposeUiTest.assertRequested(request: String) = assertRequested { it == request }

  protected fun ComposeUiTest.assertRequested(expected: (String) -> Boolean) {
    runCatching { waitUntil(timeoutMillis = 5_000) { synchronized(unrecorded) { unrecorded.isNotEmpty() } } }
    synchronized(unrecorded) {
      assertEquals(1, unrecorded.size, unrecorded.toString())
      assertTrue(expected(unrecorded.single()), unrecorded.toString())
      unrecorded.clear()
    }
    waitUntil(timeoutMillis = 5_000) { AlertManager.shared.hasAlertsShown() }
  }

  protected fun ComposeUiTest.shownTexts(): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }

  protected fun assertLocalChats(vararg ids: String) {
    assertEquals(ids.toList(), shownLocalChats.toList())
    assertTrue(chatIds().containsAll(ids.toList()))
  }

  protected fun chatIds(): List<String> = ChatModel.chatsContext.chats.value.map { it.id }

  protected fun chatInfo(id: String): ChatInfo = ChatModel.chatsContext.chats.value.first { it.id == id }.chatInfo
}

internal fun fixtureText(name: String): String = File("../../../tests/fixtures/api/$name.json").readText()
