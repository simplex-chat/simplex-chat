package chat.simplex.app

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.ChatModel
import chat.simplex.common.model.User
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.newchat.planAndConnect
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.io.File
import java.util.Locale
import java.util.TimeZone
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class NameAlertsTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val locale = Locale.getDefault()
  private val timeZone = TimeZone.getDefault()

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
    ChatModel.currentUser.value = User.sampleData
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    AlertManager.privacySensitive.hideAllAlerts()
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.currentUser.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    TimeZone.setDefault(timeZone)
    Locale.setDefault(locale)
  }

  @Test
  fun knownNameExpired() = assertAlert(
    "plan-known-expired",
    "Name expired", "alice.simplex expired on January 1, 2026. Its owner can renew it until January 1, 2099.", "Open chat", "OK"
  )

  @Test
  fun ownNameExpired() = assertAlert(
    "plan-own-expired",
    "Your name has expired", "Your name alice.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "OK"
  )

  @Test
  fun knownNameAvailable() = assertAlert(
    "plan-known-available",
    "Name no longer registered", "alice.simplex is available for registration for \$20 for 2 years.", "Register", "Open chat", "OK"
  )

  @Test
  fun ownNameAvailable() = assertAlert(
    "plan-own-available",
    "Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for \$20 for 2 years.", "Re-register", "OK"
  )

  @Test
  fun knownNameReserved() = assertAlert(
    "plan-known-reserved",
    "Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", "Connect to SimpleX team", "Open chat", "OK"
  )

  @Test
  fun knownNameMoved() = assertAlert("plan-known-moved")

  @Test
  fun nameAvailable() = assertAlert(
    "error-available",
    "Name not registered", "nobody.simplex is available for registration for \$20 for 2 years.", "Register", "OK"
  )

  @Test
  fun nameNotRegistered() = assertAlert(
    "error-not-registered",
    "Name not registered", "This SimpleX name is not registered. Please check the name.", "OK"
  )

  private fun assertAlert(fixture: String, vararg texts: String) = runComposeUiTest {
    val recorded = Json.parseToJsonElement(File("../../../tests/fixtures/names/$fixture.json").readText()).jsonObject
    val commands = recorded.getValue("commands").jsonObject
    val markdown = recorded.getValue("markdown").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands[cmd]?.toString() ?: fail("unrecorded command: $cmd") }
    chatParseMarkdown = { str -> markdown[str]?.toString() ?: fail("unrecorded markdown: $str") }
    runBlocking { planAndConnect(rhId = null, markdown.keys.single(), close = null) }
    setContent {
      AlertManager.shared.showInView()
      AlertManager.privacySensitive.showInView()
    }
    val shown = onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }
    assertEquals(texts.toList(), shown)
  }
}
