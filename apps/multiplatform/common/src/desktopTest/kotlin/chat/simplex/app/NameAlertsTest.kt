package chat.simplex.app

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatParseUri
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.chat.SimplexNameView
import chat.simplex.common.views.chatlist.ChatListView
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AnimatedViewState
import chat.simplex.common.views.newchat.planAndConnect
import kotlinx.coroutines.flow.MutableStateFlow
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
  private val parseUri = chatParseUri
  private val verifyNames = ChatController.appPrefs.privacyVerifySimplexNames.get()
  private val locale = Locale.getDefault()
  private val timeZone = TimeZone.getDefault()
  private val unrecorded = mutableListOf<String>()
  private val shownLocalChats = mutableListOf<String>()

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
    ChatController.appPrefs.privacyVerifySimplexNames.set(false)
    ChatModel.currentUser.value = User.sampleData
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    AlertManager.privacySensitive.hideAllAlerts()
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatId.value = null
    ChatModel.currentUser.value = null
    ChatController.appPrefs.privacyVerifySimplexNames.set(verifyNames)
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    chatParseUri = parseUri
    TimeZone.setDefault(timeZone)
    Locale.setDefault(locale)
    assertEquals(listOf(), unrecorded, "requests without recorded responses")
  }

  @Test
  fun knownNameExpired() = recordedAlerts("names/plan-known-expired") {
    connect("@alice.simplex")
    assertTexts("Name expired", "alice.simplex expired on January 1, 2026. Its owner can renew it until January 1, 2099.", "Open chat", "OK")
    assertLocalChats("@2")
  }

  @Test
  fun knownNameExpiredOpensChat() = recordedAlerts("names/plan-known-expired") {
    connect("@alice.simplex")
    press("Open chat")
    waitUntil(timeoutMillis = 5_000) { ChatModel.chatId.value == "@2" }
  }

  @Test
  fun ownNameExpired() = recordedAlerts("names/plan-own-expired") {
    connect("@alice.simplex")
    assertTexts("Your name has expired", "Your name alice.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "OK")
    press("Renew")
    assertTexts("Open web link?", "https://simplex.domains/#testing", "Open", "Cancel")
  }

  @Test
  fun knownNameAvailable() = recordedAlerts("names/plan-known-available") {
    connect("@alice.simplex")
    assertTexts("Name no longer registered", "alice.simplex is available for registration for \$20 for 2 years.", "Register", "Open chat", "OK")
    assertLocalChats("@2")
    press("Register")
    assertTexts("Open web link?", "https://simplex.domains/#testing", "Open", "Cancel")
  }

  @Test
  fun ownNameAvailable() = recordedAlerts("names/plan-own-available") {
    connect("@alice.simplex")
    assertTexts("Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for \$20 for 2 years.", "Re-register", "OK")
    press("Re-register")
    assertTexts("Open web link?", "https://simplex.domains/#testing", "Open", "Cancel")
  }

  @Test
  fun knownNameReserved() = recordedAlerts("names/plan-known-reserved") {
    connect("@alice.simplex")
    assertTexts("Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", "Connect to SimpleX team", "Open chat", "OK")
    assertLocalChats("@2")
    press("Connect to SimpleX team")
    waitUntil(timeoutMillis = 5_000) { unrecorded.isNotEmpty() }
    assertEquals(listOf("/_connect plan 1 simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"), unrecorded.toList())
    unrecorded.clear()
  }

  @Test
  fun knownNameMoved() = recordedAlerts("names/plan-known-moved") {
    connect("@alice.simplex")
    assertTexts("cath", "@alice.simplex", "Open new chat", "Cancel")
    assertLocalChats("@2")
  }

  @Test
  fun nameAvailable() = recordedAlerts("names/error-available") {
    connect("@nobody.simplex")
    assertTexts("Name not registered", "nobody.simplex is available for registration for \$20 for 2 years.", "Register", "OK")
  }

  @Test
  fun nameNotRegistered() = recordedAlerts("names/error-not-registered") {
    connect("acme.simplex")
    assertTexts("Name not registered", "This SimpleX name is not registered. Please check the name.", "OK")
  }

  @Test
  fun knownName() = recordedAlerts("names/plan-known") {
    connect("@alice.simplex")
    assertTexts("alice", "@alice.simplex", "Open chat", "Cancel")
    assertLocalChats("@2")
  }

  @Test
  fun nameMovedToKnownContact() = recordedAlerts("names/plan-moved-known") {
    connect("@alice.simplex")
    assertTexts("cath", "@alice.simplex", "Open chat", "Cancel")
    assertLocalChats("@3", "@2")
  }

  @Test
  fun nameMovedToKnownBusiness() = recordedAlerts("names/plan-moved-business") {
    connect("@alice.simplex")
    assertTexts("cath", "@alice.simplex", "Open chat", "Cancel")
    assertLocalChats("#1", "@2")
  }

  @Test
  fun nameOfOtherKindOffered() = recordedAlerts("names/plan-other-kind") {
    connect("team.simplex")
    assertTexts("team", "#team", "You are a subscriber", "Open channel", "Connect to @team.simplex", "Cancel")
    press("Connect to @team.simplex")
    assertTexts("alisa", "@team.simplex", "Open new chat", "Cancel")
  }

  @Test
  fun channelWithoutRelays() = recordedAlerts("names/plan-channel-no-relays") {
    connect("team.simplex")
    assertTexts("team", "#team", "Channel has no active relays. Please try to join later.", "Connect to @team.simplex", "OK")
    press("Connect to @team.simplex")
    assertTexts("Unconfirmed name", "The SimpleX name team.simplex is registered, but not added to profile. Please add it to your address or channel profile, if you are the owner.", "OK")
  }

  @Test
  fun ownChannel() = recordedAlerts("names/plan-own-channel") {
    connect("#team.simplex")
    assertTexts("team", "#team", "Open channel", "Cancel")
    assertLocalChats("#1")
  }

  @Test
  fun contactViaAddress() = recordedAlerts("connect/contact-via-address") {
    connect("CONTACT_LINK")
    assertTexts("alice", "Alice", "Open chat", "Cancel")
    assertLocalChats("@2")
  }

  @Test
  fun nameMovedEvent() = recordedAlerts("names/plan-moved-business") {
    connect("@alice.simplex")
    val event = json.decodeFromString<API>(fixtureText("names/event-name-moved"))
    val moved = event.result as CR.NameMoved
    runBlocking { ChatController.processReceivedMsg(event) }
    moved.contactIds.forEach { id -> assertEquals(DomainVerification.Moved, (chatInfo("@$id") as ChatInfo.Direct).contact.profile.contactDomainVerified) }
    moved.groupIds.forEach { id -> assertEquals(DomainVerification.Moved, (chatInfo("#$id") as ChatInfo.Group).groupInfo.groupDomainVerified) }
  }

  @Test
  fun chatListSearchFindsChatsByName() = recorded("names/search-team") {
    setContent { ChatListView(ChatModel, MutableStateFlow(AnimatedViewState.GONE), {}, false) }
    onNode(hasSetTextAction()).performTextInput("team")
    waitUntil(timeoutMillis = 5_000) { ChatModel.chatsContext.chats.value.map { it.id }.containsAll(listOf("@2", "#1")) }
    onNodeWithText("alice").assertExists()
    onNodeWithText("team").assertExists()
  }

  @Test
  fun verifiedNameIsCopied() = simplexName(DomainVerification.Verified) { verifications ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitForIdle()
    assertEquals(0, verifications())
  }

  @Test
  fun failedNameIsVerifiedAgain() = simplexName(DomainVerification.Failed) { verifications ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitUntil(timeoutMillis = 5_000) { verifications() == 1 }
  }

  @Test
  fun movedNameIsVerifiedAgain() = simplexName(DomainVerification.Moved) { verifications ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitUntil(timeoutMillis = 5_000) { verifications() == 1 }
  }

  @Test
  fun uncheckedNameOffersVerification() = simplexName(null) { verifications ->
    onNodeWithText("Verify name").performClick()
    waitUntil(timeoutMillis = 5_000) { verifications() == 1 }
  }

  private fun recordedAlerts(fixture: String, test: ComposeUiTest.() -> Unit) = recorded(fixture) {
    setContent {
      AlertManager.shared.showInView()
      AlertManager.privacySensitive.showInView()
    }
    test()
  }

  private fun recorded(fixture: String, test: ComposeUiTest.() -> Unit) = runComposeUiTest {
    val responses = Json.parseToJsonElement(fixtureText(fixture)).jsonObject
    val commands = responses.getValue("commands").jsonObject
    val markdown = responses.getValue("markdown").jsonObject
    val uris = responses.getValue("uris").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands[cmd]?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown[str]?.toString() ?: unrecordedRequest("markdown $str") }
    chatParseUri = { str, safe -> uris[str]?.takeIf { safe == 0 }?.toString() ?: unrecordedRequest("uri $str") }
    test()
  }

  private fun simplexName(verified: DomainVerification?, test: ComposeUiTest.(verifications: () -> Int) -> Unit) = runComposeUiTest {
    var verifications = 0
    setContent { SimplexNameView(SIMPLEX_NAME, verified) { verifications++; null } }
    test { verifications }
  }

  private fun unrecordedRequest(request: String): Nothing {
    synchronized(unrecorded) { unrecorded += request }
    throw IllegalStateException("request without a recorded response: $request")
  }

  private fun ComposeUiTest.connect(name: String) {
    runBlocking { planAndConnect(rhId = null, name, close = null, showLocalChats = { chats -> shownLocalChats += chats.map { it.id } }) }
    waitForIdle()
  }

  private fun ComposeUiTest.press(text: String) {
    onNodeWithText(text).performClick()
    waitForIdle()
  }

  private fun ComposeUiTest.assertTexts(vararg texts: String) {
    runCatching { waitUntil(timeoutMillis = 5_000) { shownTexts() == texts.toList() } }
    assertEquals(texts.toList(), shownTexts())
  }

  private fun ComposeUiTest.shownTexts(): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }

  private fun assertLocalChats(vararg ids: String) {
    assertEquals(ids.toList(), shownLocalChats.toList())
    assertTrue(ChatModel.chatsContext.chats.value.map { it.id }.containsAll(ids.toList()))
  }

  private fun chatInfo(id: String): ChatInfo = ChatModel.chatsContext.chats.value.first { it.id == id }.chatInfo

  private fun fixtureText(name: String): String = File("../../../tests/fixtures/api/$name.json").readText()

  private companion object {
    const val SIMPLEX_NAME = "@alice.simplex"
  }
}
