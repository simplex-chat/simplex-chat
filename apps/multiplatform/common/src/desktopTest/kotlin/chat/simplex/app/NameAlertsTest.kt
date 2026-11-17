package chat.simplex.app

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.platform.ClipboardManager
import androidx.compose.ui.platform.LocalClipboardManager
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.text.AnnotatedString
import androidx.compose.ui.text.input.TextFieldValue
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatParseUri
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.SimplexNameView
import chat.simplex.common.views.chatlist.ChatListView
import chat.simplex.common.views.chatlist.ConnectByNameRow
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AnimatedViewState
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import chat.simplex.common.views.newchat.planAndConnect
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
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
  private val locale = Locale.getDefault()
  private val timeZone = TimeZone.getDefault()
  private val unrecorded = mutableListOf<String>()
  private val shownLocalChats = mutableListOf<String>()

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

  @Test
  fun knownNameExpired() = recordedAlerts("names/plan-known-expired") {
    connect("@alice.simplex")
    assertTexts("Name expired", "alice.simplex expired on January 1, 2026. Its owner can renew it until January 1, 2099.", "Open chat", "OK")
    assertLocalChats("@2")
    press("Open chat")
    assertRequested("/_get chat @2 initial=100")
  }

  @Test
  fun ownNameExpired() = recordedAlerts("names/plan-own-expired") {
    connect("@alice.simplex")
    assertTexts("Your name has expired", "Your name alice.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "OK")
    assertLocalChats()
    press("Renew")
    assertTexts("Open web link?", "https://simplex.domains/#testing", "Cancel", "Open")
  }

  @Test
  fun knownNameAvailable() = recordedAlerts("names/plan-known-available") {
    connect("@alice.simplex")
    assertTexts("Name no longer registered", "alice.simplex is available for registration for \$20 for 2 years.", "Register", "Open chat", "OK")
    assertLocalChats("@2")
    press("Register")
    assertTexts("Open web link?", "https://simplex.domains/#testing", "Cancel", "Open")
  }

  @Test
  fun ownNameAvailable() = recordedAlerts("names/plan-own-available") {
    connect("@alice.simplex")
    assertTexts("Your name has expired", "Your name alice.simplex is no longer registered. It is available for registration for \$20 for 2 years.", "Re-register", "OK")
    press("Re-register")
    assertTexts("Open web link?", "https://simplex.domains/#testing", "Cancel", "Open")
  }

  @Test
  fun knownNameReserved() = recordedAlerts("names/plan-known-reserved") {
    connect("@alice.simplex")
    assertTexts("Name not registered", "alice.simplex is reserved for community. Please contact SimpleX Chat if you are interested in using it.", "Connect to SimpleX team", "Open chat", "OK")
    assertLocalChats("@2")
    press("Connect to SimpleX team")
    assertRequested("/_connect plan 1 simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im")
  }

  @Test
  fun knownNameMoved() = recordedAlerts("names/plan-known-moved") {
    connect("@alice.simplex")
    assertTexts("cath", "@alice.simplex", "Open new chat", "Cancel")
    assertLocalChats("@2")
    press("Open new chat")
    assertRequested { it.startsWith("/_prepare contact 1 ") && it.contains(" domain=alice.simplex ") }
  }

  @Test
  fun okClosesNameAlert() = recordedAlerts("names/plan-known-expired") {
    var cleanups = 0
    connect("@alice.simplex", cleanup = { cleanups++ })
    press("OK")
    assertTexts()
    assertEquals(1, cleanups)
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
    assertLocalChats("@3")
  }

  @Test
  fun nameMovedToKnownBusiness() = recordedAlerts("names/plan-moved-business") {
    connect("@alice.simplex")
    assertTexts("cath", "@alice.simplex", "Open chat", "Cancel")
    assertLocalChats("#1")
  }

  @Test
  fun nameOfOtherKindOffered() = recordedAlerts("names/plan-other-kind") {
    connect("team.simplex")
    assertTexts("team", "#team", "1 subscriber", "You are a subscriber", "Open channel", "Connect to @team.simplex", "Cancel")
    assertLocalChats("#1")
    press("Connect to @team.simplex")
    assertTexts("alisa", "@team.simplex", "Open new chat", "Cancel")
    assertTrue(chatIds().contains("@2"))
  }

  @Test
  fun channelWithoutRelays() = recordedAlerts("names/plan-channel-no-relays") {
    connect("team.simplex")
    assertTexts("team", "Channel has no active relays. Please try to join later.", "Connect to @team.simplex", "OK")
    assertLocalChats()
    press("Connect to @team.simplex")
    assertTexts("Unconfirmed name", "The SimpleX name team.simplex is registered, but not added to profile. Please add it to your address or channel profile, if you are the owner.", "OK")
  }

  @Test
  fun ownChannel() = recordedAlerts("names/plan-own-channel") {
    connect("#team.simplex")
    assertTexts("Your channel", "This is your link for channel team (#team)!", "Open channel", "Cancel")
    assertLocalChats("#1")
  }

  @Test
  fun knownNameExpiredWithoutGrace() = recordedAlerts("names/plan-known-expired-no-grace") {
    connect("@alice.simplex")
    assertTexts("Name expired", "alice.simplex expired on January 1, 2026.", "Open chat", "OK")
    assertLocalChats("@2")
  }

  @Test
  fun ownNameExpiredWithoutGrace() = recordedAlerts("names/plan-own-expired-no-grace") {
    connect("@alice.simplex")
    assertTexts("Your name has expired", "Your name alice.simplex expired on January 1, 2026.", "Renew", "OK")
  }

  @Test
  fun ownChannelNameExpired() = recordedAlerts("names/plan-own-channel-expired") {
    connect("#team.simplex")
    assertTexts("Your name has expired", "Your name team.simplex expired on January 1, 2026. Renew it before January 1, 2099.", "Renew", "Open chat", "OK")
    assertLocalChats("#1")
  }

  @Test
  fun channelNameMoved() = recordedAlerts("names/plan-channel-moved") {
    connect("#team.simplex")
    assertTexts("teamv2", "#team", "1 subscriber", "Open channel", "Cancel")
    assertLocalChats("#1")
  }

  @Test
  fun movedChannel() = recordedAlerts("names/plan-old-channel") {
    connect("TEAM_LINK")
    assertTexts("team", "1 subscriber", "You are a subscriber", "Open channel", "Cancel")
    assertLocalChats("#1")
    assertEquals(DomainVerification.Moved, (chatInfo("#1") as ChatInfo.Group).groupInfo.groupDomainVerified)
  }

  @Test
  fun channelNameNotVerified() = recordedAlerts("names/plan-known-failed") {
    connect("TEAM_LINK")
    assertTexts("team", "2 subscribers", "You are a subscriber", "Open channel", "Cancel")
    assertLocalChats("#1")
    assertEquals(DomainVerification.Failed, (chatInfo("#1") as ChatInfo.Group).groupInfo.groupDomainVerified)
  }

  @Test
  fun contactViaAddress() = recordedAlerts("connect/contact-via-address") {
    connect("CONTACT_LINK")
    assertTexts("alice", "Open chat", "Cancel")
    assertLocalChats("@3")
  }

  @Test
  fun groupLinkAsAddress() = recordedAlerts("connect/group-link-as-address") {
    connect("GROUP_LINK")
    assertTexts("Invalid connection link", "Please check that you used the correct link or ask your contact to send you another one.", "OK")
  }

  @Test
  fun groupLinkAsChannel() = recordedAlerts("connect/group-link-as-channel") {
    connect("GROUP_LINK")
    assertTexts("Invalid connection link", "Please check that you used the correct link or ask your contact to send you another one.", "OK")
  }

  @Test
  fun connectByNameRowShowsLocalChats() = recorded("names/plan-known") {
    val searchText = mutableStateOf(TextFieldValue("alice"))
    val candidate = mutableStateOf<String?>("@alice.simplex")
    setContent {
      ConnectByNameRow("@alice.simplex", searchText, candidate, close = null, showLocalChats = { chats -> shownLocalChats += chats.map { it.id } })
      AlertManager.shared.showInView()
      AlertManager.privacySensitive.showInView()
    }
    press("Connect to @alice.simplex")
    assertTexts("Connect to @alice.simplex", "alice", "@alice.simplex", "Open chat", "Cancel")
    assertLocalChats("@2")
    assertEquals("alice", searchText.value.text)
    assertEquals("@alice.simplex", candidate.value)
    press("Open chat")
    assertRequested("/_get chat @2 initial=100")
    assertEquals("", searchText.value.text)
    assertEquals(null, candidate.value)
  }

  @Test
  fun nameMovedEvent() = recorded("names/event-name-moved") {
    runBlocking {
      withContext(Dispatchers.Main) {
        ChatModel.chatsContext.updateChat(null, ChatInfo.Direct(Contact.sampleData.copy(contactId = 2)))
        ChatModel.chatsContext.updateChat(null, ChatInfo.Group(GroupInfo.sampleData, groupChatScope = null))
      }
      ChatController.processReceivedMsg(json.decodeFromString<API>(fixtureText("names/event-name-moved")))
    }
    assertEquals(DomainVerification.Moved, (chatInfo("@2") as ChatInfo.Direct).contact.profile.contactDomainVerified)
    assertEquals(DomainVerification.Moved, (chatInfo("#1") as ChatInfo.Group).groupInfo.groupDomainVerified)
    val terminalItem = ChatModel.terminalItems.value.last()
    assertEquals("< nameMoved", terminalItem.label)
    assertEquals("userId: 1\ncontactIds: [2]\ngroupIds: [1]", terminalItem.details)
  }

  @Test
  fun chatListSearchShowsChatsOfName() = recorded("names/search-team") {
    ChatModel.chatRunning.value = true
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        ChatListView(ChatModel, MutableStateFlow(AnimatedViewState.GONE), {}, false)
      }
    }
    val names = listOf("alice", "team")
    val shownBefore = names.associateWith { onAllNodesWithText(it).fetchSemanticsNodes().size }
    onNode(hasSetTextAction()).performTextInput("team.simplex")
    waitUntil(timeoutMillis = 5_000) { names.all { onAllNodesWithText(it).fetchSemanticsNodes().size > shownBefore.getValue(it) } }
    assertEquals(setOf("@2", "#1"), chatIds().toSet())
    onNodeWithText("Connect to team.simplex").assertExists()
  }

  @Test
  fun rejectedContactInChatList() = recorded("connect/request-rejected") {
    ChatModel.chatRunning.value = true
    runBlocking {
      val chats = ChatController.apiGetChats(null)
      withContext(Dispatchers.Main) { ChatModel.chatsContext.updateChats(chats) }
    }
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        ChatListView(ChatModel, MutableStateFlow(AnimatedViewState.GONE), {}, false)
      }
    }
    assertEquals(listOf("@2"), chatIds())
    assertFalse((chatInfo("@2") as ChatInfo.Direct).contact.active)
    waitUntil(timeoutMillis = 5_000) { onAllNodesWithText("alice").fetchSemanticsNodes().isNotEmpty() }
  }

  @Test
  fun verifiedNameIsCopied() = simplexName(DomainVerification.Verified) { verifications, copied ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitForIdle()
    assertEquals(listOf(SIMPLEX_NAME), copied)
    assertEquals(listOf("Copied to clipboard"), simplexWindowState.toasts.map { it.first })
    assertEquals(0, verifications())
  }

  @Test
  fun failedNameIsVerifiedAgain() = simplexName(DomainVerification.Failed) { verifications, _ ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitUntil(timeoutMillis = 5_000) { verifications() == 1 }
  }

  @Test
  fun failedVerificationShowsReason() = simplexName(DomainVerification.Failed, DomainVerification.Failed to "the name does not resolve to the contact's address") { verifications, _ ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitUntil(timeoutMillis = 5_000) { verifications() == 1 && AlertManager.shared.hasAlertsShown() }
    onNodeWithText("SimpleX name not verified").assertExists()
    onNodeWithText("the name does not resolve to the contact's address").assertExists()
  }

  @Test
  fun movedNameIsVerifiedAgain() = simplexName(DomainVerification.Moved) { verifications, _ ->
    onNodeWithText(SIMPLEX_NAME).performClick()
    waitUntil(timeoutMillis = 5_000) { verifications() == 1 }
  }

  @Test
  fun uncheckedNameOffersVerification() = simplexName(null) { verifications, _ ->
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
    val commands = responses["commands"]?.jsonObject
    val markdown = responses["markdown"]?.jsonObject
    val uris = responses["uris"]?.jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands?.get(cmd)?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown?.get(str)?.toString() ?: unrecordedRequest("markdown $str") }
    chatParseUri = { str, safe -> uris?.get(str)?.takeIf { safe == 0 }?.toString() ?: unrecordedRequest("uri $str") }
    test()
  }

  private fun simplexName(
    verified: DomainVerification?,
    verification: Pair<DomainVerification?, String?>? = null,
    test: ComposeUiTest.(verifications: () -> Int, copied: List<String>) -> Unit
  ) = runComposeUiTest {
    var verifications = 0
    val copied = mutableListOf<String>()
    val clipboard = object : ClipboardManager {
      override fun setText(annotatedString: AnnotatedString) { copied += annotatedString.text }
      override fun getText(): AnnotatedString? = null
    }
    setContent {
      CompositionLocalProvider(LocalClipboardManager provides clipboard) {
        SimplexNameView(SIMPLEX_NAME, verified) { verifications++; verification }
      }
      AlertManager.shared.showInView()
    }
    test({ verifications }, copied)
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private fun ComposeUiTest.connect(name: String, cleanup: (() -> Unit)? = null) {
    runBlocking { planAndConnect(rhId = null, name, close = null, cleanup = cleanup, showLocalChats = { chats -> shownLocalChats += chats.map { it.id } }) }
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

  private fun ComposeUiTest.assertRequested(request: String) = assertRequested { it == request }

  private fun ComposeUiTest.assertRequested(expected: (String) -> Boolean) {
    runCatching { waitUntil(timeoutMillis = 5_000) { synchronized(unrecorded) { unrecorded.isNotEmpty() } } }
    synchronized(unrecorded) {
      assertEquals(1, unrecorded.size, unrecorded.toString())
      assertTrue(expected(unrecorded.single()), unrecorded.toString())
      unrecorded.clear()
    }
    waitUntil(timeoutMillis = 5_000) { AlertManager.shared.hasAlertsShown() }
  }

  private fun ComposeUiTest.shownTexts(): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }

  private fun assertLocalChats(vararg ids: String) {
    assertEquals(ids.toList(), shownLocalChats.toList())
    assertTrue(chatIds().containsAll(ids.toList()))
  }

  private fun chatIds(): List<String> = ChatModel.chatsContext.chats.value.map { it.id }

  private fun chatInfo(id: String): ChatInfo = ChatModel.chatsContext.chats.value.first { it.id == id }.chatInfo

  private companion object {
    const val SIMPLEX_NAME = "@alice.simplex"
  }
}

internal fun fixtureText(name: String): String = File("../../../tests/fixtures/api/$name.json").readText()
