package chat.simplex.app

import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.chat.item.itemDisplay
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.generalGetString
import chat.simplex.common.views.helpers.parseToMarkdown
import chat.simplex.common.views.newchat.MsgLinkAction
import chat.simplex.common.views.newchat.msgLinkAction
import chat.simplex.common.views.newchat.planAndConnect
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.util.Locale
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class MessageLinksTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val locale = Locale.getDefault()
  private val unrecorded = mutableListOf<String>()

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    ChatModel.currentUser.value = User.sampleData
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    AlertManager.privacySensitive.hideAllAlerts()
    ChatModel.currentUser.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    Locale.setDefault(locale)
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun messageLinkIsParsed() = recorded("links/same-group") {
    val link = parseToMarkdown("see [q](GROUP_MSG_LINK)")?.last()?.format as Format.MessageLink
    assertEquals(Triple("q", MsgLinkType.group, "GROUP_MSG_LINK"), Triple(link.showText, link.msgLinkType, link.msgLink))
  }

  @Test
  fun receivedLinkHasShortText() = recorded("links/same-group") {
    assertEquals(mapOf("q" to CILink("Approve the deploy plan?")), lastItem(groupId = 1).links)
  }

  @Test
  fun messageLinkIsShownWithShortText() = recorded("links/same-group") {
    assertEquals(listOf("see ", "q (Approve the deploy plan?)"), itemDisplay(lastItem(groupId = 1), SimplexLinkMode.DESCRIPTION)?.parts?.map { it.first })
  }

  @Test
  fun linkToMessageOfChatOpensIt() = recorded("links/same-group") {
    val plan = ChatController.apiMsgLinkPlan(null, "GROUP_MSG_LINK", lastChat(groupId = 1).chatInfo) as MsgLinkPlan.Item
    assertEquals("Approve the deploy plan?", plan.linkText)
    assertEquals(CILinkTarget(ChatType.Group, 1, plan.chatItem.chatItem.id), (msgLinkAction(plan, null) as MsgLinkAction.OpenItem).target)
  }

  @Test
  fun linkToMessageGoneFromKnownChatOpensChat() = recorded("links/same-group") {
    val chatInfo = lastChat(groupId = 1).chatInfo
    assertEquals(chatInfo, (msgLinkAction(MsgLinkPlan.Chat(chatInfo), null) as MsgLinkAction.OpenChat).chatInfo)
  }

  @Test
  fun linkFromUnknownSourceShowsAlert() = recorded("links/same-group") {
    assertEquals("This link is from an unknown source.", alertTitle(plan("simplex:/m"), null))
  }

  @Test
  fun linkToMessageNotInChatsShowsAlert() = recorded("links/same-group") {
    assertEquals("This message is not in your chats.", alertTitle(plan("simplex:/m#g/YWJjZGVmZ2hpamtsbW5vcA?a=MTIzNDU2Nzg5MDEyMzQ1Ng"), null))
  }

  @Test
  fun linkOfUnknownKindAsksForUpdate() = recorded("links/same-group") {
    assertEquals("Update the app to open this link.", alertTitle(plan("simplex:/m#x/abc"), null))
  }

  @Test
  fun sendersLinkToOtherChatOpensTarget() = recorded("links/other-group-sender") {
    val target = assertNotNull(lastItem(groupId = 2).links?.get("q")?.target)
    assertEquals(ChatType.Group to 1L, target.chatType to target.chatId)
    assertEquals(target, (msgLinkAction(MsgLinkPlan.UnknownSource, target) as MsgLinkAction.OpenItem).target)
  }

  @Test
  fun recipientsLinkToOtherChatHasNoTarget() = recorded("links/other-group-recipient") {
    assertEquals(mapOf("q" to CILink("Approve the deploy plan?")), lastItem(groupId = 2).links)
  }

  @Test
  fun postOfChannelNotJoinedConnects() = recorded("links/channel") {
    assertEquals("CHANNEL_LINK", (msgLinkAction(plan("CHANNEL_MSG_LINK"), null) as MsgLinkAction.Connect).connTarget)
  }

  @Test
  fun pastedPostOfChannelNotJoinedStartsConnectionToChannel() = runComposeUiTest {
    useRecording("links/channel")
    setContent {
      AlertManager.shared.showInView()
      AlertManager.privacySensitive.showInView()
    }
    runBlocking { planAndConnect(rhId = null, "CHANNEL_MSG_LINK", close = null) }
    val texts = listOf("team", "Open channel")
    runCatching { waitUntil(timeoutMillis = 5_000) { shownTexts().containsAll(texts) } }
    assertTrue(shownTexts().containsAll(texts), "${shownTexts()}")
  }

  @Test
  fun linkToPostOfPublicChatIsSimplexLink() = recorded("links/channel") {
    assertEquals(true, parseToMarkdown("CHANNEL_MSG_LINK")?.single()?.format?.isSimplexLink)
  }

  @Test
  fun linkToMessageOfGroupIsNotSimplexLink() = recorded("links/same-group") {
    assertEquals(false, parseToMarkdown("see [q](GROUP_MSG_LINK)")?.last()?.format?.isSimplexLink)
  }

  @Test
  fun siteLinkAsksForUpdate() = runComposeUiTest {
    useRecording("links/channel")
    setContent {
      AlertManager.shared.showInView()
      AlertManager.privacySensitive.showInView()
    }
    runBlocking { planAndConnect(rhId = null, "SITE_LINK", close = null) }
    waitForIdle()
    assertTexts("App update required", "This site requires a newer version of the app. Please update the app to open it.", "OK")
    synchronized(unrecorded) {
      assertEquals(listOf("markdown SITE_LINK", "markdown SITE_LINK"), unrecorded.toList())
      unrecorded.clear()
    }
  }

  private fun recorded(fixture: String, test: suspend () -> Unit) = runBlocking {
    useRecording(fixture)
    test()
  }

  private fun useRecording(fixture: String) {
    val responses = Json.parseToJsonElement(fixtureText(fixture)).jsonObject
    val commands = responses["commands"]?.jsonObject
    val markdown = responses["markdown"]?.jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands?.get(cmd)?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown?.get(str)?.toString() ?: unrecordedRequest("markdown $str") }
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private suspend fun lastChat(groupId: Long): Chat =
    assertNotNull(ChatController.apiGetChat(null, ChatType.Group, groupId, null, pagination = ChatPagination.Last(1))).first

  private suspend fun lastItem(groupId: Long): ChatItem = lastChat(groupId).chatItems.last()

  private suspend fun plan(link: String): MsgLinkPlan = assertNotNull(ChatController.apiMsgLinkPlan(null, link, null))

  private fun alertTitle(plan: MsgLinkPlan, target: CILinkTarget?): String = generalGetString((msgLinkAction(plan, target) as MsgLinkAction.Alert).title)

  private fun ComposeUiTest.assertTexts(vararg texts: String) {
    runCatching { waitUntil(timeoutMillis = 5_000) { shownTexts() == texts.toList() } }
    assertEquals(texts.toList(), shownTexts())
  }

  private fun ComposeUiTest.shownTexts(): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }
}
