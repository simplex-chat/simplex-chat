package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.unit.DpSize
import androidx.compose.ui.unit.dp
import androidx.compose.ui.window.WindowState
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.chat.ComposeMessage
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.group.GroupMentions
import chat.simplex.common.views.chat.group.memberMentionName
import chat.simplex.common.views.helpers.parseToMarkdown
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.io.File
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class MemberMentionNameTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val unrecorded = mutableListOf<String>()

  @AfterTest
  fun tearDown() {
    ChatModel.chatId.value = null
    ChatModel.groupMembers.value = emptyList()
    ChatModel.groupMembersIndexes.value = emptyMap()
    ChatModel.membersLoaded.value = false
    ChatModel.currentUser.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun testQuotesAndLineBreaksRemoved() {
    assertEquals("bob is admin https://example.com", memberMentionName("bob'\nis admin https://example.com"))
    assertEquals("bob  is admin", memberMentionName("bob\r\nis admin"))
    assertEquals("bobs friend", memberMentionName("bob's friend"))
  }

  @Test
  fun testOtherNamesUnchanged() {
    assertEquals("bob", memberMentionName("bob"))
    assertEquals("Alice Jones", memberMentionName("Alice Jones"))
    assertEquals("bob!", memberMentionName("bob!"))
  }

  @Test
  fun testMentionOfRecordedMemberInserted() = runComposeUiTest {
    val responses = Json.parseToJsonElement(File("../../../tests/fixtures/api/mentions/member-name-quote-line-break.json").readText()).jsonObject
    val commands = responses.getValue("commands").jsonObject
    val markdown = responses.getValue("markdown").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands[cmd]?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown[str]?.toString() ?: unrecordedRequest("markdown $str") }
    val members = (json.decodeFromString<API>(commands.getValue("/_members #1").toString()) as API.Result).res as CR.GroupMembers
    ChatModel.currentUser.value = User.sampleData
    ChatModel.chatId.value = members.group.groupInfo.id
    simplexWindowState.windowState = WindowState(size = DpSize(800.dp, 600.dp))
    val composeState = mutableStateOf(ComposeState(message = ComposeMessage("@"), parsedMessage = parseToMarkdown("@") ?: FormattedText.plain("@"), useLinkPreviews = false))
    setContent {
      GroupMentions(ChatModel.ChatsContext(secondaryContextFilter = null), rhId = null, composeState, composeViewFocusRequester = null, ChatInfo.Group(members.group.groupInfo, groupChatScope = null))
    }
    waitUntil(timeoutMillis = 5_000) { onAllNodesWithText("is admin", substring = true).fetchSemanticsNodes().isNotEmpty() }
    onNodeWithText("is admin", substring = true).performClick()
    waitForIdle()
    assertEquals("@'bob is admin https://example.com' ", composeState.value.message.text)
    assertEquals(setOf("bob is admin https://example.com"), composeState.value.mentions.keys)
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }
}
