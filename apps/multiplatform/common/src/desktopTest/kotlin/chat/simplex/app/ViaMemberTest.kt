package chat.simplex.app

import androidx.compose.foundation.layout.Column
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.assertCountEquals
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.unit.dp
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.chat.group.GroupMemberInfoLayoutPreview
import chat.simplex.common.views.chat.group.MemberRow
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.chat.item.TerminalChatItemRow
import chat.simplex.common.views.chat.item.chatItemSender
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class ViaMemberTest {
  private val sendCmd = chatSendCmdRetry

  @AfterTest
  fun tearDown() {
    ChatModel.groupMembers.value = emptyList()
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun memberViaUserIsDecoded() = runBlocking {
    replay("via/members")
    val reviewer = ChatController.apiListMembers(null, 1).single { it.memberProfile.displayName == "reviewer" }
    assertEquals("memberVia", reviewer.memberVia)
  }

  @Test
  fun itemSentAsMemberIsDecoded() = runBlocking {
    replay("via/members")
    val (chat, _) = assertNotNull(ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(1)))
    assertEquals("reviewer", chat.chatItems.single().meta.sentAsMember?.memberProfile?.displayName)
  }

  @Test
  fun headerAndQuoteOfViaMemberNameHost() = runComposeUiTest {
    val chat = receivedChat()
    val (hi, reply) = chat.chatItems
    setContent {
      Column {
        TerminalChatItemRow(chatItemSender(chat.chatInfo, hi, remember { mutableStateOf(null) }, remember { mutableStateOf(chat.chatItems.asReversed()) }, { _, _ -> }, {}), 0.dp, 0.dp) {}
        FramedItemView(ChatModel.ChatsContext(null), chat, reply, linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false, showMenu = remember { mutableStateOf(false) }, showTimestamp = true, receiveFile = {}, scrollToItemId = remember { mutableStateOf(null) })
      }
    }
    onAllNodesWithText(" via alice", useUnmergedTree = true).assertCountEquals(2)
  }

  @Test
  fun memberListAndMemberInfoNameHost() = runComposeUiTest {
    receivedChat()
    val reviewer = ChatModel.groupMembers.value.single { it.displayName == "reviewer" }
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        Column {
          MemberRow(reviewer)
          GroupMemberInfoLayoutPreview(reviewer, connectionCode = null)
        }
      }
    }
    onNodeWithText("via alice", useUnmergedTree = true).assertExists()
    onNodeWithText("Via", useUnmergedTree = true).assertExists()
    onNodeWithText("alice", useUnmergedTree = true).assertExists()
  }

  @Test
  fun memberInfoOfViaMemberOffersNoChatCallsOrCode() = runComposeUiTest {
    val groupInfo = (receivedChat().chatInfo as ChatInfo.Group).groupInfo
    val reviewer = ChatModel.groupMembers.value.single { it.displayName == "reviewer" }
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        GroupMemberInfoLayoutPreview(reviewer, connectionCode = null, groupInfo = groupInfo, connectionLoaded = false)
      }
    }
    val offered = listOf("Message", "Call", "Video", "Chat with member", "Verify security code")
    assertEquals(emptyList(), offered.filter { onAllNodesWithText(it).fetchSemanticsNodes().isNotEmpty() })
  }

  private fun receivedChat(): Chat = runBlocking {
    replay("via/received")
    ChatModel.groupMembers.value = ChatController.apiListMembers(null, 1)
    assertNotNull(ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(2))).first
  }

  private fun replay(fixture: String) {
    val responses = Json.parseToJsonElement(fixtureText(fixture)).jsonObject.getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> responses.getValue(cmd).toString() }
  }
}
