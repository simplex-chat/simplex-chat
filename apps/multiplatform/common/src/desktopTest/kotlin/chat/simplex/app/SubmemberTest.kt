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
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.views.chat.group.GroupMemberInfoLayoutPreview
import chat.simplex.common.views.chat.group.MemberRow
import chat.simplex.common.views.chat.item.FramedItemView
import chat.simplex.common.views.chat.item.TerminalChatItemRow
import chat.simplex.common.views.chat.item.chatItemSender
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class SubmemberTest {
  private val sendCmd = chatSendCmdRetry
  private val ntf = ntfManager

  @AfterTest
  fun tearDown() {
    runBlocking { withBGApi {}.join() }
    ChatModel.groupMembers.value = emptyList()
    ChatModel.groupMembersIndexes.value = emptyMap()
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatsContext.chatItems.value.clear()
    ChatModel.chatId.value = null
    ChatModel.currentUser.value = null
    ChatModel.users.clear()
    ntfManager = ntf
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun itemOfSubmemberIsMarkedDeletedWithItsViaMember() {
    receiveRemovalOfViaMember()
    val hi = ChatModel.chatsContext.chatItems.value.single { it.text == "hi" }
    assertEquals("cath", (hi.meta.itemDeleted as? CIDeleted.Moderated)?.byGroupMember?.displayName)
  }

  @Test
  fun submemberIsRemovedWithItsViaMember() {
    val reviewer = receiveRemovalOfViaMember()
    assertEquals(GroupMemberStatus.MemRemoved, ChatModel.groupMembers.value.single { it.groupMemberId == reviewer.groupMemberId }.memberStatus)
  }

  @Test
  fun submemberIsRemovedWhenUserLeaves() = runBlocking {
    replay("submembers/left")
    ChatModel.chatId.value = "#1"
    ChatModel.groupMembers.value = ChatController.apiListMembers(null, 1).orEmpty()
    ChatModel.populateGroupMembersIndexes()
    val reviewer = ChatModel.groupMembers.value.single { it.displayName == "reviewer" }
    ChatController.leaveGroup(null, 1)
    assertEquals(GroupMemberStatus.MemRemoved, ChatModel.getGroupMember(reviewer.groupMemberId)?.memberStatus)
  }

  @Test
  fun submemberIsRemovedWhenUserIsRemoved() = recorded("submembers/user-removed") { events ->
    val reaction = (events.first().result as CR.ChatItemReaction).reaction
    val groupInfo = (reaction.chatInfo as ChatInfo.Group).groupInfo
    val reviewer = assertNotNull(reaction.chatReaction.chatItem.meta.sentAsMember)
    ChatModel.currentUser.value = User.sampleData
    ChatModel.users.add(UserInfo(User.sampleData, 0))
    withContext(Dispatchers.Main) {
      ChatModel.chatId.value = groupInfo.id
      ChatModel.chatsContext.upsertGroupMember(null, groupInfo, reviewer)
    }
    ChatController.processReceivedMsg(events.last())
    assertEquals(GroupMemberStatus.MemRemoved, ChatModel.getGroupMember(reviewer.groupMemberId)?.memberStatus)
  }

  @Test
  fun userSubmemberIsDecoded() = runBlocking {
    replay("submembers/members")
    val reviewer = ChatController.apiListMembers(null, 1).orEmpty().single { it.memberProfile.displayName == "reviewer" }
    assertEquals("memberVia", reviewer.memberVia)
  }

  @Test
  fun itemSentAsMemberIsDecoded() = runBlocking {
    replay("submembers/members")
    val (chat, _) = assertNotNull(ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(1)))
    assertEquals("reviewer", chat.chatItems.single().meta.sentAsMember?.memberProfile?.displayName)
  }

  @Test
  fun headerAndQuoteOfSubmemberNameMemberItIsVia() = runComposeUiTest {
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
  fun memberListAndMemberInfoNameMemberItIsVia() = runComposeUiTest {
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
  fun memberInfoOfSubmemberOffersNoChatCallsOrCode() = runComposeUiTest {
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

  private fun receiveRemovalOfViaMember(): GroupMember {
    lateinit var reviewer: GroupMember
    recorded("submembers/member-removed-with-messages") { (hi, _, deleted) ->
      val aci = (hi.result as CR.NewChatItems).chatItems.single()
      val groupInfo = (aci.chatInfo as ChatInfo.Group).groupInfo
      reviewer = (aci.chatItem.chatDir as CIDirection.GroupRcv).groupMember
      ntfManager = recordingNtfManager {}
      ChatModel.currentUser.value = User.sampleData
      ChatModel.users.add(UserInfo(User.sampleData, 0))
      withContext(Dispatchers.Main) {
        ChatModel.chatsContext.updateChat(null, aci.chatInfo)
        ChatModel.chatId.value = aci.chatInfo.id
        ChatModel.chatsContext.upsertGroupMember(null, groupInfo, reviewer)
      }
      ChatController.processReceivedMsg(hi)
      assertEventually(listOf("hi")) { ChatModel.chatsContext.chatItems.value.map { it.text } }
      ChatController.processReceivedMsg(deleted)
    }
    return reviewer
  }

  private fun receivedChat(): Chat = runBlocking {
    replay("submembers/received")
    ChatModel.groupMembers.value = ChatController.apiListMembers(null, 1).orEmpty()
    assertNotNull(ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(2))).first
  }

  private fun replay(fixture: String) {
    val responses = Json.parseToJsonElement(fixtureText(fixture)).jsonObject.getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> responses.getValue(cmd).toString() }
  }
}
