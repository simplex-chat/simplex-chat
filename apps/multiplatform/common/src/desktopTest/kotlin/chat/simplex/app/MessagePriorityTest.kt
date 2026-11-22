package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.simplexWindowState
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

class MessagePriorityTest {
  private val sendCmd = chatSendCmdRetry
  private val notifications = mutableListOf<String>()
  private val tag = 1L

  private data class Counts(val unread: Int, val mentions: Int, val silent: Int, val highPriority: Int, val user: Int)

  private data class Labels(val unread: Int, val silent: Int, val priority: Int)

  @BeforeTest
  fun setUp() {
    ntfManager = recordingNtfManager { synchronized(notifications) { notifications += it } }
    simplexWindowState.windowFocused.value = false
    ChatModel.currentUser.value = User.sampleData
    ChatModel.users.add(UserInfo(User.sampleData, 0))
  }

  @AfterTest
  fun tearDown() {
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatsContext.chatItems.value.clear()
    ChatModel.chatId.value = null
    ChatModel.currentUser.value = null
    ChatModel.users.clear()
    ChatModel.unreadTags.clear()
    ChatModel.silentTags.clear()
    ChatModel.priorityTags.clear()
    simplexWindowState.windowFocused.value = true
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun silentMessagesDoNotNotifyAndAreLeftOutOfTotals() = recorded("priority/silent") { (working, testing) ->
    listChat(working)
    receive(working)
    receive(testing)
    assertEventually(Counts(2, 0, 2, 0, 0)) { counts() }
    assertNotified()
    val chat = ChatModel.chatsContext.chats.value.single()
    assertEquals(UnreadTag.Silent, chat.unreadTag)
    assertTrue(chat.chatStats.allUnreadSilent)
    assertEquals(Labels(0, 1, 0), labels())
  }

  @Test
  fun chatListWithOnlySilentUnread() = runBlocking {
    val responses = Json.parseToJsonElement(fixtureText("priority/chats-silent")).jsonObject.getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> responses.getValue(cmd).toString() }
    val chats = ChatController.apiGetChats(null)
    val users = ChatController.listUsers(null)
    withContext(Dispatchers.Main) {
      ChatModel.chatsContext.updateChats(chats)
      ChatModel.updateChatTags(null)
    }
    val group = chats.single { it.chatInfo is ChatInfo.Group }
    assertEquals(2 to 2, group.chatStats.unreadCount to group.chatStats.unreadSilent)
    assertEquals(UnreadTag.Silent, group.unreadTag)
    assertTrue(group.chatStats.allUnreadSilent)
    assertFalse(chats.single { it.chatInfo is ChatInfo.Direct }.chatStats.allUnreadSilent)
    assertEquals(Labels(0, 1, 0), labels())
    assertEquals(1, users.single().unreadCount)
  }

  @Test
  fun highPriorityMessageNotifiesWithBell() = recorded("priority/high") { (hello, done) ->
    listChat(hello)
    receive(hello)
    receive(done)
    assertEventually(Counts(2, 1, 0, 1, 2)) { counts() }
    assertNotified("hello", "done")
    val chat = ChatModel.chatsContext.chats.value.single()
    assertEquals(UnreadTag.HighPriority, chat.unreadTag)
    assertTrue(chat.chatItems.last().meta.highPriority)
    assertEquals(Labels(1, 0, 1), labels())
  }

  @Test
  fun raiseNotifiesAndMovesCounters() = recorded("priority/raise") { (almost, almostRaised, compiling, compiled) ->
    listChat(almost)
    receive(almost)
    assertEventually(Counts(1, 0, 0, 0, 1)) { counts() }
    receive(almostRaised)
    assertEquals(Counts(1, 1, 0, 1, 1), counts())
    assertNotified("almost there", "almost there")
    receive(compiling)
    assertEventually(Counts(2, 1, 1, 1, 1)) { counts() }
    receive(compiled)
    assertEquals(Counts(2, 2, 0, 2, 2), counts())
    assertNotified("almost there", "almost there", "compiled")
    assertEquals(Labels(1, 0, 1), labels())
  }

  @Test
  fun lowerPriorityMovesCountersWithoutNotification() = recorded("priority/lower") { (answer, answerEdited, alert, alertSilent) ->
    listChat(answer)
    receive(answer)
    assertEventually(Counts(1, 1, 0, 1, 1)) { counts() }
    receive(answerEdited)
    assertEquals(Counts(1, 0, 0, 0, 1), counts())
    receive(alert)
    assertEventually(Counts(2, 1, 0, 1, 2)) { counts() }
    receive(alertSilent)
    assertEquals(Counts(2, 0, 1, 0, 1), counts())
    assertNotified("answer", "alert")
  }

  @Test
  fun raiseInAnotherProfileNotifiesAndCountsForIt() = recorded("priority/raise") { (almost, almostRaised, compiling, compiled) ->
    ChatModel.currentUser.value = User.sampleData.copy(userId = 2)
    receive(almost)
    assertEventually(1) { ChatModel.users[0].unreadCount }
    receive(almostRaised)
    receive(compiling)
    receive(compiled)
    assertEventually(2) { ChatModel.users[0].unreadCount }
    assertNotified("almost there", "almost there", "compiled")
  }

  private suspend fun listChat(event: API) = withContext(Dispatchers.Main) {
    ChatModel.chatsContext.updateChat(null, (event.result as CR.NewChatItems).chatItems.single().chatInfo)
  }

  private suspend fun receive(event: API) = ChatController.processReceivedMsg(event)

  private fun counts(): Counts {
    val stats = ChatModel.chatsContext.chats.value.single().chatStats
    return Counts(stats.unreadCount, stats.unreadMentions, stats.unreadSilent, stats.unreadHighPriority, ChatModel.users[0].unreadCount)
  }

  private fun labels(): Labels = Labels(ChatModel.unreadTags[tag] ?: 0, ChatModel.silentTags[tag] ?: 0, ChatModel.priorityTags[tag] ?: 0)

  private fun assertNotified(vararg texts: String) = assertEventually(texts.toList()) { synchronized(notifications) { notifications.toList() } }
}
