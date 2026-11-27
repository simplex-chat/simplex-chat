package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.NotificationAction
import chat.simplex.common.platform.NtfManager
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.helpers.withBGApi
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

class ChatItemUpdateTest {
  private val sendCmd = chatSendCmdRetry
  private val notifications = mutableListOf<String>()

  @BeforeTest
  fun setUp() {
    ntfManager = recordingNtfManager { synchronized(notifications) { notifications += it } }
    simplexWindowState.windowFocused.value = false
    ChatModel.currentUser.value = User.sampleData
    ChatModel.users.add(UserInfo(User.sampleData, 0))
  }

  @AfterTest
  fun tearDown() {
    runBlocking { withBGApi {}.join() }
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatsContext.chatItems.value.clear()
    ChatModel.chatId.value = null
    ChatModel.currentUser.value = null
    ChatModel.users.clear()
    simplexWindowState.windowFocused.value = true
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun editReplacesLoadedItem() = recorded("updates/direct-edit") { (received, edited) ->
    listChat(received)
    openChat(received)
    receive(received)
    assertNotified("hello")
    assertEquals(listOf("hello"), loadedTexts())
    receive(edited)
    assertEquals(listOf("hello there"), loadedTexts())
    assertEquals("hello there", previewText())
    assertNotified("hello")
  }

  @Test
  fun updateOfItemNotLoadedInsertsNothing() = recorded("updates/direct-edit") { (received, edited, next, earlierEdited) ->
    listChat(received)
    receive(received)
    assertNotified("hello")
    openChat(received)
    receive(edited)
    assertEquals(listOf(), loadedTexts())
    assertEquals("hello there", previewText())
    receive(next)
    assertNotified("hello", "how are you")
    assertEquals(listOf("how are you"), loadedTexts())
    receive(earlierEdited)
    assertEquals(listOf("how are you"), loadedTexts())
    assertEquals("how are you", previewText())
  }

  @Test
  fun countersFollowPreviousAndNewItem() = recorded("updates/group-mention") { (received, mentioned, unmentioned) ->
    listChat(received)
    receive(received)
    assertEventually(Triple(1, 0, 1)) { counts() }
    receive(mentioned)
    assertEquals(Triple(1, 1, 1), counts())
    receive(unmentioned)
    assertEquals(Triple(1, 0, 1), counts())
  }

  @Test
  fun mentionEditsInAnotherProfileChangeItsUnreadCount() = recorded("updates/group-mention") { (received, mentioned, unmentioned) ->
    ChatModel.currentUser.value = User.sampleData.copy(userId = 2)
    receive(received)
    receive(mentioned)
    assertEquals(1, ChatModel.users[0].unreadCount)
    receive(unmentioned)
    assertEquals(0, ChatModel.users[0].unreadCount)
    assertNotified()
  }

  @Test
  fun updateOfChatNotListedAddsItAndNotifies() = recorded("updates/direct-edit") { (_, edited) ->
    receive(edited)
    assertEquals("hello there", previewText())
    assertNotified("hello there")
  }

  @Test
  fun recreatedItemIsCountedAndNotified() = recorded("updates/direct-recreated") { (recreated) ->
    listChat(recreated)
    receive(recreated)
    assertNotified("hello again")
    assertEquals("hello again", previewText())
    assertEquals(Triple(1, 0, 1), counts())
  }

  @Test
  fun editInAnotherProfileDoesNotNotify() = recorded("updates/direct-edit") { (received, edited) ->
    ChatModel.currentUser.value = User.sampleData.copy(userId = 2)
    receive(received)
    assertNotified("hello")
    receive(edited)
    assertNotified("hello")
    assertEquals(1, ChatModel.users[0].unreadCount)
  }

  @Test
  fun missedCallDoesNotNotify() = recorded("updates/missed-call") { (invited, missed) ->
    listChat(invited)
    receive(invited)
    assertEventually(Triple(1, 0, 1)) { counts() }
    receive(missed)
    assertEquals(CICallStatus.Missed, (ChatModel.chatsContext.chats.value.single().chatItems.last().content as CIContent.RcvCall).status)
    assertNotified()
  }

  @Test
  fun missedCallInAnotherProfileDoesNotNotify() = recorded("updates/missed-call") { (invited, missed) ->
    ChatModel.currentUser.value = User.sampleData.copy(userId = 2)
    receive(invited)
    assertEventually(1) { ChatModel.users[0].unreadCount }
    receive(missed)
    assertNotified()
    assertEquals(1, ChatModel.users[0].unreadCount)
  }

  @Test
  fun responseAndEventDecodeToTheirFields() = recorded("updates/direct-edit") { (_, edited) ->
    val response = Json.parseToJsonElement(fixtureText("updates/update-response")).jsonObject.getValue("commands").jsonObject.values.single().toString()
    val sent = mutableListOf<String>()
    chatSendCmdRetry = { _, cmd, _ -> sent += cmd; response }
    val updated = ChatController.apiUpdateChatItem(null, ChatType.Direct, 2, null, 8, UpdatedMessage(MsgContent.MCText("hello there"), mapOf()))
    assertTrue(sent.single().startsWith("/_update item @2 8 "))
    assertEquals(8L, updated?.chatItem?.id)
    assertEquals("hello there", updated?.chatItem?.text)
    val event = edited.result as CR.ChatItemUpdated
    assertEquals("hello there", event.chatItemUpdate?.chatItem?.text)
    assertEquals("hello", event.chatItemUpdate?.prevChatItem?.text)
    assertNull(event.chatItem)
  }

  @Test
  fun itemUpdatedAsNewIsLast() = recorded("updates/group-as-new") { (status, hi, updated) ->
    listChat(status)
    openChat(status)
    receive(status)
    assertNotified("status")
    receive(hi)
    assertNotified("status", "hi")
    assertEquals(listOf("status", "hi"), loadedTexts())
    receive(updated)
    assertEquals(listOf("hi", "status 2"), loadedTexts())
    assertEquals("status 2", previewText())
  }

  @Test
  fun itemNotLoadedIsInsertedLast() = recorded("updates/group-as-new") { (status, hi, updated) ->
    listChat(status)
    receive(status)
    assertNotified("status")
    openChat(status)
    receive(hi)
    assertNotified("status", "hi")
    assertEquals(listOf("hi"), loadedTexts())
    receive(updated)
    assertEquals(listOf("hi", "status 2"), loadedTexts())
  }

  @Test
  fun updateAsNewAlertsAsNewMessage() = recorded("updates/group-as-new") { events ->
    readItemUpdatedAsNew(events, Triple(2, 0, 2))
    assertNotified("status", "hi", "status 2")
  }

  @Test
  fun silentUpdateAsNewAlertsNobody() = recorded("updates/group-as-new-silent") { events ->
    readItemUpdatedAsNew(events, Triple(2, 0, 1))
    assertNotified("status", "hi")
  }

  @Test
  fun updateAsNewInAnotherProfileAlertsAndCounts() = recorded("updates/group-as-new") { (status, hi, updated) ->
    ChatModel.currentUser.value = User.sampleData.copy(userId = 2)
    receive(status)
    assertNotified("status")
    receive(hi)
    assertNotified("status", "hi")
    receive(updated)
    assertNotified("status", "hi", "status 2")
    assertEquals(3, ChatModel.users[0].unreadCount)
  }

  private suspend fun readItemUpdatedAsNew(events: List<API>, expected: Triple<Int, Int, Int>) {
    val (status, hi, updated) = events
    listChat(status)
    openChat(status)
    receive(status)
    assertNotified("status")
    receive(hi)
    assertNotified("status", "hi")
    assertEquals(Triple(2, 0, 2), counts())
    withContext(Dispatchers.Main) {
      ChatModel.chatsContext.markChatItemsRead(null, chatInfo(status).id, listOf(newItem(status).id))
    }
    assertEquals(Triple(1, 0, 1), counts())
    receive(updated)
    assertEquals(expected, counts())
  }

  private suspend fun listChat(event: API) = withContext(Dispatchers.Main) {
    ChatModel.chatsContext.updateChat(null, chatInfo(event))
  }

  private fun openChat(event: API) {
    ChatModel.chatId.value = chatInfo(event).id
  }

  private suspend fun receive(event: API) = ChatController.processReceivedMsg(event)

  private fun chatInfo(event: API): ChatInfo = (event.result as CR.NewChatItems).chatItems.single().chatInfo

  private fun newItem(event: API): ChatItem = (event.result as CR.NewChatItems).chatItems.single().chatItem

  private fun loadedTexts(): List<String> = ChatModel.chatsContext.chatItems.value.map { it.text }

  private fun previewText(): String? = ChatModel.chatsContext.chats.value.single().chatItems.lastOrNull()?.text

  private fun counts(): Triple<Int, Int, Int> {
    val stats = ChatModel.chatsContext.chats.value.single().chatStats
    return Triple(stats.unreadCount, stats.unreadMentions, ChatModel.users[0].unreadCount)
  }

  private fun assertNotified(vararg texts: String) = assertEventually(texts.toList()) { synchronized(notifications) { notifications.toList() } }
}

internal fun recorded(fixture: String, test: suspend (List<API>) -> Unit) = runBlocking {
  test(json.decodeFromString(fixtureText(fixture)))
}

internal fun recordingNtfManager(record: (String) -> Unit): NtfManager = object : NtfManager() {
  override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) = record(msgText)
  override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = false
  override fun hasNotificationsForChat(chatId: String): Boolean = false
  override fun cancelNotificationsForChat(chatId: String) {}
  override fun cancelNotificationsForUser(userId: Long) {}
  override fun cancelCallNotification() {}
  override fun cancelAllNotifications() {}
  override fun showMessage(title: String, text: String) {}
  override fun androidCreateNtfChannelsMaybeShowAlert() {}
}

internal fun <T> assertEventually(expected: T, actual: () -> T) {
  val until = System.currentTimeMillis() + 5_000
  while (actual() != expected && System.currentTimeMillis() < until) Thread.sleep(10)
  assertEquals(expected, actual())
}
