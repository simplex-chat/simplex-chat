package chat.simplex.app

import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.runtime.CompositionLocalProvider
import chat.simplex.common.model.*
import chat.simplex.common.platform.NotificationAction
import chat.simplex.common.platform.NtfManager
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.simplexWindowState
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.chat.apiLoadMessages
import chat.simplex.common.views.chatlist.ActiveFilter
import chat.simplex.common.views.chatlist.ChatListView
import chat.simplex.common.views.chatlist.ChatPreviewView
import chat.simplex.common.views.chatlist.markChatRead
import chat.simplex.common.views.helpers.AnimatedViewState
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class UnreadCountsTest {
  private val sendCmd = chatSendCmdRetry
  private val notifications = mutableListOf<String>()

  @BeforeTest
  fun setUp() {
    ntfManager = object : NtfManager() {
      override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) {
        synchronized(notifications) { notifications += msgText }
      }
      override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = false
      override fun hasNotificationsForChat(chatId: String): Boolean = false
      override fun cancelNotificationsForChat(chatId: String) {}
      override fun cancelNotificationsForUser(userId: Long) {}
      override fun cancelCallNotification() {}
      override fun cancelAllNotifications() {}
      override fun showMessage(title: String, text: String) {}
      override fun androidCreateNtfChannelsMaybeShowAlert() {}
    }
    simplexWindowState.windowFocused.value = false
    ChatModel.currentUser.value = User.sampleData
    ChatModel.users.add(UserInfo(User.sampleData, 0))
  }

  @AfterTest
  fun tearDown() {
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatsContext.chatItems.value.clear()
    ChatModel.secondaryChatsContext.value = null
    ChatModel.chatId.value = null
    ChatModel.activeChatTagFilter.value = null
    ChatModel.chatRunning.value = null
    ChatModel.currentUser.value = null
    ChatModel.users.clear()
    simplexWindowState.windowFocused.value = true
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun statusOfItemNotLoadedInsertsNothing() = recorded("counts/direct-statuses") { (sent, received) ->
    val sentItem = items(sent).single()
    listChat(sent, preview = sentItem.chatItem)
    openChat(sent)
    receive(sent)
    receive(received)
    assertEquals(listOf(), loadedTexts())
    assertIs<CIStatus.SndRcvd>(ChatModel.chatsContext.chats.value.single().chatItems.single().meta.itemStatus)
  }

  @Test
  fun deletionMarkOfItemNotLoadedInsertsNothing() = recorded("counts/group-marked") { (mention, message, marked) ->
    listChat(mention)
    receive(mention)
    receive(message)
    openChat(mention)
    receive(marked)
    assertEquals(listOf(), loadedTexts())
    assertEquals(Triple(2, 1, 2), counts())
  }

  @Test
  fun removedUnreadMentionLowersBothCounters() = recorded("counts/group-marked") { (mention, message, marked) ->
    val (removed) = events("counts/group-removed")
    listChat(mention)
    receive(mention)
    receive(message)
    receive(marked)
    assertEquals(Triple(2, 1, 2), counts())
    receive(removed)
    assertEquals(Triple(1, 0, 1), counts())
  }

  @Test
  fun newItemAndItsRemovalAreAppliedInOrder() = recorded("counts/group-marked") { (mention) ->
    val (removed) = events("counts/group-removed")
    listChat(mention)
    openChat(mention)
    receive(mention)
    receive(removed)
    assertEquals(listOf(), loadedTexts())
    assertEquals(Triple(0, 0, 0), counts())
  }

  @Test
  fun profileCountFollowsMentionsOnlyGroup() = recorded("updates/group-mention") { (received, mentioned, unmentioned) ->
    listChat(received)
    receive(received)
    assertEquals(Triple(1, 0, 0), counts())
    receive(mentioned)
    assertEquals(Triple(1, 1, 1), counts())
    receive(unmentioned)
    assertEquals(Triple(1, 0, 0), counts())
  }

  @Test
  fun profileCountFollowsMutedChat() = recorded("counts/direct-muted") { (hello, howAreYou) ->
    listChat(hello)
    receive(hello)
    receive(howAreYou)
    assertEquals(Triple(2, 0, 0), counts())
    val contact = (chatInfo(hello) as ChatInfo.Direct).contact
    runBlocking {
      withContext(Dispatchers.Main) {
        ChatModel.chatsContext.updateContact(null, contact.copy(chatSettings = contact.chatSettings.copy(enableNtfs = MsgFilter.All)))
      }
    }
    assertEquals(Triple(2, 0, 2), counts())
  }

  @Test
  fun chatOutsideListCountsNewMessage() = recorded("counts/direct-muted") { (hello) ->
    receive(hello)
    assertEquals("hello", previewText())
    assertEquals(Triple(1, 0, 0), counts())
  }

  @Test
  fun openingChatKeepsRowCount() = recorded("counts/direct-muted") { (hello, howAreYou) ->
    listChat(hello)
    receive(hello)
    receive(howAreYou)
    respond("counts/chat-page")
    runBlocking { apiLoadMessages(ChatModel.chatsContext, null, ChatType.Direct, chatInfo(hello).apiId, ChatPagination.Initial(ChatPagination.INITIAL_COUNT)) }
    assertEquals(listOf("hello"), loadedTexts().takeLast(1))
    assertEquals(Triple(2, 0, 0), counts())
  }

  @Test
  fun pendingMemberRowCountsAdminMessages() = recorded("counts/pending-support") { (received, edited, next, deleted) ->
    listChat(received)
    receive(received)
    assertEquals("5", previewText())
    assertEquals(1 to 0, rowCounts())
    receive(edited)
    assertEquals("@cath 5", previewText())
    assertEquals(1 to 1, rowCounts())
    receive(next)
    assertEquals(2 to 1, rowCounts())
    receive(deleted)
    assertEquals(1 to 1, rowCounts())
    assertNotEquals("6", previewText())
    assertEquals(1, ChatModel.activeUserUnreadCount)
    respond("counts/pending-read")
    markChatRead(ChatModel.chatsContext.chats.value.single())
    assertEventually(0 to 0) { rowCounts() }
    assertEquals(0, ChatModel.activeUserUnreadCount)
  }

  @Test
  fun pendingMemberRowShowsBadge() = runComposeUiTest {
    val (received) = events("counts/pending-support")
    listChat(received)
    runBlocking { receive(received) }
    setContent {
      ChatPreviewView(ChatModel.chatsContext.chats.value.single(), true, null, null, null, false, SimplexLinkMode.DESCRIPTION, false, false) {}
    }
    onNodeWithText("1").assertExists()
  }

  @Test
  fun moderatorSupportMessageListedByUnreadFilter() = runComposeUiTest {
    val (support) = events("counts/moderator-support")
    listChat(support)
    runBlocking { receive(support) }
    val chat = ChatModel.chatsContext.chats.value.single()
    assertEquals(0 to 0, rowCounts())
    assertEquals(0, ChatModel.activeUserUnreadCount)
    assertTrue(chat.unreadTag)
    ChatModel.chatRunning.value = true
    ChatModel.activeChatTagFilter.value = ActiveFilter.Unread
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        ChatListView(ChatModel, MutableStateFlow(AnimatedViewState.GONE), {}, false)
      }
    }
    waitUntil(timeoutMillis = 5_000) { onAllNodesWithText(chat.chatInfo.displayName).fetchSemanticsNodes().isNotEmpty() }
  }

  @Test
  fun supportChatContextKeepsItsOwnChats() = recorded("counts/moderator-support") { (support, main) ->
    listChat(main)
    runBlocking {
      withContext(Dispatchers.Main) {
        ChatModel.chatsContext.updateChat(null, ChatInfo.Direct(Contact.sampleData.copy(contactId = 2)))
      }
    }
    val scope = (chatInfo(support) as ChatInfo.Group).groupChatScope!!
    val supportCtx = ChatModel.ChatsContext(SecondaryContextFilter.GroupChatScopeContext(scope))
    ChatModel.secondaryChatsContext.value = supportCtx
    openChat(support)
    receive(support)
    assertEquals(listOf("hi"), supportCtx.chatItems.value.map { it.text })
    assertEquals(1, supportCtx.chatState.unreadTotal.value)
    runBlocking { supportCtx.popChatCollector.throttlePopChat(null, chatInfo(support).id, currentPosition = 1) }
    Thread.sleep(2_500)
    assertEquals(listOf(chatInfo(support).id), supportCtx.chats.value.map { it.id })
  }

  @Test
  fun readingSupportChatKeepsGroupRow() = recorded("counts/moderator-support") { (support, main) ->
    listChat(main)
    receive(support)
    receive(main)
    assertEquals(1 to 0, rowCounts())
    respond("counts/moderator-read")
    val scope = (chatInfo(support) as ChatInfo.Group).groupChatScope!!.toChatScope()
    val itemId = items(support).single().chatItem.id
    val updated = runBlocking { ChatController.apiChatItemsRead(null, ChatType.Group, chatInfo(support).apiId, scope, listOf(itemId)) }
    assertNotNull(updated)
    runBlocking { withContext(Dispatchers.Main) { ChatModel.chatsContext.updateChatInfo(null, updated) } }
    assertEquals(1 to 0, rowCounts())
    assertFalse(ChatModel.chatsContext.chats.value.single().supportUnreadCount > 0)
  }

  private fun recorded(fixture: String, test: suspend (List<API>) -> Unit) = runBlocking {
    test(events(fixture))
  }

  private fun events(fixture: String): List<API> = json.decodeFromString(fixtureText(fixture))

  private fun respond(fixture: String) {
    val commands = Json.parseToJsonElement(fixtureText(fixture)).jsonObject.getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands[cmd]?.toString() ?: error("no recorded response: $cmd") }
  }

  private fun listChat(event: API, preview: ChatItem? = null) = runBlocking {
    val cInfo = when (val c = chatInfo(event)) {
      is ChatInfo.Group -> c.copy(groupChatScope = null)
      else -> c
    }
    withContext(Dispatchers.Main) {
      ChatModel.chatsContext.addChat(Chat(remoteHostId = null, chatInfo = cInfo, chatItems = listOfNotNull(preview)))
    }
  }

  private fun openChat(event: API) {
    ChatModel.chatId.value = chatInfo(event).id
  }

  private suspend fun receive(event: API) = ChatController.processReceivedMsg(event)

  private fun items(event: API): List<AChatItem> = when (val r = event.result) {
    is CR.NewChatItems -> r.chatItems
    is CR.ChatItemsStatusesUpdated -> r.chatItems
    is CR.ChatItemsDeleted -> r.chatItemDeletions.map { it.deletedChatItem }
    is CR.ChatItemUpdated -> listOfNotNull(r.chatItemUpdate?.let { AChatItem(it.chatInfo, it.chatItem) })
    else -> error("not an item event: ${event.responseType}")
  }

  private fun chatInfo(event: API): ChatInfo = items(event).first().chatInfo

  private fun loadedTexts(): List<String> = ChatModel.chatsContext.chatItems.value.map { it.text }

  private fun previewText(): String? = ChatModel.chatsContext.chats.value.single().chatItems.lastOrNull()?.text

  private fun rowCounts(): Pair<Int, Int> {
    val stats = ChatModel.chatsContext.chats.value.single().unreadStats
    return stats.unreadCount to stats.unreadMentions
  }

  private fun counts(): Triple<Int, Int, Int> {
    val (unread, mentions) = rowCounts()
    return Triple(unread, mentions, ChatModel.activeUserUnreadCount)
  }

  private fun <T> assertEventually(expected: T, actual: () -> T) {
    val until = System.currentTimeMillis() + 5_000
    while (actual() != expected && System.currentTimeMillis() < until) Thread.sleep(10)
    assertEquals(expected, actual())
  }
}
