package chat.simplex.app

import androidx.compose.runtime.mutableStateOf
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.NotificationAction
import chat.simplex.common.platform.NtfManager
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatParseUri
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.platform.ntfManager
import chat.simplex.common.views.call.RcvCallInvitation
import chat.simplex.common.views.chat.item.FramedItemView
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.io.File
import java.util.Locale
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class ChatItemsTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val parseUri = chatParseUri
  private val locale = Locale.getDefault()
  private val acceptImages = appPrefs.privacyAcceptImages.get()
  private val unrecorded = mutableListOf<String>()

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    appPrefs.privacyAcceptImages.set(true)
    ChatModel.currentUser.value = User.sampleData
    chatSendCmdRetry = { _, cmd, _ -> unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> unrecordedRequest("markdown $str") }
    chatParseUri = { str, _ -> unrecordedRequest("uri $str") }
  }

  @AfterTest
  fun tearDown() {
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.currentUser.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    chatParseUri = parseUri
    appPrefs.privacyAcceptImages.set(acceptImages)
    Locale.setDefault(locale)
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun filesOfBlockedMembersAreNotReceived() {
    ntfManager = silentNtfManager
    val recorded = recordedChat("files/images-of-blocked-members", "/_get chat #1 count=4")
    val event = CR.NewChatItems(recorded.user, recorded.chat.chatItems.map { AChatItem(recorded.chat.chatInfo, it) })
    runBlocking {
      ChatController.processReceivedMsg(API.Result(null, event))
      withTimeout(5_000) { while ("/freceive 3" !in receivedFiles()) delay(50) }
    }
    assertEquals(listOf("/freceive 3"), receivedFiles())
    synchronized(unrecorded) { unrecorded.clear() }
  }

  @Test
  fun receivedChannelCardIsNotFromOwner() = channelCard("chat-links/channel-card-received", "Channel link")

  @Test
  fun sentChannelCardIsFromOwner() = channelCard("chat-links/channel-card-sent", "Channel link (from owner)")

  private fun channelCard(fixture: String, infoLine: String) = runComposeUiTest {
    val chat = recordedChat(fixture, "/_get chat @2 count=1").chat
    val card = chat.chatItems.last()
    assertNotNull((card.content.msgContent as MsgContent.MCChat).ownerSig)
    setContent {
      FramedItemView(
        ChatModel.ChatsContext(secondaryContextFilter = null), chat, card,
        linkMode = SimplexLinkMode.DESCRIPTION, showViaProxy = false, showMenu = mutableStateOf(false), showTimestamp = true,
        receiveFile = {}, scrollToItemId = mutableStateOf(null)
      )
    }
    assertEquals(listOf(infoLine), shownTexts().filter { it.startsWith("Channel link") })
  }

  private fun recordedChat(fixture: String, command: String): CR.ApiChat {
    val responses = Json.parseToJsonElement(File("../../../tests/fixtures/api/$fixture.json").readText()).jsonObject
    val response = responses.getValue("commands").jsonObject.getValue(command)
    return (json.decodeFromString<API>(response.toString()) as API.Result).res as CR.ApiChat
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private fun receivedFiles(): List<String> = synchronized(unrecorded) { unrecorded.map { it.split(" ").take(2).joinToString(" ") } }

  private fun ComposeUiTest.shownTexts(): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }

  private val silentNtfManager = object : NtfManager() {
    override fun notifyCallInvitation(invitation: RcvCallInvitation): Boolean = false
    override fun hasNotificationsForChat(chatId: String): Boolean = false
    override fun cancelNotificationsForChat(chatId: String) {}
    override fun cancelNotificationsForUser(userId: Long) {}
    override fun displayNotification(user: UserLike, chatId: String, displayName: String, msgText: String, image: String?, actions: List<Pair<NotificationAction, () -> Unit>>) {}
    override fun cancelCallNotification() {}
    override fun cancelAllNotifications() {}
    override fun showMessage(title: String, text: String) {}
    override fun androidCreateNtfChannelsMaybeShowAlert() {}
  }
}
