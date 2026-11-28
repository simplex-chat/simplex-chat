package chat.simplex.app

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.platform.LocalUriHandler
import androidx.compose.ui.platform.UriHandler
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatParseUri
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.ui.theme.SimpleXTheme
import chat.simplex.common.views.chat.SendReceipts
import chat.simplex.common.views.chat.group.GroupChatInfoLayout
import chat.simplex.common.views.chat.item.openBrowserAlert
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.ModalData
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.io.File
import java.util.Locale
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class LinkAlertsTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val parseUri = chatParseUri
  private val locale = Locale.getDefault()
  private val unrecorded = mutableListOf<String>()
  private val opened = mutableListOf<String>()
  private val uriHandler = object : UriHandler {
    override fun openUri(uri: String) {
      opened += uri
    }
  }

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    ChatModel.currentUser.value = User.sampleData
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    ChatModel.currentUser.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    chatParseUri = parseUri
    Locale.setDefault(locale)
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun shortLinkShownInFull() = receivedLink(SHORT_LINK) {
    assertTexts("Open web link?", SHORT_LINK, "Cancel", "Open")
  }

  @Test
  fun longPathShortened() = receivedLink(LONG_PATH_LINK) {
    assertTexts("Open web link?", "${LONG_PATH_LINK.take(159)}…", "Cancel", "Open")
  }

  @Test
  fun longHostShownInFull() = receivedLink(LONG_HOST_LINK) {
    assertTexts("Open web link?", "https://$LONG_HOST…", "Cancel", "Open")
  }

  @Test
  fun longUserInfoShownInFull() = receivedLink(LONG_USER_INFO_LINK) {
    assertTexts("Open web link?", "https://$LONG_USER_INFO@example.com…", "Cancel", "Open")
  }

  @Test
  fun cleanLinkKeepsReceivedHost() = receivedLink(TRACKED_LINK) {
    assertTexts("Open web link?", TRACKED_LINK, "Open full link", "Open clean link", "Cancel")
    press("Open clean link")
    assertEquals(listOf("https://user%40name@example.com/page"), opened)
  }

  @Test
  fun otherSchemeNotOpened() = receivedLink(FTP_LINK) {
    assertTexts("Invalid link", "Unsupported URI scheme: ftp\n$FTP_LINK", "OK")
  }

  @Test
  fun channelWebPageShownInLinkAlert() = recorded("links/channel-web-page") {
    val chat = assertNotNull(runBlocking { ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(0)) }).first
    val modal = ModalData()
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides modal.appBarHandler, LocalUriHandler provides uriHandler) {
        SimpleXTheme {
          modal.GroupChatInfoLayout(
            chat = chat,
            groupInfo = (chat.chatInfo as ChatInfo.Group).groupInfo,
            currentUser = User.sampleData,
            sendReceipts = remember { mutableStateOf(SendReceipts.Yes) },
            setSendReceipts = {},
            chatItemTTL = remember { mutableStateOf(null) },
            setChatItemTTL = {},
            activeSortedMembers = listOf(),
            developerTools = false,
            onLocalAliasChanged = {},
            groupLink = null,
            groupLinkLoaded = true,
            selectedItems = remember { mutableStateOf(null) },
            appBar = remember { mutableStateOf(null) },
            scrollToItemId = remember { mutableStateOf(null) },
            addMembers = {},
            showMemberInfo = { _, _ -> },
            editGroupProfile = {},
            addOrEditWelcomeMessage = {},
            openMemberSupport = {},
            openPreferences = {},
            deleteGroup = {},
            deleteAndBan = {},
            clearChat = {},
            leaveGroup = {},
            manageGroupLink = {},
            manageWebPage = {},
            setSimplexName = {},
            onSearchClicked = {},
            deletingItems = remember { mutableStateOf(false) }
          )
        }
      }
      AlertManager.shared.showInView()
    }
    val shownBefore = shownTexts()
    press(WEB_PAGE)
    assertEquals(listOf(), opened, "opened without the link alert")
    assertTexts("Open web link?", WEB_PAGE, "Cancel", "Open", shownBefore = shownBefore)
  }

  private fun receivedLink(uri: String, test: ComposeUiTest.() -> Unit) = recorded("links/received-links") {
    setContent {
      AlertManager.shared.showInView()
    }
    openBrowserAlert(uri, uriHandler)
    test()
  }

  private fun recorded(fixture: String, test: ComposeUiTest.() -> Unit) = runComposeUiTest {
    val responses = Json.parseToJsonElement(File("../../../tests/fixtures/api/$fixture.json").readText()).jsonObject
    val commands = responses.getValue("commands").jsonObject
    val markdown = responses.getValue("markdown").jsonObject
    val uris = responses.getValue("uris").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands[cmd]?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown[str]?.toString() ?: unrecordedRequest("markdown $str") }
    chatParseUri = { str, safe -> uris[str]?.takeIf { safe == 0 }?.toString() ?: unrecordedRequest("uri $str") }
    test()
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private fun ComposeUiTest.press(text: String) {
    onNodeWithText(text).performClick()
    waitForIdle()
  }

  private fun ComposeUiTest.assertTexts(vararg texts: String, shownBefore: List<String> = listOf()) {
    runCatching { waitUntil(timeoutMillis = 5_000) { shownTexts(shownBefore) == texts.toList() } }
    assertEquals(texts.toList(), shownTexts(shownBefore))
  }

  private fun ComposeUiTest.shownTexts(shownBefore: List<String> = listOf()): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }
      .toMutableList()
      .apply { shownBefore.forEach { remove(it) } }

  private companion object {
    const val SHORT_LINK = "https://example.com/page?q=1"
    val LONG_PATH_LINK = "https://example.com/" + "b".repeat(200)
    val LONG_HOST = List(3) { "a".repeat(60) }.joinToString(".") + ".example.com"
    val LONG_HOST_LINK = "https://$LONG_HOST/" + "b".repeat(200)
    val LONG_USER_INFO = "a".repeat(180)
    val LONG_USER_INFO_LINK = "https://$LONG_USER_INFO@example.com?q=" + "b".repeat(200)
    const val TRACKED_LINK = "https://user%40name@example.com/page?ref=123"
    const val FTP_LINK = "ftp://example.com/file"
    const val WEB_PAGE = "https://example.com/news"
  }
}
