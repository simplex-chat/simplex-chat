package chat.simplex.app

import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.SaverScope
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
import chat.simplex.common.views.chat.group.GroupLinkView
import chat.simplex.common.views.chat.group.GroupMemberInfoView
import chat.simplex.common.views.chatlist.ChatListView
import chat.simplex.common.views.helpers.AlertManager
import chat.simplex.common.views.helpers.AnimatedViewState
import chat.simplex.common.views.helpers.LocalAppBarHandler
import chat.simplex.common.views.helpers.rememberAppBarHandler
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withContext
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import java.io.File
import java.net.URLClassLoader
import java.util.Locale
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class MemberRolesTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val parseUri = chatParseUri
  private val locale = Locale.getDefault()
  private val unrecorded = mutableListOf<String>()

  @BeforeTest
  fun setUp() {
    Locale.setDefault(Locale.US)
    ChatModel.currentUser.value = User.sampleData
    chatSendCmdRetry = { _, cmd, _ -> unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> unrecordedRequest("markdown $str") }
    chatParseUri = { str, _ -> unrecordedRequest("uri $str") }
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    ChatModel.chatsContext.chats.value.clear()
    ChatModel.chatId.value = null
    ChatModel.chatRunning.value = null
    ChatModel.currentUser.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    chatParseUri = parseUri
    Locale.setDefault(locale)
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun groupOfUnknownRoleIsListed() {
    recorded("members/unknown-roles")
    ChatModel.chatRunning.value = true
    loadChats()
    runComposeUiTest {
      setContent {
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
          ChatListView(ChatModel, MutableStateFlow(AnimatedViewState.GONE), {}, false)
        }
      }
      runCatching { waitUntil(timeoutMillis = 5_000) { "team" in shownTexts() } }
      assertContains(shownTexts(), "team")
      assertTrue(shownTexts().any { "changed your role to vip" in it }, shownTexts().toString())
      assertFalse("invalid data" in shownTexts())
    }
  }

  @Test
  fun memberOfUnknownRoleIsShown() {
    recorded("members/unknown-roles")
    loadChats()
    ChatModel.chatId.value = "#1"
    val groupInfo = (ChatModel.chatsContext.chats.value.first { it.id == "#1" }.chatInfo as ChatInfo.Group).groupInfo
    val member = runBlocking { ChatController.apiListMembers(null, 1) }.first { it.displayName == "cath" }
    runComposeUiTest {
      setContent {
        CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
          GroupMemberInfoView(
            null, groupInfo, member, mutableStateOf<Long?>(null), mutableStateOf<ConnectionStats?>(null), mutableStateOf<String?>(null), mutableStateOf(true),
            ChatModel, openedFromSupportChat = false, close = {}, closeAll = {}
          )
        }
      }
      runCatching { waitUntil(timeoutMillis = 5_000) { "vip" in shownTexts() } }
      assertContains(shownTexts(), "Role")
      assertContains(shownTexts(), "vip")
    }
  }

  @Test
  fun roleChangesToUnknownRolesAreShown() {
    recorded("members/unknown-roles")
    val (chat, _) = assertNotNull(runBlocking { ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(2)) })
    assertEquals("vip", (chat.chatInfo as ChatInfo.Group).groupInfo.membership.memberRole.memberRole)
    assertEquals(listOf("changed role of cath to vip", "changed your role to vip"), chat.chatItems.map { it.content.text })
  }

  @Test
  fun membersOfUnknownRoleAreListed() {
    recorded("members/unknown-roles")
    val members = runBlocking { ChatController.apiListMembers(null, 1) }
    assertEquals(listOf("alice" to "owner", "cath" to "vip"), members.map { it.displayName to it.memberRole.memberRole })
    assertEquals(listOf("owner", "vip"), members.map { it.memberRole.text(isChannel = false) })
    assertEquals(listOf("vip", "owner"), members.map { it.memberRole }.sorted().map { it.memberRole })
  }

  @Test
  fun groupLinkIsCreatedWithMemberRole() = groupLinkView(null) {
    assertRequested("/_create link #1 member")
  }

  @Test
  fun groupLinkRoleIsChanged() = groupLinkView(groupLink(GroupMemberRole.Member)) {
    onNodeWithText("member").performClick()
    onNodeWithText("observer").performClick()
    assertRequested("/_set link role #1 observer")
  }

  @Test
  fun groupLinkRoleIsRestored() {
    val link = groupLink(GroupMemberRole.Observer)
    val saved = assertNotNull(with(GroupLink.nullableStateSaver) { SaverScope { true }.save(link) })
    assertEquals(link, GroupLink.nullableStateSaver.restore(saved))
  }

  @Test
  fun groupLinkRoleIsShownInTerminal() {
    assertContains(groupLink(GroupMemberRole.Observer).toString(), "acceptMemberRole=observer")
  }

  @Test
  fun selectableRolesAfterRoleInitialized() {
    val classPath = System.getProperty("java.class.path").split(File.pathSeparator).map { File(it).toURI().toURL() }
    URLClassLoader(classPath.toTypedArray(), ClassLoader.getPlatformClassLoader()).use { loader ->
      val roles = loader.loadClass(SelectableRoles::class.java.name).getField("INSTANCE").get(null)
      assertEquals(listOf("owner", "observer", "member", "moderator", "admin", "owner"), roles.javaClass.getMethod("afterOwner").invoke(roles))
    }
  }

  object SelectableRoles {
    fun afterOwner(): List<String> = listOf(GroupMemberRole.Owner.memberRole) + GroupMemberRole.selectableRoles.map { it.memberRole }
  }

  private fun recorded(fixture: String) {
    val responses = Json.parseToJsonElement(File("../../../tests/fixtures/api/$fixture.json").readText()).jsonObject
    val commands = responses.getValue("commands").jsonObject
    val markdown = responses.getValue("markdown").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> commands[cmd]?.toString() ?: unrecordedRequest(cmd) }
    chatParseMarkdown = { str -> markdown[str]?.toString() ?: unrecordedRequest("markdown $str") }
  }

  private fun groupLinkView(link: GroupLink?, test: ComposeUiTest.() -> Unit) = runComposeUiTest {
    setContent {
      CompositionLocalProvider(LocalAppBarHandler provides rememberAppBarHandler()) {
        GroupLinkView(ChatModel, null, GroupInfo.sampleData, link, onGroupLinkUpdated = null)
      }
    }
    test()
  }

  private fun groupLink(role: GroupMemberRole) =
    GroupLink(1, CreatedConnLink("connFullLink", "connShortLink"), shortLinkDataSet = true, shortLinkLargeDataSet = false, groupLinkId = "groupLinkId", acceptMemberRole = role)

  private fun ComposeUiTest.assertRequested(request: String) {
    runCatching { waitUntil(timeoutMillis = 5_000) { synchronized(unrecorded) { unrecorded.isNotEmpty() } } }
    synchronized(unrecorded) {
      assertEquals(listOf(request), unrecorded.toList())
      unrecorded.clear()
    }
  }

  private fun loadChats() = runBlocking {
    withContext(Dispatchers.Main) { ChatModel.chatsContext.updateChats(ChatController.apiGetChats(null)) }
  }

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private fun ComposeUiTest.shownTexts(): List<String> =
    onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }
}
