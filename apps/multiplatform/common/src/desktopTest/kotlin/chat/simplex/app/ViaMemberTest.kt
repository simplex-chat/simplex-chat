package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatSendCmdRetry
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlin.test.*

class ViaMemberTest {
  private val sendCmd = chatSendCmdRetry

  @BeforeTest
  fun setUp() {
    val responses = Json.parseToJsonElement(fixtureText("via/members")).jsonObject.getValue("commands").jsonObject
    chatSendCmdRetry = { _, cmd, _ -> responses.getValue(cmd).toString() }
  }

  @AfterTest
  fun tearDown() {
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun memberViaUserIsDecoded() = runBlocking {
    val reviewer = ChatController.apiListMembers(null, 1).single { it.memberProfile.displayName == "reviewer" }
    assertEquals("memberVia", reviewer.memberVia)
  }

  @Test
  fun itemSentAsMemberIsDecoded() = runBlocking {
    val (chat, _) = assertNotNull(ChatController.apiGetChat(null, ChatType.Group, 1, null, pagination = ChatPagination.Last(1)))
    assertEquals("reviewer", chat.chatItems.single().meta.sentAsMember?.memberProfile?.displayName)
  }
}
