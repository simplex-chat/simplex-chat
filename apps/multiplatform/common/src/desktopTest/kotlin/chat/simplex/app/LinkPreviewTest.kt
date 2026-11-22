package chat.simplex.app

import chat.simplex.common.model.*
import chat.simplex.common.platform.chatSendCmdRetry
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.*
import kotlin.test.*

class LinkPreviewTest {
  private val sendCmd = chatSendCmdRetry
  private val requests = mutableListOf<String>()
  private val commands = Json.parseToJsonElement(fixtureText("previews/link_preview")).jsonObject.getValue("commands").jsonObject

  @BeforeTest
  fun setUp() {
    chatSendCmdRetry = { _, cmd, _ ->
      synchronized(requests) { requests += cmd }
      commands[cmd]?.toString() ?: "{}"
    }
  }

  @AfterTest
  fun tearDown() {
    chatSendCmdRetry = sendCmd
  }

  @Test
  fun recordedPreviewIsReturned() {
    val (request, response) = recorded("result")
    val url = request.removePrefix(LINK_PREVIEW)
    val preview = runBlocking { ChatController.apiGetLinkPreview(url) }
    assertEquals(listOf(request), requests)
    assertEquals(url, preview?.uri)
    assertEquals(recordedFields(response), listOf(preview?.title, preview?.description, preview?.image))
  }

  private fun recorded(outcome: String): Pair<String, JsonObject> =
    commands.entries
      .single { (cmd, response) -> cmd.startsWith(LINK_PREVIEW) && outcome in response.jsonObject }
      .let { (cmd, response) -> cmd to response.jsonObject }

  private fun recordedFields(response: JsonObject): List<String> {
    val preview = response.getValue("result").jsonObject.getValue("linkPreview").jsonObject
    return listOf("title", "description", "image").map { preview.getValue(it).jsonPrimitive.content }
  }

  private companion object {
    const val LINK_PREVIEW = "/_link preview "
  }
}
