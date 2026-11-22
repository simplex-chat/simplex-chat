package chat.simplex.app

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.runComposeUiTest
import chat.simplex.common.model.*
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.chat.ComposePreview
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.ComposeView
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.*
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class LinkPreviewTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
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
    AlertManager.shared.hideAllAlerts()
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
  }

  @Test
  fun recordedPreviewIsReturned() {
    val (request, response) = recorded("result")
    val url = request.removePrefix(LINK_PREVIEW)
    val preview = runBlocking { ChatController.apiGetLinkPreview(url) }
    assertEquals(listOf(request), requests)
    assertEquals(url, preview?.uri)
    assertEquals(listOf("title", "description", "image").map { recordedPreview(response).text(it) }, listOf(preview?.title, preview?.description, preview?.image))
  }

  @Test
  fun typedLinkShowsPreviewWithoutAlert() = composeView { composeState ->
    val (request, response) = recorded("result")
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { composeState.value.linkPreview != null }
    onNodeWithText(recordedPreview(response).text("title")).assertExists()
    assertEquals(listOf(request), requests)
    assertFalse(AlertManager.shared.hasAlertsShown())
  }

  @Test
  fun failedLinkShowsNoPreview() = composeView { composeState ->
    val (request) = recorded("error")
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } && composeState.value.preview is ComposePreview.NoPreview }
    assertEquals(listOf(request), requests)
    assertFalse(AlertManager.shared.hasAlertsShown())
  }

  private fun composeView(test: ComposeUiTest.(MutableState<ComposeState>) -> Unit) = runComposeUiTest {
    chatParseMarkdown = { text -> if (text.isEmpty()) "{}" else linkMarkdown(text) }
    val composeState = mutableStateOf(ComposeState(useLinkPreviews = true))
    setContent {
      ComposeView(rhId = null, chatModel = ChatModel, ChatModel.chatsContext, Chat.sampleData, composeState, remember { mutableStateOf(false) }, remember { mutableStateOf(null) }, {}, remember { FocusRequester() })
      AlertManager.shared.showInView()
    }
    test(composeState)
  }

  private fun recorded(outcome: String): Pair<String, JsonObject> =
    commands.entries
      .single { (cmd, response) -> cmd.startsWith(LINK_PREVIEW) && outcome in response.jsonObject }
      .let { (cmd, response) -> cmd to response.jsonObject }

  private fun recordedPreview(response: JsonObject): JsonObject =
    response.getValue("result").jsonObject.getValue("linkPreview").jsonObject

  private fun JsonObject.text(key: String): String = getValue(key).jsonPrimitive.content

  private fun linkMarkdown(text: String): String =
    buildJsonObject {
      putJsonArray("formattedText") {
        addJsonObject {
          putJsonObject("format") { put("type", "uri") }
          put("text", text)
        }
      }
    }.toString()

  private companion object {
    const val LINK_PREVIEW = "/_link preview "
  }
}
