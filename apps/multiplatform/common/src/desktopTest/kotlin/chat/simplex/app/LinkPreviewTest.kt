package chat.simplex.app

import androidx.compose.runtime.MutableState
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.ui.focus.FocusRequester
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.test.ComposeUiTest
import androidx.compose.ui.test.ExperimentalTestApi
import androidx.compose.ui.test.SemanticsMatcher
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performTextClearance
import androidx.compose.ui.test.performTextInput
import androidx.compose.ui.test.performTextInputSelection
import androidx.compose.ui.test.runComposeUiTest
import androidx.compose.ui.text.TextRange
import chat.simplex.common.model.*
import chat.simplex.common.model.ChatController.appPrefs
import chat.simplex.common.platform.chatParseMarkdown
import chat.simplex.common.platform.chatSendCmdRetry
import chat.simplex.common.views.chat.ComposePreview
import chat.simplex.common.views.chat.ComposeState
import chat.simplex.common.views.chat.ComposeView
import chat.simplex.common.views.chat.processPickedFile
import chat.simplex.common.views.helpers.AlertManager
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.*
import java.nio.file.Path
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import kotlin.test.*

@OptIn(ExperimentalTestApi::class)
class LinkPreviewTest {
  private val sendCmd = chatSendCmdRetry
  private val parseMarkdown = chatParseMarkdown
  private val testPrefs = mapOf(
    appPrefs.privacyLinkPreviews to true,
    appPrefs.privacySanitizeLinks to false,
    appPrefs.developerTools to false,
    appPrefs.showLinkPreviewErrors to false,
  )
  private val savedPrefs = testPrefs.keys.associateWith { it.get() }
  private val requests = mutableListOf<String>()
  private val unrecorded = mutableListOf<String>()
  private val recording = Json.parseToJsonElement(fixtureText("previews/link_preview")).jsonObject
  private val commands = recording.getValue("commands").jsonObject
  private val markdown = recording.getValue("markdown").jsonObject

  @BeforeTest
  fun setUp() {
    testPrefs.forEach { (pref, value) -> pref.set(value) }
    chatSendCmdRetry = { _, cmd, _ ->
      synchronized(requests) { requests += cmd }
      commands[cmd]?.toString() ?: unrecordedRequest(cmd)
    }
    chatParseMarkdown = { text -> if (text.isEmpty()) "{}" else markdown[text]?.toString() ?: unrecordedRequest("markdown $text") }
  }

  @AfterTest
  fun tearDown() {
    AlertManager.shared.hideAllAlerts()
    ChatModel.chatId.value = null
    ChatModel.draft.value = null
    ChatModel.draftChatId.value = null
    chatSendCmdRetry = sendCmd
    chatParseMarkdown = parseMarkdown
    savedPrefs.forEach { (pref, value) -> pref.set(value) }
    assertEquals(listOf(), unrecorded.toList(), "requests without recorded responses")
  }

  @Test
  fun recordedPreviewIsReturned() {
    val (request, response) = recorded("result")
    val url = request.removePrefix(LINK_PREVIEW)
    val preview = runBlocking { ChatController.apiGetLinkPreview(url) }
    assertEquals(listOf(request), requests)
    assertEquals(url, preview?.uri)
    assertNotEquals("", recordedPreview(response).text("description"))
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
    awaitFailedFetches(composeState, 1)
    assertEquals(listOf(request), requests)
    assertFalse(AlertManager.shared.hasAlertsShown())
  }

  @Test
  fun failedLinkIsNotFetchedAgain() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    field.performTextInputSelection(TextRange.Zero)
    runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.size > 1 } } }
    assertEquals(listOf(request), requests)
  }

  @Test
  fun failedLinkIsFetchedAgainAfterTheTextIsCleared() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    field.performTextClearance()
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 2)
    assertEquals(listOf(request, request), requests)
  }

  @Test
  fun failedLinkIsFetchedAgainInAnotherChat() = composeView { composeState ->
    val (request) = recorded("error")
    val field = onNode(hasSetTextAction())
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 1)
    ChatModel.chatId.value = "@2"
    waitForIdle()
    field.performTextInput(request.removePrefix(LINK_PREVIEW))
    awaitFailedFetches(composeState, 2)
    assertEquals(listOf(request, request), requests)
  }

  @Test
  fun fileAttachedBeforeTheFetchKeepsItsPreview() = composeView { composeState ->
    val (request) = recorded("result")
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    withTempDir { dir ->
      composeState.attachFile(dir)
      runCatching { waitUntil(timeoutMillis = 3_000) { synchronized(requests) { requests.isNotEmpty() } } }
      waitForIdle()
      assertTrue(composeState.value.preview is ComposePreview.FilePreview)
      assertEquals(listOf(), requests)
    }
  }

  @Test
  fun fileAttachedDuringTheFetchKeepsItsPreview() = composeView { composeState ->
    val (request) = recorded("result")
    val release = CountDownLatch(1)
    chatSendCmdRetry = { _, cmd, _ ->
      synchronized(requests) { requests += cmd }
      release.await(10, TimeUnit.SECONDS)
      commands[cmd]?.toString() ?: unrecordedRequest(cmd)
    }
    onNode(hasSetTextAction()).performTextInput(request.removePrefix(LINK_PREVIEW))
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.isNotEmpty() } }
    withTempDir { dir ->
      composeState.attachFile(dir)
      release.countDown()
      runCatching { waitUntil(timeoutMillis = 3_000) { composeState.value.preview is ComposePreview.CLinkPreview } }
      assertTrue(composeState.value.preview is ComposePreview.FilePreview)
      assertEquals(listOf(request), requests)
    }
  }

  @Test
  fun failureShowsNoAlert() = failedPreview(developerTools = false, showLinkPreviewErrors = false) {
    assertTexts()
  }

  @Test
  fun failureShowsNoAlertWithDeveloperOptionsOnly() = failedPreview(developerTools = true, showLinkPreviewErrors = false) {
    assertTexts()
  }

  @Test
  fun failureShowsNoAlertWithoutDeveloperOptions() = failedPreview(developerTools = false, showLinkPreviewErrors = true) {
    assertTexts()
  }

  @Test
  fun failureShowsCoreErrorWithBothOptions() = failedPreview(developerTools = true, showLinkPreviewErrors = true) { message ->
    assertTexts("Link preview error", message, "OK")
  }

  private fun failedPreview(developerTools: Boolean, showLinkPreviewErrors: Boolean, test: ComposeUiTest.(message: String) -> Unit) = runComposeUiTest {
    appPrefs.developerTools.set(developerTools)
    appPrefs.showLinkPreviewErrors.set(showLinkPreviewErrors)
    setContent { AlertManager.shared.showInView() }
    val (request, response) = recorded("error")
    assertNull(runBlocking { ChatController.apiGetLinkPreview(request.removePrefix(LINK_PREVIEW)) })
    assertEquals(listOf(request), requests)
    waitForIdle()
    test(recordedMessage(response))
  }

  private fun ComposeUiTest.awaitFailedFetches(composeState: MutableState<ComposeState>, count: Int) =
    waitUntil(timeoutMillis = 10_000) { synchronized(requests) { requests.size == count } && composeState.value.preview is ComposePreview.NoPreview }

  private fun ComposeUiTest.assertTexts(vararg texts: String) {
    val shown = onAllNodes(SemanticsMatcher.keyIsDefined(SemanticsProperties.Text), useUnmergedTree = true)
      .fetchSemanticsNodes()
      .flatMap { node -> node.config[SemanticsProperties.Text].map { it.text } }
    assertEquals(texts.toList(), shown)
  }

  private fun composeView(test: ComposeUiTest.(MutableState<ComposeState>) -> Unit) = runComposeUiTest {
    val composeState = mutableStateOf(ComposeState(useLinkPreviews = true))
    setContent {
      ComposeView(rhId = null, chatModel = ChatModel, ChatModel.chatsContext, Chat.sampleData, composeState, remember { mutableStateOf(false) }, remember { mutableStateOf(null) }, {}, remember { FocusRequester() })
      AlertManager.shared.showInView()
    }
    test(composeState)
  }

  private fun MutableState<ComposeState>.attachFile(dir: Path) =
    processPickedFile(dir.resolve("notes.txt").toFile().apply { writeText("notes") }.toURI(), null)

  private fun recorded(outcome: String): Pair<String, JsonObject> =
    commands.entries
      .first { (cmd, response) -> cmd.startsWith(LINK_PREVIEW) && outcome in response.jsonObject }
      .let { (cmd, response) -> cmd to response.jsonObject }

  private fun recordedPreview(response: JsonObject): JsonObject =
    response.getValue("result").jsonObject.getValue("linkPreview").jsonObject

  private fun recordedMessage(response: JsonObject): String {
    val errorType = response.getValue("error").jsonObject.getValue("errorType").jsonObject
    assertEquals("linkPreview", errorType.text("type"))
    return errorType.text("message")
  }

  private fun JsonObject.text(key: String): String = getValue(key).jsonPrimitive.content

  private fun unrecordedRequest(request: String): String {
    synchronized(unrecorded) { unrecorded += request }
    return "{}"
  }

  private companion object {
    const val LINK_PREVIEW = "/_link preview "
  }
}
